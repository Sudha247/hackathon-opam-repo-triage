# opam-repository CI triage

Tools to triage the failures reported by opam-repo-ci
(<https://opam.ci.ocaml.org>) on ocaml/opam-repository pull requests, and to
suggest fixes such as dependency bounds.

- `fetch_ci_logs.py`: download the failed job logs of a CI run into `ci-logs/<sha>/`.
- `classify_logs.py`: classify and group those logs with rules, marking the groups that need an LLM.
- `opam_repo_ci_triage.py`: parse a CI run, compare it with a baseline, and suggest fixes (see below).

## opam_repo_ci_triage.py

Works only from results CI already computed: nothing is built, no Docker, and
no GitHub token is needed. Python 3 standard library only; `git` is used only
as a fallback (see "Data sources").

Each command takes a commit sha, an opam.ci.ocaml.org URL, or `--pr N` (the
PR's current head).

| Command | What it does |
|---|---|
| `summary` | One record per CI job: status, category (build, build-or-test, dependency, network, cancelled, solver, ignored, not-available, lint), job kind, platform; reverse-dependency failures grouped by package. |
| `logs` | For failed jobs: the first `#=== ERROR while …` block, solver output, the final error, the builder (classic or day10) and the master commit the PR was merged onto. |
| `compare` | Classifies failing reverse dependencies against the CI run of the PR that published the previous version of the package: NEW, pre-existing, builder-specific, no-baseline. |
| `suggest` | Proposes fixes, with the evidence they are based on (see below). Fixes to the PR come first, then workarounds (platform restrictions), problems in other packages or CI, and explanations without a change. |
| `review` | The same as a GitHub pull-request review: each fix that can be placed on a line of the PR's diff becomes a line comment with a "suggested change" block (one click to apply); the rest goes in the review text. Needs `--pr N` and CI results for the PR's current head. Printed as JSON by default; `--post` posts it with `gh api` (review it first). |
| `comment` | A draft PR comment in Markdown from the `suggest` results: diffs for the PR's opam files with their evidence, the rest folded in `<details>`. Printed by default; `--post` posts it with `gh pr comment` (review it first). |

```sh
python3 opam_repo_ci_triage.py summary --pr 31275
python3 opam_repo_ci_triage.py logs 1e2d45eb8b297fea701407d41e2d2af656bd5d9a --match 'revdeps,osc'
python3 opam_repo_ci_triage.py compare --pr 31275
python3 opam_repo_ci_triage.py suggest 55ad97d637d37d80b5d186bd7b3850ad263d05f1
```

Common options:

- `--json`: machine-readable output.
- `--match REGEX`: only jobs whose variant matches, e.g. `'revdeps,passage'`,
  `'lower-bounds'`, `'^compilers,'` (classic builder only), `',5\.5,'` (OCaml 5.5).
  A variant is the last part of a job URL, e.g.
  `compilers,4.14,lwt.5.10.2,revdeps,osc.0.1.4` or
  `day10,debian-13,x86_64,5.5,lwt.5.10.2,tests`.
- `--max N`: analyse at most N distinct failures (default 30).
- `--cache-dir DIR`: cache location (default `.opam-ci-cache/`).

### suggest

| Failure | Suggestion | Confidence |
|---|---|---|
| Unbound value / module / constructor / record field / type from a library | `>=` the version that introduced it, or `<` the version that removed it, found from per-version API docs; `{= version}` when both packages come from the same repository | high |
| Same, from the OCaml stdlib | `"ocaml" {>= "X"}` / `{< "X"}` from the stdlib `.mli` of each release | high |
| `string` used where `bytes` is expected | `"ocaml" {< "4.06"}` (safe-string) | high |
| Test output diff (cram/mdx) | `"dep" {with-test & < "X"}` for dependencies whose version differs from a passing job, or that were released after the failing package | low (candidates) |
| `Version X of the dune language is not supported` | `"dune" {>= "X"}` | high |
| Missing tool (`Program awk not found`, `"gcc": command not found`) | `"conf-tool" {build}`, or a depext gap in the conf package / the failing dependency (Cygwin package name when known, e.g. `gawk`) | medium |
| Missing library (`Library "x" not found`, ocamlfind `Package 'x' not found`) | add the dependency (`{with-test}` under `test/`), or "internal library, not shipped" | medium |
| Missing C header | which platform lacks it; add a depext or `x-ci-accept-failures` | low |
| Job matrix: fails on every older (or newer) OCaml version, passes on the others | `"ocaml" {>= "X"}` / `{< "X"}`, `{with-test & …}` if only tests fail | medium |
| Job matrix: fails only on Windows / one arch / some distributions | `"@runtest" {with-test & os != "win32"}`, `available: arch != "s390x"`, `x-ci-accept-failures: [...]`; aimed at the failing dependency when it is the one failing | medium |
| Fails only on the day10 builder while the classic builder passes on the same distribution and OCaml version | reported as a CI-environment problem, with the evidence (e.g. a conf package that declares the depext the image didn't install) | high |
| The PR removes or relaxes the `available` filter of an existing version, and its packages now fail on platforms the old filter excluded | restore the old filter, or narrow it to the platforms that build (read from the PR's diff: one GitHub call) | high/medium |
| A conf package fails on a platform listed in its `x-ci-accept-failures` | CI should have ignored it: CI-environment problem | high |
| Lint error already reported in the previous release's CI | noted, with the PR number | high |
| A PR package installs on no platform | the constraint every solver conflict goes through (e.g. `js_of_ocaml-compiler < 5.0.0`) | high |
| Lint warning 62 / 41, known false positives | SPDX id detected from the LICENSE text, `%{_:VAR}%`, or "known waiver" | high |
| Lint warning 68 (missing `license`) | the license of another version of the package in opam-repository, or detected from the upstream LICENSE file | high |
| Platform or compiler failure on an old release | alternative: `x-maintained: false` when a newer release exists (lets the next archiving round remove it) | medium |
| Lint warning 47 / 59, weak checksums, `extra-files`, error 46 / 60 | fixed synopsis line, `checksum: ["sha256=…"]` with the command to compute it, `extra-source` template, `conf` flag advice, archive changed upstream | high |
| Install failure: files not built | the earlier build error that did not stop the build, then the usual rules on it | low–high |
| `These packages are still missing: …` (system packages) | the distribution lacking them; correct the depext or `x-ci-accept-failures` | medium |

The matrix rules use only the job list (no fetch); the others reuse the log
already downloaded, plus at most one small lookup.

Example (the earlier, failing commit of PR 30908):

```
error           : Unbound value Ortools.Sat.proto_solve
looked up in    : ocaml.org docs
evidence        : ortools 9.15.0-1=no, ortools 9.15.0-2=yes, ortools 9.15.0 (installed in failing job)=no
suggestion (library-api, high):
  - "ortools"	{>= "9.15"}
  + "ortools" {>= "9.15.0-2"}
  or "ortools" {= version}  (same upstream repository and version)
```

### LLM fallback (optional)

`--llm` (on `suggest` and `comment`) asks Claude about the failures no rule
explains: one call per distinct failure, at most `--llm-max` (default 5).
The prompt holds the error excerpt, the failing package's opam file, the
dependency versions CI installed and the package's results on the other CI
jobs; the answer is structured JSON (diagnosis, where the fix goes, the opam
change if any, confidence, what to verify), shown with `rule: llm`.

```sh
pip install anthropic          # and ANTHROPIC_API_KEY, or `ant auth login`
python3 opam_repo_ci_triage.py suggest --pr 29613 --llm
python3 opam_repo_ci_triage.py suggest --pr 29613 --llm-dry-run   # print the prompts, no API call
```

Model `claude-opus-5-5` (`--llm-model`), medium effort, server-side refusal
fallbacks enabled. Responses are cached in `.opam-ci-cache/`, so re-runs cost
nothing. The log and opam file are passed as untrusted data.

## Scheduled page

`.github/workflows/triage-page.yml` runs every hour (and on demand) and
publishes the results on the repository's GitHub Pages site:

1. `scripts/triage_open_prs.py` lists the open ocaml/opam-repository PRs with
   one GraphQL query (needs a token; the workflow's `GITHUB_TOKEN` is enough),
   keeps those whose `opam-ci` status is `failure`, and runs `suggest <sha>
   --json --max 6` (and `summary`, from the cached page) on each new or updated
   one, sequentially. PRs whose head sha is unchanged reuse their result from
   `state/prs.json`; closed PRs, and PRs no longer failing, are dropped. It
   writes `site/data.json`. `--time-budget` stops starting new PRs; the next
   run continues.
2. `scripts/render_page.py` turns it into `site/index.html`: one static page,
   no external resources, with filters (fix for the PR, blocked by other
   packages, CI only), the suggestions of each PR by class, the `review`
   command to run by hand, and the reverse dependencies that fail on several
   PRs. PR titles, logs and suggestions are HTML-escaped.

`.opam-ci-cache/` and `state/` are kept between runs with `actions/cache`
(files older than 30 days are pruned). Rules only: the LLM fallback is not
enabled (see the comment in the workflow).

```sh
python3 scripts/triage_open_prs.py --limit 4   # locally: needs GITHUB_TOKEN, GH_TOKEN or gh
python3 scripts/render_page.py                 # site/index.html
```

To enable it:

1. Push the workflow to the default branch (scheduled workflows run only from there).
2. Settings → Pages → Build and deployment → Source: **GitHub Actions**.
3. Actions → "Triage page" → Run workflow, once. The page is then at
   `https://<owner>.github.io/<repository>/`, refreshed every hour.

## Tests

`tests/test_triage.py` runs `suggest` on 15 recorded CI runs (ortools,
colibri2, why3find, hol_light, binaryen, oonnxx, chatoyant, lwt revdeps,
graphql-async, ocaml-r, git-paf, websocket, lambdapi, dns-certify, unikraft) and checks
the fixes verified by hand or made by the PR authors. They run offline
(`OPAM_CI_OFFLINE=1`): pages, logs, opam files, doc lookups and git objects come
from `tests/fixtures/<case>/` (2.2 MB), and any cache miss fails the test.

```sh
python3 tests/test_triage.py              # ~4 s, no network
python3 tests/test_triage.py -k why3find  # one case
python3 tests/test_triage.py --record     # re-record the fixtures (online)
```

To add a case: put its commit and expected suggestions in `CASES`, then
`--record <case>`.

## Evaluation on merged PRs

`evaluate_closed_prs.py` runs `suggest` on the initial version (before the
first force-push, or the first commit) of recently merged PRs whose opam-ci
failed, and compares the suggestions with what the author changed before the
merge. Needs a logged-in `gh`.

```sh
python3 evaluate_closed_prs.py --since 2026-08-01 --limit 200
```

On 200 PRs merged since 2026-08-01 (16 evaluable, rules only, no LLM): 13 of
17 suggested changes matched the author's fix (bound suggestions exactly in 3
of 4 cases), and 13 of the 41 changes authors made in failing packages were
anticipated.

## Data sources, cheapest first

1. The CI commit page and job logs (opam.ci.ocaml.org).
2. The ocaml.org MCP endpoint (`https://ocaml.org/mcp`, JSON-RPC, no session, no token): module lists and plain-text signatures for every documented package version.
3. opam.ocaml.org pages: the versions of a package, and the release date of a version.
4. Raw files: opam files of opam-repository, `stdlib/*.mli` of each OCaml release.
5. A shallow, blob-less `git fetch` of one upstream tag, only when ocaml.org has no docs for a version.
6. The GitHub REST API, only for `--pr` and for `compare`'s baseline lookup (2–3 calls). It works without a token (60 requests per hour); `GITHUB_TOKEN`, `GH_TOKEN` or a logged-in `gh` are used if present.

Results are cached in `.opam-ci-cache/`. `suggest` prints the number of
network requests it actually sent.

Limitations:

- opam-repo-ci deletes old runs: the CI page of a PR from many months ago returns 404, so `compare` has no baseline for it.
- The version lookups assume upstream tags named after the opam version (`1.2.3`, `v1.2.3`, `name-1.2.3`, …).
