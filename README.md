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

The code is in the `triage/` package, one module per layer, each using only
the ones before it: `net` (HTTP, GitHub API, git, caches), `ci` (CI pages and
logs, job names), `sources` (opam files, versions, ocaml.org docs, upstream
sources), `rules` (one job's log), `matrix` (pass/fail patterns), `context`
(PR-level checks: pre-existing failures, opam-health-check, lint, reruns),
`llm`, `suggest` (runs and merges everything), `render` (text, comment,
review) and `cli`. `opam_repo_ci_triage.py` is the entry point, and re-exports
the package for the scripts and tests.

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

Each suggestion in the `--json` output has machine fields (`rule`, `variant`, `fix_in`, `jobs`) and fields
for people: `title` (the rule in words), `job` (the CI job in words, e.g. "frama-clang.0.0.21~beta on
archlinux x86_64, OCaml 5.5 (day10 builder)") and `where` (where the fix goes). Failures that no rule
recognises are grouped in one "Not recognised" entry listing their jobs.

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
| mdx/toplevel test output differs (types, values, error locations) | the expected output depends on the OCaml version: `"ocaml" {< "X" \| !with-test}` (keeps the package installable), or `"@runtest" {with-test & ocaml:version < "X"}` | medium |
| C/C++ error introduced by a newer compiler: `-Wimplicit-int` and friends (GCC 14), `uint64_t` without `<cstdint>` (GCC 13), `bool` as a keyword (GCC 15, C23), `-Werror=…` | the cause and the upstream fix; `x-maintained: false` for an old release | high |
| System library at the wrong version (`pkg-config --atleast-version`, `Requested 'x >= V'`, conf-libclang's LLVM) | the required and the installed version per distribution (read from the depext installation); exclude those distributions | high |
| Network errors, cancelled jobs, CI tool crash (`Segmentation fault day10-install`) | one entry listing the jobs to rebuild | high |
| Source archive that cannot be unpacked (symlinks on msys2) | replace the symlinks upstream, or exclude the platform | high |
| A day10 failure on a platform the failing package already lists in `x-ci-accept-failures` | nothing to change: day10 does not honour it there (cygwin, freebsd) | high |
| Lint "conf packages should always use …" on npm depexts (`npm-version`) | known false positive for Melange bindings | high |
| Known messages: `no such function: unixepoch` (SQLite < 3.38), `Could NOT find Python3`, `fatal: not a git repository` from `git describe`, an install into `/usr/...` (read-only), `No known cycle-counting instruction`, Core_kernel shadowing Unix, `Unbound value Conv.…` with cmdliner ≥ 2, `checking for ar... no` on Windows, Clang 19's removed APIs, OCaml 5.4's labelled tuples in Parsetree patterns, `configure: error: x.pc not found` | the cause and the change: a bound, a dependency, `available`, or the install variables | high/medium |
| Lower-bounds job: a dependency downgraded to a version that no longer builds | `>=` the version used in the other jobs, on the direct dependency that pulls it in (found through opam's "required by") | medium |
| Lower-bounds job: the package itself fails and no other rule applies | its downgraded direct dependencies as candidates (passing and failing versions), those named in the error first | low |
| A new release fails with a dependency version that the previous release excludes on master (a bound maintainers added after a breakage, missing from the new opam file) | the same bound, citing the previous release; skipped when the new release changed that constraint on purpose | medium |
| A reverse dependency builds on master (opam-health-check) but fails with the new release | the usual fix in the same PR: `"pkg" {< "new version"}` in its depends, or a `conflicts` entry when it does not depend on it directly; not for a PR that modifies an existing version, nor for a new conf package | medium |
| A package recompiled at the same version, removed, then failing to build or install (it has a `remove:` field; seen on lower-bounds jobs with opam 2.6) | an opam bug (ocaml/opam#6440 moves the same source directory for both steps), not a bound: the job can be ignored | high |

A suggestion that restricts platforms (`x-ci-accept-failures`, or `available`
on Windows) says which other versions of the package (up to 3) already accept
failures on those platforms, and then proposes `x-ci-accept-failures` as the
alternative to restricting `available`.

Every suggested change is checked before it is shown: a constraint no version
satisfies, a bound that still allows the version the failing job installed, or
(when opam is installed) a change on which `opam lint` reports a new problem is
moved to the note as text to check, at low confidence.

Failures that would happen without the PR are marked "Pre-existing" in the note:

- packages the PR only modifies by adding an upper bound that excludes a version it adds (their dependency solution is the same as on master; read from the PR's diff, the same GitHub call as above);
- jobs that also failed in the CI run of the PR that published the previous release of the package (same platform, same job; 2 GitHub calls and one page per package, skipped when the old run has been purged);
- reverse dependencies that opam-health-check also sees failing on master (from ocaml.org's MCP `ocaml_package_build_information`; same version and OCaml version, build only, as opam-health-check builds the newest version of each package without tests; one request per package, cached for a day).

Reverse dependencies that opam-health-check sees *building* on master are listed in a separate entry, "Builds on master, fails here": those are the failures most likely caused by the PR. On the triage page, a suggestion about a reverse dependency that also fails in 3 or more other open PRs says so, with links.

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

Every Monday, `.github/workflows/llm-digest.yml` groups the LLM answers shown
on the triage page by the error line of their log (`scripts/llm_digest.py`;
paths, numbers and identifiers masked) and keeps one issue, "LLM diagnoses to
turn into rules", up to date with the errors answered in two or more PRs: the
diagnoses, suggested changes and logs of each. Turning one into a rule (most
go in `known_errors` in `triage/rules.py`, with a case in
`tests/test_triage.py`) takes it out of the LLM's hands: free, deterministic
and tested. It does nothing while the page has no LLM answers.

To evaluate it on the PRs of the triage page that have failures no rule recognises, run
`.venv/bin/python scripts/llm_eval.py --first 5` (then without `--first`): it saves each PR's
result in `llm-eval/` and prints the tokens used; `--dry-run` lists the PRs and prompts without
calling the API.

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

   The page also publishes an Atom feed, `feed.xml`, with one entry per PR,
   whose id changes with the PR's head commit: a feed reader shows each new
   analysis without visiting the page.

   When a PR gets a new commit, the page says what changed since the
   previous one: the problems gone, the new ones, and how many remain.

   PRs whose CI is still running are included once some job has failed,
   with a "CI running" badge, and analysed again on every run until their CI
   finishes. The tool never caches the CI page of a commit while jobs are
   still running (it fetches it again next time), and says when the results
   are partial.

   With an `ANTHROPIC_API_KEY` repository secret, failures that no rule
   recognises also get an LLM diagnosis (`suggest --llm --llm-max 2`, at most
   `--llm-budget 40` calls per run; answers are cached, so only new failures
   cost anything). Without the secret, or if the API fails, the page is built
   from the rules alone. On the page, LLM answers are labelled "unchecked",
   version bounds and depexts they propose are shown as text to check, and
   low-confidence answers are folded: in an evaluation on 25 answers, 15 were
   right, 7 plausible, 1 wrong and 2 harmful, all the harmful ones being
   version bounds.
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

A weekly GitHub Action (`.github/workflows/accuracy.yml`, Mondays, or by
hand) runs the evaluation on the PRs merged in the last 90 days and compares
it with the previous run (`scripts/accuracy_report.py`). The totals move with
the sample; the check that matters is on the PRs evaluated both times: a
suggestion that matched the author's fix and no longer does is a regression
of the tool. The job then fails (so GitHub notifies), as it does when
precision drops by 15 points or more. The history (`history.jsonl`, and the
latest evaluation) is kept on the `accuracy` branch; the report is in the
job's summary.

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
