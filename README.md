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
| `summary` | One record per CI job: status, category (build, build-or-test, dependency, network, cancelled, solver, ignored, not-available, lint, analysis), job kind, platform; reverse-dependency failures grouped by package. |
| `logs` | For failed jobs: the first `#=== ERROR while …` block, solver output, the final error, the builder (main or day10) and the master commit the PR was merged onto. |
| `compare` | Classifies failing reverse dependencies against the CI run of the PR that published the previous version of the package: NEW, pre-existing, builder-specific, no-baseline. |
| `suggest` | Proposes fixes, with the evidence they are based on (see below). Fixes to the PR come first, then workarounds (changes that skip platforms or tests), failures whose fix goes elsewhere (another package, upstream, the CI), and other failures without a change. |
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
  `'lower-bounds'`, `'^compilers,'` (main builder only), `',5\.5,'` (OCaml 5.5).
  A variant is the last part of a job URL, e.g.
  `compilers,4.14,lwt.5.10.2,revdeps,osc.0.1.4` or
  `day10,debian-13,x86_64,5.5,lwt.5.10.2,tests`.
- `--max N`: analyse at most N distinct failures (default 30): those of the
  PR's own packages first, then those of their reverse dependencies, and
  experimental platforms last. `suggest --json` gives the number of the others
  that no suggestion explains in `unanalysed`; the PR comment and the triage
  page say how many, and such a PR is not listed as seeming ready.
- `--cache-dir DIR`: cache location (default `.opam-ci-cache/`).
- `--refresh`: fetch the commit's CI page again and drop the cached logs of
  its jobs (after a rebuild: a finished page and the logs are otherwise cached
  for good).

### suggest

Each suggestion in the `--json` output has machine fields (`rule`, `variant`, `fix_in`, `jobs`, and
`where_kind`: where the fix goes, one of `pr`, `other-package`, `upstream`, `ci`, `rerun`, `none` or
`unknown`, with `where_pkg` naming the other package when known) and fields for people: `title` (the rule
in words), `job` (the CI job in words, e.g. "frama-clang.0.0.21~beta on archlinux x86_64, OCaml 5.5 (day10
builder)") and `where` (where the fix goes: "this PR", "pkg (another package)", "upstream", "the CI",
"rerun" or "nothing to fix"; absent when unknown). The class of a suggestion, the page's verdicts and
groups follow `where_kind`; `fix_in` is the same place in words (results stored without `where_kind` get it
from `fix_in`, in `triage/where.py`). Failures that no rule recognises are grouped in one "Unexplained
failures" entry listing their jobs.

| Failure | Suggestion | Confidence |
|---|---|---|
| Unbound value / module / constructor / record field / type from a library | `>=` the version that introduced it, or `<` the version that removed it, found from per-version API docs; `{= version}` when both packages come from the same repository | high |
| Same, from the OCaml stdlib | `"ocaml" {>= "X"}` / `{< "X"}` from the stdlib `.mli` of each release | high |
| `string` used where `bytes` is expected | `"ocaml" {< "4.06"}` (safe-string) | high |
| Test output diff (cram/mdx) | `"dep" {with-test & < "X"}` for dependencies whose version differs from a passing job, or that were released after the failing package | low (possible causes) |
| `Version X of the dune language is not supported` | `"dune" {>= "X"}` | high |
| Missing tool (`Program awk not found`, `"gcc": command not found`) | `"conf-tool" {build}`, or a depext gap in the conf package / the failing dependency (Cygwin package name when known, e.g. `gawk`) | medium |
| Missing library (`Library "x" not found`, ocamlfind `Package 'x' not found`) | add the dependency (`{with-test}` under `test/`), or "internal library, not shipped" | medium |
| Missing C header | which platform lacks it; add a depext or `x-ci-accept-failures` | low |
| Job matrix: fails on every older (or newer) OCaml version, passes on the others | `"ocaml" {>= "X"}` / `{< "X"}`, `{with-test & …}` if only tests fail | medium |
| Job matrix: fails only on Windows / one arch / some distributions | `"@runtest" {with-test & os != "win32"}`, `available: arch != "s390x"`, `x-ci-accept-failures: [...]`; aimed at the failing dependency when it is the one failing | medium |
| Fails only on the day10 builder while the main builder passes on the same distribution, architecture and OCaml version (and day10 does not pass on another architecture there) | reported as a CI-environment problem, with the evidence (e.g. a conf package that declares the depext the image didn't install) | high |
| The PR removes or relaxes the `available` filter of an existing version, and that version (or its reverse dependencies) now fails on platforms the old filter excluded and the new one allows (filters on `os`, `arch`, `os-distribution` and `os-family` are evaluated; medium confidence when another variable decides) | restore the old filter, or narrow it to the platforms that build (read from the PR's diff: one GitHub call) | high/medium |
| A conf package fails on a platform listed in its `x-ci-accept-failures` | CI should have ignored it: CI-environment problem | high |
| Lint error already reported in the previous release's CI | noted, with the PR number | high |
| A PR package installs on no platform | the constraint every solver conflict goes through (e.g. `js_of_ocaml-compiler < 5.0.0`); nothing to fix when every conflict goes through an upper bound the PR adds to (or lowers in) `depends`, unless no version satisfies that bound (`{>= "3.0" & < "2.0"}`, or no published version: `"dune" {< "0.3"}`, a mistake in the PR; when ocaml.org gives no versions, the bound is not assumed intended either) | high |
| The analysis job fails (no build job runs) | its reason, from the CI page or the end of its log; fix in this PR, or rerun when the CI cancelled it | medium |
| The lint job fails with only `Warning in pkg.ver: …` | a finding of this PR (opam-ci-check fails the job on warnings too); a lint job failing with neither an error nor a warning is a CI error to rerun | high |
| Lint warning 62 / 41, known false positives | SPDX id detected from the LICENSE text, `%{_:VAR}%`, or "known waiver" | high |
| Lint warning 68 (missing `license`) | the license of another version of the package in opam-repository, or detected from the upstream LICENSE file | high |
| Platform or compiler failure on an old release | alternative: `x-maintained: false` when a newer release exists (lets the next archiving round remove it) | medium |
| Lint warning 47 / 59, weak checksums, `extra-files`, error 46 / 60 | fixed synopsis line, `checksum: ["sha256=…"]` with the command to compute it, `extra-source` template, `conf` flag advice, archive changed upstream | high |
| Install failure: files not built | the earlier build error that did not stop the build, then the usual rules on it | low–high |
| `These packages are still missing: …` (system packages) | the distribution lacking them; correct the depext or `x-ci-accept-failures` | medium |
| mdx/toplevel test output differs (types, values, error locations) | the expected output depends on the OCaml version: `"ocaml" {< "X" \| !with-test}` (keeps the package installable), or `"@runtest" {with-test & ocaml:version < "X"}` | medium |
| C/C++ error introduced by a newer compiler: `-Wimplicit-int` and friends (GCC 14), `uint64_t` without `<cstdint>` (GCC 13), `bool` as a keyword (GCC 15, C23), `-Werror=…` | the cause and the upstream fix; `x-maintained: false` for an old release | high |
| System library at the wrong version (`pkg-config --atleast-version`, `Requested 'x >= V'`, conf-libclang's LLVM) | the required and the installed version per distribution (read from the depext installation); exclude those distributions | high |
| Network errors, cancelled jobs, CI tool crash (`Segmentation fault day10-install`), jobs CI did not run to the end (`aborted`, the analysis included, or `not started` when nothing else runs; those waiting for a failed analysis are the analysis's) | one entry listing the jobs to rebuild | high |
| Source archive that cannot be unpacked (symlinks on msys2) | replace the symlinks upstream, or exclude the platform | high |
| A day10 failure on a platform the failing package already lists in `x-ci-accept-failures` | nothing to change: day10 does not honour it there (cygwin, freebsd) | high |
| Lint "conf packages should always use …" on npm depexts (`npm-version`) | known false positive for Melange bindings | high |
| Known messages: `no such function: unixepoch` (SQLite < 3.38), `Could NOT find Python3`, `fatal: not a git repository` from `git describe`, an install into `/usr/...` (read-only), `No known cycle-counting instruction`, Core_kernel shadowing Unix, `Unbound value Conv.…` with cmdliner ≥ 2, `checking for ar... no` on Windows, Clang 19's removed APIs, OCaml 5.4's labelled tuples in Parsetree patterns, `configure: error: x.pc not found` | the cause and the change: a bound, a dependency, `available`, or the install variables | high/medium |
| Lower-bounds job: a dependency downgraded to a version that no longer builds | `>=` the version used in the other jobs, on the direct dependency that pulls it in (found through opam's "required by") | medium |
| Lower-bounds job: the package itself fails and no other rule applies | its downgraded direct dependencies as candidates (passing and failing versions), those named in the error first | low |
| A new release fails with a dependency version that the previous release excludes on master (a bound maintainers added after a breakage, missing from the new opam file) | the same bound, citing the previous release; skipped when the new release changed that constraint on purpose | medium |
| A reverse dependency builds on master (opam-health-check) but fails with the new release | the usual fix in the same PR: `"pkg" {< "new version"}` in its depends, or a `conflicts` entry when it does not depend on it directly; not for a PR that modifies an existing version, nor for a new conf package | medium |
| `undefined reference to …` at link time | the missing symbol, with known causes (LAPACKE missing from the system LAPACK, core's `core_unix_gettid`) | medium |
| `implicit declaration of function caml_…` | an OCaml runtime function this OCaml does not have (another version, or OxCaml): an `ocaml` constraint or an upstream fix | medium |
| `lib/X/META: No such file or directory` | X's installation was incomplete while the package built (not a missing dependency) | medium |
| x86-only C flags (`-mpopcnt`, …) rejected on arm64; a build script that FreeBSD cannot run (`#!/bin/bash`); a stack overflow only with flambda | the cause and the upstream fix or platform restriction | medium–high |
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
- jobs that also failed in the CI run of the PR that published the previous release of the package (same platform, same job, and the same failure: category and reason, the version left out; 2 GitHub calls and one page per package, skipped when the old run has been purged). When the release's own build fails at the same step but the reason does not show the error (`The compilation of … failed at "dune build …"`), the note says so, and the triage page does not count the failure as happening without the PR;
- reverse dependencies that opam-health-check also sees failing on master (from ocaml.org's MCP `ocaml_package_build_information`; same version and OCaml version, build only, as opam-health-check builds the newest version of each package without tests; one request per package, cached for a day). A run older than 7 days (a stuck service) is not used, and the tool says so (`note:` on stderr).

Reverse dependencies that opam-health-check sees *building* on master are listed in a separate entry, "Broken by this PR (builds on master)": those are the failures most likely caused by the PR. On the triage page, a suggestion whose jobs are all reverse-dependency jobs, and whose reverse dependencies all fail in the same 3 or more other open PRs, says so, with links (and does not keep the PR from seeming ready). Another PR counts only when its failing job tests another package (two releases of one package do not vouch for each other) and fails the same way (the job's category), and 3 of those PRs must test different packages (the packages of their matching jobs do not overlap: three PRs each releasing dune, dune-configurator and dune-site are one piece of evidence); the note lists every PR that counts; a failure the analysis puts on this PR ("Broken by this PR") is never marked.

The matrix rules use only the job list (no fetch); the others reuse the log
already downloaded, plus at most one small lookup.

Example (the earlier, failing commit of PR 30908):

```
API change in a dependency: ortools_solvers.9.15.0-3 (lower bounds) on OCaml 4.14
  same failure on : 3 other jobs: ortools_solvers.9.15.0-3 (lower bounds) on OCaml 5.2, …
  failing package : ortools_solvers.9.15.0-3   fix in this PR
  error           : Unbound value Ortools.Sat.proto_solve
  looked up in    : ocaml.org docs
  defined in      : ortools 9.15.0 (used by CI) no, 9.15.0-1 no, 9.15.0-2 yes
  suggested change (high confidence):
    - "ortools"	{>= "9.15"}
    + "ortools" {>= "9.15.0-2"}
    or "ortools" {= version}  (same upstream repository and version)
```

### LLM fallback (optional)

`--llm` (on `suggest` and `comment`) asks Claude about the failures the tool
cannot explain: one call per distinct failure, at most `--llm-max` (default 5).
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
fallbacks enabled, a 120 s timeout per request and one retry. Responses are
cached in `.opam-ci-cache/`, so re-runs cost nothing. The log and opam file are
passed as untrusted data. While CI is still running, the LLM is not asked: the
prompt holds the job's results on the other platforms, which change as jobs
finish, so each analysis would pay again. `suggest --json` says how many
unexplained failures were left without an answer that a later call may give
(`llm_unanswered`: beyond `--llm-max`, a transient API error, or CI running).

The LLM gets the failures the rules flag as unexplained (also when a note was
added since: pre-existing, or opam-health-check's "builds on master"). Its
answer is checked: an answer that does not follow the schema, an SDK or API
error, or a log that cannot be read leaves the failure unrecognised (with an
`llm_error`), it never fails the analysis. The package it names must be one of
the PR's or the failing one, or it is dropped (with its claim of where the fix
goes). Only a few shapes of change are shown as a diff: a dependency without a
version (possibly `{with-test}`, `{build}` or `{with-doc}`) whose name the
model was given (the PR's packages, the failing one, the installed
dependencies, the opam file or the error excerpt), `available`,
`x-ci-accept-failures` and `x-maintained`, without version bounds or depexts.
Anything else (bounds, depexts, build or install commands, URLs) is shown as
text to check.

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
   one, sequentially. PRs whose head sha and status are unchanged reuse their result from
   `state/prs.json`; closed PRs, and PRs no longer failing, are dropped. It
   writes `site/data.json`. `--time-budget` stops starting new PRs; the next
   run continues.

   The page also publishes an Atom feed, `feed.xml`, with one entry per PR,
   whose id changes with the PR's head commit: a feed reader shows each new
   analysis without visiting the page.

   When a PR gets a new commit, the page says what changed since the
   previous one: the problems gone, the new ones, and how many remain.

   PRs whose CI is still running are included once some job has failed,
   with a "CI running" badge, and analysed again when the statuses of their
   jobs change (a digest of them is kept in the state), until their CI
   finishes. The tool never caches the CI page of a commit while jobs are
   still running (it fetches it again next time), and says when the results
   are partial. An `aborted` job has finished, and so has a `not started`
   one when the analysis job failed (lint never starts then) or when nothing
   else runs and some job failed: `opam-ci`'s status stays pending for good
   then, but the page is cached and the PR is shown as finished; those jobs
   (the aborted ones too) are listed as jobs to rerun, so such a PR is never
   "not this PR's fault". A failed or
   timed-out analysis is tried three times a day at most, while CI runs too.

   The GraphQL query also gives the date of the `opam-ci` status. When a
   finished CI's status changes for the same commit (another state, or a
   status posted again after a maintainer's "Rebuild failed", even one that
   finished within the hour), the PR is analysed again with `--refresh`: the
   CI page is fetched again and the cached logs of its jobs are dropped (a
   rebuilt job keeps its URL), so each log the analysis reads comes from the
   new build. The same happens when there is no previous analysis of the
   build: a new PR or commit (free: nothing is cached), a PR whose entry was
   dropped when its CI passed and that fails again, or an entry saved before
   the status dates were recorded. Nothing is fetched again when the status
   has not changed.

   A PR left for the next run (`--time-budget`) keeps showing its previous
   result when it is for the same commit. When analysing a commit again fails
   (a timeout, a server down), the previous result is kept, with the error
   beside it (`last_error` in the state; the page says the result may be out
   of date), and tried again three times a day at most. After a rebuild too:
   the state records the CI run the failed attempts were for
   (`last_error_ci_state`, `last_error_ci_created`), so their attempts add
   up, and only the first one passes `--refresh` (the next ones read the logs
   it cached); a newer run is analysed at once, with `--refresh`. A PR whose
   CI is running keeps its result when its CI page cannot be read.

   With an `ANTHROPIC_API_KEY` repository secret, failures that no rule
   recognises also get an LLM diagnosis (`suggest --llm --llm-max 2`, at most
   `--llm-budget 40` calls per run; answers are cached, so only new failures
   cost anything). Without the secret, or if the API fails, the page is built
   from the rules alone. Once a run's budget is spent, PRs analysed without
   the LLM are left as they are until a run with budget left. A PR counts as
   analysed with the LLM (`llm` in the state) only when every unexplained
   failure got its answer: one whose `--llm-max` was cut to the run's
   remaining budget, or whose API call failed, is analysed again in a run
   with budget left (`llm_unanswered` says how many were left); not while
   its CI runs. On the page, LLM diagnoses are labelled "unchecked",
   the changes they propose are shown as a diff only for a few safe shapes
   (see above), as text to check otherwise, and
   low-confidence ones are folded, as "LLM diagnosis (low confidence)": in an evaluation on 25 answers, 15 were
   right, 7 plausible, 1 wrong and 2 harmful, all the harmful ones being
   version bounds.
2. `scripts/render_page.py` turns it into `site/index.html`: one static page,
   no external resources. Each failing PR gets one verdict, the first that
   applies: not this PR's fault (every failure also happens without the PR,
   comes from the CI's setup, or is on an experimental platform; an LLM
   answer never counts as the CI's), change this
   PR, blocked elsewhere, not this PR's, on hold (no failure is the PR's, as
   for the first verdict, but the PR is a draft, labelled, or its CI still
   runs), needs a rerun
   (jobs that CI did not test: cancelled, aborted or never started, network
   errors, a CI tool crash; nothing else points at the PR), needs a look (anything else, including
   failures nothing explains). It is used by
   the filters, and a headline says what the problem is; its details show
   the suggested changes in full and the other failures folded, one line
   each, near-duplicates merged. Above the list, the PRs that are "Ready as
   far as CI goes";
   below it, the reverse dependencies that fail on several PRs. PR titles,
   logs and suggestions are HTML-escaped.

`.opam-ci-cache/` and `state/` are kept between runs with `actions/cache`
(files older than 30 days are pruned; the git mirrors of `.opam-ci-cache/git/`
are pruned whole, a month after their last use). Rules only: the LLM fallback is not
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
first force-push, or the first commit) of recently merged PRs whose opam-repo-ci
run failed, and compares the suggestions with what the author changed before the
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
2. The ocaml.org MCP endpoint (`https://ocaml.org/mcp`, JSON-RPC, no session, no token): module lists and plain-text signatures for every documented package version; the versions of a package with their release dates (`ocaml_package_versions`, kept a day in the cache); and, for a test-output failure, the changelog sections of each suspect dependency between the passing version and the first release after it (`ocaml_package_changelog`).
3. Raw files: opam files of opam-repository, `stdlib/*.mli` of each OCaml release.
4. A shallow, blob-less `git fetch` of one upstream tag, only when ocaml.org has no docs for a version.
5. The GitHub REST API, only for `--pr` and for `compare`'s baseline lookup (2–3 calls). It works without a token (60 requests per hour); `GITHUB_TOKEN`, `GH_TOKEN` or a logged-in `gh` are used if present.

Results are cached in `.opam-ci-cache/` (the tool's own errors from ocaml.org,
documentation answers whose status is not `success` (not built yet), pages
that do not exist and opam files of master are asked again after a day).
`suggest` prints the number of network requests it actually sent, with
`mcp-errors`, the ocaml.org MCP calls that failed (after three, ocaml.org is
not called again in that run), and `net-errors`, the other requests that
failed (a CI page or job log, an opam file, a GitHub API call, a `git
ls-remote` or `git fetch` that could not reach the server or timed out; a 404,
a missing tag or repository is an answer, not counted); `suggest --json` then
says `"incomplete": true`. Such an analysis lacks
some versions, docs or health-check results, and states what it could not
check instead of concluding; the triage page does it again in its next runs
(three attempts a day at most, as for a failed or timed-out analysis), and
shows "analysis incomplete (N failed lookups)" beside the result. A git URL
that failed in a way a dead host fails every time (no DNS name, a TLS or
certificate error, nothing listening, a timeout) is not asked again for a
day: the later analyses count it in `git-unreachable` (shown as a failed
lookup too), not as a new `net-errors`, so a dead dev-repo host neither
costs a timeout on each analysis nor makes the PR retried. A rule
that crashes on a log gives a `rule-error` result for that job only.

Limitations:

- opam-repo-ci deletes old runs: the CI page of a PR from many months ago returns 404, so `compare` has no baseline for it.
- The version lookups assume upstream tags named after the opam version (`1.2.3`, `v1.2.3`, `name-1.2.3`, …).
