#!/usr/bin/env python3
"""Parse opam-repo-ci (opam.ci.ocaml.org) results for an opam-repository commit.

Usage:
  opam_repo_ci_triage.py summary <sha|url|--pr N> [--json] [--all]
  opam_repo_ci_triage.py logs    <sha|url|--pr N> [--match REGEX] [--max N] [--json]
  opam_repo_ci_triage.py compare <sha|url|--pr N> [--baseline SHA] [--json]
  opam_repo_ci_triage.py suggest <sha|url|--pr N> [--match REGEX] [--max N] [--json]
  opam_repo_ci_triage.py comment <sha|url|--pr N> [--post]
  opam_repo_ci_triage.py review  <--pr N> [--post]   (line-level suggestion blocks)
  (suggest, comment: --llm [--llm-max N] asks Claude about failures no rule explains)

`summary` parses the commit page into one record per job (variant).
`logs` fetches the logs of failed jobs and extracts the relevant error excerpt.
`compare` classifies failing revdeps against the CI results of the PR that
published the previous version of the package (2 GitHub REST calls, token
optional): NEW,
pre-existing, builder-specific or no-baseline. No builds are run.
`suggest` proposes dependency-bound fixes for compile/test failures. Rules:
unbound identifiers -> the version that introduced/removed them (library or
OCaml stdlib), safe-string errors, and test-output diffs (dependency versions
differing from a passing job, or released after the package). Nothing is
built and no GitHub token is needed. Sources, cheapest first: the job log,
the ocaml.org MCP endpoint (per-version API docs), opam.ocaml.org pages
(versions, release dates), raw files (opam files, OCaml stdlib .mli), and a
shallow blob-less git fetch only when ocaml.org has no docs for a version.
The number of network requests is printed at the end.

Only the Python standard library is used. Pages and logs are cached in
--cache-dir (default: ./.opam-ci-cache) since finished CI results do not change.
"""


import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Everything is in the triage/ package; it is re-exported here for the scripts
# and tests that import this file.
from triage.words import *  # noqa: E402,F401,F403
from triage.net import *  # noqa: E402,F401,F403
from triage.ci import *  # noqa: E402,F401,F403
from triage.sources import *  # noqa: E402,F401,F403
from triage.rules import *  # noqa: E402,F401,F403
from triage.matrix import *  # noqa: E402,F401,F403
from triage.context import *  # noqa: E402,F401,F403
from triage.llm import *  # noqa: E402,F401,F403
from triage.suggest import *  # noqa: E402,F401,F403
from triage.render import *  # noqa: E402,F401,F403
from triage.cli import *  # noqa: E402,F401,F403
from triage.net import _GH_CACHE  # noqa: E402,F401


if __name__ == "__main__":
    main()  # noqa: F405
