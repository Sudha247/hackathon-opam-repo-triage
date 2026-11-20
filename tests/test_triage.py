#!/usr/bin/env python3
"""Regression tests for opam_repo_ci_triage.py `suggest`, without network.

Each case runs `suggest --json` on a recorded CI run with OPAM_CI_OFFLINE=1:
every page, log, opam file, doc lookup and git object must come from
tests/fixtures/<case>/, and a cache miss fails the test.

  python3 tests/test_triage.py                 # run the tests (offline)
  python3 tests/test_triage.py -k why3find     # one case (unittest -k)
  python3 tests/test_triage.py --record [case ...]
                                               # (re)record fixtures online

The expectations are fixes checked by hand against the CI logs, upstream
history, or the change the PR author actually made.
"""

import contextlib
import datetime
import gzip
import http.client
import io
import json
import re
import os
import shutil
import subprocess
import sys
import time
import unittest
import urllib.error
import unittest.mock

HERE = os.path.dirname(os.path.abspath(__file__))
TOOL = os.path.join(HERE, "..", "opam_repo_ci_triage.py")
FIXTURES = os.path.join(HERE, "fixtures")
BASE_URL = "https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/"

# name: (commit, extra arguments, expectations)
# An expectation matches a suggestion whose fields contain the given strings.
CASES = {
    "ortools-lower-bound": (
        "55ad97d637d37d80b5d186bd7b3850ad263d05f1", [],
        [{"failing": "ortools_solvers.9.15.0-3", "new": '"ortools" {>= "9.15.0-2"}', "alternative": "{= version}"}]),
    "colibri2-stdlib": (
        "e162f8cea2ee791d6b3c418bb56611b92a93e286", ["--max", "1"],
        [{"failing": "colibri2.0.7", "rule": "stdlib", "new": '"ocaml" {>= "5.1"}'}]),
    "why3find-gitlab-fallback": (
        "00f688e6e6e43a9d4720c3fbf49df8f262e6f232", ["--max", "1"],
        [{"failing": "why3find.1.3.1", "new": '"why3" {>= "1.8.0" & < "1.9.0"}'}]),
    "hol_light-lint": (
        "d9fecfe04f055d0029dcea6042a23c2c6c04925b", [],
        [{"failing": "hol_light.3.2.0", "new": 'license: "BSD-2-Clause"'},
         {"failing": "hol_light.3.2.0", "new": "%{_:VAR}%"}]),
    "binaryen-uninstallable": (
        "ffd281c8603d21293b920f7cd26b60bbab4dec39", ["--max", "1"],
        [{"rule": "uninstallable", "note": "js_of_ocaml-compiler < 5.0.0"}]),
    "oonnxx-platforms": (
        "f60998e840bc2ad6a3e0f26fe2d465c4ba781551", ["--max", "3"],
        [{"failing": "oonnxx.0.2.0", "new": 'x-ci-accept-failures: ["centos-9" "ubuntu-22.04"]'},
         {"failing": "oonnxx.0.2.0", "new": 'arch != "s390x"'}]),
    "chatoyant-melange-awk": (
        "8bd7f3b5699246c467c2698927e1a3e8e8f5b43d", ["--max", "3"],
        [{"failing": "melange.7.0.1-55", "new": 'depexts: ["gawk"] {os-distribution = "cygwin"}'}]),
    "lwt-revdeps": (
        "1e2d45eb8b297fea701407d41e2d2af656bd5d9a",
        ["--max", "6", "--match",
         r"^compilers,(4\.14,lwt.5.10.2,revdeps,kinetic-client|5\.5,lwt.5.10.2,revdeps,passage\.0\.1\.7)"],
        [{"failing": "kinetic-client.0.0.9", "rule": "safe-string", "new": '< "4.06"'},
         {"failing": "passage.0.1.7", "candidate": '"cmdliner" {with-test & < "2.0.0"}',
          "changelog": "no longer allows command names"}]),
    "graphql-async-license": (
        "765646df4b8a6b4fcf3b68949aa3a82236d89e0d", ["--max", "6"],
        [{"failing": "graphql-async.0.13.0", "new": 'license: "MIT"'}]),
    "ocaml-r-unmaintained": (
        "ab3adebc3709da97cafdc3546a50c8158b229dcd", ["--max", "6"],
        [{"failing": "ocaml-r.0.5.0", "alternative": "x-maintained: false"}]),
    "git-paf-missing-library": (
        "1b03be6378915db91ffe28eaa264a242da693e1c", ["--max", "6"],
        [{"failing": "git-paf.3.19.0", "new": '"bstr"'}]),
    "websocket-bound": (
        "f290556e5d03adbda8dc847188358e06abe16436", ["--max", "6"],
        [{"failing": "websocket-lwt-unix.2.18", "new": '"mirage-crypto-rng" {>= "1.2.0"}'}]),
    "lambdapi-bound": (
        "5fe799b6cebb0d22cd7b963e1103bd17aa545bae", ["--max", "6"],
        [{"failing": "lambdapi.4.0.0", "new": '"sedlex" {>= "3.3"}'}]),
    "unikraft-pr-context": (
        "a2a330c034bbf7e443ef9eadad19fd68ceeb991d", ["--max", "8"],
        [{"failing": "unikraft.0.20.0", "rule": "pr-relaxed-available", "new": 'os = "linux"'},
         {"rule": "builder-specific", "note": "conf-bison declares a depext for centos"},
         {"rule": "platform-matrix", "note": "relaxed `available` of unikraft.0.20.0"},
         {"failing": "ocaml-unikraft.1.2.1", "rule": "lint", "note": "#30209"}]),
    # The error is printed before ~850 lines of warnings (it used to be cut from the
    # excerpt); pgn_parser < 1.0.2 does not install its library (no `(package ...)`).
    "pgn-parser-sibling-bound": (
        "d524ea6c8a2ed6050068c8cedb293e595330dc67", ["--max", "6", "--match", "lower-bounds"],
        [{"failing": "chess_com_api.1.0.2", "rule": "missing-library", "new": '"pgn_parser" {>= "1.0.2"}'},
         {"failing": "lichess_api.1.0.2", "rule": "missing-library", "new": '"pgn_parser" {>= "1.0.2"}'}]),
    "dns-certify-bound": (
        "d1951c34e014d331928d961b53113a61c93b17fe", ["--max", "6"],
        [{"failing": "dns-certify.10.2.7", "new": '"ca-certs-nss" {>= "3.113.1"}'}]),
    "optint-revdeps": (
        "83910a8f6a20733de90fcdd822827279971ac3ba", ["--max", "12"],
        [{"failing": "topojsone.0.1.0", "rule": "test-output-ocaml", "new": '< "5.5" | !with-test'},
         {"rule": "rerun", "error": "Segmentation fault of the CI tool day10-install"},
         {"rule": "rerun", "error": "7 cancelled"},
         {"failing": "solo5-bindings-hvt.0.6.9", "rule": "newer-c-compiler", "note": "-Werror"},
         {"failing": "tar-eio.3.5.0", "rule": "builder-specific"},
         {"rule": "health-check-preexisting", "failing": "cca.0.7"}]),
    "frama-clang-llvm": (
        "8f970cb1aaa9971525c904c59e168a4ab5169d00", ["--max", "8"],
        [{"failing": "conf-libclang.22", "rule": "system-library-version", "error": "archlinux has LLVM 23"},
         {"rule": "rerun", "error": "HTTP 503"},
         {"failing": "frama-c.34.0~beta", "rule": "opam-same-version-reinstall"},
         {"not": {"error": "brew"}}]),
    "glfw-accepted-failures": (
        "bac9c3858cff51b3d7c9a6f6990799bbe7582c01", ["--max", "6"],
        [{"failing": "conf-glfw3.3", "rule": "accept-failures-ignored", "note": "cygwin"},
         {"failing": "imguiml.v1.90.6", "rule": "builder-specific"},
         {"not": {"new": 'os != "win32"'}}]),
    "irmin-watcher-bound-only": (
        "79e6e2140ee3e82c58d607f1e294e74b3123089a", ["--max", "8"],
        [{"failing": "slipshow.0.2.0", "note": 'Pre-existing: this PR only adds irmin-watcher {< "0.6.0"}'},
         {"failing": "irmin-watcher.0.6.0", "rule": "platform-matrix"},
         {"not": {"failing": "irmin-watcher.0.6.0", "note": "Pre-existing"}},
         {"rule": "rerun", "error": "9 cancelled"}]),
    "goblint-cil-baseline": (
        "eafb2a69786279b98b31f3e5cd8a62ce432cf343", ["--max", "6"],
        [{"rule": "platform-matrix", "note": "goblint-cil.2.2.0 failed the same job in the CI of #31249"},
         {"rule": "accept-failures-ignored", "note": "freebsd-15.1"}]),
    "aslref-sources": (
        "5a2833e2b7ed465175d6685cd3de0960d38cbabc", ["--max", "4"],
        [{"rule": "sources", "error": "tar cannot create the symlink", "note": "msys2"}]),
    "melange-npm-lint": (
        "4615b601a5d1860114fb109cff54ec33bec76e27", ["--max", "2"],
        [{"rule": "lint", "note": "npm-version"}]),
    "binaryen-cstdint": (
        "6818539645347aa49316df894e8160a9db5a7789", ["--max", "2", "--match", "binaryen"],
        [{"failing": "libbinaryen.109.0.1", "rule": "newer-c-compiler", "note": "<cstdint>"}]),
    "bun-afl-implicit-int": (
        "eb1089be5f315d8d5f5d749ad06f42bd92ae5b4b", ["--max", "2", "--match", "revdeps,bun"],
        [{"failing": "afl.2.52b", "rule": "newer-c-compiler", "note": "-Wimplicit-int"},
         {"rule": "health-check-preexisting", "failing": "xen-gnt-unix.4.0.2"},
         {"rule": "health-check-new", "failing": "jext.0.1.1", "new": 'conflicts: [ "lwt" {>= "6.1.3"} ]'}]),
    # Rules for causes found by grading the LLM fallback's answers.
    "ordma-git-describe": (
        "3670238c506b9ba55e76e0956102b5a5eca57118", ["--max", "1", "--match", "^compilers,4.11"],
        [{"failing": "ordma.0.0.6", "rule": "needs-git-checkout"}]),
    "llvm-python3": (
        "e3b282a9c0eb1ef824245e3ffb935dc8fcc9b1ea", ["--max", "2", "--match", "llvm.20-shared"],
        [{"rule": "missing-tool", "new": '"conf-python-3" {build}'},
         {"not": {"error": "llvm-config-"}}]),
    "slipshow-cmdliner2": (
        "5697d67430433b6e7f81fe26a2cc05f4a4f816b6", ["--max", "2", "--match", "slipshow.0.2.0"],
        [{"failing": "slipshow.0.2.0", "new": '< "2.0.0"'}]),
    "mirage-crypto-riscv64": (
        "79e6e2140ee3e82c58d607f1e294e74b3123089a", ["--max", "1", "--match", "riscv64.*current_git.0.6.2"],
        [{"rule": "unsupported-arch", "new": 'arch != "riscv64"', "fix_in": "mirage-crypto.0.8.0"}]),
    "biocaml-core-kernel-unix": (
        "eb1089be5f315d8d5f5d749ad06f42bd92ae5b4b", ["--max", "1", "--match", "revdeps,biocaml"],
        [{"failing": "biocaml.0.11.0", "rule": "library-api", "note": "Core_kernel"}]),
    "mlcuddidl-win32": (
        "e162f8cea2ee791d6b3c418bb56611b92a93e286", ["--max", "1", "--match", "cygwin"],
        [{"rule": "platform-unsupported", "error": "mlcuddidl"}]),
    "alt-ergo-prefix": (
        "6818539645347aa49316df894e8160a9db5a7789", ["--max", "1", "--match", "frama_c_codex"],
        [{"rule": "install-outside-prefix", "error": "/usr/local/bin"}]),
    "frama-clang-clang19": (
        "5f64e545c42823195503816088208e2d89a257c4", ["--max", "1", "--match", "revdeps,frama-clang.0.0.16"],
        [{"rule": "system-library-version", "note": "Clang 19 removed"},
         {"not": {"new": "conf-libclang"}}]),
    "mlfront-labelled-tuples": (
        "fa5fb3a6a806e832e4c7791145889a6bbe8b64ef", ["--max", "1", "--match", "revdeps,MlFront_Config.2.3.1"],
        [{"failing": "MlFront_Config.2.3.1", "new": '< "5.4"'}]),
    "pds-sqlite-unixepoch": (
        "95c70e7638f27c185540b2efc1bb6fe8f64dcdfa", ["--max", "1", "--match", "centos-9.*tests"],
        [{"rule": "system-library-version", "error": "3.34.1"}]),
    "dspml-day10-excerpt": (
        "d721bc4476e2693a8ae955aa0c8e8647d207ca16", ["--max", "1", "--match", "^day10"],
        [{"failing": "dspml.1.0.2", "rule": "missing-library", "new": '"gnuplot"'}]),
    # Lower-bounds jobs: a downgraded dependency that no longer builds, and
    # the package's own failure with its downgraded dependencies as candidates.
    "colibri2-lower-bound-dep": (
        "e0a519e15c3951b6ed6a838cda48da8742cdad33", ["--max", "1", "--match", "4.14,colibri2.0.6,lower-bounds"],
        [{"failing": "colibri2.0.6", "rule": "lower-bound", "new": '"mlcuddidl" {>= "3.0.8"}'}]),
    "orrandomforest-lower-bound-chain": (
        "41de505ec8ff3637ab3cb06a41ccbc29683f566c", ["--max", "1", "--match", "4.14,orrandomForest.3.0.0,lower"],
        [{"rule": "lower-bound", "new": '"parany" {>= "14.0.1"}', "error": "pulls in ocamlnet.4.1.8"}]),
    "miaou-lower-bound-candidates": (
        "a8e39569b4d59572ae01a7800cf55dcd9d854d1e", ["--max", "1", "--match", "5.2,miaou-core.0.6.0,lower"],
        [{"failing": "miaou-registry.0.6.0", "rule": "lower-bound", "candidate": '"dune" {>= "3.24.2"}'}]),
    # The previous release bounds dolog, and dolog 6 is installed, but the job
    # fails in an R script that downloads keras: no previous-release bound.
    "odnnr-previous-release-unrelated": (
        "6b62b33f94fa0d7f64aa4da5cb0c75e78fa3b3ee", ["--max", "1", "--match", "^compilers,4.11,odnnr.2.0.3$"],
        [{"not": {"rule": "previous-release-bound"}}]),
    # Platforms other versions of the package already accept failures on.
    "ocaml-r-accepted-elsewhere": (
        "e77d15c9d7a33bd5ce6b889bcaff4fd2ee733e9c", ["--max", "6"],
        [{"failing": "ocaml-r.0.7.0", "rule": "platform-matrix",
          "note": "Other versions already accept failures there: ocaml-r.0.4.0"}]),
    # A new release breaking reverse dependencies through another one: the
    # bound goes on the package that actually breaks (git-mirage), as the
    # author did, not on the 14 packages that fail through it.
    "awa-revdeps-through-dependency": (
        "b00f7d80717a0cc3f6d2777cb5e55331607d53d3", ["--max", "8"],
        [{"rule": "health-check-new", "failing": "git-mirage.3.18.0", "new": '< "0.7.0"'},
         {"not": {"rule": "health-check-new", "failing": "diffast.0.4"}}]),
    # From a scan of 190 merged PRs whose first version failed.
    "diffast-incomplete-install": (
        "1b03be6378915db91ffe28eaa264a242da693e1c", ["--max", "1", "--match", "^compilers,4.14,.*revdeps,diffast.0.1.1$"],
        [{"rule": "broken-install", "error": "uutf/META"}, {"not": {"rule": "missing-library", "new": '"uutf"'}}]),
    "cohttp-curl-x86-flags": (
        "9a18be928fec732b4b376fff7cfcbbcdfddb9c0e", ["--max", "1", "--match", "^macos,.*cohttp-curl-async"],
        [{"rule": "platform-unsupported", "error": "-mpopcnt"}]),
    "owl-link-error": (
        "2cadc04b9adb2b758b70dab842ca5094c42c543b", ["--max", "1", "--match", "^compilers,4.14,.*revdeps,owl.1.0.2$"],
        [{"rule": "link-error", "error": "LAPACKE_"}]),
    "ocsipersist-freebsd-script": (
        "094df7d8541218ef134342c3d82e9e191ffb2c59", ["--max", "1", "--match", "^freebsd,.*ocsipersist-pgsql"],
        [{"rule": "platform-unsupported", "error": "genconfig.sh"}]),
    "alcobar-flambda-stack": (
        "952c1bf57515980ee939ade07e8a09b3040e161e", ["--max", "1", "--match", "^extras,ocaml-4.14-flambda,alcobar"],
        [{"rule": "test-platform", "error": "flambda"}]),
    "janestreet-cpuid-old-day10-log": (
        "e1268d425980a62d80b8507a0148af0f5c54d2c2", ["--max", "1", "--match", "^day10,day10-archlinux-ocaml-5.5,janestreet_cpuid"],
        [{"rule": "ocaml-runtime-api", "error": "caml_alloc_local"}]),
    # Every build job skipped: the solver chain is "pkg >= 0.7 -> …", not "pkg = 0.7 -> …".
    "opam-dune-lint-uninstallable": (
        "1c9cedbb0aed129d82fd932138d24126c7e06ebe", [],
        [{"rule": "uninstallable", "note": "`dune-private-libs < 3.0`"}]),
    "obs-loki-eio-no-matching-version": (
        "feb8cffb7befd056276f7fe38943de8faef7eac8", [],
        [{"rule": "uninstallable", "note": "No version of `obs-eio`"}]),
    # A dependency missing from opam-repository, and a lint job that failed before linting.
    "omni-irc-unknown-package-lint-ci-error": (
        "7386318651c3e60f273db07ad6d31c6c377ca249", [],
        [{"rule": "uninstallable", "note": "`omni-irc-sig` is not in opam-repository"},
         {"rule": "lint-ci-error", "class": "elsewhere", "note": "git could not check out"}]),
}


def run(case, offline=True):
    sha, extra, _ = CASES[case]
    env = {**os.environ, "OPAM_CI_OFFLINE": "1" if offline else "0", "OPAM_CI_RECORD": "0" if offline else "1"}
    out = subprocess.run([sys.executable, TOOL, "suggest", sha, "--json", "--cache-dir",
                          os.path.join(FIXTURES, case)] + extra,
                         capture_output=True, text=True, env=env, timeout=900)
    return out


def matches(s, exp):
    for k, v in exp.items():
        if k == "candidate":
            if not any(v in c.get("new", "") for c in s.get("candidates", [])):
                return False
        elif k == "changelog":
            if not any(v in (c.get("changelog") or {}).get("text", "") for c in s.get("candidates", [])):
                return False
        elif v not in str(s.get(k) or ""):
            return False
    return True


def record(cases):
    for case in cases:
        d = os.path.join(FIXTURES, case)
        shutil.rmtree(d, ignore_errors=True)
        out = run(case, offline=False)
        print(f"{case}: rc={out.returncode} {out.stderr.strip().splitlines()[-1:]}")
        # compress pages, logs and JSON answers
        for name in os.listdir(d):
            p = os.path.join(d, name)
            if name.endswith((".html", ".json")):
                with open(p, "rb") as f, gzip.open(p + ".gz", "wb", compresslevel=9) as g:
                    g.write(f.read())
                os.remove(p)
        size = sum(os.path.getsize(os.path.join(r, f)) for r, _, fs in os.walk(d) for f in fs)
        print(f"  fixture size: {size // 1024} KiB")


def top_up(cases, rounds=4):
    """Add to existing fixtures the ocaml.org MCP answers they miss (after a
    change that makes new MCP calls), without fetching anything else again."""
    sys.path.insert(0, os.path.join(HERE, ".."))
    import opam_repo_ci_triage as t
    for case in cases:
        d = os.path.join(FIXTURES, case)
        added = 0
        for _ in range(rounds):
            out = run(case)
            misses = re.findall(r"^  miss: mcp (.*)$", out.stderr, re.M)
            others = re.findall(r"^  miss: (?!mcp )(.*)$", out.stderr, re.M)
            if others:
                print(f"{case}: {len(others)} other misses, re-record it: {others[:2]}")
            if not misses:
                break
            src = t.Sources(d)
            for key in dict.fromkeys(misses):
                tool, args = json.loads(key)
                src.mcp(tool, **args)
                added += 1
        for name in os.listdir(d):
            p = os.path.join(d, name)
            if name.startswith("mcp-") and name.endswith(".json"):
                with open(p, "rb") as f, gzip.open(p + ".gz", "wb", compresslevel=9) as g:
                    g.write(f.read())
                os.remove(p)
        print(f"{case}: {added} MCP answers added")


class TriageTest(unittest.TestCase):
    pass


class ChecksTest(unittest.TestCase):
    """The checks of suggested changes (no network)."""

    def setUp(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        import opam_repo_ci_triage as t
        self.t = t

    def test_unsatisfiable(self):
        terms = self.t.VERSION_TERM.findall
        self.assertTrue(self.t.unsatisfiable(terms('>= "11.0.0" & < "11.0.0"')))
        self.assertTrue(self.t.unsatisfiable(terms('>= "v0.9.15" & < "v0.9.15"')))
        self.assertFalse(self.t.unsatisfiable(terms('>= "4.0.0" & < "5.0.0"')))

    def test_allows(self):
        terms = self.t.VERSION_TERM.findall
        self.assertTrue(self.t.allows("6.0.0", terms('>= "4.0.0"')))
        self.assertFalse(self.t.allows("6.0.0", terms('>= "4.0.0" & < "5.0.0"')))

    def test_applied(self):
        text = 'opam-version: "2.0"\ndepends: [\n  "ocaml" {>= "4.08"}\n]\n'
        self.assertIn('"ocaml" {>= "4.08" & < "5.4"}',
                      self.t.applied(text, {"old": '"ocaml" {>= "4.08"}', "new": '"ocaml" {>= "4.08" & < "5.4"}'}))
        self.assertIn('  "conf-python-3" {build}\n  "ocaml"',
                      self.t.applied(text, {"new": '"conf-python-3" {build}   (new line in depends)'}))


class WhereTest(unittest.TestCase):
    """Where a fix goes, as data (`where_kind`), and derived from the words of
    `fix_in` for results stored before it existed."""

    def setUp(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        from triage import where, suggest
        self.w, self.s = where, suggest

    def test_legacy(self):
        kind = self.w.where_kind
        self.assertEqual(kind({"fix_in": "this PR"}), "pr")
        self.assertEqual(kind({"fix_in": "lwt.5.9.0 (other package)"}), "other-package")
        self.assertEqual(self.w.where_pkg({"fix_in": "lwt.5.9.0 (other package)"}), "lwt.5.9.0")
        self.assertEqual(kind({"fix_in": "the dependency declaring these depexts"}), "other-package")
        self.assertIsNone(self.w.where_pkg({"fix_in": "the dependency declaring these depexts"}))
        self.assertEqual(kind({"fix_in": "CI (rerun)"}), "rerun")
        self.assertEqual(kind({"fix_in": "the CI environment (day10 builder)"}), "ci")
        self.assertEqual(kind({"fix_in": "x.1 (upstream archive)"}), "upstream")
        self.assertEqual(kind({"fix_in": "nothing (this PR's bound)"}), "none")
        self.assertEqual(kind({"fix_in": "the PR, a shared dependency, or the CI"}), "unknown")
        shared = {"fix_in": "the PR, a shared dependency, or the CI", "failing_dep": "dep.1"}
        self.assertEqual((kind(shared), self.w.where_pkg(shared)), ("other-package", "dep.1"))
        self.assertEqual(kind({"fix_in": "somewhere new"}), "unknown")

    def test_structured_wins(self):
        # The words of fix_in are only read when where_kind is missing.
        r = {"fix_in": "this PR", "where_kind": "ci"}
        self.assertEqual(self.w.where_kind(r), "ci")
        self.assertEqual(self.s.where_text(r), "the CI")
        self.assertEqual(self.s.classify(dict(r, new="x")), "elsewhere")
        r = {"fix_in": "the maintainers of y (new wording)", "where_kind": "other-package", "where_pkg": "y.1"}
        self.assertEqual(self.s.where_text(r), "y.1 (another package)")
        self.assertEqual(self.s.where_text({"where_kind": "other-package", "rule": "missing-system-package"}),
                         "the dependency that declares these depexts (another package)")


class LintTest(unittest.TestCase):
    """The messages of a failed lint job: findings of this PR, or a CI error."""

    def setUp(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        from triage import context
        self.c = context

    def rules(self, *messages):
        data = {"jobs": [{"variant": "(lint)", "kind": "lint", "status": "failed", "target": None,
                          "url": "https://x/(lint)", "messages": list(messages)}]}
        return self.c.lint_rules(data, None, None, "/nonexistent")

    def test_warning_is_a_finding(self):
        # opam-ci-check fails the lint job on a warning alone (#30007).
        [r] = self.rules("Warning in ocaml-r.0.7.0: Dubious use of 'dune subst'. 'dune subst' should always only "
                         "be called with {dev}")
        self.assertEqual((r["rule"], r["failing"], r["where_kind"]), ("lint", "ocaml-r.0.7.0", "pr"))
        self.assertIn("Dubious use", r["error"])
        self.assertIn("fails on warnings too", r["note"])

    def test_ci_error(self):
        [r] = self.rules('Command "git" "-C" "/tmp/git-checkout5e08f64" "reset" "--hard" "-q" "7386318" '
                         'exited with status 128')
        self.assertEqual((r["rule"], r["where_kind"]), ("lint-ci-error", "rerun"))
        self.assertIn("git could not check out", r["note"])

    def test_failed_analysis(self):
        # The analysis job failed: no build job ran (#31172). Its reason is shown, the fix is in the PR,
        # unless the CI cancelled it.
        def data(reason, messages=()):
            return {"jobs": [{"variant": "(analysis)", "kind": "analysis", "status": "failed", "reason": reason,
                              "category": "analysis", "url": "https://x/(analysis)", "experimental": False,
                              "messages": list(messages), "target": None}]}
        [r] = self.c.analysis_rules(data("Failed to parse packages/x/x.1/opam: unexpected token"), "/nonexistent")
        self.assertEqual((r["rule"], r["where_kind"]), ("analysis", "pr"))
        self.assertIn("unexpected token", r["error"])
        self.assertIn("no build job ran", r["note"])
        [r] = self.c.analysis_rules(data("Cancelled"), "/nonexistent")
        self.assertEqual(r["where_kind"], "rerun")
        [r] = self.c.analysis_rules(data(None, ["Error: x.1 changes files outside packages/"]), "/nonexistent")
        self.assertIn("outside packages/", r["error"])


def job(variant, category="build", reason="The compilation of x failed", experimental=False):
    """A synthetic CI job, as parse_commit_page makes it."""
    sys.path.insert(0, os.path.join(HERE, ".."))
    from triage import ci
    parts = variant.split(",")
    i = 4 if parts[0] == "day10" else 2
    platform, rest = parts[:i], parts[i:]
    kind = "revdeps" if rest[1:2] == ["revdeps"] else rest[1] if len(rest) > 1 else "build"
    status = "passed" if category == "passed" else "failed"
    return {"variant": variant, "platform": platform, "target": rest[0], "kind": kind,
            "revdep": rest[2] if kind == "revdeps" else None, "status": status, "category": category,
            "reason": "" if status == "passed" else reason, "experimental": experimental,
            "url": ci.BASE + "0" * 40 + "/variant/" + variant}


class RulesTest(unittest.TestCase):
    """Rules on synthetic job lists (no fixture)."""

    def setUp(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        import opam_repo_ci_triage as t
        self.t = t

    def test_rerun_experimental(self):
        # An entry listing only experimental jobs to rerun says so (it does not hold the PR back).
        exp = job("macos-homebrew,4.14,x.1", "cancelled", "Cancelled", experimental=True)
        [r] = self.t.rerun_rules({"jobs": [exp]}, [])
        self.assertEqual((r["where_kind"], r["experimental"]), ("rerun", True))
        [r] = self.t.rerun_rules({"jobs": [exp, job("compilers,4.14,x.1", "network", "curl failed")]}, [])
        self.assertFalse(r["experimental"])

    def test_jobs_not_run(self):
        # Jobs CI never ran to the end (aborted, or never started while CI
        # counts as finished) are jobs to rerun: the PR is not "not this
        # PR's fault" (the reviewer's scenario: an aborted build and a
        # pre-existing failure).
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import render_page as R
        aborted = dict(job("compilers,5.3,foo.1"), status="aborted", category="aborted", reason=None)
        data = {"jobs": [aborted, job("compilers,5.3,foo.1,revdeps,X.1", reason="x")]}
        [r] = self.t.rerun_rules(data, [])
        self.assertEqual((r["where_kind"], r["error"], r["jobs"], r["experimental"]),
                         ("rerun", "1 aborted job", ["compilers,5.3,foo.1"], False))
        pre = {"rule": "health-check-preexisting", "where_kind": "other-package", "class": "elsewhere"}
        pr = {"number": 5, "ci_state": "FAILURE", "ci_unfinished": 0, "result": {"suggestions": [pre]}}
        self.assertEqual(R.verdict(pr), "ready")
        pr["result"]["suggestions"].append(r)
        self.assertFalse(R.seems_ready(pr))
        self.assertEqual(R.verdict(pr), "rerun")
        # Aborted on riscv64 or s390x, where CI aborts jobs routinely: listed, but does not hold the PR back.
        for v in ("extras,riscv64-ocaml-4.14,foo.1", "day10,day10-riscv64-ocaml-5.5,foo.1,revdeps,X.1",
                  "extras,s390x-ocaml-5.4,foo.1"):
            routine = dict(job(v), status="aborted", category="aborted", reason=None)
            [r] = self.t.rerun_rules({"jobs": [routine]}, [])
            self.assertTrue(r["experimental"], v)
            self.assertIn("routinely", r["note"])
            pr = {"number": 5, "ci_state": "FAILURE", "ci_unfinished": 0, "result": {"suggestions": [pre, r]}}
            self.assertEqual(R.verdict(pr), "ready", v)
        # Not a package whose name mentions riscv64.
        other = dict(job("compilers,5.3,riscv64-tools.1"), status="aborted", category="aborted", reason=None)
        [r] = self.t.rerun_rules({"jobs": [other]}, [])
        self.assertFalse(r["experimental"])
        # An aborted analysis: lint never starts; both to rerun.
        an = lambda status: {"variant": "(analysis)", "kind": "analysis", "status": status, "target": None,
                             "category": "analysis" if status == "failed" else status, "reason": None,
                             "experimental": False}
        lint = {"variant": "(lint)", "kind": "lint", "status": "not-started", "target": None,
                "category": "not-started", "reason": None, "experimental": False}
        [r] = self.t.rerun_rules({"jobs": [an("aborted"), lint]}, [])
        self.assertEqual((r["error"], sorted(r["jobs"])), ("1 aborted job, 1 job not run", ["(analysis)", "(lint)"]))
        # A failed analysis: the analysis rule explains it, lint included.
        self.assertEqual(self.t.rerun_rules({"jobs": [an("failed"), lint]}, []), [])
        # Still running: a job waiting for another one is not finished.
        running = dict(job("compilers,5.3,foo.1"), status="active", category="active")
        self.assertEqual(self.t.rerun_rules({"jobs": [an("passed"), lint, running]}, []), [])
        # Aborted experimental jobs alone do not hold the PR back.
        [r] = self.t.rerun_rules({"jobs": [dict(aborted, experimental=True)]}, [])
        self.assertTrue(r["experimental"])

    def test_unanalysed_order(self):
        # The PR's own failures are analysed before those of its reverse
        # dependencies, experimental platforms last; classic builder first.
        jobs = [job("compilers,5.4,a.1,revdeps,r.1", reason="Error in r"),
                job("freebsd,freebsd-15.1-ocaml-5.5-amd64,a.1", reason="Error on freebsd", experimental=True),
                job("day10,debian-13,x86_64,5.4,a.1", reason="Error in a"),
                job("compilers,5.4,a.1", reason="Error in a"),
                job("compilers,5.4,a.1,revdeps,s.1", category="passed")]
        groups = self.t.distinct_failures(jobs)
        self.assertEqual([[j["variant"] for j in g] for g in groups],
                         [["compilers,5.4,a.1", "day10,debian-13,x86_64,5.4,a.1"],
                          ["compilers,5.4,a.1,revdeps,r.1"],
                          ["freebsd,freebsd-15.1-ocaml-5.5-amd64,a.1"]])

    def test_builder_specific_same_arch(self):
        def rules(*jobs):
            return [r["rule"] for r in self.t.matrix_rules(list(jobs), [], None, {"x.1"})]
        classic = job("compilers,5.3,x.1", category="passed")             # debian-13, x86_64
        self.assertIn("builder-specific", rules(job("day10,debian-13,x86_64,5.3,x.1"), classic))
        # day10 fails on arm64 only: the classic builder passing on x86_64 says nothing about the builder.
        self.assertNotIn("builder-specific", rules(job("day10,debian-13,arm64,5.3,x.1"), classic,
                                                   job("day10,debian-13,x86_64,5.3,x.1", category="passed")))
        self.assertNotIn("builder-specific", rules(job("day10,debian-13,arm64,5.3,x.1"), classic))
        # FreeBSD's classic jobs say amd64.
        self.assertIn("builder-specific", rules(job("day10,freebsd-15.1,x86_64,5.5,x.1"),
                                                job("freebsd,freebsd-15.1-ocaml-5.5-amd64,x.1", category="passed")))
        # A classic pass on the same arch, but day10 passes on another arch: the arch matters.
        self.assertNotIn("builder-specific", rules(
            job("day10,macos-homebrew,arm64,5.5,x.1"), job("macos,macos-homebrew-ocaml-5.5-arm64,x.1", category="passed"),
            job("day10,macos-homebrew,x86_64,5.5,x.1", category="passed")))

    def test_eval_filter(self):
        f = self.t.eval_filter
        linux = self.t.platform_vars(job("compilers,5.4,x.1"))
        cygwin = self.t.platform_vars(job("day10,cygwin,x86_64,5.4,x.1"))
        self.assertEqual((linux["os"], linux["arch"], cygwin["os-distribution"]), ("linux", "x86_64", "cygwin"))
        old = 'os = "linux" & (arch = "arm64" | arch = "x86_64")'
        self.assertEqual((f(old, linux), f(old, cygwin)), (True, False))
        self.assertTrue(f("", cygwin))
        self.assertFalse(f('!(os-distribution = "cygwin")', cygwin))
        self.assertIsNone(f('opam-version >= "2.1"', linux))      # not a platform variable
        self.assertFalse(f('opam-version >= "2.1" & os != "win32"', cygwin))

    def test_relaxed_available(self):
        from unittest import mock
        from triage import context
        jobs = [job("day10,cygwin,x86_64,5.4,x.1"), job("day10,cygwin,x86_64,5.4,y.1"),
                job("day10,macos-homebrew,arm64,5.4,x.1"), job("compilers,5.4,x.1", category="passed")]

        def rules(old, new):
            with mock.patch.object(context, "pr_opam_changes", lambda *a: {"x.1": {"available": (old, new)}}):
                return context.relaxed_available_rules({"jobs": jobs}, "s", "b", "d", ["x.1", "y.1"])
        r = rules('os = "linux"', "")
        self.assertEqual(len(r), 1)
        self.assertEqual(r[0]["confidence"], "high")
        # Only x.1's jobs: y.1 failing on cygwin is not caused by x.1's `available`.
        self.assertEqual(r[0]["jobs"], ["day10,cygwin,x86_64,5.4,x.1", "day10,macos-homebrew,arm64,5.4,x.1"])
        # Only the platforms the old filter excluded and the new one allows.
        r = rules('os != "macos" & os != "win32"', 'os != "win32"')
        self.assertEqual(r[0]["jobs"], ["day10,macos-homebrew,arm64,5.4,x.1"])
        self.assertEqual(rules('os != "win32"', 'os != "win32" & opam-version >= "2.1"'), [])
        self.assertEqual(rules('arch != "arm32"', 'arch != "arm32" & arch != "x86_32"'), [])

    def test_preexisting_same_failure(self):
        from unittest import mock
        from triage import context
        build = 'The compilation of {} failed at "dune build -p x -j {}"'
        new = [job("compilers,5.4,x.2", reason=build.format("x.2", 32)),
               job("compilers,5.3,x.2", reason="Error: Unbound module Foo"),
               job("compilers,5.2,x.2", category="dependency", reason="y.1 failed to build"),
               job("compilers,5.1,x.2", category="dependency", reason="z.1 failed to build"),
               job("compilers,5.4,x.2,revdeps,r.1", reason=build.format("r.1", 8))]
        old = [job("compilers,5.4,x.1", reason=build.format("x.1", 8)),
               job("compilers,5.3,x.1", reason="Error: Unbound module Bar"),          # another error
               job("compilers,5.2,x.1", category="dependency", reason="y.1 failed to build"),
               job("compilers,5.1,x.1", reason="z.1 failed to build"),                # another category
               job("compilers,5.4,x.1,revdeps,r.1", reason=build.format("r.1", 8))]
        notes = {}
        with mock.patch.object(context, "find_baseline_commit", lambda *a: {"prev": "x.1", "pr": 7, "sha": "b" * 40}), \
                mock.patch.object(context, "fetch", lambda *a: ""), \
                mock.patch.object(context, "parse_commit_page", lambda page: {"jobs": old}):
            pre = context.preexisting_failures({"jobs": new}, "s", None, "d", notes=notes)
        key = {j["variant"]: self.t.failure_key(j) for j in new}
        self.assertEqual(set(pre), {key["compilers,5.4,x.2"], key["compilers,5.2,x.2"],
                                    key["compilers,5.4,x.2,revdeps,r.1"]})
        self.assertIn("x.1 failed the same job in the CI of #7", pre[key["compilers,5.2,x.2"]])
        # The release's own build failing at the same step may fail with another error.
        self.assertEqual(notes["unverified"], {key["compilers,5.4,x.2"]})
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import render_page
        self.assertFalse(render_page.not_this_pr({"preexisting": "x", "preexisting_unverified": True}))
        self.assertTrue(render_page.not_this_pr({"preexisting": "x"}))

    def test_old_health_check_run_ignored(self):
        # An opam-health-check run older than a week describes an older
        # master: neither "pre-existing" nor "Broken by this PR" from it.
        from unittest import mock
        import contextlib
        from triage import context
        today = datetime.date.today()
        jobs = [job("compilers,4.14,x.2,revdeps,r.1"), job("compilers,4.14,x.2,revdeps,g.1")]

        class Src:
            def __init__(self, date):
                self.date = date

            def mcp(self, tool, _max_age=None, package=None, **a):
                status = "bad" if package == "r" else "good"
                return {"run": {"date": self.date}, "results": [
                    {"package": package, "version": "1", "builds": [{"compiler": "4.14", "status": status}]}]}

            def versions(self, name):
                return []
        for days, used in ((2, True), (30, False)):
            notes, err = {}, io.StringIO()
            date = (today - datetime.timedelta(days=days)).isoformat()
            with mock.patch.object(context, "OFFLINE", False), contextlib.redirect_stderr(err), \
                    mock.patch.object(context, "find_baseline_commit", lambda *a: None):
                pre = context.preexisting_failures({"jobs": jobs}, "s", None, "d", Src(date), notes)
            self.assertEqual(bool(pre), used, days)
            self.assertEqual(bool(notes.get("new")), used, days)
            self.assertEqual("not used" in err.getvalue(), not used, err.getvalue())
        self.assertFalse(context.health_run_stale(None))
        self.assertTrue(context.health_run_stale("2026-11-01", today=datetime.date(2026, 11, 20)))
        self.assertFalse(context.health_run_stale("2026-11-14", today=datetime.date(2026, 11, 20)))

    def test_unanalysed_count(self):
        # Failures beyond --max are counted in the JSON output.
        env = {**os.environ, "OPAM_CI_OFFLINE": "1", "OPAM_CI_RECORD": "0"}
        out = subprocess.run([sys.executable, TOOL, "suggest", CASES["websocket-bound"][0], "--json", "--max", "2",
                              "--cache-dir", os.path.join(FIXTURES, "websocket-bound")],
                             capture_output=True, text=True, env=env, timeout=300)
        self.assertIn("offline cache misses: 0", out.stderr)
        self.assertGreater(json.loads(out.stdout)["unanalysed"], 0)


def check_where(test, sugg):
    """Every suggestion says where its fix goes (`where_kind`), and the same
    place is derived from its `fix_in` alone, as for results stored before
    `where_kind` existed."""
    sys.path.insert(0, os.path.join(HERE, ".."))
    from triage import where
    for s in sugg:
        test.assertIn(s.get("where_kind"), where.WHERE_KINDS, s)
        old = {k: v for k, v in s.items() if k not in ("where_kind", "where_pkg")}
        test.assertEqual((where.where_kind(old), where.where_pkg(old)),
                         (s["where_kind"], s.get("where_pkg")), (s.get("rule"), s.get("fix_in")))


def make_test(case):
    def test(self):
        if not os.path.isdir(os.path.join(FIXTURES, case)):
            self.skipTest("no fixture (run with --record)")
        out = run(case)
        self.assertEqual(out.returncode, 0, out.stderr[-1000:])
        self.assertIn("offline cache misses: 0", out.stderr, out.stderr[-1500:])
        sugg = json.loads(out.stdout)["suggestions"]
        check_where(self, sugg)
        for exp in CASES[case][2]:
            if "not" in exp:
                bad = [s for s in sugg if matches(s, exp["not"])]
                self.assertFalse(bad, f"unexpected suggestion matching {exp['not']}: {bad[:1]}")
                continue
            self.assertTrue(any(matches(s, exp) for s in sugg),
                            f"no suggestion matching {exp}; got:\n" + "\n".join(
                                f"  {s.get('failing')} [{s.get('rule')}] {s.get('new') or s.get('note')}" for s in sugg))
    return test


for _case in CASES:
    setattr(TriageTest, "test_" + _case.replace("-", "_"), make_test(_case))


class McpFailureTest(unittest.TestCase):
    """A failed ocaml.org MCP call is counted, not cached, and the triage page
    analyses the PR again (a few times at most)."""

    def test_failed_call_counted(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        import tempfile
        import opam_repo_ci_triage as t
        from triage import net, sources
        sources.MCP_FAILURES["n"] = 0
        if net.OFFLINE:
            self.skipTest("offline")
        with tempfile.TemporaryDirectory() as d:
            src = t.Sources(d)
            src.MCP_URL = "http://127.0.0.1:9/mcp"          # nothing listens there
            before = t.NET.get("mcp-errors", 0)
            self.assertIsNone(src.mcp("ocaml_package_versions", package="x"))
            self.assertEqual(t.NET.get("mcp-errors", 0), before + 1)
            self.assertEqual(os.listdir(d), [])             # not cached
            self.assertIsNone(src.versions("x"))           # unknown, not "not a package"

    def test_reuse(self):
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import triage_open_prs as T
        p = {"sha": "a", "ci_state": "FAILURE"}
        prev = {"sha": "a", "ci_state": "FAILURE", "tool_version": "v", "result": {}, "attempts": 1}
        self.assertEqual(T.reuse(prev, p, "v", False), (True, True, False))
        self.assertEqual(T.reuse(dict(prev, mcp_errors=2), p, "v", False), (False, True, True))
        self.assertEqual(T.reuse(dict(prev, mcp_errors=2, attempts=T.MAX_ATTEMPTS), p, "v", False),
                         (True, True, False))
        self.assertEqual(T.reuse(prev, dict(p, sha="b"), "v", False), (False, False, False))


def run_patched(case, patch, extra=()):
    """Run `suggest --json` on a fixture (offline) in a process where `patch`
    (Python code) has been run first."""
    sha, args, _ = CASES[case]
    code = ("import sys; sys.path.insert(0, %r)\n" % os.path.join(HERE, "..") + patch +
            "\nimport triage.cli\nsys.argv = ['opam_repo_ci_triage.py'] + %r\ntriage.cli.main()\n"
            % (["suggest", sha, "--json", "--cache-dir", os.path.join(FIXTURES, case)] + list(args) + list(extra)))
    env = {**os.environ, "OPAM_CI_OFFLINE": "1", "OPAM_CI_RECORD": "0"}
    return subprocess.run([sys.executable, "-c", code], capture_output=True, text=True, env=env, timeout=900)


class Serve:
    """A local TCP server for one test: `answer(request bytes) -> response
    bytes` (None: close the connection without answering)."""

    def __init__(self, answer):
        import socket
        import threading
        self.answer, self.requests = answer, []
        self.sock = socket.socket()
        self.sock.bind(("127.0.0.1", 0))
        self.sock.listen(8)
        self.url = "http://127.0.0.1:%d/mcp" % self.sock.getsockname()[1]
        threading.Thread(target=self.loop, daemon=True).start()

    def loop(self):
        while True:
            try:
                c, _ = self.sock.accept()
            except OSError:
                return
            with c:
                data = b""
                while b"\r\n\r\n" not in data:
                    chunk = c.recv(65536)
                    if not chunk:
                        break
                    data += chunk
                head, _, body = data.partition(b"\r\n\r\n")
                n = int((re.search(rb"(?i)content-length: (\d+)", head) or [0, b"0"])[1])
                while len(body) < n:
                    body += c.recv(65536)
                self.requests.append(body)
                out = self.answer(body)
                if out is not None:
                    c.sendall(out)

    def close(self):
        self.sock.close()


def http_json(v):
    body = json.dumps(v).encode()
    return (b"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: close\r\nContent-Length: %d\r\n\r\n"
            % len(body)) + body


def mcp_answer(v, error=False):
    return http_json({"jsonrpc": "2.0", "id": 1,
                      "result": {"isError": error, "content": [{"type": "text", "text": json.dumps(v)}]}})


class NetTestBase(unittest.TestCase):
    def setUp(self):
        import tempfile
        sys.path.insert(0, os.path.join(HERE, ".."))
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import opam_repo_ci_triage as t
        from triage import net, sources
        if net.OFFLINE:
            self.skipTest("offline")
        self.t, self.net, self.sources = t, net, sources
        sources.MCP_FAILURES["n"] = 0
        self.addCleanup(sources.MCP_FAILURES.update, n=0)
        self.tmp = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.tmp, True)

    def errors(self, k="mcp-errors"):
        return self.t.NET.get(k, 0)

    def age(self, path, days):
        t = time.time() - days * 86400
        os.utime(path, (t, t))


class McpRobustnessTest(NetTestBase):
    """ocaml.org's MCP server failing in various ways: counted as failures, not
    cached, not raised; after a few failures it is not called any more."""

    def test_malformed_answers(self):
        answers = [http_json({"jsonrpc": "2.0", "id": 1, "result": None}),     # "result": null
                   http_json({"jsonrpc": "2.0", "id": 1, "error": {"code": -1}}),
                   b"HELLO\r\n\r\n",                                               # not HTTP
                   None]                                                           # connection closed
        for a in answers:
            self.sources.MCP_FAILURES["n"] = 0
            srv = Serve(lambda body, a=a: a)
            self.addCleanup(srv.close)
            src = self.t.Sources(self.tmp)
            src.MCP_URL = srv.url
            before = self.errors()
            self.assertIsNone(src.mcp("ocaml_package_versions", package="x"), a)
            self.assertEqual(self.errors(), before + 1, a)
            self.assertEqual(os.listdir(self.tmp), [], a)          # not cached

    def test_circuit_breaker(self):
        src = self.t.Sources(self.tmp)
        src.MCP_URL = "http://127.0.0.1:9/mcp"                   # nothing listens there
        sent, before = self.t.NET["mcp"], self.errors()
        for i in range(6):
            self.assertIsNone(src.mcp("ocaml_package_versions", package=f"p{i}"))
        self.assertEqual(self.t.NET["mcp"] - sent, src.MCP_MAX_FAILURES)    # then no more calls
        self.assertEqual(self.errors(), before + 6)                          # each one counted
        self.assertLessEqual(src.MCP_TIMEOUT, 30)

    def test_versions_unknown_vs_no_answer(self):
        srv = Serve(lambda body: mcp_answer("Package not found", error=True))
        self.addCleanup(srv.close)
        src = self.t.Sources(self.tmp)
        src.MCP_URL = srv.url
        self.assertEqual(src.versions("not-a-package"), [])       # the tool's answer: not a package
        src = self.t.Sources(self.tmp)
        src.MCP_URL = "http://127.0.0.1:9/mcp"
        self.assertIsNone(src.versions("x"))                      # no answer: unknown
        srv2 = Serve(lambda body: mcp_answer({"versions": [{"version": "1.0"}, {"version": "0.9"}]}))
        self.addCleanup(srv2.close)
        src.MCP_URL = srv2.url
        self.assertEqual(src.versions("y"), ["0.9", "1.0"])

    def test_cached_error_expires(self):
        srv = Serve(lambda body: mcp_answer("no docs yet", error=True))
        self.addCleanup(srv.close)
        src = self.t.Sources(self.tmp)
        src.MCP_URL = srv.url
        self.assertIsNone(src.mcp("ocaml_package_documentation", package="x", version="1"))
        self.assertIsNone(self.t.Sources(self.tmp).mcp("ocaml_package_documentation", package="x", version="1"))
        self.assertEqual(len(srv.requests), 1)                    # the error is cached...
        for f in os.listdir(self.tmp):
            self.age(os.path.join(self.tmp, f), 2)
        src = self.t.Sources(self.tmp)
        src.MCP_URL = srv.url
        src.mcp("ocaml_package_documentation", package="x", version="1")
        self.assertEqual(len(srv.requests), 2)                    # ...for a day

    def test_docs_not_built_expire(self):
        # Documentation not built (yet) is an answer, not an error, but it
        # changes when ocaml.org builds the docs: kept a day; built docs are kept.
        answers = {"1": {"documentation_status": "unknown"}, "2": {"documentation_status": "success",
                                                                    "libraries": []}}
        srv = Serve(lambda body: mcp_answer(answers[json.loads(body)["params"]["arguments"]["version"]]))
        self.addCleanup(srv.close)

        def ask(version):
            src = self.t.Sources(self.tmp)
            src.MCP_URL = srv.url
            return src.mcp("ocaml_package_documentation", package="x", version=version)
        for v in ("1", "2"):
            self.assertEqual(ask(v), answers[v])
            ask(v)
        self.assertEqual(len(srv.requests), 2)                    # cached
        for f in os.listdir(self.tmp):
            self.age(os.path.join(self.tmp, f), 2)
        ask("1")
        ask("2")
        self.assertEqual(len(srv.requests), 3)                    # only the one without docs asked again

    def test_rules_do_not_assert_on_no_answer(self):
        from triage import rules

        class Src:
            def __init__(self, vs):
                self.vs = vs

            def versions(self, name):
                return self.vs
        job = {"platform": ["compilers", "5.3"], "variant": "compilers,5.3,foo.1", "kind": "build"}
        cases = [(["bash: frobnicate: command not found"], "conf-frobnicate"),
                 (["Package libfoo was not found in the pkg-config search path."], "conf-libfoo"),
                 (['Error: Library "barlib" not found.'], "barlib")]
        for err, pkg in cases:
            r = rules.log_rules(Src(None), err, "foo", 'depends: [\n  "ocaml"\n]\n',
                                {"installed": {}, "ocaml": "5.3"}, job)
            self.assertIn("could not check", r["note"], err)
            self.assertNotIn(f"no {pkg} package", r["note"])
            self.assertNotIn("is not an opam package", r["note"])
            self.assertFalse(r.get("new"))
            r = rules.log_rules(Src(["1"]), err, "foo", 'depends: [\n  "ocaml"\n]\n',
                                {"installed": {}, "ocaml": "5.3"}, job)
            self.assertIn(f'"{pkg}"', r["new"])


class NetErrorTest(NetTestBase):
    """Network errors of the plain HTTP and GitHub API fetches: None, counted,
    not cached."""

    ERRORS = [ConnectionResetError(104, "reset"), TimeoutError("timed out"),
              urllib.error.URLError("no route"), http.client.BadStatusLine("x"),
              http.client.IncompleteRead(b"")]

    def test_raw_and_gh_api(self):
        from unittest import mock
        for e in self.ERRORS:
            with mock.patch("urllib.request.urlopen", side_effect=e), \
                    mock.patch.dict(os.environ, {"GITHUB_TOKEN": "x"}):     # no `gh auth token`
                before = self.errors("net-errors")
                self.assertIsNone(self.t.Sources(self.tmp).raw("https://example.invalid/a"), e)
                self.t._GH_CACHE.clear()
                self.assertIsNone(self.t.gh_api("repos/x/y/pulls/1", self.tmp), e)
                self.assertEqual(self.errors("net-errors"), before + 2, e)
            self.assertEqual(os.listdir(self.tmp), [], e)

    def test_upstream_path_quoted(self):
        # A path with a space or non-ASCII characters is quoted: neither an
        # InvalidURL (counted as a network error) nor a UnicodeEncodeError (a rule error).
        from unittest import mock
        urls = []

        class Resp(io.BytesIO):
            def __enter__(self):
                return self

            def __exit__(self, *a):
                pass

        def urlopen(req, timeout=None):
            urls.append(req.full_url)
            req.full_url.encode("ascii")
            return Resp(b"body")
        with mock.patch("urllib.request.urlopen", urlopen):
            body = self.t.Sources(self.tmp).file_at("https://github.com/o/r", "v1.0", "doc/a b/é+x.ml")
        self.assertEqual(body, "body")
        self.assertEqual(urls, ["https://raw.githubusercontent.com/o/r/v1.0/doc/a%20b/%C3%A9+x.ml"])

    def test_not_found_and_master_files_expire(self):
        from unittest import mock

        class Resp(io.BytesIO):
            def __enter__(self):
                return self

            def __exit__(self, *a):
                pass
        url = "https://example.invalid/missing"
        open(self.t.cache_path(url, self.tmp) + ".404", "w").close()
        with mock.patch("urllib.request.urlopen", side_effect=lambda *a, **k: Resp(b"now here")) as m:
            with self.assertRaises(urllib.error.HTTPError):
                self.t.fetch(url, self.tmp)
            self.age(self.t.cache_path(url, self.tmp) + ".404", 2)
            self.assertEqual(self.t.fetch(url, self.tmp), "now here")
            # An opam file on master is fetched again after a day; one at a commit is not.
            src = self.t.Sources(self.tmp)
            for ref in ("master", "a" * 40):
                with open(self.t.cache_path(f"https://raw.githubusercontent.com/{self.t.REPO}/{ref}/packages/"
                                            f"x/x.1/opam", self.tmp), "w") as f:
                    f.write("old")
            for f in os.listdir(self.tmp):
                self.age(os.path.join(self.tmp, f), 2)
            self.assertEqual(self.t.Sources(self.tmp).opam_file("x", "1"), "now here")
            self.assertEqual(self.t.Sources(self.tmp).opam_file("x", "1", "a" * 40), "old")
            self.assertEqual(m.call_count, 2)


class FailedFetchTest(NetTestBase):
    """A page, a log or a git listing that could not be fetched is counted in
    NET["net-errors"] (the analysis is incomplete and is retried); a clean
    negative answer (404, a missing tag) is not."""

    def test_fetch_counts_failures(self):
        from unittest import mock
        url = "https://example.invalid/log"
        for e, counted in [(ConnectionResetError(104, "reset"), 1), (TimeoutError("timed out"), 1),
                           (urllib.error.HTTPError(url, 503, "busy", {}, None), 1),
                           (urllib.error.HTTPError(url, 404, "Not Found", {}, None), 0)]:
            with mock.patch("urllib.request.urlopen", side_effect=e), mock.patch.object(self.net.time, "sleep"):
                before = self.errors("net-errors")
                with self.assertRaises(type(e)):
                    self.t.fetch(url, self.tmp, refresh=True)
                self.assertEqual(self.errors("net-errors"), before + counted, e)
                # Sources.raw() catches it: counted once, not twice.
                before = self.errors("net-errors")
                self.assertIsNone(self.t.Sources(self.tmp).raw(url + "2"))
                self.assertEqual(self.errors("net-errors"), before + counted, e)

    def test_git_failures(self):
        from unittest import mock
        cases = [("fatal: unable to access 'https://e.x/r/': Could not resolve host: e.x", 128, 1),
                 ("error: RPC failed; HTTP 503 curl 22 The requested URL returned error: 503", 128, 1),
                 ("fatal: unable to access 'https://e.x/r/': Failed to connect to e.x port 443", 128, 1),
                 (None, None, 1),                                           # git killed after GIT_TIMEOUT
                 ("fatal: couldn't find remote ref refs/tags/v1.0", 128, 0),
                 ("remote: Repository not found.\nfatal: repository 'https://e.x/r/' not found", 128, 0),
                 ("fatal: could not read Username for 'https://github.com': terminal prompts disabled", 128, 0)]
        for stderr, code, counted in cases:
            def run(cmd, **kw):
                if stderr is None:
                    raise subprocess.TimeoutExpired(cmd, kw.get("timeout"))
                return subprocess.CompletedProcess(cmd, code, "", stderr)
            for args in (["ls-remote", "--tags", "--refs", "--", "https://e.x/r"],
                         ["fetch", "-q", "--depth", "1", "origin", "+refs/tags/v1.0:refs/tags/v1.0"]):
                with mock.patch.object(self.net.subprocess, "run", run):
                    before = self.errors("net-errors")
                    self.assertNotEqual(self.net.git(args, check=False).returncode, 0)
                    self.assertEqual(self.errors("net-errors"), before + counted, (stderr, args[0]))
        # Other git commands failing are not network errors.
        with mock.patch.object(self.net.subprocess, "run",
                               lambda cmd, **kw: subprocess.CompletedProcess(cmd, 1, "", "Connection reset")):
            before = self.errors("net-errors")
            self.net.git(["rev-parse", "--verify", "-q", "v1^{tree}"], check=False)
            self.assertEqual(self.errors("net-errors"), before)


class UnreadableLogTest(unittest.TestCase):
    def test_unreadable_log_makes_the_analysis_incomplete(self):
        # The log of one job cannot be fetched (connection reset): the analysis
        # says so (`incomplete`), and the triage page analyses the PR again.
        out = run_patched("lwt-revdeps", """
import tempfile, unittest.mock as mock
import triage.suggest as s, triage.net as n
_orig = s.fetch
def fetch(url, cache_dir, **k):
    if "kinetic-client" not in url:
        return _orig(url, cache_dir, **k)
    with mock.patch("urllib.request.urlopen", side_effect=ConnectionResetError(104, "reset")), \\
            mock.patch.object(n, "OFFLINE", False):
        return n.fetch(url, tempfile.mkdtemp(), **k)
s.fetch = fetch
""")
        self.assertEqual(out.returncode, 0, out.stderr[-1500:])
        data = json.loads(out.stdout)
        self.assertTrue(any(s.get("rule") == "unrecognised" and "kinetic-client" in json.dumps(s.get("jobs"))
                            for s in data["suggestions"]), data["suggestions"])
        self.assertEqual((data["incomplete"], data["errors"]), (True, {"net-errors": 1}))


class CacheWriteTest(NetTestBase):
    """Cache files are written whole or not at all, and a damaged one is a miss."""

    def test_truncated_cache_is_a_miss(self):
        p = os.path.join(self.tmp, "a.json")
        with open(p, "w") as f:
            f.write('{"versions": [')
        self.assertIsNone(self.t.cached_json(p))
        with open(p + "x", "wb") as f:
            f.write(gzip.compress(b'["a", "b"]')[:-6])
        os.rename(p + "x", os.path.join(self.tmp, "b.json.gz"))
        os.remove(p)
        self.assertIsNone(self.t.cached_json(os.path.join(self.tmp, "b.json")))

    def test_killed_write_leaves_no_file(self):
        from unittest import mock
        from triage import sources
        url = "https://example.invalid/repo"
        ok = subprocess.CompletedProcess([], 0, "abc\trefs/tags/v1.0\n", "")
        with mock.patch.object(sources, "git", return_value=ok), \
                mock.patch("os.replace", side_effect=KeyboardInterrupt):
            with self.assertRaises(KeyboardInterrupt):
                self.t.Sources(self.tmp).tags(url)
        self.assertEqual(os.listdir(self.tmp), [])
        p = os.path.join(self.tmp, "c.json")
        self.t.write_atomic(p, '"old"')
        with mock.patch("os.replace", side_effect=KeyboardInterrupt):
            with self.assertRaises(KeyboardInterrupt):
                self.t.write_atomic(p, '"new"')
        self.assertEqual(self.t.cached_json(p), {"value": "old"})
        self.assertEqual(os.listdir(self.tmp), ["c.json"])


class TagsTest(NetTestBase):
    """A failed `git ls-remote` is not cached: the next run asks again."""

    def test_failed_listing_not_cached(self):
        from unittest import mock
        from triage import sources
        url = "https://example.invalid/repo"
        bad = subprocess.CompletedProcess([], 128, "", "fatal: unable to access")
        good = subprocess.CompletedProcess([], 0, "abc\trefs/tags/v1.0\n", "")
        with mock.patch.object(sources, "git", return_value=bad) as g:
            self.assertEqual(self.t.Sources(self.tmp).tags(url), [])
            self.assertEqual(self.t.Sources(self.tmp).tags(url), [])
            self.assertEqual(g.call_count, 2)
        self.assertEqual(os.listdir(self.tmp), [])
        with mock.patch.object(sources, "git", return_value=good):
            self.assertEqual(self.t.Sources(self.tmp).tags(url), ["v1.0"])
        with mock.patch.object(sources, "git", return_value=bad) as g:
            self.assertEqual(self.t.Sources(self.tmp).tags(url), ["v1.0"])     # cached
            self.assertEqual(g.call_count, 0)

    def test_unreachable_host_remembered(self):
        # A host that cannot be reached (DNS, TLS, nothing listening, no
        # answer) is not asked again for a day: one net error, then cached
        # failures that are not new net errors. A transient error is not remembered.
        from unittest import mock
        url = "https://git.dead.example/repo"
        calls = []

        def run(stderr):
            def f(cmd, **kw):
                sub = cmd[len(self.net.GIT_PROTOCOLS) + 1]
                calls.append(sub)
                if sub in ("ls-remote", "fetch"):
                    return subprocess.CompletedProcess(cmd, 128, "", stderr)
                return subprocess.CompletedProcess(cmd, 1 if sub == "rev-parse" else 0, "", "")   # a new mirror
            return f
        dead = run("fatal: unable to access 'https://git.dead.example/repo/': Could not resolve host: "
                   "git.dead.example")
        with mock.patch.object(self.net.subprocess, "run", dead):
            n, u = self.errors("net-errors"), self.errors("git-unreachable")
            self.assertEqual(self.t.Sources(self.tmp).tags(url), [])
            self.assertEqual((calls, self.errors("net-errors")), (["ls-remote"], n + 1))
            calls.clear()
            s = self.t.Sources(self.tmp)
            self.assertEqual(s.tags(url), [])
            self.assertIsNone(s._ensure(url, "v1.0", True))         # the fetch of a tag too
            self.assertNotIn("ls-remote", calls)
            self.assertNotIn("fetch", calls)
            self.assertEqual((self.errors("net-errors"), self.errors("git-unreachable")), (n + 1, u + 1))
            # A day later, asked again.
            for f in os.listdir(self.tmp):
                if f.startswith("gitdown-"):
                    self.age(os.path.join(self.tmp, f), 2)
            calls.clear()
            self.assertEqual(self.t.Sources(self.tmp).tags(url), [])
            self.assertEqual(calls, ["ls-remote"])
        # A transient error (a 503) is asked again in the next analysis.
        url2 = url + "2"
        with mock.patch.object(self.net.subprocess, "run",
                               run("error: RPC failed; HTTP 503 curl 22 The requested URL returned error: 503")):
            calls.clear()
            self.t.Sources(self.tmp).tags(url2)
            self.t.Sources(self.tmp).tags(url2)
            self.assertEqual(calls, ["ls-remote", "ls-remote"])

    def test_unbound_stdlib_value_without_tags(self):
        out = run_patched("colibri2-stdlib", "import triage.sources as s\ns.Sources.tags = lambda self, url: []")
        self.assertEqual(out.returncode, 0, out.stderr[-1500:])
        sugg = json.loads(out.stdout)["suggestions"]
        # Was an IndexError (no OCaml release): now the identifier is looked up in the libraries.
        self.assertFalse([s for s in sugg if s.get("rule") == "rule-error"], sugg)
        self.assertTrue(any(s.get("failing") == "colibri2.0.7" for s in sugg), sugg)


class RuleErrorTest(unittest.TestCase):
    """An exception in the rules for one job does not lose the other jobs."""

    def test_one_job_fails(self):
        out = run_patched("lwt-revdeps", """
import triage.suggest as s
_orig = s.suggest_for_job
def boom(src, job, *a):
    if "kinetic-client" in job["variant"]:
        raise ValueError("boom " + "x" * 1000)
    return _orig(src, job, *a)
s.suggest_for_job = boom
import triage.net
triage.net.NET["mcp-errors"] = 2
""")
        self.assertEqual(out.returncode, 0, out.stderr[-1500:])
        data = json.loads(out.stdout)
        sugg = data["suggestions"]
        err = [s for s in sugg if s.get("rule") == "rule-error"]
        self.assertEqual(len(err), 1, sugg)
        self.assertTrue(err[0]["error"].startswith("ValueError: boom"))
        self.assertLess(len(err[0]["error"]), 400)
        self.assertEqual((err[0]["class"], err[0]["fix_in"], err[0]["title"]),
                         ("explanation", "unknown", "Analysis failed (tool error)"))
        self.assertTrue(any(s.get("failing") == "passage.0.1.7" for s in sugg))
        self.assertEqual((data["incomplete"], data["errors"]), (True, {"mcp-errors": 2}))


class GitMirrorTest(NetTestBase):
    """A git mirror whose files were deleted (cache pruning) is made again, and
    git never runs in a repository found in a parent directory."""

    def test_broken_mirror_reinitialised(self):
        url = "https://example.invalid/repo"
        cache = os.path.join(self.tmp, "checkout", ".opam-ci-cache")
        os.makedirs(cache)
        subprocess.run(["git", "init", "-q", os.path.join(self.tmp, "checkout")], check=True)
        d = self.t.Sources(cache)._gitdir(url)
        self.assertTrue(os.path.exists(os.path.join(d, "opam-ci-last-used")))
        for f in ("HEAD", "config"):
            os.remove(os.path.join(d, f))
        d2 = self.t.Sources(cache)._gitdir(url)
        self.assertEqual(d, d2)
        cfg = subprocess.run(["git", "config", "--get", "remote.origin.url"], capture_output=True, text=True,
                             env={**os.environ, "GIT_DIR": d})
        self.assertEqual(cfg.stdout.strip(), url)
        self.assertTrue(os.path.exists(os.path.join(d, "HEAD")))

    def test_prune_step(self):
        with open(os.path.join(HERE, "..", ".github", "workflows", "triage-page.yml")) as f:
            wf = f.read()
        m = re.search(r"- name: Prune cache[^\n]*\n(\s+)run: \|\n((?:\1  .*\n|\s*\n)+)", wf)
        script = "\n".join(line[len(m.group(1)) + 2:] for line in m.group(2).splitlines())
        c = os.path.join(self.tmp, ".opam-ci-cache")
        files = {"old.json": 40, "new.json": 1, "git/used/HEAD": 40, "git/used/opam-ci-last-used": 2,
                 "git/unused/HEAD": 40, "git/unused/opam-ci-last-used": 40, "git/nostamp/HEAD": 40}
        for f, days in files.items():
            p = os.path.join(c, f)
            os.makedirs(os.path.dirname(p), exist_ok=True)
            open(p, "w").close()
            self.age(p, days)
        subprocess.run(["bash", "-e", "-c", script], cwd=self.tmp, check=True, capture_output=True)
        left = sorted(os.path.relpath(os.path.join(r, f), c) for r, _, fs in os.walk(c) for f in fs)
        self.assertEqual(left, ["git/used/HEAD", "git/used/opam-ci-last-used", "new.json"])


class PageRunTest(unittest.TestCase):
    """scripts/triage_open_prs.py: retries, timeouts and GitHub errors."""

    def setUp(self):
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import triage_open_prs as T
        self.T = T

    def test_attempts_expire(self):
        T = self.T
        p = {"sha": "a", "ci_state": "FAILURE"}
        now = time.time()
        recent = datetime.datetime.fromtimestamp(now - 3600, datetime.timezone.utc).isoformat()
        old = datetime.datetime.fromtimestamp(now - 2 * 86400, datetime.timezone.utc).isoformat()
        prev = {"sha": "a", "ci_state": "FAILURE", "tool_version": "v", "result": {}, "attempts": T.MAX_ATTEMPTS,
                "mcp_errors": 2, "analysed_at": recent}
        self.assertEqual(T.reuse(prev, p, "v", False, now), (True, True, False))
        self.assertEqual(T.reuse(dict(prev, analysed_at=old), p, "v", False, now), (False, True, True))
        self.assertEqual(T.reuse(dict(prev, mcp_errors=0, net_errors=1, attempts=1), p, "v", False, now),
                         (False, True, True))
        self.assertEqual(T.attempts_used(dict(prev, analysed_at=old), now), 0)
        failed = dict(prev, result=None, mcp_errors=0)
        self.assertTrue(T.keep_failed(failed, now))
        self.assertFalse(T.keep_failed(dict(failed, analysed_at=old), now))
        self.assertFalse(T.keep_failed(dict(failed, attempts=1), now))

    def test_timeout_keeps_counts(self):
        import tempfile
        with tempfile.TemporaryDirectory() as d:
            tool = os.path.join(d, "slow_tool.py")
            with open(tool, "w") as f:
                f.write("import sys, time\nsys.path.insert(0, %r)\nfrom triage.net import NET\n"
                        "NET['llm'] = 1\nNET['mcp-errors'] = 2\ntime.sleep(60)\n" % os.path.join(HERE, ".."))
            t0 = time.monotonic()
            result, err, net = self.T.run_tool([], 3, tool=tool)
        self.assertLess(time.monotonic() - t0, 30)
        self.assertIsNone(result)
        self.assertIn("timeout", err)
        self.assertEqual((net.get("llm"), net.get("mcp-errors"), net.get("timeout")), (1, 2, 1))

    def test_graphql_errors(self):
        from unittest import mock

        class Resp(io.BytesIO):
            def __enter__(self):
                return self

            def __exit__(self, *a):
                pass
        data = {"repository": {"pullRequests": {"pageInfo": {"hasNextPage": False}, "nodes": [None]}}}
        answers = [{"errors": [{"message": "Something went wrong"}]},
                   {"data": {"repository": None}, "errors": [{"message": "timeout"}]},
                   {"data": data, "errors": [{"message": "one node failed"}]}]
        with mock.patch.object(self.T.urllib.request, "urlopen",
                               side_effect=[Resp(json.dumps(a).encode()) for a in answers]), \
                mock.patch.object(self.T.time, "sleep"):
            self.assertEqual(self.T.graphql("q", {}, "tok"), data)
        with mock.patch.object(self.T, "graphql", return_value=data):
            self.assertEqual(self.T.list_open_prs("tok"), ([], 1))

class RefreshTest(unittest.TestCase):
    """After a rebuild of the same commit, the triage page analyses the PR
    again from a fresh CI page and fresh logs."""

    def test_ci_changed(self):
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import triage_open_prs as T
        prev = {"sha": "a", "ci_state": "FAILURE", "ci_created": "2026-11-20T10:00:00Z", "tool_version": "v",
                "result": {}, "attempts": 1}
        p = {"sha": "a", "ci_state": "FAILURE", "ci_created": "2026-11-20T10:00:00Z"}
        self.assertFalse(T.ci_changed(prev, p))
        self.assertEqual(T.reuse(prev, p, "v", False), (True, True, False))
        # "Rebuild failed", finished within the hour: same state, newer status.
        rebuilt = dict(p, ci_created="2026-11-20T10:30:00Z")
        self.assertTrue(T.ci_changed(prev, rebuilt))
        self.assertEqual(T.reuse(prev, rebuilt, "v", False), (False, False, False))
        self.assertTrue(T.ci_changed(prev, dict(p, ci_state="PENDING")))
        # A previous analysis without a date (state saved before dates were
        # recorded) does not say which build it saw: changed. Without a date
        # now either, nothing says it changed.
        self.assertTrue(T.ci_changed(dict(prev, ci_created=None), rebuilt))
        self.assertFalse(T.ci_changed(dict(prev, ci_created=None), dict(p, ci_created=None)))
        # Another commit is not a change of status, but it is analysed with --refresh
        # (free), as is a PR without a previous entry.
        self.assertFalse(T.ci_changed(prev, dict(rebuilt, sha="b")))
        self.assertTrue(T.needs_refresh(prev, dict(rebuilt, sha="b")))
        self.assertTrue(T.needs_refresh(None, p))
        self.assertFalse(T.needs_refresh(prev, p))

    def test_unfinished_jobs(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        from triage import ci

        def data(*statuses):
            kinds = ["analysis", "lint"] + ["build"] * (len(statuses) - 2)
            return {"jobs": [{"variant": f"v{i}", "kind": k, "status": s}
                             for i, (k, s) in enumerate(zip(kinds, statuses))]}
        n = lambda *s: len(ci.unfinished_jobs(data(*s)))
        # Lint never starts after a failed or aborted analysis (opam-ci's status stays pending).
        self.assertEqual(n("failed", "not-started"), 0)
        self.assertEqual(n("aborted", "not-started"), 0)
        self.assertEqual(n("passed", "passed", "aborted", "failed"), 0)        # aborted: finished
        # A job waiting for another one that failed, nothing else running: finished.
        self.assertEqual(n("passed", "passed", "failed", "not-started"), 0)
        # Still running: a job is active, or nothing failed yet (CI starting).
        self.assertEqual(n("passed", "passed", "active", "not-started"), 2)
        self.assertEqual(n("passed", "not-started", "passed"), 1)
        self.assertEqual(n("active", "not-started"), 2)
        # The statuses as the CI page writes them.
        page = ('<details><summary><b>Main results</b></summary><ul class="statuses">'
                '<li class="failed"><a href="/github/ocaml/opam-repository/commit/a/variant/(analysis)">'
                '(analysis) (failed: x)</a></li><li class="not-started"><a href="/github/ocaml/'
                'opam-repository/commit/a/variant/(lint)">(lint) (not started)</a></li></ul></details>')
        d = ci.parse_commit_page(page)
        self.assertEqual([j["status"] for j in d["jobs"]], ["failed", "not-started"])
        self.assertEqual(ci.unfinished_jobs(d), [])


class PageMainTest(unittest.TestCase):
    """scripts/triage_open_prs.py main() on synthetic states and a fake tool:
    which PRs are analysed again, with which arguments, and what is kept."""

    SHA = "a" * 40
    FAILED_ANALYSIS = [{"variant": "(analysis)", "status": "failed", "category": "analysis"},
                       {"variant": "(lint)", "status": "not-started", "category": "not-started"}]

    def setUp(self):
        import tempfile
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import triage_open_prs as T
        self.T = T
        self.tmp = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.tmp, True)
        self.state = os.path.join(self.tmp, "prs.json")

    def pr(self, **kw):
        return {"number": 1, "title": "t", "url": "u", "draft": False, "updated": "2026-11-20T00:00:00Z",
                "sha": self.SHA, "author": "x", "labels": [], "stale_labels": [], "ci_state": "FAILURE",
                "ci_url": None, "ci_created": "2026-11-20T10:00:00Z", **kw}

    def run_main(self, prs, jobs=None, suggest_ok=True, args=(), llm=False, version="v", summary_ok=True,
                 suggest_out=None, tool=None):
        """One run; (state after it, the tool's calls, `changed` output).
        `tool(args)`: the suggest runs' (result, error, request counts)."""
        from unittest import mock
        import contextlib
        calls = []

        def run_tool(a, timeout, tool_=None):
            calls.append(a)
            if a[0] == "summary":
                if not summary_ok:
                    return None, "timeout after 120s", {"timeout": 1}
                return ({"jobs": jobs if jobs is not None else self.FAILED_ANALYSIS}, None,
                        {"refreshed": 1} if "--refresh" in a else {})
            if tool:
                return tool(a)
            if not suggest_ok:
                # Killed after it refreshed the cache, when asked to (see test_refresh_not_done).
                return None, "timeout after 600s", {"timeout": 1, **({"refreshed": 1} if "--refresh" in a else {})}
            if suggest_out is not None:
                return dict(suggest_out), None, {"http": 1}
            return {"suggestions": [{"rule": "x", "variant": "v"}], "ci_unfinished": 0}, None, {"http": 1}
        gh_out = os.path.join(self.tmp, "gh-out")
        open(gh_out, "w").close()
        with mock.patch.object(self.T, "github_token", return_value="tok"), \
                mock.patch.object(self.T, "list_open_prs", return_value=(prs, 1)), \
                mock.patch.object(self.T, "run_tool", run_tool), \
                mock.patch.object(self.T, "llm_available", return_value=llm), \
                mock.patch.object(self.T, "tool_version", return_value=version), \
                mock.patch.object(sys, "argv", ["triage_open_prs.py", "--state", self.state,
                                                "--out", os.path.join(self.tmp, "data.json")] + list(args)), \
                mock.patch.dict(os.environ, {"GITHUB_OUTPUT": gh_out}), \
                contextlib.redirect_stderr(io.StringIO()):
            self.T.main()
        with open(self.state) as f:
            state = json.load(f)["prs"]
        with open(gh_out) as f:
            return state, calls, f.read().strip()

    def save(self, prs):
        with open(self.state, "w") as f:
            json.dump({"prs": prs}, f)

    def kinds(self, calls):
        return [c[0] + (" --refresh" if "--refresh" in c else "") for c in calls]

    def test_pending_with_finished_page(self):
        # opam-ci's status stays pending (lint never starts after a failed
        # analysis): analysed once, then kept while the job statuses are the same.
        p = self.pr(ci_state="PENDING")
        state, calls, changed = self.run_main([p])
        self.assertEqual(self.kinds(calls), ["summary --refresh", "suggest"])
        self.assertEqual(changed, "changed=true")
        state, calls, changed = self.run_main([p])
        self.assertEqual(self.kinds(calls), ["summary"])          # the page only (cached once finished)
        self.assertEqual(changed, "changed=false")
        self.assertIsNotNone(state["1"]["result"])
        # A job status changed: analysed again.
        jobs = [dict(self.FAILED_ANALYSIS[0], status="passed"), dict(self.FAILED_ANALYSIS[1], status="failed")]
        state, calls, changed = self.run_main([p], jobs=jobs)
        self.assertEqual(self.kinds(calls), ["summary", "suggest"])

    def test_pending_failures_limited(self):
        # A PR whose analysis times out while CI runs: MAX_ATTEMPTS attempts,
        # even though jobs keep finishing, then not tried again for a while.
        p = self.pr(ci_state="PENDING")
        for i in range(self.T.MAX_ATTEMPTS):
            jobs = self.FAILED_ANALYSIS + [{"variant": f"b{j}", "status": "passed"} for j in range(i)]
            state, calls, _ = self.run_main([p], jobs=jobs, suggest_ok=False)
            self.assertIn("suggest", self.kinds(calls), i)
            self.assertEqual(state["1"]["attempts"], i + 1)
        state, calls, changed = self.run_main([p], jobs=self.FAILED_ANALYSIS + [{"variant": "z", "status": "passed"}],
                                              suggest_ok=False)
        self.assertEqual(calls, [])
        self.assertEqual(changed, "changed=false")

    def test_refresh(self):
        p = self.pr()
        # No previous entry (new PR, or one dropped when its CI passed and failing again).
        state, calls, _ = self.run_main([p])
        self.assertEqual(self.kinds(calls), ["suggest --refresh", "summary"])
        # Nothing changed: nothing fetched.
        state, calls, _ = self.run_main([p])
        self.assertEqual(calls, [])
        # An entry saved before the status dates were recorded.
        self.save({"1": {k: v for k, v in state["1"].items() if k != "ci_created"}})
        state, calls, _ = self.run_main([p])
        self.assertEqual(self.kinds(calls), ["suggest --refresh", "summary"])
        # Another commit.
        state, calls, _ = self.run_main([self.pr(sha="b" * 40)])
        self.assertEqual(self.kinds(calls), ["suggest --refresh", "summary"])
        # The tool changed: analysed again from the cache.
        state, calls, _ = self.run_main([self.pr(sha="b" * 40)], version="w")
        self.assertEqual(self.kinds(calls), ["suggest", "summary"])

    def test_deferred_keeps_result_of_the_same_commit(self):
        self.run_main([self.pr()])
        # The tool changed, and the time budget is spent: the result stays.
        state, calls, _ = self.run_main([self.pr()], args=["--time-budget", "-1"], version="w")
        self.assertEqual(calls, [])
        self.assertIsNotNone(state["1"]["result"])
        self.assertFalse(state["1"].get("pending"))
        # Another commit: the result of the old one is not shown.
        state, calls, _ = self.run_main([self.pr(sha="b" * 40)], args=["--time-budget", "-1"], version="w")
        self.assertIsNone(state["1"].get("result"))
        self.assertTrue(state["1"]["pending"])

    def test_failed_redo_keeps_result(self):
        state, _, _ = self.run_main([self.pr()])
        good = state["1"]["result"]
        for i in range(self.T.MAX_ATTEMPTS):
            state, calls, _ = self.run_main([self.pr()], version="w", suggest_ok=False)
            self.assertEqual(self.kinds(calls), ["suggest"], i)
            self.assertEqual(state["1"]["result"], good)
            self.assertEqual((state["1"]["last_error"], state["1"]["attempts"]), ("timeout after 600s", i + 1))
            self.assertEqual(state["1"]["tool_version"], "v")        # what the result saw
        state, calls, _ = self.run_main([self.pr()], version="w", suggest_ok=False)
        self.assertEqual(calls, [])                                   # kept, until the attempts expire
        self.assertEqual(state["1"]["result"], good)
        # A rebuild of the same commit is analysed at once, from a fresh CI page.
        rebuilt = self.pr(ci_created="2026-11-20T11:00:00Z")
        _, calls, _ = self.run_main([rebuilt], version="w", suggest_ok=False)
        self.assertEqual(self.kinds(calls), ["suggest --refresh"])
        # The attempts expired: tried again, and this time it works.
        state["1"]["last_error_at"] = "2026-01-01T00:00:00+00:00"
        self.save(state)
        state, calls, _ = self.run_main([self.pr()], version="w")
        self.assertEqual(self.kinds(calls), ["suggest", "summary"])
        self.assertNotIn("last_error", state["1"])
        self.assertEqual(state["1"]["tool_version"], "w")

    def test_failed_redo_of_a_rebuild_limited(self):
        # After a rebuild, a redo that keeps failing: the attempts at that CI
        # run add up (MAX_ATTEMPTS, then the result is kept), and only the
        # first one refreshes the cache. A newer rebuild is analysed at once.
        state, _, _ = self.run_main([self.pr()])
        good = state["1"]["result"]
        rebuilt = self.pr(ci_created="2026-11-20T11:00:00Z")
        for i in range(self.T.MAX_ATTEMPTS):
            state, calls, _ = self.run_main([rebuilt], suggest_ok=False)
            self.assertEqual(self.kinds(calls), ["suggest --refresh" if i == 0 else "suggest"], i)
            self.assertEqual((state["1"]["result"], state["1"]["attempts"]), (good, i + 1))
            self.assertEqual(state["1"]["ci_created"], "2026-11-20T10:00:00Z")       # what the result saw
        state, calls, _ = self.run_main([rebuilt], suggest_ok=False)
        self.assertEqual(calls, [])
        newer = self.pr(ci_created="2026-11-20T12:00:00Z")
        state, calls, _ = self.run_main([newer], suggest_ok=False)
        self.assertEqual(self.kinds(calls), ["suggest --refresh"])
        self.assertEqual(state["1"]["attempts"], 1)
        # It works: the attempt's fields are gone.
        state, calls, _ = self.run_main([newer])
        self.assertEqual(self.kinds(calls), ["suggest", "summary"])
        self.assertFalse([k for k in state["1"] if k.startswith("last_error")])

    def test_deferred_new_commit_drops_old_fields(self):
        # A new commit left for a later run: nothing of the old commit's analysis stays.
        self.save({"1": {**self.pr(ci_state="PENDING"), "result": {"suggestions": []}, "ci_unfinished": 7,
                         "llm": True, "mcp_errors": 2, "net_errors": 1, "tool_version": "v",
                         "analysed_at": "2026-11-20T10:00:00+00:00"}})
        state, calls, _ = self.run_main([self.pr(sha="b" * 40)], args=["--time-budget", "-1"])
        self.assertEqual(calls, [])
        e = state["1"]
        self.assertTrue(e["pending"])
        self.assertEqual(e["ci_state"], "FAILURE")
        for k in ("result", "ci_unfinished", "llm", "mcp_errors", "net_errors", "ci_created"):
            self.assertNotIn(k, e)

    def test_pending_summary_failed_keeps_result(self):
        # A pending PR whose CI page could not be read (`summary` failed): its result stays.
        p = self.pr(ci_state="PENDING")
        state, calls, _ = self.run_main([p])
        good = state["1"]["result"]
        state, calls, _ = self.run_main([p], summary_ok=False)
        self.assertEqual(self.kinds(calls), ["summary"])
        self.assertEqual(state["1"]["result"], good)

    def test_llm_budget_spent(self):
        # Analysed without the LLM; this run has the LLM but no budget left:
        # not analysed again (it would be without the LLM again).
        self.run_main([self.pr()])
        state, calls, _ = self.run_main([self.pr()], llm=True, args=["--llm-budget", "0"])
        self.assertEqual(calls, [])
        state, calls, _ = self.run_main([self.pr()], llm=True, args=["--llm-budget", "5"])
        self.assertEqual(self.kinds(calls), ["suggest", "summary"])
        self.assertIn("--llm", calls[0])

    def test_llm_flag_needs_every_answer(self):
        # `llm` is stored only when every unexplained failure got an answer: a PR left with some
        # (--llm-max cut to the run's budget, an API error) gets LLM calls in a later run.
        out = {"suggestions": [{"rule": "x", "variant": "v"}], "ci_unfinished": 0, "llm_unanswered": 1}
        state, calls, _ = self.run_main([self.pr()], llm=True, suggest_out=out)
        self.assertEqual((state["1"]["llm"], state["1"]["llm_unanswered"]), (False, 1))
        state, calls, _ = self.run_main([self.pr()], llm=True, suggest_out=dict(out, llm_unanswered=0))
        self.assertEqual(self.kinds(calls), ["suggest", "summary"])
        self.assertEqual((state["1"]["llm"], state["1"]["llm_unanswered"]), (True, 0))
        state, calls, _ = self.run_main([self.pr()], llm=True)
        self.assertEqual(calls, [])
        # While CI runs, the LLM is not asked (see suggest): no analysis again for it.
        p = self.pr(ci_state="PENDING")
        running = dict(out, ci_unfinished=4)
        state, calls, _ = self.run_main([p], llm=True, suggest_out=running)
        self.assertEqual((state["1"]["llm"], state["1"]["ci_unfinished"]), (False, 4))
        state, calls, _ = self.run_main([p], llm=True, suggest_out=running)
        self.assertEqual(self.kinds(calls), ["summary"])

    def test_llm_retries_limited(self):
        # The LLM asked, no answer came (API errors, the LLM off for the run): analysed again as a
        # retry, MAX_ATTEMPTS times a day. With answers coming (the per-PR limit), analysed again
        # until every failure has its answer.
        out = {"suggestions": [{"rule": "x", "variant": "v"}], "ci_unfinished": 0, "llm_unanswered": 1}
        for i in range(self.T.MAX_ATTEMPTS):
            state, calls, _ = self.run_main([self.pr()], llm=True, tool=lambda a: (dict(out), None, {"http": 1}))
            self.assertEqual([c[0] for c in calls], ["suggest", "summary"], i)
            self.assertEqual(state["1"]["attempts"], i + 1)
        state, calls, _ = self.run_main([self.pr()], llm=True, tool=lambda a: (dict(out), None, {"http": 1}))
        self.assertEqual(calls, [])
        # Two answers this run, one left: analysed again (not a retry), until none is left.
        self.save({})
        for left in (2, 1, 0):
            state, calls, _ = self.run_main([self.pr()], llm=True,
                                            tool=lambda a: (dict(out, llm_unanswered=left), None, {"llm": 2}))
            self.assertEqual([c[0] for c in calls], ["suggest", "summary"], left)
            self.assertEqual((state["1"]["attempts"], state["1"]["llm"]), (1, left == 0))
        state, calls, _ = self.run_main([self.pr()], llm=True)
        self.assertEqual(calls, [])

    def test_llm_not_asked_while_page_unfinished(self):
        # opam-ci says failure, but the CI page had unfinished jobs: the LLM was not asked (see
        # suggest), and the PR is not analysed again for it until something changes.
        out = {"suggestions": [{"rule": "x", "variant": "v"}], "ci_unfinished": 2, "llm_unanswered": 1}
        self.run_main([self.pr()], llm=True, suggest_out=out)
        state, calls, _ = self.run_main([self.pr()], llm=True, suggest_out=out)
        self.assertEqual(calls, [])

    def test_llm_off_for_the_run(self):
        # The tool turned the LLM off (credentials refused), or its API failed LLM_BREAKER times in
        # a row over the PRs analysed so far: the next PRs are analysed without it.
        out = {"suggestions": [{"rule": "x", "variant": "v"}], "ci_unfinished": 0, "llm_unanswered": 1}
        prs = [self.pr(number=n, sha=str(n) * 40) for n in (1, 2, 3)]
        for nets in ([{"llm-off": 1}], [{"llm-streak": 1}, {"llm-streak": 1}],
                     [{"llm-streak": 1, "llm": 1}, {"llm-streak": 1}]):
            self.save({})
            it = iter(nets + [{}] * 3)
            state, calls, _ = self.run_main(prs, llm=True, tool=lambda a: (dict(out), None, next(it)))
            asked = [n for n, c in zip((1, 2, 3), [c for c in calls if c[0] == "suggest"]) if "--llm" in c]
            self.assertEqual(asked, [1, 2, 3][:len(nets)], nets)
            self.assertIsNone(state[str(len(nets) + 1)]["llm_unanswered"])     # analysed without the LLM
        # A success between failures: not in a row.
        self.save({})
        it = iter([{"llm-streak": 1}, {"llm": 1, "llm-streak": 0}, {"llm-streak": 1}])
        state, calls, _ = self.run_main(prs, llm=True, tool=lambda a: (dict(out), None, next(it)))
        self.assertTrue(all("--llm" in c for c in calls if c[0] == "suggest"))

    def test_refresh_not_done(self):
        # The CI page of a rebuild could not be fetched (503 after the retries): the cache still
        # holds the previous build's page and logs, so the next attempt refreshes it again, with
        # a previous result kept or without one; then the cache is not refreshed again.
        down = lambda a: (None, "HTTP Error 503: Service Unavailable", {"http": 1, "net-errors": 1})
        refreshed_down = lambda a: (None, "timeout after 600s", {"timeout": 1, "refreshed": 1})
        self.run_main([self.pr()])
        rebuilt = self.pr(ci_created="2026-11-20T11:00:00Z")
        state, calls, _ = self.run_main([rebuilt], tool=down)
        self.assertEqual(self.kinds(calls), ["suggest --refresh"])
        self.assertFalse(state["1"]["last_error_refreshed"])
        state, calls, _ = self.run_main([rebuilt], tool=refreshed_down)
        self.assertEqual(self.kinds(calls), ["suggest --refresh"])
        self.assertTrue(state["1"]["last_error_refreshed"])
        state, calls, _ = self.run_main([rebuilt])
        self.assertEqual(self.kinds(calls), ["suggest", "summary"])
        # No previous entry (dropped when its CI passed): the failed attempt records that the
        # refresh is still to do, and the attempts add up.
        self.save({})
        state, calls, _ = self.run_main([rebuilt], tool=down)
        self.assertTrue(state["1"]["refresh_pending"])
        state, calls, _ = self.run_main([rebuilt], tool=down)
        self.assertEqual((self.kinds(calls), state["1"]["attempts"]), (["suggest --refresh", "summary"], 2))
        state, calls, _ = self.run_main([rebuilt])
        self.assertEqual(self.kinds(calls), ["suggest --refresh", "summary"])
        self.assertNotIn("refresh_pending", state["1"])
        # A running PR: `summary` refreshed the page.
        self.save({})
        p = self.pr(ci_state="PENDING")
        state, calls, _ = self.run_main([p], tool=down)
        self.assertEqual(self.kinds(calls), ["summary --refresh", "suggest"])
        self.assertNotIn("refresh_pending", state["1"])

    def test_kept_result_during_a_rebuild(self):
        # A result that says "not this PR's fault", then a rebuild runs and its analysis is
        # deferred, or its CI page cannot be read: on hold while CI runs, not ready.
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import render_page as R
        out = {"suggestions": [{"rule": "x", "variant": "v", "class": "elsewhere", "where_kind": "other-package",
                                "preexisting": "on master"}], "ci_unfinished": 0}
        state, _, _ = self.run_main([self.pr()], suggest_out=out)
        self.assertEqual(R.verdict(state["1"]), "ready")
        rebuilding = self.pr(ci_state="PENDING", ci_created="2026-11-20T11:00:00Z")
        state, calls, _ = self.run_main([rebuilding], args=["--time-budget", "-1"])
        self.assertEqual(calls, [])
        self.assertEqual((state["1"]["ci_state"], state["1"]["ci_state_now"]), ("FAILURE", "PENDING"))
        self.assertEqual(R.verdict(state["1"]), "ci")
        self.assertTrue(R.ci_running(state["1"]))
        self.assertIn("CI running</span>", R.render_pr(state["1"]))       # no count of jobs left: unknown
        state, calls, _ = self.run_main([rebuilding], summary_ok=False)
        self.assertEqual(R.verdict(state["1"]), "ci")
        # The rebuild failed again: the result kept is shown for what it is (a rerun to analyse).
        rebuilt = self.pr(ci_created="2026-11-20T11:00:00Z")
        state, calls, _ = self.run_main([rebuilt], suggest_out=out, args=["--time-budget", "-1"])
        self.assertEqual(state["1"]["ci_state_now"], "FAILURE")
        self.assertEqual(R.verdict(state["1"]), "ready")
        # Analysed: the field is gone.
        state, calls, _ = self.run_main([rebuilt], suggest_out=out)
        self.assertNotIn("ci_state_now", state["1"])

    def test_since_last_push_after_a_deferred_or_failed_analysis(self):
        # The first analysis of a new commit is deferred, then fails: what changed since the
        # previous commit is still computed when an analysis of the new commit lands.
        old = {"suggestions": [{"rule": "lint", "variant": "(lint)", "failing": "x.1"},
                               {"rule": "library-api", "variant": "v", "failing": "y.1", "title": "API"}]}
        new = {"suggestions": [{"rule": "library-api", "variant": "v", "failing": "y.1", "title": "API"}]}
        self.run_main([self.pr()], suggest_out=dict(old, ci_unfinished=0))
        b = self.pr(sha="b" * 40)
        state, _, _ = self.run_main([b], args=["--time-budget", "-1"])
        self.assertEqual(state["1"]["since_base"]["sha"], self.SHA)
        state, _, _ = self.run_main([b], suggest_ok=False)
        self.assertIsNone(state["1"]["result"])
        state, _, _ = self.run_main([b], suggest_out=dict(new, ci_unfinished=0))
        since = state["1"]["since_last_push"]
        self.assertEqual((since["previous_sha"], since["still"], since["new"]), (self.SHA, 1, []))
        self.assertNotIn("since_base", state["1"])

    def test_pending_summary_failed_keeps_failed_analysis(self):
        # A running PR whose analysis failed, then whose CI page cannot be read: the entry stays,
        # so its attempts keep adding up.
        p = self.pr(ci_state="PENDING")
        state, _, _ = self.run_main([p], suggest_ok=False)
        self.assertEqual((state["1"]["result"], state["1"]["attempts"]), (None, 1))
        state, calls, _ = self.run_main([p], summary_ok=False)
        self.assertEqual((self.kinds(calls), state["1"]["attempts"]), (["summary"], 1))
        jobs = self.FAILED_ANALYSIS + [{"variant": "b", "status": "passed"}]
        state, calls, _ = self.run_main([p], jobs=jobs, suggest_ok=False)
        self.assertEqual(state["1"]["attempts"], 2)

    def test_git_host_down_asked_again_a_day_later(self):
        # A git host found unreachable is not asked for a day (Sources.git_down); the analysis that
        # skipped it is done again once that day has passed.
        T = self.T
        p = {"sha": "a", "ci_state": "FAILURE"}
        now = time.time()
        at = lambda h: datetime.datetime.fromtimestamp(now - h * 3600, datetime.timezone.utc).isoformat()
        prev = {"sha": "a", "ci_state": "FAILURE", "tool_version": "v", "result": {}, "attempts": 2,
                "git_unreachable": 1, "analysed_at": at(3)}
        self.assertEqual(T.reuse(prev, p, "v", False, now), (True, True, False))
        self.assertEqual(T.reuse(dict(prev, analysed_at=at(25)), p, "v", False, now)[0], False)
        self.assertEqual(T.reuse(dict(prev, git_unreachable=0, analysed_at=at(25)), p, "v", False, now)[0], True)

    def test_refresh_drops_logs(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        import tempfile
        from triage import ci, net
        sha = "eafb2a69786279b98b31f3e5cd8a62ce432cf343"
        with gzip.open(net.cache_path(net.BASE + sha, os.path.join(FIXTURES, "goblint-cil-baseline")) + ".gz",
                       "rt", encoding="utf-8") as f:
            page = f.read()
        fetched = []

        def fake_fetch(url, cache_dir, refresh=False):
            fetched.append((url, refresh))
            return page
        with tempfile.TemporaryDirectory() as d, unittest.mock.patch.object(ci, "fetch", fake_fetch):
            data = ci.parse_commit_page(page)
            job = next(j for j in data["jobs"] if j["status"] == "failed")
            log = net.cache_path(job["url"], d)
            open(log, "w").close()
            ci.load_commit(sha, d)
            self.assertTrue(os.path.exists(log))           # no rebuild: the cached log is kept
            net.NET.pop("refreshed", None)
            ci.load_commit(sha, d)
            self.assertNotIn("refreshed", net.NET)
            ci.load_commit(sha, d, refresh=True)
            self.assertFalse(os.path.exists(log))          # rebuilt: fetched again when needed
            self.assertEqual(fetched, [(net.BASE + sha, False)] * 2 + [(net.BASE + sha, True)])
            self.assertEqual(net.NET.pop("refreshed"), 1)       # said, for the triage page

        # The page could not be fetched: the logs of the previous build stay, and it is not said.
        def down(url, cache_dir, refresh=False):
            raise urllib.error.URLError("503 after the retries")
        with tempfile.TemporaryDirectory() as d, unittest.mock.patch.object(ci, "fetch", down):
            open(net.cache_path(job["url"], d), "w").close()
            with self.assertRaises(urllib.error.URLError):
                ci.load_commit(sha, d, refresh=True)
            self.assertTrue(os.path.exists(net.cache_path(job["url"], d)))
            self.assertNotIn("refreshed", net.NET)


class BoundedOutTest(unittest.TestCase):
    """A PR package that installs nowhere because of an upper bound the PR
    adds is explained as intended, not as a problem to fix."""

    PATCH = """@@ -10,7 +10,7 @@ depends: [
-  "ocaml" {>= "4.02.0"}
+  "ocaml" {>= "4.02.0" & < "4.06"}
   "dune"
-  "cmdliner" {with-test}
+  "cmdliner" {with-test & < "2.0.0"}
+  "fmt" {>= "0.9"}
"""
    LOG = """kinetic-client.0.0.9 is not installed. Install it? [Y/n] y
  * Missing dependency:
    - kinetic-client < 0.0.11 -> ocaml < 4.06 -> ocaml-base-compiler < 4.11.0 | ocaml-system (<= 3.07+1 | = 3.07+2 | >= 3.08.0) | ocaml-variants < 4.05.1~
    no matching version
"""

    def setUp(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        from triage import context
        self.c = context

    def compare(self, patch):
        return {"files": [{"filename": "packages/kinetic-client/kinetic-client.0.0.9/opam",
                           "status": "modified", "patch": patch}]}

    def test_added_bounds(self):
        with unittest.mock.patch.object(self.c, "gh_api", lambda *a, **k: self.compare(self.PATCH)):
            b = self.c.pr_added_bounds("base", "sha", "/nonexistent")
        self.assertEqual(sorted(b["kinetic-client.0.0.9"]), ["cmdliner", "ocaml"])   # not fmt: no upper bound

    def added(self, patch):
        with unittest.mock.patch.object(self.c, "gh_api", lambda *a, **k: self.compare(patch)):
            return sorted(self.c.pr_added_bounds("base", "sha", "/nonexistent").get("kinetic-client.0.0.9") or {})

    def test_only_tightened_depends(self):
        # Only a `depends` line whose upper bound is added or lowered: not a changed lower bound, a
        # loosened bound, nor `conflicts` or `depopts`.
        self.assertEqual(self.added("""@@ -10,7 +10,7 @@ depends: [
-  "ocaml" {>= "4.02.0" & < "5.0"}
+  "ocaml" {>= "4.08.0" & < "5.0"}
-  "lwt" {< "5.0"}
+  "lwt" {< "6.0"}
-  "fmt" {< "0.10"}
+  "fmt" {< "0.9.1"}
 ]
 conflicts: [
-  "base-domains"
+  "base-domains" {< "0.2"}
 ]
 depopts: [
+  "zarith" {< "1.0"}
 ]
"""), ["fmt"])
        # The field comes from the hunk header when the hunk starts inside it.
        self.assertEqual(self.added("""@@ -20,3 +20,3 @@ conflicts: [
-  "base-domains"
+  "base-domains" {< "0.2"}
"""), [])

    def test_unsatisfiable_bound(self):
        # A typo such as `{>= "3.0" & < "2.0"}` excludes every version: a mistake in the PR, not intended.
        [r] = self.run_rule({"kinetic-client.0.0.9": {"ocaml": '"ocaml" {>= "4.07" & < "4.06"}'}})
        self.assertEqual(r["rule"], "uninstallable")
        self.assertEqual(r["where_kind"], "pr")
        self.assertIn("no version of ocaml satisfies it", r["note"])

    class Src:
        def __init__(self, versions):
            self.v = versions

        def versions(self, name):
            return self.v.get(name) if self.v is not None else None

    OCAML = {"ocaml": ["4.02.3", "4.05.0", "4.14.2", "5.4.0"]}

    def run_rule(self, bounds, versions=OCAML):
        jobs = [{"target": "kinetic-client.0.0.9", "kind": "build", "category": "not-available",
                 "variant": f"compilers,{v},kinetic-client.0.0.9", "url": "https://x/" + v}
                for v in ("4.14", "5.4", "5.5")]
        with unittest.mock.patch.object(self.c, "fetch", lambda *a, **k: ""), \
                unittest.mock.patch.object(self.c, "log_text", lambda *a: self.LOG):
            return self.c.skipped_rules({"jobs": jobs}, self.Src(versions), "/nonexistent",
                                        ["kinetic-client.0.0.9"], bounds)

    def test_no_published_version(self):
        # A typo such as `{< "0.3"}` that no published version satisfies: a mistake, not intended.
        [r] = self.run_rule({"kinetic-client.0.0.9": {"ocaml": '"ocaml" {< "4.0"}'}})
        self.assertEqual((r["rule"], r["where_kind"]), ("uninstallable", "pr"))
        self.assertIn("no published version of ocaml satisfies it", r["note"])
        # The versions are unknown (ocaml.org did not answer): not assumed intended either.
        [r] = self.run_rule({"kinetic-client.0.0.9": {"ocaml": '"ocaml" {>= "4.02.0" & < "4.06"}'}}, None)
        self.assertEqual((r["rule"], r["where_kind"]), ("uninstallable", "pr"))
        self.assertIn("could not be read", r["note"])

    def test_intended(self):
        [r] = self.run_rule({"kinetic-client.0.0.9": {"ocaml": '"ocaml" {>= "4.02.0" & < "4.06"}'}})
        self.assertEqual(r["rule"], "bounded-out")
        self.assertNotEqual(r["fix_in"], "this PR")
        self.assertIn('< "4.06"', r["note"])

    def test_bound_with_alternatives_or_variables(self):
        # A bound with alternatives is not checked: not counted as intended. A comparison of a
        # variable (`os = "linux"`) is not a version bound: no "lower bound above an upper bound".
        [r] = self.run_rule({"kinetic-client.0.0.9": {"ocaml": '"ocaml" {< "4.06" | = "4.14.2"}'}})
        self.assertEqual((r["rule"], r["where_kind"], r["confidence"]), ("uninstallable", "pr", "medium"))
        self.assertIn("has alternatives", r["note"])
        [r] = self.run_rule({"kinetic-client.0.0.9": {"ocaml": '"ocaml" {os = "linux" & < "4.06"}'}})
        self.assertEqual(r["rule"], "bounded-out")
        terms = __import__("triage.suggest", fromlist=["x"]).VERSION_TERM.findall
        self.assertEqual(terms('"x" {os = "linux" & >= "1.0" & ocaml:version < "5.0" & os-family != "bsd"}'),
                         [(">=", "1.0")])
        self.assertEqual(terms('>= "1.0" & < "2.0"'), [(">=", "1.0"), ("<", "2.0")])

    def test_not_intended(self):
        # Without a bound added by the PR, it is a problem, and the alternatives
        # of a disjunction are not read as one package's constraint.
        [r] = self.run_rule({})
        self.assertEqual(r["rule"], "uninstallable")
        self.assertNotIn("No version of `ocaml-base-compiler`", r.get("note") or "")
        self.assertIn("`ocaml < 4.06`", r.get("note") or "")


class PageTest(unittest.TestCase):
    """The triage page: which PRs seem ready, labels, the Markdown comment."""

    def setUp(self):
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import render_page
        self.r = render_page

    def pr(self, suggestions, **kw):
        base = {"number": 1, "title": "t", "sha": "a" * 40, "ci_state": "FAILURE", "labels": [],
                "jobs": {"total": 10, "failed": {"build": 1}}, "ci_unfinished": 0,
                "result": {"suggestions": suggestions, "ci_unfinished": 0}}
        base.update(kw)
        return base

    day10 = {"class": "elsewhere", "rule": "builder-specific", "fix_in": "the CI environment (day10 builder)",
             "failing": "x.1", "variant": "v", "title": "Fails only on the day10 builder"}
    revdep = {"class": "elsewhere", "rule": "library-api", "fix_in": "y.1 (other package)",
              "failing": "y.1", "variant": "v", "title": "API change"}
    lint = {"class": "fix", "rule": "lint", "fix_in": "this PR", "failing": "x.1", "variant": "v", "title": "Lint"}

    def test_help_texts(self):
        # Jobs aborted on riscv64 or s390x do not hold a PR back: not "every job ran".
        self.assertNotIn("every job ran", self.r.VERDICT_HELP["ready"])
        self.assertIn("riscv64", self.r.VERDICT_HELP["ready"])
        page = self.r.render_pr(dict(self.pr([self.day10]), git_unreachable=1))
        self.assertIn("a git host that did not answer is asked again a day later", page)
        self.assertNotIn("a few times a day", page)

    def test_seems_ready(self):
        self.assertTrue(self.r.seems_ready(self.pr([self.day10])))
        # A revdep failure is the PR's, unless it happens without the PR.
        self.assertFalse(self.r.seems_ready(self.pr([self.day10, self.revdep])))
        self.assertTrue(self.r.seems_ready(self.pr([self.day10, dict(self.revdep, preexisting="on master")])))
        self.assertTrue(self.r.seems_ready(self.pr([dict(self.revdep, rule="missing-library",
                                                          also_failing_in=[2, 3])])))
        # ... but not when a rule ties it to the PR's package (its API changed), nor for jobs to rerun.
        self.assertFalse(self.r.seems_ready(self.pr([dict(self.revdep, also_failing_in=[2, 3])])))
        rerun = {"class": "elsewhere", "rule": "rerun", "fix_in": "CI (rerun)", "where_kind": "rerun",
                 "variant": "1 job"}
        self.assertFalse(self.r.seems_ready(self.pr([dict(rerun, also_failing_in=[2, 3, 4])])))
        # ... but not against evidence that it is this PR's (stored before mark_shared_failures skipped them).
        new = {"class": "fix", "rule": "health-check-new", "fix_in": "this PR", "failing": "y.1", "variant": "v"}
        self.assertFalse(self.r.seems_ready(self.pr([dict(new, also_failing_in=[2, 3, 4])])))
        self.assertFalse(self.r.seems_ready(self.pr([dict(self.lint, also_failing_in=[2, 3, 4])])))
        self.assertTrue(self.r.seems_ready(self.pr([dict(self.lint, experimental=True)])))
        # A lint job that failed before linting did not check anything: not ready, to rerun.
        self.assertFalse(self.r.seems_ready(self.pr([{"class": "elsewhere", "rule": "lint-ci-error",
                                                       "fix_in": "CI (rerun)", "variant": "(lint)"}])))
        self.assertFalse(self.r.seems_ready(self.pr([self.lint])))
        # Unexplained failures (only skipped jobs), CI not finished, drafts, labels.
        self.assertFalse(self.r.seems_ready(self.pr([])))
        self.assertFalse(self.r.seems_ready(self.pr([self.day10], ci_unfinished=2)))
        # Distinct failures beyond --max were not analysed: nothing says they are not the PR's.
        unanalysed = self.pr([self.day10])
        unanalysed["result"]["unanalysed"] = 3
        self.assertFalse(self.r.seems_ready(unanalysed))
        self.assertIn("3 more failures were not analysed", self.r.render_pr(unanalysed))
        self.assertIn("3 more failures were not analysed.", self.r.comment_markdown(unanalysed))
        self.assertFalse(self.r.seems_ready(self.pr([self.day10], draft=True)))
        self.assertFalse(self.r.seems_ready(self.pr([self.day10], labels=["needs reporter action"])))
        self.assertTrue(self.r.seems_ready(self.pr([self.day10], labels=["consider for merge"])))
        # A "needs reporter action" label the author acted on since may be stale.
        self.assertTrue(self.r.seems_ready(self.pr([self.day10], labels=["needs reporter action"],
                                                   stale_labels=["needs reporter action"])))
        self.assertIn("(author replied since)", self.r.label_badges(["needs reporter action"],
                                                                 ["needs reporter action"]))

    def test_pending_status_with_finished_page(self):
        # opam-ci's status stays pending when a job never starts: the CI page
        # has finished (no unfinished job), so the PR is not "CI running".
        done = self.pr([self.day10], ci_state="PENDING", ci_unfinished=0)
        self.assertTrue(self.r.seems_ready(done))
        self.assertNotIn("CI running", self.r.render_pr(done))
        running = self.pr([self.day10], ci_state="PENDING", ci_unfinished=3)
        self.assertFalse(self.r.seems_ready(running))
        self.assertIn("CI running, 3 jobs left", self.r.render_pr(running))

    def test_kept_result_after_a_failed_analysis(self):
        pr = self.pr([self.day10], last_error="timeout after 600s <b>", last_error_at="2026-11-20T10:00:00+00:00")
        html = self.r.render_pr(pr)
        self.assertIn("a newer analysis failed 2026-11-20 10:00 UTC; this result may be out of date", html)
        self.assertIn('title="timeout after 600s &lt;b&gt;"', html)
        self.assertNotIn("newer analysis failed", self.r.render_pr(self.pr([self.day10])))

    def test_incomplete_analysis_shown(self):
        # Failed lookups (ocaml.org, HTTP, git, a git host down earlier today) are shown.
        html = self.r.render_pr(self.pr([self.day10], mcp_errors=2, net_errors=1, git_unreachable=1))
        self.assertIn("analysis incomplete (4 failed lookups)", html)
        self.assertIn("analysis incomplete (1 failed lookup)", self.r.render_pr(self.pr([self.day10], net_errors=1)))
        self.assertNotIn("analysis incomplete", self.r.render_pr(self.pr([self.day10], mcp_errors=0)))
        self.assertNotIn("analysis incomplete", self.r.render_pr(self.pr([self.day10], net_errors=1, pending=True)))

    def test_shared_failures(self):
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import triage_open_prs as T
        rd = "compilers,5.4,x.1,revdeps,{}"

        def entry(n, failing, *suggestions, target="x.1", category="build"):
            # The failing reverse dependencies of a PR testing `target`, as jobs_overview stores them.
            return {"number": n,
                    "jobs": {"failing_revdeps": {r: [{"variant": f"compilers,5.4,{target},revdeps,{r}",
                                                      "category": category, "target": target}]
                                                 for r in failing}},
                    "result": {"suggestions": [{"variant": "v", "jobs": js} for js in suggestions]}}
        prs = {"1": entry(1, ["a.1", "b.1", "c.1", "d.1"],
                          [rd.format("a.1"), rd.format("b.1"), rd.format("c.1")],   # each in one other PR
                          [rd.format("d.1")],                                         # in 3 other PRs
                          [rd.format("d.1"), "compilers,5.4,x.1"],                    # and a non-revdep job
                          [rd.format("a.1"), rd.format("d.1")]),
               "2": entry(2, ["a.1", "d.1"], target="y.1"), "3": entry(3, ["b.1", "d.1"], target="z.2"),
               "4": entry(4, ["c.1", "d.1"], target="w.1")}
        T.mark_shared_failures(prs)
        got = [s.get("also_failing_in") for s in prs["1"]["result"]["suggestions"]]
        self.assertEqual(got, [None, [2, 3, 4], None, None])

        # Other releases of the same package do not vouch for each other (two lwt releases break the
        # same reverse dependencies), nor do other PRs where the reverse dependency fails another way.
        def shared(*others, suggestion=None):
            prs = {"1": entry(1, ["d.1"], [rd.format("d.1")])}
            prs["1"]["result"]["suggestions"] = [dict(suggestion or {}, variant="v", jobs=[rd.format("d.1")])]
            for k, (target, category) in enumerate(others, 2):
                prs[str(k)] = entry(k, ["d.1"], target=target, category=category)
            T.mark_shared_failures(prs)
            return prs["1"]["result"]["suggestions"][0].get("also_failing_in")
        self.assertEqual(shared(("y.1", "build"), ("z.1", "build"), ("w.1", "build")), [2, 3, 4])
        self.assertIsNone(shared(("x.2", "build"), ("x.3", "build"), ("w.1", "build")))
        self.assertIsNone(shared(("y.1", "dependency"), ("z.1", "build"), ("w.1", "build")))
        # Two releases of one package (y) and one of another: two packages, not three PRs' evidence.
        self.assertIsNone(shared(("y.1", "build"), ("y.2", "build"), ("w.1", "build")))
        self.assertEqual(shared(("y.1", "build"), ("y.2", "build"), ("w.1", "build"), ("v.1", "build")),
                         [2, 3, 4, 5])
        # PRs releasing several packages each (dune, dune-configurator, dune-site): their tested
        # packages overlap, so three such PRs are one piece of evidence (the reviewer's scenario).
        def multi(n, targets, category="build"):
            return {"number": n, "jobs": {"failing_revdeps": {"X.1": [
                {"variant": f"compilers,5.3,{t},revdeps,X.1", "category": c, "target": t, "url": "u"}
                for t, c in targets]}}, "result": {"suggestions": []}}
        dune = lambda n, v: multi(n, [(f"{p}.{v}", "build") for p in ("dune", "dune-configurator", "dune-site")])
        mine = {"rule": "missing-library", "where_kind": "other-package", "where_pkg": "X.1", "class": "elsewhere",
                "variant": "compilers,5.3,foo.1,revdeps,X.1", "jobs": ["compilers,5.3,foo.1,revdeps,X.1"]}
        prs = {"1": dune(1, "3.1"), "2": dune(2, "3.2"), "3": dune(3, "3.3"),
               "4": dict(multi(4, [("foo.1", "build")]), result={"suggestions": [dict(mine)]})}
        T.mark_shared_failures(prs)
        self.assertIsNone(prs["4"]["result"]["suggestions"][0].get("also_failing_in"))
        # A PR releasing bar, and one releasing dune-site alone (it overlaps the dune PRs): two
        # independent PRs. A third one (baz) makes it shared; every PR that fails the same way is listed.
        prs.update({"5": multi(5, [("bar.1", "build")]), "6": multi(6, [("dune-site.3.4", "build")])})
        T.mark_shared_failures(prs)
        self.assertIsNone(prs["4"]["result"]["suggestions"][0].get("also_failing_in"))
        prs["7"] = multi(7, [("baz.1", "build")])
        T.mark_shared_failures(prs)
        self.assertEqual(prs["4"]["result"]["suggestions"][0].get("also_failing_in"), [1, 2, 3, 5, 6, 7])
        # Only the jobs that fail the same way name the tested packages: another
        # package failing differently in the same PR does not add evidence.
        prs = {"2": multi(2, [("y.1", "build"), ("a.1", "dependency")]),
               "3": multi(3, [("y.2", "build"), ("b.1", "dependency")]), "4": multi(4, [("w.1", "build")]),
               "1": dict(multi(1, [("foo.1", "build")]), result={"suggestions": [dict(mine)]})}
        T.mark_shared_failures(prs)
        self.assertIsNone(prs["1"]["result"]["suggestions"][0].get("also_failing_in"))
        # Never on a failure the evidence puts on this PR: it builds on master.
        self.assertIsNone(shared(("y.1", "build"), ("z.1", "build"), ("w.1", "build"),
                                 suggestion={"rule": "health-check-new", "fix_in": "this PR"}))
        self.assertIsNone(shared(("y.1", "build"), ("z.1", "build"), ("w.1", "build"),
                                 suggestion={"rule": "library-api", "fix_in": "this PR", "where_kind": "pr"}))
        # Nor when a rule ties the failure to the PR's package (its API changed, a bound on it), or
        # suggests a change for it, even with the fix in the reverse dependency.
        for sugg in ({"rule": "library-api"}, {"rule": "previous-release-bound"}, {"rule": "lower-bound"},
                     {"rule": "missing-library", "new": '"x" {< "1"}'}):
            self.assertIsNone(shared(("y.1", "build"), ("z.1", "build"), ("w.1", "build"),
                                     suggestion=dict(sugg, fix_in="d.1 (other package)",
                                                     where_kind="other-package", where_pkg="d.1")), sugg)
        self.assertEqual(shared(("y.1", "build"), ("z.1", "build"), ("w.1", "build"),
                                suggestion={"rule": "missing-library", "fix_in": "d.1 (other package)",
                                            "where_kind": "other-package", "where_pkg": "d.1"}), [2, 3, 4])
        # Nor on jobs to rerun: cancelled jobs are not failing revdeps, so nothing says how they fail.
        self.assertIsNone(shared(("y.1", "build"), ("z.1", "build"), ("w.1", "build"),
                                 suggestion={"rule": "rerun", "fix_in": "CI (rerun)", "where_kind": "rerun"}))
        prs = {"1": {"number": 1, "jobs": {"failing_revdeps": {}}, "result": {"suggestions": [
            {"rule": "rerun", "where_kind": "rerun", "variant": "1 job", "jobs": [rd.format("d.1")]}]}}}
        for k, t in ((2, "y.1"), (3, "z.1"), (4, "w.1")):
            prs[str(k)] = entry(k, ["d.1"], target=t)
        T.mark_shared_failures(prs)
        self.assertIsNone(prs["1"]["result"]["suggestions"][0].get("also_failing_in"))

    def test_shared_failures_same_platform(self):
        # The other PRs must fail on the same platform as each job of the suggestion: a reverse
        # dependency failing elsewhere on OCaml 5.5 only (as on master) says nothing about its
        # failure on OCaml 4.14 here.
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import triage_open_prs as T

        def job(plat, target, rd="bar.1"):
            return {"variant": f"compilers,{plat},{target},revdeps,{rd}", "category": "build", "target": target}
        mine = [job("4.14", "foo.2"), job("5.5", "foo.2")]
        sugg = {"rule": "missing-library", "where_kind": "other-package", "where_pkg": "bar.1",
                "class": "elsewhere", "variant": "2 jobs", "jobs": [j["variant"] for j in mine]}

        def run(plats):
            prs = {"1": {"number": 1, "jobs": {"failing_revdeps": {"bar.1": mine}},
                         "result": {"suggestions": [dict(sugg)]}}}
            for n, t in ((2, "lwt.6"), (3, "dune.3"), (4, "cmdliner.2")):
                prs[str(n)] = {"number": n, "jobs": {"failing_revdeps": {"bar.1": [job(p, t) for p in plats]}},
                               "result": {"suggestions": []}}
            T.mark_shared_failures(prs)
            return prs["1"]["result"]["suggestions"][0].get("also_failing_in")
        self.assertIsNone(run(["5.5"]))
        self.assertIsNone(run(["4.14-flambda", "5.5"]))
        self.assertEqual(run(["4.14", "5.5"]), [2, 3, 4])
        self.assertEqual(T.job_platform("compilers,4.14,foo.2,revdeps,bar.1"), "compilers,4.14,")

    def test_stale_labels(self):
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import triage_open_prs as T
        lab = {"__typename": "LabeledEvent", "createdAt": "2026-11-10T00:00:00Z",
               "label": {"name": "needs reporter action"}}
        def node(*items):
            return {"author": {"login": "me"}, "labels": {"nodes": [{"name": "needs reporter action"}]},
                    "timelineItems": {"nodes": [lab, *items]}}
        self.assertEqual(T.stale_labels(node()), [])
        removed = dict(node({"__typename": "HeadRefForcePushedEvent", "createdAt": "2026-11-11T00:00:00Z"}),
                       labels={"nodes": []})
        self.assertEqual(T.stale_labels(removed), [])
        self.assertEqual(T.stale_labels(node({"__typename": "HeadRefForcePushedEvent",
                                              "createdAt": "2026-11-11T00:00:00Z"})), ["needs reporter action"])
        self.assertEqual(T.stale_labels(node({"__typename": "PullRequestCommit",
                                              "commit": {"committedDate": "2026-11-11T00:00:00Z"}})),
                         ["needs reporter action"])
        # A comment counts only from the author, and only after the label.
        self.assertEqual(T.stale_labels(node({"__typename": "IssueComment", "createdAt": "2026-11-11T00:00:00Z",
                                              "author": {"login": "maintainer"}})), [])
        self.assertEqual(T.stale_labels(node({"__typename": "IssueComment", "createdAt": "2026-11-09T00:00:00Z",
                                              "author": {"login": "me"}})), [])
        self.assertEqual(T.stale_labels(node({"__typename": "IssueComment", "createdAt": "2026-11-11T00:00:00Z",
                                              "author": {"login": "me"}})), ["needs reporter action"])

    def test_ready_section_and_labels(self):
        data = {"prs": [self.pr([self.day10], number=5, labels=["question"]), self.pr([self.lint], number=6)],
                "passing": [{"number": 7, "title": "p", "labels": ["consider for merge"], "updated": "2026-01-01"},
                            {"number": 8, "title": "d", "labels": [], "draft": True},
                            {"number": 9, "title": "n", "labels": ["do not merge"]}]}
        html_ = self.r.ready_section(data)
        self.assertIn("(2)", html_)
        self.assertLess(html_.index("/pull/7"), html_.index("/pull/5"))   # labelled first
        for n in (6, 8, 9):
            self.assertNotIn(f"/pull/{n}", html_)
        self.assertNotIn("<table", html_)                                  # a list, which wraps on a phone
        self.assertIn('href="#pr-5"', html_)
        # More than `shown`: the rest is folded.
        many = {"passing": [{"number": n, "title": "p", "labels": []} for n in range(10, 17)]}
        self.assertIn("2 more</summary>", self.r.ready_section(many, shown=5))
        page = self.r.render_pr(self.pr([self.day10], labels=["consider for merge"]))
        self.assertIn('data-v="ready"', page)
        self.assertIn("data-merge", page)
        self.assertIn("&lt;b&gt;", self.r.label_badges(["<b>"]))

    def test_verdict(self):
        v = self.r.verdict
        workaround = dict(self.lint, **{"class": "workaround", "rule": "platform-matrix", "new": 'available: os != "win32"'})
        self.assertEqual(v(self.pr([self.day10])), "ready")
        self.assertEqual(v(self.pr([self.lint, self.revdep])), "change")          # change first, then blocked
        self.assertEqual(v(self.pr([workaround])), "change")
        self.assertEqual(v(self.pr([self.day10, self.revdep])), "blocked")
        self.assertEqual(v(self.pr([self.day10], ci_unfinished=3)), "ci")          # not ready while CI runs
        self.assertEqual(v(self.pr([dict(self.day10, **{"class": "explanation", "fix_in": "this PR",
                                                        "rule": "uninstallable"})])), "other")
        self.assertEqual(v(self.pr([], pending=True)), "error")
        self.assertEqual(v(self.pr([], result=None)), "error")
        # A draft is never ready: then "on hold".
        self.assertEqual(v(self.pr([self.day10], draft=True)), "ci")

    def test_ci_only_is_an_allow_list(self):
        # "On hold" (key "ci") needs every failure to be not the PR's; anything else
        # not this PR's change nor another package's needs a look.
        v = self.r.verdict
        draft = {"draft": True}           # never ready: the verdicts below are not "ready"
        self.assertEqual(v(self.pr([self.day10], **draft)), "ci")
        # No suggestion at all, though jobs failed (a failed analysis job, stored before its rule).
        empty = self.pr([], jobs={"total": 2, "failed": {"analysis": 1}})
        self.assertEqual(v(empty), "other")
        self.assertIn("no explanation for the failed jobs (1 analysis failed)", self.r.headline(empty))
        self.assertIn("No explanation for the failed jobs", self.r.render_pr(empty))
        unknown = {"class": "explanation", "rule": "unrecognised", "fix_in": "unknown", "failing": "x.1",
                   "variant": "1 job"}
        self.assertEqual(v(self.pr([self.day10, unknown], **draft)), "other")
        self.assertEqual(v(self.pr([dict(unknown, rule="rule-error")], **draft)), "other")
        systemic = {"class": "explanation", "rule": "platform-matrix-systemic", "variant": "6 packages [p]",
                    "fix_in": "the PR, a shared dependency, or the CI"}
        self.assertEqual(v(self.pr([systemic], **draft)), "other")
        # ... unless every package fails through one dependency: the fix goes there.
        self.assertEqual(v(self.pr([dict(systemic, failing_dep="dep.1")])), "blocked")
        own_sources = {"class": "elsewhere", "rule": "sources", "failing": "x.1", "fix_in": "x.1 (upstream archive)",
                       "variant": "v"}
        self.assertEqual(v(self.pr([own_sources], **draft)), "other")
        llm_upstream = {"class": "elsewhere", "rule": "llm", "failing": "x.1", "variant": "v",
                        "fix_in": "upstream (the package's source)"}
        self.assertEqual(v(self.pr([self.day10, llm_upstream], **draft)), "other")
        # Failures beyond --max were not analysed: they may be anything.
        unanalysed = self.pr([self.day10], **draft)
        unanalysed["result"]["unanalysed"] = 2
        self.assertEqual(v(unanalysed), "other")

    def test_on_hold(self):
        # No failure is the PR's, but it is a draft, labelled, or its CI runs: "not this PR's, on
        # hold", whatever makes the failures not the PR's (it was "needs a look" unless all came from the CI).
        v = self.r.verdict
        pre = dict(self.revdep, preexisting="on master")
        hc = {"class": "elsewhere", "rule": "health-check-preexisting", "fix_in": "y.1 (other package)",
              "where_kind": "other-package", "failing": "y.1", "variant": "v"}
        for kw in ({"draft": True}, {"labels": ["do not merge"]}, {"ci_unfinished": 3}):
            self.assertEqual(v(self.pr([pre], **kw)), "ci", kw)
            exp = {"class": "explanation", "rule": "unrecognised", "fix_in": "unknown", "variant": "v",
                   "experimental": True}
            self.assertEqual(v(self.pr([hc, exp], **kw)), "ci", kw)
            self.assertEqual(v(self.pr([self.day10, pre], **kw)), "ci", kw)
        self.assertIn("No failure is this PR's fault", self.r.VERDICT_HELP["ci"])
        self.assertEqual(self.r.VERDICT_LABEL["ci"], "On hold")

    def test_llm_never_ci_side(self):
        # An old stored LLM answer saying "the CI environment" (its where_kind is ci): not a reason
        # for "not this PR's fault", nor for "on hold" or "needs a rerun".
        llm_ci = {"class": "elsewhere", "rule": "llm", "failing": "x.1", "variant": "v",
                  "fix_in": "the CI environment"}
        self.assertEqual(self.r.where_kind(llm_ci), "ci")
        self.assertFalse(self.r.not_this_pr(llm_ci))
        self.assertFalse(self.r.seems_ready(self.pr([self.day10, llm_ci])))
        self.assertEqual(self.r.verdict(self.pr([self.day10, llm_ci])), "other")
        self.assertEqual(self.r.verdict(self.pr([self.day10, llm_ci], draft=True)), "other")
        rerun = {"class": "elsewhere", "rule": "rerun", "fix_in": "CI (rerun)", "variant": "7 jobs"}
        self.assertEqual(self.r.verdict(self.pr([rerun, llm_ci])), "other")
        # Still shared with other PRs, pre-existing or experimental: those do not depend on the model.
        self.assertTrue(self.r.not_this_pr(dict(llm_ci, experimental=True)))

    def test_needs_a_rerun(self):
        # Jobs that failed because of the CI were not tested: such a PR is not ready, even when nothing
        # else points at it.
        v = self.r.verdict
        rerun = {"class": "elsewhere", "rule": "rerun", "fix_in": "CI (rerun)", "variant": "7 jobs",
                 "error": "4 cancelled, 3 network errors (curl failed)"}
        for sugg in ([rerun], [self.day10, rerun], [dict(self.revdep, preexisting="on master"), rerun]):
            pr = self.pr(sugg)
            self.assertFalse(self.r.seems_ready(pr))
            self.assertEqual(v(pr), "rerun")
        self.assertEqual(self.r.headline(self.pr([self.day10, rerun]), as_html=False),
                         "Jobs to rerun (4 cancelled, 3 network errors (curl failed))")
        lint_ci = {"class": "elsewhere", "rule": "lint-ci-error", "fix_in": "CI (rerun)", "variant": "(lint)"}
        self.assertEqual(v(self.pr([lint_ci])), "rerun")
        # Reruns of experimental jobs only do not hold a PR back.
        self.assertEqual(v(self.pr([dict(rerun, experimental=True)])), "ready")
        # Anything else of the PR comes first.
        self.assertEqual(v(self.pr([self.lint, rerun])), "change")
        self.assertEqual(v(self.pr([self.revdep, rerun])), "blocked")
        self.assertEqual(v(self.pr([dict(self.lint, new=None, **{"class": "explanation"}), rerun])), "other")
        page = self.r.render({"prs": [self.pr([rerun])], "generated": "2026-11-20T10:00:00+00:00"})
        self.assertIn('data-f="rerun"', page)
        self.assertIn('.v-rerun{', page)
        self.assertNotIn('<section id="ready">', page)
        self.assertIn("Needs a rerun", self.r.help_section({}))

    def test_bounded_out(self):
        # A checked bound (where_kind set by the rule) is the point of the change; one stored before the
        # check (no where_kind) keeps the PR out of the ready list.
        b = {"class": "elsewhere", "rule": "bounded-out", "fix_in": "nothing (this PR's bound)", "failing": "x.1",
             "variant": "x.1 [all build jobs skipped]"}
        self.assertTrue(self.r.seems_ready(self.pr([dict(b, where_kind="none")])))
        self.assertFalse(self.r.seems_ready(self.pr([b])))
        self.assertEqual(self.r.verdict(self.pr([b])), "other")

    def test_unknown_is_an_explanation(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        from triage import suggest
        self.assertEqual(suggest.classify({"rule": "unrecognised", "fix_in": "unknown"}), "explanation")
        self.assertEqual(suggest.classify({"rule": "platform-matrix-systemic",
                                           "fix_in": "the PR, a shared dependency, or the CI"}), "explanation")
        self.assertEqual(suggest.classify({"rule": "platform-matrix-systemic", "failing_dep": "d.1",
                                           "fix_in": "the PR, a shared dependency, or the CI"}), "elsewhere")
        self.assertEqual(suggest.classify({"rule": "builder-specific-systemic",
                                           "fix_in": "the CI environment (day10 builder)"}), "elsewhere")

    def test_verdict_counts_add_up(self):
        prs = [self.pr([self.day10], number=1), self.pr([self.lint, self.revdep], number=2),
               self.pr([self.day10, self.revdep], number=3), self.pr([], number=4, pending=True),
               self.pr([self.day10], number=5, draft=True), self.pr([self.lint], number=6, ci_state="PENDING",
                                                                     labels=["consider for merge"])]
        page = self.r.render({"prs": prs, "generated": "2026-11-20T10:00:00+00:00", "open_prs": 9,
                              "failing_prs": 6})
        counts = [int(c) for c in re.findall(r'data-f="(?!all)\w+"[^>]*>.*?<b>(\d+)</b>', page)]
        self.assertEqual(sum(counts), len(prs))
        self.assertIn(f"All<b>{len(prs)}</b>", page)
        self.assertEqual(page.count('<details class="pr '), len(prs))
        self.assertIn("consider for merge (1)", page)
        self.assertIn("CI running", page)
        self.assertNotIn('class="stat', page)
        self.assertNotIn('id="expand"', page)
        self.assertIn('rel="alternate" type="application/atom+xml"', page)
        feed = self.r.render_feed({"prs": prs})
        self.assertIn("Change this PR: ", feed)

    def test_merge_similar(self):
        a = dict(self.day10, failing="a.1", jobs=["day10,a,1", "day10,a,2"], note="a.1 note", error="E")
        b = dict(self.day10, failing="b.1", jobs=["day10,b,1"], note="b.1 note", error="E", also_failing_in=[7])
        c = dict(self.revdep)
        llm = {"class": "elsewhere", "rule": "llm", "title": "LLM", "fix_in": "x", "failing": "c.1"}
        merged = self.r.merge_similar([a, c, b, llm, dict(llm, failing="d.1")])
        self.assertEqual(len(merged), 4)                     # a and b merged; LLM answers never
        m = merged[0]
        self.assertEqual(m["merged"], 2)
        self.assertEqual(m["packages"], ["a.1", "b.1"])
        self.assertEqual(m["jobs"], ["day10,a,1", "day10,a,2", "day10,b,1"])
        self.assertEqual(m["errors"], ["E"])
        self.assertEqual(m["notes"], ["a.1 note", "b.1 note"])
        self.assertEqual(m["also_failing_in"], [7])
        self.assertIs(merged[1], c)
        # A different new constraint is a different suggestion.
        self.assertEqual(len(self.r.merge_similar([dict(self.lint, new="x"), dict(self.lint, new="y")])), 2)
        item = self.r.render_item(m, "a" * 40)
        self.assertIn("2 failures, 3 jobs", item)
        self.assertIn("a.1, b.1", item)
        self.assertNotIn("Fix in: this PR", item)
        page = self.r.render_pr(self.pr([a, b], number=3))
        self.assertIn("Fix goes elsewhere (1)", page)
        self.assertEqual(page.count('<details class="it">'), 1)

    def test_compact_job_links(self):
        link = self.r.job_link("a" * 40, "compilers,5.4,x.1")
        self.assertIn('data-job="compilers,5.4,x.1"', link)
        self.assertNotIn("href", link)
        self.assertNotIn("<a", self.r.job_link("a" * 40, "x\"><script>"))     # not a variant: plain text
        page = self.r.render_pr(self.pr([dict(self.day10, jobs=["v1", "v2"])]))
        self.assertIn(f'data-ci="{self.r.BASE}{"a" * 40}/variant/"', page)

    def test_headline_blocked_without_package(self):
        depext = {"class": "elsewhere", "rule": "missing-system-package", "failing": "p.1",
                  "fix_in": "the dependency declaring these depexts", "variant": "v",
                  "title": "System package not found"}
        pr = self.pr([depext])
        self.assertEqual(self.r.verdict(pr), "blocked")
        h = self.r.headline(pr, as_html=False)
        self.assertNotIn("the dependency declaring", h)
        self.assertIn("System package not found (p.1)", h)

    def test_where_kind(self):
        # The verdict, the headline and the shared-failures table follow
        # where_kind and where_pkg, whatever the words of fix_in.
        other = {"class": "elsewhere", "rule": "library-api", "failing": "y.1", "variant": "v",
                 "fix_in": "the y maintainers", "where_kind": "other-package", "where_pkg": "y.1"}
        self.assertEqual(self.r.verdict(self.pr([self.day10, other])), "blocked")
        self.assertEqual(self.r.headline(self.pr([self.day10, other]), as_html=False), "needs a fix in y.1")
        not_other = dict(self.revdep, where_kind="pr")
        self.assertNotEqual(self.r.verdict(self.pr([self.day10, not_other])), "blocked")
        table = self.r.chronic([self.pr([other], number=n) for n in (1, 2)])
        self.assertIn("<b>y.1</b>", table)

    def test_skipped_jobs(self):
        pr = self.pr([dict(self.lint, rule="uninstallable", new=None, **{"class": "explanation"})],
                     jobs={"total": 10, "failed": {"not-available": 9}})
        page = self.r.render_pr(pr)
        self.assertIn("9 skipped jobs", page)
        self.assertNotIn("0 failed jobs", page)

    def test_lint_warning_title(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        from triage import context, suggest
        data = {"jobs": [{"kind": "lint", "status": "failed", "url": "u", "target": None, "messages": [
            "Warning in x.1: Unnecessary field 'version'", "Error in x.1: Missing field 'license'"]}]}
        rs = context.lint_rules(data, None)
        self.assertEqual([suggest.title_of(r) for r in rs], ["opam lint warning", "opam lint error"])

    def test_stale_health_check_note(self):
        pr = self.pr([self.lint])
        pr["result"]["health_stale"] = "2026-10-01"
        self.assertIn("opam-health-check's latest results are from", self.r.render_pr(pr))
        self.assertNotIn("opam-health-check's latest", self.r.render_pr(self.pr([self.lint])))

    def test_headline(self):
        h = self.r.headline
        fix = dict(self.lint, new='"ocaml" {>= "5.1"}   (a comment)', failing="colibri2.0.7")
        workaround = dict(self.lint, **{"class": "workaround", "new": 'available: os != "win32"'})
        self.assertEqual(h(self.pr([fix])), '<code>&quot;ocaml&quot; {&gt;= &quot;5.1&quot;}</code> for colibri2.0.7')
        self.assertIn("for x.1 (workaround)", h(self.pr([workaround])))
        self.assertIn("also blocked by y.1", h(self.pr([fix, self.revdep])))
        self.assertEqual(h(self.pr([self.day10, self.revdep])), "needs a fix in y.1")
        two = [dict(self.revdep, fix_in=f"z.{i} (other package)") for i in (1, 2)] + [self.revdep]
        self.assertEqual(h(self.pr(two)), "needs fixes in z (2 versions), y.1")
        self.assertEqual(h(self.pr([self.day10])), "Fails only on the day10 builder (x.1)")
        self.assertEqual(h(self.pr([fix]), as_html=False), '`"ocaml" {>= "5.1"}` for colibri2.0.7')
        self.assertIn("&lt;", h(self.pr([dict(self.day10, rule="<b>")])))     # an unknown rule is its own title
        # Fix in "this PR" goes without saying; confidence only when not high.
        card = self.r.render_suggestion(dict(fix, confidence="high"))
        self.assertNotIn("Fix in", card)
        self.assertNotIn("confidence", card)
        self.assertIn("medium confidence", self.r.render_suggestion(dict(fix, confidence="medium")))

    def test_changelog_in_comment(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        import opam_repo_ci_triage as t
        c = {"dep": "x", "changelog": {"file": "CHANGES", "versions": ["2.0.0", "<img src=a>"],
                                       "text": "- uses ``` fences\n- @someone [link](https://e.x)", "cut": True}}
        md = t.changelog_details(c)
        self.assertIn("````text\n", md)              # a fence longer than any backtick run inside
        self.assertTrue(md.rstrip().endswith("````\n\n</details>".rstrip()))
        self.assertNotIn("<img", md)                   # not a version: left out of the summary
        self.assertIn("\n…\n", md)
        page = self.r.render_suggestion({"rule": "test-output", "candidates": [dict(c, new='"x" {< "2.0.0"}')]})
        self.assertIn("&lt;img src=a&gt;", page)
        self.assertNotIn("<img", page)

    def test_comment_markdown(self):
        md = self.r.comment_markdown(self.pr([self.day10]))
        self.assertIn("None of the failures is this PR's fault", md)
        llm_low = {"class": "fix", "rule": "llm", "confidence": "low", "fix_in": "this PR", "failing": "z.1",
                   "variant": "v", "title": "LLM diagnosis (unchecked)"}
        md = self.r.comment_markdown(self.pr([self.lint, llm_low]))
        self.assertNotIn("None of the failures", md)
        self.assertNotIn("z.1", md)
        page = self.r.render_pr(self.pr([self.lint]))
        self.assertIn('data-for="md-1"', page)
        self.assertIn("</textarea>", page)
        self.assertEqual(page.count("<textarea"), 1)
        # Only workarounds: the comment says so, as the verdict does ("Change this PR").
        workaround = dict(self.lint, **{"class": "workaround", "rule": "platform-matrix"})
        md = self.r.comment_markdown(self.pr([workaround]))
        self.assertIn("The workarounds below skip", md)
        self.assertNotIn("No change to this PR", md)


HOSTILE = "@ocaml/maintainers ![x](https://evil.example/x.png) <img src=x> see #123, ocaml/dune#4 GH-5 www.evil.example"
ALLOWED_TAGS = re.compile(r"</?(details|summary|sub|br)>")
TRUSTED_LINK = re.compile(r"\]\(https://opam\.ci\.ocaml\.org/[\w.~,+%/=-]+\)")


def outside_code(md):
    """What GitHub would parse as Markdown in `md`: without fenced code blocks,
    code spans and backslash-escaped characters (a rough CommonMark reading)."""
    out, fence = [], None
    for line in md.split("\n"):
        m = re.match(r"\s*(`{3,})", line)
        if fence:
            if m and line.strip() == m.group(1) and len(m.group(1)) >= len(fence):
                fence = None
            continue
        if m:
            fence = m.group(1)
            continue
        out.append(line)
    text, res, i = "\n".join(out), [], 0
    while i < len(text):
        if text[i] == "\\" and i + 1 < len(text) and not text[i + 1].isalnum():
            i += 2
            continue
        if text[i] == "`":
            run = re.match(r"`+", text[i:]).group(0)
            m = re.compile(r"(?<!`)" + run + r"(?!`)").search(text, i + len(run))
            if m:
                i = m.end()
                continue
            res.append(run)
            i += len(run)
            continue
        res.append(text[i])
        i += 1
    return "".join(res)


class LlmFallbackTest(unittest.TestCase):
    """The LLM fallback (no API call: llm_suggest or the SDK are faked): which
    failures it gets, which changes are shown as a diff, which package names
    are kept, and errors that leave the failure unrecognised."""

    JOB = {"variant": "v", "target": "x.1", "revdep": None, "kind": "build"}

    def setUp(self):
        import tempfile
        sys.path.insert(0, os.path.join(HERE, ".."))
        import triage.llm as L
        self.L = L
        self.tmp = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.tmp, True)
        # The LLM's state is process-wide (its breaker): each test starts with it on.
        L.LLM_STATE.update(streak=0, off=None)
        self.addCleanup(L.LLM_STATE.update, streak=0, off=None)

    def result(self, **kw):
        return {"variant": "v", "url": "https://opam.ci.ocaml.org/x", "note": "Unrecognised error.", "error": "E",
                "failing": "x.1", "_unexplained": True, **kw}

    def answer(self, **kw):
        # `_names`: the package names in the prompt (llm_suggest adds them).
        return {"diagnosis": "d", "fix_in": "this-pr", "package": "x.1", "change_old": "", "change_new": "",
                "confidence": "low", "verify": "v", "_names": ["x", "x.1", "zarith"], **kw}

    def apply(self, r, res=None, suggest=None, pr_packages=("x.1",)):
        from unittest import mock
        L = self.L
        with mock.patch.object(L, "fetch", lambda *a: "log"), mock.patch.object(L, "log_text", lambda t: t), \
                mock.patch.object(L, "excerpt", lambda *a, **k: {"error_block": "e"}), \
                mock.patch.object(L, "key_error_line", lambda t: "e"), \
                mock.patch.object(L, "llm_suggest", suggest or (lambda *a, **k: dict(res))), \
                contextlib.redirect_stderr(io.StringIO()):
            L.apply_llm([r], {"jobs": [self.JOB]}, None, self.tmp, set(pr_packages), 5, "m", False)
        return r

    def test_change_shapes(self):
        ok = lambda old, new: self.L.llm_change_as_diff(old, new, {"foo", "bar"})
        for new in ['"foo"', '"foo" {with-test}', '"foo" {build & with-test}', 'available: os != "win32"',
                    'x-ci-accept-failures: ["alpine-3.22"]', "x-maintained: false", '"foo" {with-test}\n"bar"']:
            self.assertTrue(ok("", new), new)
        self.assertTrue(ok('"foo"', '"foo" {with-test}'))
        # A dependency the model was not given (an invented name): text, not a diff.
        for new in ['"anything"', '"foo"\n"anything" {with-test}']:
            self.assertFalse(ok("", new), new)
        self.assertFalse(self.L.llm_change_as_diff("", '"foo"'))          # names unknown
        self.assertTrue(self.L.llm_change_as_diff("", "x-maintained: false"))
        for old, new in [("", 'build: [["dune" "build" "-p" name]]'), ("", 'install: [make "install"]'),
                         ("", 'url { src: "https://e.x/a.tgz" }'), ("", 'src: "https://e.x/a.tgz"'),
                         ("", '"foo" {>= "1.0"}'), ("", 'depexts: ["libgmp-dev"]'),
                         ("", 'available: ocaml:version < "5.0"'), ("", 'depends: [\n"foo"\n]'),
                         ('build: [["make"]]', '"foo"'), ('"foo" {>= "1.0"}', '"foo"')]:
            self.assertFalse(ok(old, new), (old, new))
        # Through apply_llm: a build command is text in the note, a missing dependency is a diff.
        r = self.apply(self.result(), self.answer(change_new='build: [["make" "-j1"]]'))
        self.assertNotIn("new", r)
        self.assertIn('Possible change, not checked: `build: [["make" "-j1"]]`.', r["note"])
        r = self.apply(self.result(), self.answer(change_new='"zarith" {with-test}'))
        self.assertEqual((r["new"], r["old"]), ('"zarith" {with-test}', None))
        r = self.apply(self.result(), self.answer(change_new='"anything"'))
        self.assertNotIn("new", r)
        self.assertIn('Possible change, not checked: `"anything"`.', r["note"])

    def test_prompt_names(self):
        # The names the model was given: the PR's packages, the failing one, the installed
        # dependencies, the opam file and the error excerpt; not the earlier analysis.
        prompt = """Failing job: v (blocking platform)
Packages of this PR: x.1, y.2
Failing package: z.3 (not part of this PR)

Dependency versions installed by CI:
{"dune": "3.20.2", "fmt": "0.9.0"}

<opam_file package="z.3">
depends: [ "ocaml" "lwt" {>= "5.0"} ]
</opam_file>

<error_excerpt>
Error: Library "ppx_deriving.show" not found.
</error_excerpt>

Earlier analysis: anything goes"""
        names = self.L.prompt_names(prompt)
        for n in ("x", "y", "z", "dune", "fmt", "ocaml", "lwt", "ppx_deriving", "ppx_deriving.show"):
            self.assertIn(n, names)
        self.assertNotIn("anything", names)

    def test_unanswered_counted(self):
        # apply_llm says how many distinct failures are left without an answer a later call may give:
        # beyond the limit, or a transient API error; not a refusal (asking again would cost the same).
        from unittest import mock
        L = self.L
        rs = [self.result(variant=f"v{i}", error=f"E{chr(97 + i)}") for i in range(3)]
        jobs = [dict(self.JOB, variant=f"v{i}") for i in range(3)]
        answers = {"v0": self.answer(), "v1": {"llm_error": "API error 529: overloaded", "transient": True},
                   "v2": {"llm_error": "stopped: refusal"}}

        def ask(s, r, *a, call=True):
            return dict(answers[r["variant"]]) if call else {"llm_skipped": "beyond the limit"}

        def run(limit):
            rr = [dict(r) for r in rs]
            with mock.patch.object(L, "fetch", lambda *a: "log"), mock.patch.object(L, "log_text", lambda t: t), \
                    mock.patch.object(L, "excerpt", lambda *a, **k: {"error_block": "e"}), \
                    mock.patch.object(L, "key_error_line", lambda t: "e"), \
                    mock.patch.object(L, "llm_suggest", ask), \
                    contextlib.redirect_stderr(io.StringIO()):
                return L.apply_llm(rr, {"jobs": jobs}, None, self.tmp, {"x.1"}, limit, "m", False)
        self.assertEqual(run(3), 1)          # the transient error
        self.assertEqual(run(1), 2)          # two beyond the limit
        self.assertEqual(run(0), 3)

    def test_not_asked_while_ci_runs(self):
        # While CI runs, the prompt (the job's results on other platforms) changes as jobs finish:
        # the LLM is asked once CI has finished, and the output says how many are left.
        patch = """
import triage.suggest as s, triage.llm as L, triage.cli as c
_orig = s.suggest_for_job
def f(src, job, *a):
    r = _orig(src, job, *a)
    if "kinetic-client" in job["variant"]:
        r = {k: r[k] for k in ("variant", "url", "kind", "experimental", "failing")}
        r.update(error="Unbound value frobnicate", note="Unrecognised error.", _unexplained=True)
    return r
s.suggest_for_job = f
def ask(*a, **k):
    print("LLM CALLED", file=sys.stderr)
    return {"llm_error": "fake", "transient": True}
L.llm_suggest = ask
_load = c.load_commit
def load(*a, **k):
    page, data = _load(*a, **k)
    data["unfinished"] = %d
    return page, data
c.load_commit = load
"""
        out = run_patched("lwt-revdeps", patch % 3, ["--llm"])
        self.assertEqual(out.returncode, 0, out.stderr[-1500:])
        self.assertNotIn("LLM CALLED", out.stderr)
        self.assertEqual(json.loads(out.stdout)["llm_unanswered"], 1)
        out = run_patched("lwt-revdeps", patch % 0, ["--llm"])
        self.assertIn("LLM CALLED", out.stderr)
        self.assertEqual(json.loads(out.stdout)["llm_unanswered"], 1)      # a transient error

    def test_package_checked(self):
        # A package the input does not name: dropped, with the claim of where the fix goes.
        r = self.apply(self.result(), self.answer(package="evil.6.6", fix_in="other-package",
                                                  change_new='"zarith"'))
        self.assertEqual((r["failing"], r["fix_in"]), ("x.1", "unknown"))
        self.assertNotIn("new", r)
        self.assertIn("Possible change to `evil.6.6`, not checked", r["note"])
        # One of the PR's packages, or the failing one (a name alone is enough).
        r = self.apply(self.result(failing="y.2"), self.answer(package="x", change_new='"zarith"'))
        self.assertEqual((r["failing"], r["fix_in"], r["new"]), ("x.1", "this PR", '"zarith"'))
        r = self.apply(self.result(failing="y.2"), self.answer(package="y.2", fix_in="other-package"))
        self.assertEqual((r["failing"], r["fix_in"]), ("y.2", "y.2 (other package)"))
        r = self.apply(self.result(), self.answer(package=""))
        self.assertEqual((r["failing"], r["fix_in"]), ("x.1", "this PR"))

    def test_errors_leave_the_failure_unrecognised(self):
        from unittest import mock

        def boom(*a, **k):
            raise ConnectionResetError(104, "reset")
        r = self.apply(self.result(), suggest=boom)
        self.assertNotIn("rule", r)
        self.assertIn("ConnectionResetError", r["llm_error"])
        # The SDK: no credentials, an answer that is not JSON, no text, a bad field.
        import types

        class AnthropicError(Exception):
            pass

        class APIStatusError(AnthropicError):
            pass

        class APIConnectionError(AnthropicError):
            pass

        clients = []

        def sdk(answer=None, client_error=None):
            m = types.ModuleType("anthropic")
            m.AnthropicError, m.APIStatusError, m.APIConnectionError = \
                AnthropicError, APIStatusError, APIConnectionError
            blocks = [] if answer is None else [types.SimpleNamespace(type="text", text=answer)]
            resp = types.SimpleNamespace(stop_reason="end_turn", content=blocks, model="m",
                                         usage=types.SimpleNamespace(input_tokens=1, output_tokens=1))

            class Anthropic:
                def __init__(self, **kw):
                    clients.append(kw)
                    if client_error:
                        raise client_error
                    self.beta = types.SimpleNamespace(messages=types.SimpleNamespace(create=lambda **k: resp))
            m.Anthropic = Anthropic
            return m
        cases = [(sdk(client_error=AnthropicError("no api_key")), "no api_key"),
                 (sdk("not json"), "no JSON answer"), (sdk(None), "no JSON answer"),
                 (sdk(json.dumps(self.answer(fix_in="bogus"))), "unknown fix_in"),
                 (sdk(json.dumps({"diagnosis": "d"})), "no fix_in"), (sdk("[1]"), "not a JSON object")]
        for i, (mod, why) in enumerate(cases):
            self.L.LLM_STATE.update(off=None)       # no credentials turn the LLM off for the run
            with mock.patch.dict(sys.modules, {"anthropic": mod}), \
                    mock.patch.object(self.L, "llm_prompt", lambda *a: f"prompt {i}"):
                res = self.L.llm_suggest(None, {}, self.JOB, "log", {}, [], self.tmp, "m", False)
            self.assertIn(why, res.get("llm_error", ""), (why, res))
        self.assertEqual([f for f in os.listdir(self.tmp) if f.startswith("llm-")], [])     # nothing cached
        # A client timeout, one retry: not the SDK's 10 minutes and 2 retries.
        self.assertEqual(clients[0], {"timeout": 120, "max_retries": 1})
        # A good answer is cached; a damaged cached one is an error, not a crash.
        with mock.patch.dict(sys.modules, {"anthropic": sdk(json.dumps(self.answer()))}), \
                mock.patch.object(self.L, "llm_prompt", lambda *a: "good"):
            self.assertEqual(self.L.llm_suggest(None, {}, self.JOB, "log", {}, [], self.tmp, "m", False)["fix_in"],
                             "this-pr")
            cached = [f for f in os.listdir(self.tmp) if f.startswith("llm-")]
            self.assertEqual(len(cached), 1)
            with open(os.path.join(self.tmp, cached[0]), "w") as f:
                json.dump({"diagnosis": 3}, f)
            self.assertIn("unusable cached answer",
                          self.L.llm_suggest(None, {}, self.JOB, "log", {}, [], self.tmp, "m", False)["llm_error"])

    def sdk_run(self, create, n=3, limit=2, runs=1):
        """apply_llm on `n` distinct failures, `runs` times (each run a new
        process: LLM_STATE reset), through llm_suggest and its cache with a
        fake SDK whose messages.create is `create(calls)`; [(API calls,
        failures left)] per run."""
        from unittest import mock
        import types
        L = self.L

        class AnthropicError(Exception):
            pass

        class APIStatusError(AnthropicError):
            def __init__(self, status_code, message):
                super().__init__(message)
                self.status_code, self.message = status_code, message

        class APIConnectionError(AnthropicError):
            pass
        m = types.ModuleType("anthropic")
        m.AnthropicError, m.APIStatusError, m.APIConnectionError = AnthropicError, APIStatusError, APIConnectionError
        calls = []

        class Anthropic:
            def __init__(self, **kw):
                self.beta = types.SimpleNamespace(messages=types.SimpleNamespace(create=lambda **k: create(m, calls)))
        m.Anthropic = Anthropic
        jobs = [dict(self.JOB, variant=f"v{i}") for i in range(n)]
        out = []
        for _ in range(runs):
            L.LLM_STATE.update(streak=0, off=None)
            before = len(calls)
            rs = [self.result(variant=f"v{i}", error=f"E{chr(97 + i)}") for i in range(n)]
            with mock.patch.dict(sys.modules, {"anthropic": m}), \
                    mock.patch.object(L, "llm_prompt", lambda src, r, *a: f"prompt {r['variant']}"), \
                    mock.patch.object(L, "fetch", lambda *a: "log"), mock.patch.object(L, "log_text", lambda t: t), \
                    mock.patch.object(L, "excerpt", lambda *a, **k: {"error_block": "e"}), \
                    mock.patch.object(L, "key_error_line", lambda t: "e"), \
                    mock.patch.dict(L.NET, {}, clear=True), contextlib.redirect_stderr(io.StringIO()):
                left = L.apply_llm(rs, {"jobs": jobs}, None, self.tmp, {"x.1"}, limit, "m", False)
                off = L.NET.get("llm-off")
            out.append((len(calls) - before, left, off))
        return out

    def test_limit_counts_api_calls_only(self):
        # Three distinct failures, two calls per run: cached answers do not count toward the
        # limit, so the second run asks about the third failure, and the third run asks nothing.
        import types
        answer = json.dumps({k: v for k, v in self.answer().items() if k != "_names"})

        def create(m, calls):
            calls.append(1)
            return types.SimpleNamespace(stop_reason="end_turn", model="m",
                                         content=[types.SimpleNamespace(type="text", text=answer)],
                                         usage=types.SimpleNamespace(input_tokens=1, output_tokens=1))
        self.assertEqual(self.sdk_run(create, runs=3), [(2, 1, None), (1, 0, None), (0, 0, None)])

    def test_credentials_refused(self):
        # A bad key (401) or no permission (403): one call, then the LLM is off for the run, and the
        # failures count as unanswered (asked again once the key is fixed).
        def create(m, calls):
            calls.append(1)
            raise m.APIStatusError(401, "invalid x-api-key")
        self.assertEqual(self.sdk_run(create, limit=5), [(1, 3, 1)])

    def test_breaker(self):
        # An outage: after LLM_BREAKER transient failures in a row, no more calls in this run.
        def create(m, calls):
            calls.append(1)
            raise m.APIConnectionError("timed out")
        self.assertEqual(self.sdk_run(create, n=4, limit=5), [(self.L.LLM_BREAKER, 4, 1)])

        # A success in between: not in a row.
        def flaky(m, calls):
            calls.append(1)
            if len(calls) % 2:
                raise m.APIStatusError(529, "overloaded")
            raise m.APIStatusError(400, "bad request")       # not transient: not an outage
        self.assertEqual(self.sdk_run(flaky, n=4, limit=5), [(4, 2, None)])

    def test_new_on_master_reaches_the_llm(self):
        # opam-health-check sees the reverse dependency build on master: the
        # hint is put before the note; the failure is still unexplained.
        hint = "y.2 builds on master (opam-health-check, 17 Nov), so this PR probably causes this failure."
        r = self.apply(self.result(note=hint + " Unrecognised error.", new_on_master=hint), self.answer())
        self.assertEqual(r["rule"], "llm")
        self.assertTrue(r["note"].startswith(hint + " d Suggested next step"), r["note"])
        r = self.apply(self.result(note="Pre-existing: z. Unrecognised error.", preexisting="z"), self.answer())
        self.assertEqual(r["rule"], "llm")
        # Without the rules' flag (a rule explained it), the LLM is not asked.
        r = self.apply(self.result(_unexplained=False), self.answer())
        self.assertNotIn("rule", r)

    def test_flag_set_by_the_rules_and_not_output(self):
        from triage import rules
        job = {"variant": "compilers,5.3,x.1", "url": "u", "kind": "build", "experimental": False,
               "platform": ["compilers", "5.3"], "target": "x.1", "revdep": None}
        text = "#=== ERROR while compiling x.1 ===#\n# exit-code 2\n### output ###\n# something odd happened\n"

        class Src:
            def opam_file(self, *a):
                return None
        r = rules.suggest_for_job(Src(), job, text, ["x.1"], "a" * 40)
        self.assertEqual((r.get("note"), r.get("_unexplained")), ("Unrecognised error.", True))
        # The flag is not in the output.
        out = run_patched("lwt-revdeps", """
import triage.suggest as s
_orig = s.suggest_for_job
def f(src, job, *a):
    r = _orig(src, job, *a)
    if "kinetic-client" in job["variant"]:
        r = {k: r[k] for k in ("variant", "url", "kind", "experimental", "failing")}
        r.update(error="Unbound value frobnicate", note="Unrecognised error.", _unexplained=True)
    return r
s.suggest_for_job = f
""")
        self.assertEqual(out.returncode, 0, out.stderr[-1500:])
        self.assertIn("frobnicate", out.stdout)
        self.assertNotIn("_unexplained", out.stdout)


class EscapingTest(unittest.TestCase):
    """Untrusted text (logs, opam files, LLM answers) in the Markdown posted on
    GitHub: no link, image, HTML, mention or reference, and no way out of a
    code block."""

    def setUp(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        import opam_repo_ci_triage as t
        self.t = t

    def assertInert(self, md):
        rest = TRUSTED_LINK.sub("]", outside_code(md))
        rest = ALLOWED_TAGS.sub("", rest)
        for bad in (r"@\w", r"#\d", r"<", r"\]\(", r"://", r"www\.", r"GH-\d"):
            self.assertIsNone(re.search(bad, rest), f"{bad!r} in:\n{rest}\n--- from:\n{md}")

    def test_md_text(self):
        from triage.md import md_text, md_code, md_fence
        for s in (HOSTILE, "a `b` c", "x@y`z` ``a`b``", "tick ` alone", "line1\n```\n@x", "\\`@x`",
                  "[a](javascript:x) | *b* _c_ &#64;d", "`` ` `` @e"):
            md = md_text(s)
            self.assertNotIn("\n", md)
            self.assertInert(md)
        self.assertEqual(md_text("see #123."), "see `#123`.")
        self.assertEqual(md_text("Add a `license:` field"), "Add a `license:` field")
        self.assertEqual(md_text("ppx_deriving < 6"), "ppx\\_deriving \\< 6")
        self.assertTrue(md_text("x" * 50, 10).endswith("…"))
        self.assertEqual(md_code("a`b"), "``a`b``")
        self.assertEqual(md_code("`x"), "`` `x ``")
        self.assertEqual(md_fence("a\n````\nb"), "`````\na\n````\nb\n`````")

    def hostile(self, **kw):
        r = {"rule": "lint", "class": "fix", "confidence": "high", "fix_in": "this PR", "failing": "x.1",
             "variant": "v", "title": HOSTILE, "error": HOSTILE, "note": HOSTILE,
             "old": 'depends: "a" @old', "new": 'depends: "b"\n```\n' + HOSTILE + "\n```",
             "alternative": "`@x` [y](https://e)  (" + HOSTILE + ")"}
        r.update(kw)
        return r

    def test_render_comment(self):
        sha = "a" * 40
        rs = [self.hostile(),
              self.hostile(failing=HOSTILE, new="x-ci-accept-failures: [" + HOSTILE + "]   " + HOSTILE),
              self.hostile(**{"class": "workaround"}),
              self.hostile(**{"class": "elsewhere", "fix_in": HOSTILE, "rule": "rerun",
                              "jobs": ["day10,debian-13,x86_64,5.5,x.1", "x)[evil](https://e.x", HOSTILE]}),
              self.hostile(**{"class": "explanation", "failing": None, "variant": HOSTILE,
                              "url": "https://evil.example/log", "rule": "unrecognised", "jobs": [HOSTILE]}),
              self.hostile(**{"class": "explanation", "url": BASE_URL + "a" * 40 + "/variant/v,1"}),
              self.hostile(evidence={"x 1.0": True, HOSTILE: False}),
              self.hostile(candidates=[{"dep": HOSTILE, "new": HOSTILE, "changelog": {
                  "text": HOSTILE, "versions": [HOSTILE, "1.0"]}}], compared_with=HOSTILE)]
        md = self.t.render_comment(sha, {"jobs": []}, rs)
        self.assertInert(md)
        # Our own links stay.
        self.assertIn(f"]({BASE_URL}{sha}/variant/day10,debian-13,x86_64,5.5,x.1)", md)
        self.assertIn(f"([log]({BASE_URL}{sha}/variant/v,1))", md)
        self.assertNotIn("](https://e", outside_code(md))
        # A hostile commit is not linked.
        self.assertInert(self.t.render_comment("x)[a](https://e", {"jobs": []}, rs[:1]))

    def test_build_review(self):
        from unittest import mock
        sha = "b" * 40
        text = 'opam-version: "2.0"\ndepends: "a" @old\nlicense: "x"\n'
        patch = "@@ -0,0 +1,3 @@\n+" + "\n+".join(text.splitlines())

        def gh_api(path, cache=True):
            return ([{"filename": "packages/x/x.1/opam", "patch": patch}] if path.endswith("files?per_page=100")
                    else {"head": {"sha": sha}})

        class Src:
            def raw(self, url):
                return text
        ok = self.hostile(new='depends: "b" `@x` ```', alternative=None)
        llm = self.hostile(rule="llm", new='depends: "c"')
        multi = self.hostile(new='depends: "d"\n```\n@x')
        with mock.patch("triage.render.gh_api", gh_api):
            review = self.t.build_review(sha, {"pr": 1, "jobs": []}, [ok, llm, multi], Src())
        self.assertEqual(len(review["comments"]), 1)     # not the LLM answer, not a multi-line change
        body = review["comments"][0]["body"]
        self.assertIn('\n````suggestion\ndepends: "b" `@x` ```\n````', body)
        self.assertInert(body)
        self.assertInert(review["body"])
        self.assertFalse(llm.get("_placed"))
        self.assertIn('+ depends: "c"', review["body"])   # the LLM change stays in the summary, as a diff

    def test_llm_change_is_code(self):
        from unittest import mock
        import triage.llm as L
        r = {"variant": "v", "url": "https://opam.ci.ocaml.org/x", "note": "Unrecognised error.", "error": "E",
             "_unexplained": True}
        res = {"fix_in": "this-pr", "confidence": "low", "diagnosis": "d", "verify": "v",
               "change_new": 'depexts: ["a"] {os-distribution = "debian"}\n```\n@x `y`', "package": "x.1"}
        with mock.patch.object(L, "fetch", lambda *a: "log"), mock.patch.object(L, "log_text", lambda t: t), \
                mock.patch.object(L, "excerpt", lambda *a, **k: {"error_block": "e"}), \
                mock.patch.object(L, "key_error_line", lambda t: "e"), \
                mock.patch.object(L, "llm_suggest", lambda *a, **k: dict(res)):
            L.apply_llm([r], {"jobs": [{"variant": "v"}]}, None, None, {"x.1"}, 5, "m", False)
        self.assertIn('not checked: ```` depexts: ["a"] {os-distribution = "debian"} ``` @x `y` ````.', r["note"])
        self.assertNotIn("new", r)
        self.assertInert(self.t.render_comment("c" * 40, {"jobs": []}, [dict(r, **{"class": "explanation"})]))

    def test_checksum_command(self):
        from triage.context import checksum_command
        good = "https://github.com/o/r/releases/download/v1.0/r-1.0.tbz"
        self.assertEqual(checksum_command(good), f"curl -sL '{good}' | sha256sum")
        for bad in (None, "<archive URL>", "http://e.x/a.tgz", "https://e.x/a.tgz; rm -rf ~",
                    "https://e.x/$(id).tgz", "https://e.x/a'b.tgz", "https://e.x/`id`", "https://e.x/a b",
                    "https://e.x/a|b", "https://e.x/a&b", "https://-e.x/a", "file:///etc/passwd"):
            self.assertIsNone(checksum_command(bad), bad)

    def test_git_urls(self):
        from triage import net
        for url, exp in [("https://github.com/o/r.git", "https://github.com/o/r"),
                         ("git+https://gitlab.com/g/s/r.git#main", "https://gitlab.com/g/s/r.git"),
                         ("git@github.com:o/r.git", "https://github.com/o/r"),
                         ("git://git.example.org/r", "https://git.example.org/r"),
                         ("--upload-pack=touch /tmp/x", None), ("ssh://git@e.x/r", None),
                         ("file:///tmp/r", None), ("ext::sh -c id", None), ("https://e.x/a b", None),
                         ("https://-e.x/r", None), ("http://e.x/r", None)]:
            self.assertEqual(net.repo_url(url), exp, url)

    def test_git_command(self):
        import tempfile
        from unittest import mock
        from triage import net, sources
        calls = []

        def run(cmd, **kw):
            calls.append((cmd, kw))
            raise subprocess.TimeoutExpired(cmd, kw.get("timeout"))
        with mock.patch.object(net.subprocess, "run", run):
            r = net.git(["rev-parse", "HEAD"], check=False)
            self.assertNotEqual(r.returncode, 0)
            with self.assertRaises(RuntimeError):
                net.git(["rev-parse", "HEAD"])
        cmd, kw = calls[0]
        self.assertEqual(cmd[:5], ["git", "-c", "protocol.allow=never", "-c", "protocol.https.allow=always"])
        self.assertTrue(kw.get("timeout"))
        # An unsafe URL never reaches git.
        with tempfile.TemporaryDirectory() as d, mock.patch.object(sources, "git", side_effect=AssertionError):
            src = sources.Sources(d)
            self.assertEqual(src.tags("--upload-pack=x"), [])
            self.assertIsNone(src.file_at("ssh://e.x/r", "v1", "a"))
            self.assertEqual(src.tree("file:///r", "v1"), [])
        with tempfile.TemporaryDirectory() as d, mock.patch.object(sources, "git") as g:
            g.return_value = subprocess.CompletedProcess([], 0, "", "")
            sources.Sources(d).tags("https://e.x/r")
            self.assertEqual(g.call_args[0][0], ["ls-remote", "--tags", "--refs", "--", "https://e.x/r"])

    def test_llm_digest(self):
        sys.path.insert(0, os.path.join(HERE, "..", "scripts"))
        import llm_digest as D
        r = {"rule": "llm", "log_error": "Error: `x` " + HOSTILE, "diagnosis": HOSTILE, "failing": HOSTILE,
             "fix_in": HOSTILE, "confidence": HOSTILE, "new": "a ``` " + HOSTILE,
             "url": "https://evil.example/" + HOSTILE}
        good = dict(r, url=BASE_URL + "a" * 40 + "/variant/v")
        items = [(123, r), (456, good), (789, dict(r, log_error="other " + HOSTILE))]
        for page in (None, "https://x.github.io/triage/"):
            md = D.report(items, 2, *([page] if page else []))
            self.assertInert(re.sub(r"\[`#\d+`\]\(https://x\.github\.io/triage/#pr-\d+\)", "", md))
            self.assertNotIn("github.com/ocaml/opam-repository/pull", md)
        self.assertIn("`ocaml/opam-repository#123`", D.report(items, 2))
        self.assertIn("[`#123`](https://x.github.io/triage/#pr-123)", md)
        self.assertIn(f"[log]({BASE_URL}", md)


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--record":
        record(sys.argv[2:] or list(CASES))
    elif len(sys.argv) > 1 and sys.argv[1] == "--top-up":
        top_up(sys.argv[2:] or list(CASES))
    else:
        unittest.main()
