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

import gzip
import json
import re
import os
import shutil
import subprocess
import sys
import unittest
import unittest.mock

HERE = os.path.dirname(os.path.abspath(__file__))
TOOL = os.path.join(HERE, "..", "opam_repo_ci_triage.py")
FIXTURES = os.path.join(HERE, "fixtures")

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
         {"rule": "rerun", "error": "HTTP error code 503"},
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

    def test_unanalysed_count(self):
        # Failures beyond --max are counted in the JSON output.
        env = {**os.environ, "OPAM_CI_OFFLINE": "1", "OPAM_CI_RECORD": "0"}
        out = subprocess.run([sys.executable, TOOL, "suggest", CASES["websocket-bound"][0], "--json", "--max", "2",
                              "--cache-dir", os.path.join(FIXTURES, "websocket-bound")],
                             capture_output=True, text=True, env=env, timeout=300)
        self.assertIn("offline cache misses: 0", out.stderr)
        self.assertGreater(json.loads(out.stdout)["unanalysed"], 0)


def make_test(case):
    def test(self):
        if not os.path.isdir(os.path.join(FIXTURES, case)):
            self.skipTest("no fixture (run with --record)")
        out = run(case)
        self.assertEqual(out.returncode, 0, out.stderr[-1000:])
        self.assertIn("offline cache misses: 0", out.stderr, out.stderr[-1500:])
        sugg = json.loads(out.stdout)["suggestions"]
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
        from triage import net
        if net.OFFLINE:
            self.skipTest("offline")
        with tempfile.TemporaryDirectory() as d:
            src = t.Sources(d)
            src.MCP_URL = "http://127.0.0.1:9/mcp"          # nothing listens there
            before = t.NET.get("mcp-errors", 0)
            self.assertIsNone(src.mcp("ocaml_package_versions", package="x"))
            self.assertEqual(t.NET.get("mcp-errors", 0), before + 1)
            self.assertEqual(os.listdir(d), [])             # not cached
            self.assertEqual(src.versions("x"), [])

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
        # Unknown dates (state saved before they were recorded), another commit.
        self.assertFalse(T.ci_changed(dict(prev, ci_created=None), rebuilt))
        self.assertFalse(T.ci_changed(prev, dict(rebuilt, sha="b")))

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
            ci.load_commit(sha, d, refresh=True)
            self.assertFalse(os.path.exists(log))          # rebuilt: fetched again when needed
            self.assertEqual(fetched, [(net.BASE + sha, False), (net.BASE + sha, True)])


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

    def test_seems_ready(self):
        self.assertTrue(self.r.seems_ready(self.pr([self.day10])))
        # A revdep failure is the PR's, unless it happens without the PR.
        self.assertFalse(self.r.seems_ready(self.pr([self.day10, self.revdep])))
        self.assertTrue(self.r.seems_ready(self.pr([self.day10, dict(self.revdep, preexisting="on master")])))
        self.assertTrue(self.r.seems_ready(self.pr([dict(self.revdep, also_failing_in=[2, 3])])))
        self.assertTrue(self.r.seems_ready(self.pr([dict(self.lint, experimental=True)])))
        self.assertTrue(self.r.seems_ready(self.pr([{"class": "elsewhere", "rule": "lint-ci-error",
                                                      "fix_in": "CI (rerun)", "variant": "(lint)"}])))
        self.assertFalse(self.r.seems_ready(self.pr([self.lint])))
        # Unexplained failures (only skipped jobs), CI not finished, drafts, labels.
        self.assertFalse(self.r.seems_ready(self.pr([])))
        self.assertFalse(self.r.seems_ready(self.pr([self.day10], ci_unfinished=2)))
        # Distinct failures beyond --max were not analysed: nothing says they are not the PR's.
        unanalysed = self.pr([self.day10])
        unanalysed["result"]["unanalysed"] = 3
        self.assertFalse(self.r.seems_ready(unanalysed))
        self.assertIn("3 more distinct failures were not analysed", self.r.render_pr(unanalysed))
        self.assertIn("3 more distinct failure(s) were not analysed", self.r.comment_markdown(unanalysed))
        self.assertFalse(self.r.seems_ready(self.pr([self.day10], draft=True)))
        self.assertFalse(self.r.seems_ready(self.pr([self.day10], labels=["needs reporter action"])))
        self.assertTrue(self.r.seems_ready(self.pr([self.day10], labels=["consider for merge"])))
        # A "needs reporter action" label the author acted on since may be stale.
        self.assertTrue(self.r.seems_ready(self.pr([self.day10], labels=["needs reporter action"],
                                                   stale_labels=["needs reporter action"])))
        self.assertIn("author active since", self.r.label_badges(["needs reporter action"],
                                                                 ["needs reporter action"]))

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
        self.assertEqual(self.r.pr_cats(self.pr([self.day10], labels=["consider for merge"])) & {"ready", "merge"},
                         {"ready", "merge"})
        self.assertIn("&lt;b&gt;", self.r.label_badges(["<b>"]))

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
        self.assertIn("No failure seems to come from this PR", md)
        llm_low = {"class": "fix", "rule": "llm", "confidence": "low", "fix_in": "this PR", "failing": "z.1",
                   "variant": "v", "title": "LLM diagnosis (unchecked)"}
        md = self.r.comment_markdown(self.pr([self.lint, llm_low]))
        self.assertNotIn("No failure seems", md)
        self.assertNotIn("z.1", md)
        page = self.r.render_pr(self.pr([self.lint]))
        self.assertIn('data-for="md-1"', page)
        self.assertIn("</textarea>", page)
        self.assertEqual(page.count("<textarea"), 1)


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--record":
        record(sys.argv[2:] or list(CASES))
    elif len(sys.argv) > 1 and sys.argv[1] == "--top-up":
        top_up(sys.argv[2:] or list(CASES))
    else:
        unittest.main()
