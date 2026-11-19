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
import os
import shutil
import subprocess
import sys
import unittest

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
         {"failing": "passage.0.1.7", "candidate": '"cmdliner" {with-test & < "2.0.0"}'}]),
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
         {"failing": "tar-eio.3.5.0", "rule": "builder-specific"}]),
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
        [{"failing": "afl.2.52b", "rule": "newer-c-compiler", "note": "-Wimplicit-int"}]),
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
        elif v not in str(s.get(k) or ""):
            return False
    return True


def record(cases):
    for case in cases:
        d = os.path.join(FIXTURES, case)
        shutil.rmtree(d, ignore_errors=True)
        out = run(case, offline=False)
        print(f"{case}: rc={out.returncode} {out.stderr.strip().splitlines()[-1:]}")
        # compress pages and logs; drop git objects that only speed up later fetches
        for name in os.listdir(d):
            p = os.path.join(d, name)
            if name.endswith(".html"):
                with open(p, "rb") as f, gzip.open(p + ".gz", "wb", compresslevel=9) as g:
                    g.write(f.read())
                os.remove(p)
        size = sum(os.path.getsize(os.path.join(r, f)) for r, _, fs in os.walk(d) for f in fs)
        print(f"  fixture size: {size // 1024} KiB")


class TriageTest(unittest.TestCase):
    pass


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


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--record":
        record(sys.argv[2:] or list(CASES))
    else:
        unittest.main()
