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
    "dns-certify-bound": (
        "d1951c34e014d331928d961b53113a61c93b17fe", ["--max", "6"],
        [{"failing": "dns-certify.10.2.7", "new": '"ca-certs-nss" {>= "3.113.1"}'}]),
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
