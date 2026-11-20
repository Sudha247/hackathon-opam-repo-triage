"""Rules on the pass/fail pattern of the jobs of a package (no fetch)."""

import argparse
import collections
import concurrent.futures
import functools
import gzip
import hashlib
import html
import html.parser
import json
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

from .ci import (  # noqa: F401
    FAILING_CATS,
    NEUTRAL,
    failing_dep,
    failure_key,
    job_dims,
    merge_jobs,
    platform_words,
    split_pkg,
    vkey,
)


def matrix_groups(jobs, match):
    groups = {}
    for j in jobs:
        if j["kind"] not in ("build", "tests", "revdeps") or not j["target"]:
            continue
        if match and not re.search(match, j["variant"]):
            continue
        groups.setdefault((j["target"], j["revdep"], j["kind"]), []).append(j)
    return groups


def compiler_matrix(js):
    """Fails on every job of the older (or newer) OCaml versions, passes on the
    others -> ('>=' | '<', version, failing versions, passing versions)."""
    per = {}
    for j in js:
        if j["experimental"] or j["category"] in NEUTRAL:
            continue
        v = job_dims(j)["ocaml"]
        s = "fail" if j["category"] in FAILING_CATS else "pass" if j["category"] == "passed" else None
        if v and s:
            per.setdefault(v, set()).add(s)
    if len(per) < 2 or any(len(s) > 1 for s in per.values()):
        return None
    vs = sorted(per, key=vkey)
    seq = ["".join(per[v]) for v in vs]
    fails = [v for v, s in zip(vs, seq) if s == "fail"]
    passes = [v for v, s in zip(vs, seq) if s == "pass"]
    if not fails or not passes:
        return None
    if seq == sorted(seq):                    # fail ... fail pass ... pass
        return (">=", passes[0], fails, passes)
    if seq == sorted(seq, reverse=True):      # pass ... pass fail ... fail
        return ("<", fails[0], fails, passes)
    return None


def platform_matrix(js):
    """Fails only on Windows, one arch, or some distros -> (dimension, values)."""
    fails = [j for j in js if not j["experimental"] and j["category"] in FAILING_CATS]
    passes = [j for j in js if not j["experimental"] and j["category"] == "passed"]
    if not fails or not passes:
        return None
    fd, pd = [job_dims(j) for j in fails], [job_dims(j) for j in passes]
    if all(d["os"] == "win32" for d in fd) and any(d["os"] != "win32" for d in pd):
        return ("os", sorted({d["distro"] for d in fd}))
    archs = {d["arch"] for d in fd}
    if len(archs) == 1 and archs != {"x86_64"} and not archs & {d["arch"] for d in pd}:
        return ("arch", sorted(archs))
    dists = {d["distro"] for d in fd}
    if None not in dists and not dists & {d["distro"] for d in pd} and \
       {d["ocaml"] for d in fd} <= {d["ocaml"] for d in pd}:
        return ("distro", sorted(dists))
    return None


def job_arch(job):
    """The architecture of a job, with one name per architecture (the
    FreeBSD jobs say amd64)."""
    a = job_dims(job)["arch"]
    return {"amd64": "x86_64", "aarch64": "arm64"}.get(a, a)


CYGWIN_PKG = {"awk": "gawk", "gawk": "gawk", "gcc": "gcc-core", "cc": "gcc-core", "g++": "gcc-g++",
              "make": "make", "autoconf": "autoconf", "automake": "automake", "pkg-config": "pkgconf",
              "m4": "m4", "patch": "patch", "perl": "perl", "python3": "python3", "bison": "bison", "flex": "flex"}


def matrix_rules(jobs, earlier, match, pr_packages, failing_in_log=None):
    """Suggestions from the pass/fail pattern of the jobs only (no fetch)."""
    failing_in_log = failing_in_log or {}
    done = {(r.get("failing"), r.get("dep")) for r in earlier}
    out = []
    for (target, revdep, kind), js in sorted(matrix_groups(jobs, match).items(), key=lambda kv: str(kv[0])):
        pkg = revdep or target
        fails = [j for j in js if not j["experimental"] and j["category"] in FAILING_CATS]
        if not fails:
            continue
        # Who has to change: a failing dependency (named by CI, or found in the
        # log's ERROR block), or the package itself.
        named = set()
        for j in fails:
            d = failing_dep(j["reason"]) if j["category"] == "dependency" else None
            d = d or failing_in_log.get(failure_key(j))
            named.add(d if d and split_pkg(d)[0] != split_pkg(pkg)[0] else None)
        culprit = named.pop() if len(named) == 1 and None not in named else None
        owner = culprit or pkg
        tests_only = kind == "tests" or all(j["category"] == "build-or-test" for j in fails)
        where = "this PR" if owner in pr_packages else f"{owner} (other package)"
        label = f"{pkg} [{kind}{', revdep of ' + target if revdep else ''}]"
        # A day10 failure whose distribution, architecture and OCaml version
        # pass on the classic builder is a problem of the day10 environment,
        # not of the package: report it as such, and leave it out of the
        # analyses below. Not when day10 passes on another architecture of
        # the same distribution and OCaml version: the failure then depends
        # on the architecture.
        passes = [j for j in js if not j["experimental"] and j["category"] == "passed"]

        def classic_twin(j):
            d = job_dims(j)
            same = [p for p in passes if job_dims(p)["distro"] == d["distro"] and d["distro"]
                    and job_dims(p)["ocaml"] == d["ocaml"]]
            if any(p["platform"][:1] == ["day10"] and job_arch(p) != job_arch(j) for p in same):
                return None
            return next((p for p in same if p["platform"][:1] != ["day10"] and job_arch(p) == job_arch(j)), None)
        twins = [(j, classic_twin(j)) for j in fails if j["platform"][:1] == ["day10"]]
        twins = [(j, t) for j, t in twins if t]
        if twins:
            dists = sorted({job_dims(j)["distro"] for j, _ in twins})
            evidence = next((r.get("note") for r in earlier
                             if r.get("failing") == culprit and r.get("rule") == "missing-tool"), None)
            out.append({"variant": label, "failing": culprit or pkg,
                        "fix_in": "the CI environment (day10 builder)", "rule": "builder-specific",
                        "error": f"Fails only on the day10 builder on {', '.join(dists)}: the classic "
                                 f"builder passes on the same distributions, architectures and OCaml versions",
                        "confidence": "high",
                        "note": f"For example, it fails on {platform_words(twins[0][0])} and passes on "
                                f"{platform_words(twins[0][1])}."
                                + (f" {evidence}" if evidence else "")
                                + ("" if "Nothing to change" in (evidence or "")
                                   else " Nothing to change in the package metadata."),
                        "_keys": [failure_key(j) for j, _ in twins],
                        "jobs": [j["variant"] for j, _ in twins]})
            gone = {id(j) for j, _ in twins}
            js = [j for j in js if id(j) not in gone]
            fails = [j for j in fails if id(j) not in gone]
            if not fails:
                continue
        cm = compiler_matrix(js)
        if cm and (owner, "ocaml") not in done:
            op, v, fv, pv = cm
            filt = f'with-test & {op} "{v}"' if tests_only else f'{op} "{v}"'
            gap = ""
            if op == ">=":
                gap = f" OCaml versions between {fv[-1]} and {v} were not tested, so the bound may be lower."
            out.append({"variant": label, "failing": owner, "fix_in": where, "rule": "compiler-matrix",
                        "error": f"Fails on OCaml {', '.join(fv)}, passes on {', '.join(pv)}"
                                 + (" (tests only)" if tests_only else "")
                                 + (f"; the failure is in the dependency {culprit}" if culprit else ""),
                        "dep": "ocaml", "old": None,
                        "new": f'"ocaml" {{{filt}}}' + ("   (new line in depends)" if tests_only else ""),
                        "confidence": "medium", "note": ("Based only on which jobs pass and fail." + gap),
                        "_keys": [failure_key(j) for j in fails], "jobs": [j["variant"] for j in fails]})
            continue
        pm = platform_matrix(js)
        if not pm:
            continue
        dim, vals = pm
        if dim == "os":
            cond, what = 'os != "win32"', "Windows (" + ", ".join(vals) + ")"
        elif dim == "arch":
            cond, what = " & ".join(f'arch != "{a}"' for a in vals), "arch " + ", ".join(vals)
        else:
            cond, what = None, "distributions " + ", ".join(vals)
        if culprit:
            tool = None
            for r in earlier:
                if r.get("failing") == culprit and r.get("rule") == "missing-tool":
                    tool = re.search(r'(?:Program "?|")([\w.+-]+)', r.get("error", ""))
            dep = CYGWIN_PKG.get(tool.group(1)) if tool and dim == "os" else None
            if dep and any(v.startswith("cygwin") for v in vals):
                new = f'depexts: ["{dep}"] {{os-distribution = "cygwin"}}   (in {culprit})'
            else:
                plats = ", ".join(vals)
                new = f"in {culprit}: a depext for {plats}, or x-ci-accept-failures / available"
        elif cond and tests_only:
            new = f'"@runtest" {{with-test & {cond}}}   (or the equivalent filter on the test command)'
        elif cond:
            new = f"available: {cond}   (combined with any existing `available`)"
        else:
            new = "x-ci-accept-failures: [" + " ".join(f'"{d}"' for d in vals) + "]"
        reason = sorted({(j["reason"] or "")[:120] for j in fails})[0]
        out.append({"variant": label, "failing": owner, "fix_in": where, "rule": "platform-matrix",
                    "_distros": (sorted({job_dims(j)["distro"] or job_dims(j)["os"] for j in fails})
                                 if all(j["platform"][:1] == ["day10"] for j in fails) else None),
                    "error": f"Fails only on {what}" + (" (tests only)" if tests_only else "")
                             + (f"; the failure is in the dependency {culprit}" if culprit else "")
                             + f": {reason}",
                    "old": None, "new": new, "confidence": "medium",
                    "note": (f"Nothing to change in {pkg}." if culprit else
                             "This only hides the problem on those platforms: an upstream fix, or a missing "
                             "depext, is better."),
                    "_keys": [failure_key(j) for j in fails], "jobs": [j["variant"] for j in fails]})
    return out


def collapse_systemic(out, threshold=5):
    """The same matrix pattern on many packages is one cause (the PR, a shared
    dependency or the CI environment), not one fix per package: merge them."""
    def sig(r):
        pattern = r["error"].split(": ", 1)[0] if r["rule"] == "platform-matrix" else r["error"]
        return (r["rule"], re.sub(r"the dependency \S+", "a dependency", pattern), r.get("new", "").split("   ")[0]
                if r["rule"] == "compiler-matrix" else "")
    groups = collections.defaultdict(list)
    for r in out:
        groups[sig(r)].append(r)
    merged = []
    for (rule, pattern, _), rs in groups.items():
        if len(rs) < threshold:
            merged += rs
            continue
        names = sorted({r["variant"].split(" [", 1)[0] for r in rs})
        culprits = collections.Counter(r["failing"] for r in rs if r["failing"] not in names)
        merged.append({
            "variant": f"{len(rs)} packages [{rule}]", "failing": ", ".join(names[:6]) + (" ..." if len(names) > 6 else ""),
            "fix_in": (rs[0]["fix_in"] if rule == "builder-specific"
                       else "the PR, a shared dependency, or the CI"),
            "rule": rule + "-systemic", "error": f"{len(rs)} packages: {pattern[:1].lower()}{pattern[1:]}",
            "confidence": "medium",
            "note": (rs[0].get("note") if rule == "builder-specific" else
                     (("Failing dependencies: " + ", ".join(f"{d} ({n} packages)" for d, n in culprits.most_common(3))
                       + ". ") if culprits else "")
                     + f"Fix per package if needed: {rs[0].get('new', '')}"),
            "_keys": [k for r in rs for k in r.get("_keys", [])],
            "jobs": merge_jobs([], [v for r in rs for v in r.get("jobs", [])])})
    return merged
