"""PR-level checks: pre-existing failures, opam-health-check, accepted failures, reruns, sources, the PR's own diff, lint, uninstallable packages."""

import argparse
import collections
import concurrent.futures
import datetime
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

from .net import (  # noqa: F401
    BASE,
    OFFLINE,
    REPO,
    fetch,
    gh_api,
)
from .ci import (  # noqa: F401
    CLASSIC_DISTRO,
    FAILING_CATS,
    excerpt,
    failure_key,
    job_dims,
    log_text,
    merge_jobs,
    opam_vcmp,
    parse_commit_page,
    split_pkg,
    failing_dep,
)
from .sources import (  # noqa: F401
    constraint_edit,
    find_baseline_commit,
    license_id,
    opam_field,
    url_src,
)
from .where import fix_where
from .rules import (  # noqa: F401
    direct_deps,
)
from .words import (  # noqa: F401
    counted,
    short_date,
)


# A URL that can be pasted in a shell command between single quotes: https,
# no space, quote, `;|&$`, backtick, parenthesis, `<>`, `!`, `#`...
SHELL_SAFE_URL = re.compile(r"https://[A-Za-z0-9][A-Za-z0-9.-]*(?::\d{1,5})?(?:/[A-Za-z0-9._~%+=,:/?-]*)?")


def checksum_command(url):
    """`curl -sL '<url>' | sha256sum` for the archive URL of a PR's opam file
    (untrusted: its `src:` can hold any character but `"`), or None if the
    URL is not a plain https URL."""
    if url and SHELL_SAFE_URL.fullmatch(url):
        return f"curl -sL '{url}' | sha256sum"
    return None


def accepted_platforms(opam_text):
    """The platforms listed in x-ci-accept-failures."""
    m = re.search(r"x-ci-accept-failures:\s*\[([^\]]*)\]", opam_text or "")
    return set(re.findall(r'"([^"]+)"', re.sub(r"#[^\n]*", "", m.group(1)))) if m else set()


def accepted_but_failing(results, src, sha, pr_packages):
    """A failure on a platform the failing package already lists in
    x-ci-accept-failures: the classic builder ignores those, the day10 builder
    does not (cygwin, freebsd): nothing to change in the package."""
    for r in results:
        ds, owner = r.get("_distros"), r.get("failing") or ""
        name, ver = split_pkg(owner)
        if not ds or not ver or " " in owner or r.get("rule") in ("rerun", "lint", "transient"):
            continue
        text = (src.opam_file(name, ver, sha) if owner in pr_packages else None) or src.opam_file(name, ver)
        if set(ds) <= accepted_platforms(text):
            for k in ("new", "old", "alternative", "candidates"):
                r.pop(k, None)
            r.update(fix_where("ci", "the CI environment (day10 builder)"), rule="accept-failures-ignored",
                     confidence="high",
                     note=f"{owner} already lists {', '.join(ds)} in x-ci-accept-failures, but the day10 builder "
                          f"ignores that field there. Nothing to change in the package.")


def bound_only_changes(base, sha, cache_dir):
    """{"name.version": 'dep {< "V"}'} for the opam files this PR modifies only
    by adding an upper bound that excludes a version the PR adds: their
    dependency solutions are the same as on master."""
    d = gh_api(f"repos/{REPO}/compare/{base}...{sha}", cache_dir) if base else None
    files = (d or {}).get("files", [])
    added = {m.group(1) for f in files if f.get("status") == "added"
             for m in [re.match(r"packages/[^/]+/([^/]+)/opam$", f.get("filename", ""))] if m}
    out = {}
    for f in files:
        m = re.match(r"packages/[^/]+/([^/]+)/opam$", f.get("filename", ""))
        if not m or f.get("status") != "modified":
            continue
        lines = (f.get("patch") or "").splitlines()
        rem = [l[1:].strip() for l in lines if l[:1] == "-" and not l.startswith("---")]
        add = [l[1:].strip() for l in lines if l[:1] == "+" and not l.startswith("+++")]
        if not rem or len(rem) != len(add):
            continue
        bounds = []
        for r, a in zip(rem, add):
            ma = re.match(r'"([\w+-]+)"\s*\{(.*)\}$', a)
            mr = re.match(r'"([\w+-]+)"\s*(?:\{(.*)\})?$', r)
            if not (ma and mr and ma.group(1) == mr.group(1)):
                break
            terms = lambda f: [t.strip() for t in (f or "").split("&") if t.strip()]
            extra = [t for t in terms(ma.group(2)) if t not in terms(mr.group(2))]
            mv = re.fullmatch(r'<\s*"([^"]+)"', extra[0]) if len(extra) == 1 else None
            if not (mv and len(terms(ma.group(2))) == len(terms(mr.group(2))) + 1
                    and f"{ma.group(1)}.{mv.group(1)}" in added):
                break
            bounds.append(f'{ma.group(1)} {{< "{mv.group(1)}"}}')
        else:
            out[m.group(1)] = ", ".join(bounds)
    return out


def health_check(src, name, cache):
    """opam-health-check's results for the newest version of a package, from
    ocaml.org's MCP: ({compiler: (status, log url)}, version, run date), or
    None. It builds the newest version of every package on master, without
    tests; its results change daily, so they are cached for a day."""
    if name not in cache:
        r = src.mcp("ocaml_package_build_information", _max_age=86400, package=name, limit=5)
        res = next((x for x in (r or {}).get("results", []) if x.get("package") == name), None)
        cache[name] = res and ({b["compiler"]: (b["status"], b.get("log_url")) for b in res.get("builds", [])},
                               res.get("version"), ((r or {}).get("run") or {}).get("date"))
    return cache[name]


# opam-health-check runs about daily: a run older than this (the service is
# stuck, or ocaml.org's copy is) describes an older master, and its results
# are not used.
HEALTH_MAX_AGE_DAYS = 7


def health_run_stale(date, today=None):
    """Whether an opam-health-check run of `date` (YYYY-MM-DD...) is older than
    HEALTH_MAX_AGE_DAYS. An undated run is used; offline, the test fixtures
    are snapshots: never stale."""
    if OFFLINE or not date:
        return False
    try:
        d = datetime.date.fromisoformat(str(date)[:10])
    except ValueError:
        return False
    return ((today or datetime.date.today()) - d).days > HEALTH_MAX_AGE_DAYS


def health_summary(notes, src=None):
    """Entries comparing the failing reverse dependencies with opam-health-check
    (master): the ones that also fail there, and the ones that build there."""
    h, run = notes.get("health") or {}, notes.get("run")
    when = f"opam-health-check, {short_date(run)}" if run else "opam-health-check"
    out = []

    def listing(d):
        return ", ".join(f"{rd} (OCaml {', '.join(sorted({v for v, _ in xs}))})" for rd, xs in sorted(d.items()))

    def names(xs, n=6):
        if len(xs) > n:
            return ", ".join(xs[:n]) + f" and {len(xs) - n} more"
        return xs[0] if len(xs) == 1 else ", ".join(xs[:-1]) + " and " + xs[-1]
    good = h.get("good") or {}
    for rd, xs in sorted(good.items()):
        # Broken by this PR: the usual fix in opam-repository is an upper
        # bound on the new release in the reverse dependency, in the same PR.
        targets = sorted({m.group(1) for _, v in xs for m in [re.search(r",([^,]+),revdeps,", v)] if m})
        target = targets[0] if targets else None
        via = sorted((notes.get("via") or {}).get(rd, ()))
        ocaml = f"OCaml {', '.join(sorted({v for v, _ in xs}))}"
        if via:
            one = len(via) == 1
            error = (f"{rd} fails with this PR on {ocaml}, and so {'does' if one else 'do'} {names(via)}, which "
                     f"{'depends' if one else 'depend'} on it and {'builds' if one else 'build'} on master "
                     f"({when}).")
        else:
            error = f"{rd} fails with this PR on {ocaml} but builds on master ({when})."
        r = {"variant": f"{rd}, reverse dependency" + (f" of {target}" if target else ""),
             "rule": "health-check-new", **fix_where("pr", "this PR"), "confidence": "medium", "failing": rd,
             "error": error,
             "note": "Probably broken by this PR: look at it first. opam-health-check builds the newest "
                     "version of each package, without tests.",
             "jobs": sorted({v for _, v in xs})}
        rn, rv = split_pkg(rd)
        added = notes.get("added")
        opam = src.opam_file(rn, rv) if src and target and (added is None or target in added) else None
        if target and added is not None and target not in added:
            r["note"] = (f"This PR modifies {target} rather than adding a new version: check that the change "
                         f"does not break {rd}. opam-health-check builds the newest version of each package, "
                         f"without tests.")
        if opam and target.startswith("conf-"):
            r["note"] = (f"{target} is a new version of a conf package: when it breaks {rd}, the conf package "
                         f"is usually what is wrong (its depexts or its check), not {rd}.")
            opam = None
        if opam:
            tn, tv = split_pkg(target)
            if tn in direct_deps(opam):
                old, new = constraint_edit(opam, tn, "<", tv)
                what = "upper bound"
            else:
                old, new = None, f'conflicts: [ "{tn}" {{>= "{tv}"}} ]   (added to any existing `conflicts`)'
                what = "conflict"
            r.update(dep=tn, old=old, new=new,
                     note=f"Probably broken by {target}. Usual fix: add this {what} to {rd}'s opam file in this "
                          f"PR, or release a fixed {rn}. opam-health-check builds the newest version of each "
                          f"package, without tests.")
        out.append(r)
    bad = {**(h.get("bad") or {}), **(h.get("partial") or {})}
    if bad:
        out.append({"variant": counted(len(bad), "reverse dependency"),
                    "rule": "health-check-preexisting",
                    **fix_where("other-package", "the reverse dependencies (other packages)"),
                    "confidence": "high", "failing": ", ".join(sorted(bad)),
                    "error": f"These fail on master too ({when}): {listing(bad)}",
                    "note": "They, or one of their dependencies, fail without this PR. Nothing to change in "
                            "this PR.",
                    "jobs": sorted({v for xs in bad.values() for _, v in xs})})
    return out


def preexisting_failures(data, sha, base, cache_dir, src=None, notes=None):
    """{failure_key: why} for the failing jobs that would fail without this PR:
    packages the PR only bounds, failures that the CI run of the previous
    release of a package also had (same platform, same job), and reverse
    dependencies that opam-health-check also sees failing on master. Build
    failures that opam-health-check sees passing on master get a note in
    `notes` instead: they are probably caused by the PR."""
    failing = [j for j in data["jobs"] if j["status"] == "failed" and j["category"] in FAILING_CATS]
    per_job = {}
    bound = bound_only_changes(base, sha, cache_dir) if base else {}
    for j in failing:
        pkg = j["target"] if not j["revdep"] else None
        if pkg in bound:
            per_job[j["variant"]] = (f"this PR only adds {bound[pkg]} to {pkg}, so its dependencies are "
                                     f"the same as on master")
    added = sorted({j["target"] for j in failing if j["target"] and j["target"] not in bound})
    for target in added[:3]:
        info = find_baseline_commit(target, cache_dir)
        if not info or "sha" not in info:
            continue
        try:
            prev = parse_commit_page(fetch(BASE + info["sha"], cache_dir))
        except Exception:
            continue                         # old runs are purged (404)
        idx = {(tuple(b["platform"]), b["kind"], b["revdep"]): b for b in prev["jobs"]
               if b["target"] == info["prev"]}
        name = split_pkg(target)[0]

        def reason(job, version):
            # The reason with the release's version left out, normalised as in failure_key.
            return re.sub(r"[`\"']|-j \d+", "", (job["reason"] or "").replace(version, name)).strip()
        for j in failing:
            if j["target"] != target or j["variant"] in per_job:
                continue
            b = idx.get((tuple(j["platform"]), j["kind"], j["revdep"]))
            # The same job failing is not enough: the new release may fail
            # differently. The failure must be the same.
            if not (b and b["category"] == j["category"] and reason(b, info["prev"]) == reason(j, target)):
                continue
            pr = f"#{info['pr']}" if info.get("pr") else info["sha"][:12]
            why = f"{info['prev']} failed the same job in the CI of {pr}"
            if not j["revdep"] and re.match(r"The (compilation|installation) of \S+ failed at ", j["reason"] or ""):
                # The release's own build, and the reason names only the
                # step that failed, not the error: its code changed, so the
                # error may be another one. Noted, but not counted as
                # happening without the PR (`preexisting_unverified`).
                if notes is None:
                    continue
                notes.setdefault("unverified", set()).add(failure_key(j))
                why += " (failing at the same step; the errors were not compared)"
            per_job[j["variant"]] = why
    # opam-health-check: master, newest version of each package, no tests, on
    # the classic builder's default platform.
    if notes is not None and base:
        d = gh_api(f"repos/{REPO}/compare/{base}...{sha}", cache_dir)
        if d:
            notes["added"] = {m.group(1) for f in d.get("files", []) if f.get("status") == "added"
                              for m in [re.match(r"packages/[^/]+/([^/]+)/opam$", f.get("filename", ""))] if m}
    if src is not None:
        cache, asked, newest, stale = {}, set(), {}, set()

        def is_newest(target):
            # opam-health-check builds a reverse dependency with the newest
            # versions; a job testing it against an older version of the
            # target (a PR fixing old releases) is not comparable to that.
            if target not in newest:
                tn, tv = split_pkg(target)
                vs = src.versions(tn)
                newest[target] = not vs or all(opam_vcmp(tv, v) >= 0 for v in vs)
            return newest[target]
        for j in failing:
            rd, d = j["revdep"], job_dims(j)
            if (not rd or j["variant"] in per_job or j["category"] == "build-or-test" or j["kind"] == "tests"
                    or d["distro"] != CLASSIC_DISTRO or d["arch"] != "x86_64" or not d["ocaml"]):
                continue
            name, ver = split_pkg(rd)
            if name not in asked and len(asked) >= 25:
                continue
            asked.add(name)
            hc = health_check(src, name, cache)
            if not hc or hc[1] != ver or d["ocaml"] not in hc[0]:
                continue
            if health_run_stale(hc[2]):
                stale.add(hc[2])
                continue
            status, log = hc[0][d["ocaml"]]
            when = (f"opam-health-check, OCaml {d['ocaml']}, {short_date(hc[2])}" if hc[2]
                    else f"opam-health-check, OCaml {d['ocaml']}")
            if status == "bad":
                per_job[j["variant"]] = f"{rd} also fails to build on master ({when})"
            elif status == "partial":
                per_job[j["variant"]] = f"a dependency of {rd} also fails on master ({when})"
            if status == "good" and not is_newest(j["target"]):
                continue
            # A reverse dependency failing because one of its dependencies
            # failed: the package to bound is that dependency (awa 0.7.0 broke
            # git-mirage, and 16 packages failed through it).
            who = (failing_dep(j["reason"]) if j["category"] == "dependency" else None) or rd
            if notes is not None and status in ("bad", "partial", "good"):
                notes.setdefault("health", {}).setdefault(status, {}).setdefault(who, set()).add(
                    (d["ocaml"], j["variant"]))
                if who != rd:
                    notes.setdefault("via", {}).setdefault(who, set()).add(rd)
                notes.setdefault("run", hc[2])
            if status == "good" and notes is not None:
                notes.setdefault("new", {})[failure_key(j)] = (
                    f"{rd} builds on master ({when}), so this PR probably causes this failure.")
        if stale and notes is not None:
            notes["health_stale"] = max(stale)
        if stale:
            print(f"note: opam-health-check's latest results are from {short_date(max(stale))}, more than "
                  f"{HEALTH_MAX_AGE_DAYS} days ago: not used to tell pre-existing failures from new ones",
                  file=sys.stderr)
    out = {}
    for j in failing:
        out.setdefault(failure_key(j), []).append(per_job.get(j["variant"]))
    return {k: v[0] for k, v in out.items() if None not in v}


def sources_rules(data, cache_dir, match=None):
    """Source archives that were downloaded but could not be unpacked, one log
    per package (e.g. symlinks, which tar cannot create on msys2/cygwin)."""
    out, seen = [], set()
    for j in data["jobs"]:
        if j["status"] != "failed" or j["category"] != "sources" or (match and not re.search(match, j["variant"])):
            continue
        pkg = (re.search(r"sources of (\S+?):", j["reason"] or "") or [None, j["revdep"] or j["target"]])[1]
        dims = job_dims(j)
        key = (pkg, dims["os"])
        if key in seen:
            out[-1].setdefault("also_seen_on", []).append(j["variant"])
            out[-1]["jobs"].append(j["variant"])
            continue
        seen.add(key)
        try:
            text = log_text(fetch(j["url"], cache_dir))
        except Exception:
            text = ""
        m = re.search(r"([^\s:]+): Cannot create symlink to '([^']+)'", text)
        r = {"variant": j["variant"], "url": j["url"], "rule": "sources", "failing": pkg, "jobs": [j["variant"]],
             **fix_where("upstream", f"{pkg} (upstream archive)"), "experimental": j["experimental"],
             "error": (j["reason"] or "")[:200], "confidence": "medium"}
        if m:
            r.update(error=f"tar cannot create the symlink {m.group(1)} -> {m.group(2)} on {dims['distro']}",
                     confidence="high",
                     note=f"The source archive contains symlinks, which tar cannot create on {dims['distro']}. "
                          f"Replace them upstream by copies, or add `x-ci-accept-failures: [\"{dims['distro']}\"]` "
                          f"or `available: os != \"win32\"` to this release.")
        else:
            r["note"] = "The archive was downloaded but could not be unpacked. Check the url and the checksum."
        out.append(r)
    return out


def rerun_rules(data, results, match=None):
    """Jobs that failed for reasons unrelated to the packages (network errors,
    cancelled jobs, CI tool crashes): one entry listing what to rebuild."""
    jobs = [j for j in data["jobs"] if j["status"] == "failed" and j["category"] in ("network", "cancelled")
            and j["kind"] not in ("lint", "analysis") and (not match or re.search(match, j["variant"]))]
    transient = [r for r in results if r.get("rule") == "transient"]
    if not jobs and not transient:
        return results
    counts = collections.Counter()
    for j in jobs:
        m = re.search(r"HTTP error code (\d+)|curl failed|Could not resolve", j["reason"] or "")
        detail = (f"HTTP {m.group(1)}" + (", rate limit" if m.group(1) == "429" else "") if m and m.group(1)
                  else m.group(0) if m else None)
        counts[(j["category"], detail)] += 1
    for r in transient:
        counts[(None, r["error"])] += len(r.get("jobs") or [r["variant"]])

    def what(k, n):
        cat, detail = k
        if cat is None:              # a CI tool crash, from the log
            return f"{detail} ({counted(n, 'job')})"
        noun = {"network": "network error", "cancelled": "cancelled job"}.get(cat, f"{cat} job")
        return counted(n, noun) + (f" ({detail})" if detail else "")
    variants = merge_jobs([j["variant"] for j in jobs], [v for r in transient for v in r.get("jobs") or [r["variant"]]])
    exp = sum(j["experimental"] for j in jobs)
    entry = {"variant": counted(len(variants), "job"), "rule": "rerun", **fix_where("rerun", "CI (rerun)"),
             "error": ", ".join(what(k, n) for k, n in counts.most_common()),
             "confidence": "high", "jobs": variants,
             # Only experimental jobs to rerun do not hold the PR back.
             "experimental": exp == len(jobs) and all(r.get("experimental") for r in transient),
             "note": "These jobs failed because of the CI, not the packages. An opam-repository maintainer can "
                     "rerun them with \"Rebuild failed\" on the CI page."
                     + ((" One of them is on an experimental platform." if exp == 1 else
                         f" {exp} of them are on experimental platforms.") if exp else "")}
    return [r for r in results if r.get("rule") != "transient"] + [entry]


def pr_opam_changes(base, sha, cache_dir):
    """{"name.version": {"available": (old, new)}} for the opam files the commit
    `sha` modifies relative to master commit `base` (one GitHub API call)."""
    d = gh_api(f"repos/{REPO}/compare/{base}...{sha}", cache_dir) if base else None
    out = {}
    for f in (d or {}).get("files", []):
        m = re.match(r"packages/[^/]+/([^/]+)/opam$", f.get("filename", ""))
        if not m or f.get("status") != "modified":
            continue
        lines = (f.get("patch") or "").splitlines()

        def block(sign):
            got, inside = [], False
            for l in lines:
                if l[:1] == sign and l[1:].startswith("available:"):
                    inside = True
                    got.append(l[1:].strip())
                elif inside and l[:1] == sign and l[1:2] in (" ", "\t"):
                    got.append(l[1:].strip())
                else:
                    inside = False
            return " ".join(got).removeprefix("available:").strip()
        old, new = block("-"), block("+")
        if old != new:
            out[m.group(1)] = {"available": (old, new)}
    return out


# opam's os-distribution and os-family of the CI distributions (those known for sure).
OS_DISTRIBUTION = {"debian": "debian", "ubuntu": "ubuntu", "fedora": "fedora", "centos": "centos",
                   "alpine": "alpine", "archlinux": "arch", "cygwin": "cygwin", "msys2": "msys2",
                   "macos-homebrew": "homebrew", "freebsd": "freebsd"}
OS_FAMILY = {"debian": "debian", "ubuntu": "debian", "alpine": "alpine", "archlinux": "arch",
             "cygwin": "windows", "msys2": "windows", "freebsd": "bsd"}


def platform_vars(job):
    """The opam variables of a job's platform that `available` filters use."""
    d = job_dims(job)
    dist = re.sub(r"-[\d.]+$|-(unstable|testing)$", "", d["distro"] or "")
    v = {"os": d["os"], "arch": {"amd64": "x86_64", "aarch64": "arm64"}.get(d["arch"], d["arch"])}
    if dist in OS_DISTRIBUTION:
        v["os-distribution"] = OS_DISTRIBUTION[dist]
    if dist in OS_FAMILY:
        v["os-family"] = OS_FAMILY[dist]
    return v


FILTER_TOKEN = re.compile(r'\s*("(?:[^"\\]|\\.)*"|<=|>=|!=|[=<>&|!()]|[A-Za-z_][\w:+.-]*)')


def eval_filter(text, env):
    """An opam filter (e.g. an `available` field) on the variables `env`:
    True, False, or None when a variable not in `env` decides the result (or
    the filter is not understood). An empty filter is true."""
    text = (text or "").strip()
    if text.startswith("[") and text.endswith("]"):
        text = text[1:-1]
    toks, pos = [], 0
    while text[pos:].strip():
        m = FILTER_TOKEN.match(text, pos)
        if not m:
            return None
        toks.append(m.group(1))
        pos = m.end()
    i = 0

    def peek():
        return toks[i] if i < len(toks) else None

    def value():
        nonlocal i
        t = peek()
        if t is None or t in ("=", "!=", "<", "<=", ">", ">=", "&", "|", "!", "(", ")"):
            raise ValueError(t)
        i += 1
        if t.startswith('"'):
            return t[1:-1]
        return t if t in ("true", "false") else env.get(t)

    def atom():
        nonlocal i
        if peek() == "!":
            i += 1
            v = atom()
            return None if v is None else not v
        if peek() == "(":
            i += 1
            v = disj()
            if peek() != ")":
                raise ValueError(")")
            i += 1
            return v
        a = value()
        op = peek()
        if op in ("=", "!=", "<", "<=", ">", ">="):
            i += 1
            b = value()
            if a is None or b is None:
                return None
            c = (0 if a == b else 1) if op in ("=", "!=") else opam_vcmp(a, b)
            return {"=": c == 0, "!=": c != 0, "<": c < 0, "<=": c <= 0, ">": c > 0, ">=": c >= 0}[op]
        return {"true": True, "false": False}.get(a)

    def conj():
        nonlocal i
        vs = [atom()]
        while peek() == "&":
            i += 1
            vs.append(atom())
        return False if False in vs else None if None in vs else True

    def disj():
        nonlocal i
        vs = [conj()]
        while peek() == "|":
            i += 1
            vs.append(conj())
        return True if True in vs else None if None in vs else False
    if not toks:
        return True
    try:
        v = disj()
    except ValueError:
        return None
    return v if i == len(toks) else None


def upper_bound(line):
    """The lowest upper bound (`<` or `<=` version) of a dependency line, or None."""
    ups = [v for _, v in re.findall(r'(<=|<)\s*"([^"]+)"', line)]
    return min(ups, key=functools.cmp_to_key(opam_vcmp)) if ups else None


def pr_added_bounds(base, sha, cache_dir):
    """{"name.version": {dep: new line}} for the `depends` lines whose upper
    bound (`<`, `<=`) the commit `sha` adds or lowers in an opam file it
    modifies, relative to master commit `base`: not a changed lower bound,
    nor `conflicts` or `depopts`. The same (cached) compare call as
    pr_opam_changes."""
    d = gh_api(f"repos/{REPO}/compare/{base}...{sha}", cache_dir) if base else None
    out = {}
    for f in (d or {}).get("files", []):
        m = re.match(r"packages/[^/]+/([^/]+)/opam$", f.get("filename", ""))
        if not m or f.get("status") != "modified":
            continue
        old, new, field = {}, {}, None
        for l in (f.get("patch") or "").splitlines():
            if l.startswith("@@"):
                # The hunk header names the field the hunk starts in (git's default function context).
                h = re.match(r"@@[^@]*@@\s*([a-z][\w-]*):", l)
                field = h.group(1) if h else None
                continue
            fm = re.match(r"[-+ ]?([a-z][\w-]*):", l)
            if fm:
                field = fm.group(1)
            dm = re.match(r'([-+])\s*"([\w+.-]+)"\s*(\{[^}]*\})?', l)
            if dm and field == "depends":
                (old if dm.group(1) == "-" else new)[dm.group(2)] = l[1:].strip()
        added = {}
        for dep, line in new.items():
            up, was = upper_bound(line), upper_bound(old.get(dep) or "")
            if up and (was is None or opam_vcmp(up, was) < 0):
                added[dep] = line
        if added:
            out[m.group(1)] = added
    return out


def relaxed_available_rules(data, sha, base, cache_dir, pr_packages):
    """A PR that removes or relaxes the `available` filter of an existing
    version makes CI try it on platforms where it never ran: failures there
    are most likely caused by that change. Only the failures of that package
    (its own jobs, and those of its reverse dependencies) on platforms the old
    filter excluded and the new one allows count."""
    changes = pr_opam_changes(base, sha, cache_dir)
    out = []

    def uses(f, names):
        return any(re.search(rf"(?<![\w-]){n}(?![\w-])", f or "") for n in names)
    for pkg, ch in sorted(changes.items()):
        old, new = ch["available"]
        if not old:
            continue                     # a filter was added: not a relaxation
        failing, exact = [], True
        for j in data["jobs"]:
            if j["category"] not in FAILING_CATS or pkg not in (j["target"], j["revdep"]):
                continue
            env = platform_vars(j)
            was, now = eval_filter(old, env), eval_filter(new, env)
            if was is None or now is None:
                # Not evaluated: at least, the old filter restricts the
                # platform's os (or arch, on Linux) and the new one no longer does.
                names = ("os", "os-distribution", "os-family") if env["os"] != "linux" else ("arch",)
                if not (uses(old, names) and not uses(new, names)):
                    continue
                exact = False
            elif was or not now:
                continue
            failing.append(j)
        if not failing:
            continue
        plats = sorted({(job_dims(j)["distro"] or job_dims(j)["os"]) + (" (experimental)" if j["experimental"] else "")
                        for j in failing})
        out.append({"variant": f"{pkg} [available changed by this PR]", "failing": pkg, **fix_where("pr", "this PR"),
                    "rule": "pr-relaxed-available",
                    "error": f"This PR changes `available` of {pkg} from `{old}` to "
                             f"`{new or '(removed)'}`; {pkg} now fails on {', '.join(plats)}",
                    "old": f"available: {new}" if new else None,
                    "new": f"available: {old}   (restore it, or narrow it to the platforms that build)",
                    "confidence": "high" if exact else "medium",
                    "jobs": [j["variant"] for j in failing],
                    "note": "The previous filter excluded each of these platforms." if exact else
                            "Check that the previous filter excluded these platforms."})
    return out


# A job that failed because of the CI rather than what it checks.
CI_FAILURE = re.compile(r"Cancelled|curl failed|HTTP error code|Could not resolve|Connection (?:refused|reset|timed out)"
                        r"|exited with status 128")


def analysis_rules(data, cache_dir, match=None):
    """A failed analysis job: opam-repo-ci could not work out which packages
    the PR changes, so no build job ran. Its reason comes from the CI page,
    or from the end of its log."""
    out = []
    for j in data["jobs"]:
        if j["kind"] != "analysis" or j["status"] != "failed" or (match and not re.search(match, j["variant"])):
            continue
        why = j.get("reason") or next((m for m in j.get("messages") or [] if m.strip()), "")
        if not why:
            try:
                ex = excerpt(log_text(fetch(j["url"], cache_dir)))
                why = ex.get("job_failed") or ex.get("final") or ""
            except Exception:
                pass
        r = {"variant": j["variant"], "url": j["url"], "rule": "analysis", "jobs": [j["variant"]],
             "experimental": j["experimental"], "confidence": "medium",
             "error": " ".join(why.split())[:300] or "The CI page gives no reason"}
        if CI_FAILURE.search(why):
            r.update(fix_where("rerun", "CI (rerun)"),
                     note="The analysis job failed because of the CI, so no build job ran. Rerun it.")
        else:
            r.update(fix_where("pr", "this PR"),
                     note="opam-repo-ci could not analyse this PR, so no build job ran. The cause is usually in "
                          "the PR's files (an opam file that cannot be read, or a file in an unexpected place): "
                          "see the reason above, or the job's log.")
        out.append(r)
    return out


def lint_baseline(data, cache_dir):
    """(PR number, {(package name, message)}) of the lint errors in the CI run
    of the PR that published the previous version of a package of this PR."""
    for target in sorted({j["target"] for j in data["jobs"] if j["target"]}):
        info = find_baseline_commit(target, cache_dir)
        if not info or "sha" not in info:
            continue
        try:
            base = parse_commit_page(fetch(BASE + info["sha"], cache_dir))
        except (urllib.error.HTTPError, RuntimeError):
            return None
        msgs = set()
        for j in base["jobs"]:
            for m in j.get("messages", []):
                mm = LINT_MESSAGE.match(m)
                if mm:
                    msgs.add((mm.group(2), mm.group(4)))
        return info.get("pr"), msgs
    return None


# A finding of opam-ci-check: "Error in pkg.ver: …" or "Warning in pkg.ver: …".
# It fails the lint job on warnings too.
LINT_MESSAGE = re.compile(r"(Error|Warning) in ([\w+-]+)\.(\S+?): (.*)$")


def lint_rules(data, src, sha=None, cache_dir=".opam-ci-cache"):
    out = []
    baseline = None
    if any(j["kind"] == "lint" and j["status"] == "failed" for j in data["jobs"]):
        baseline = lint_baseline(data, cache_dir)
    for j in data["jobs"]:
        if j["kind"] != "lint" or j["status"] != "failed":
            continue
        if not any(LINT_MESSAGE.match(m) for m in j["messages"]):
            # No finding: the job failed before linting (the CI could not fetch or check out the PR).
            msg = next((m for m in j["messages"] if m.strip()), "") or "no message"
            git = re.search(r'"git"', msg) and re.search(r"status 128", msg)
            out.append({"variant": "(lint)", **fix_where("rerun", "CI (rerun)"), "rule": "lint-ci-error",
                        "error": msg[:220], "url": j.get("url"),
                        "note": ("The lint job failed before linting anything"
                                 + (": git could not check out the PR's commit" if git else "")
                                 + ". Rerun it; if it fails again the same way, push the branch again.")})
            continue
        for msg in j["messages"]:
            m = LINT_MESSAGE.match(msg)
            if not m:
                continue
            level, name, ver, text = m.groups()
            r = {"variant": "(lint)", "failing": f"{name}.{ver}", **fix_where("pr", "this PR"), "rule": "lint",
                 "error": text[:220], "lint_level": level.lower()}
            if "warning 62" in text:
                val = (re.findall(r'"([^"]+)"', text) or [""])[-1]
                lid = None
                gm = re.match(r"https://github\.com/([^/]+/[^/]+)/blob/(.+)$", val)
                if gm:
                    lid = license_id(src.raw(f"https://raw.githubusercontent.com/{gm.group(1)}/{gm.group(2)}"))
                if lid:
                    r.update(old=f'license: "{val}"', new=f'license: "{lid.split(" ")[0]}"',
                             confidence="high" if "check" not in lid else "medium",
                             note=f"Detected from the license text ({lid}).")
                else:
                    r.update(note="Use an SPDX identifier (https://spdx.org/licenses/).", confidence="medium")
            elif "warning 68" in text and "license" in text:
                lic, origin = None, None
                for v in [v for v in reversed(src.versions(name) or []) if v != ver][:2]:
                    lic = opam_field(src.opam_file(name, v), "license")
                    if lic:
                        origin = f"{name}.{v} in opam-repository"
                        break
                if not lic:
                    up = src.upstream(name, ver, sha) if sha else None
                    gm = re.match(r"https://github\.com/([^/]+/[^/]+)$", up or "")
                    for fn in (("LICENSE", "LICENSE.md", "LICENSE.txt", "COPYING") if gm else ()):
                        lid = license_id(src.raw(f"https://raw.githubusercontent.com/{gm.group(1)}/HEAD/{fn}"))
                        if lid:
                            lic, origin = lid.split(" ")[0], f"{fn} in {up}" + (" (check)" if "check" in lid else "")
                            break
                if lic:
                    r.update(old=None, new=f'license: "{lic}"', confidence="high" if "(check)" not in origin else "medium",
                             note=f"Taken from {origin}.")
                else:
                    r.update(note="Add a `license:` field with an SPDX identifier.", confidence="medium")
            elif "warning 47" in text:
                opam = src.opam_file(name, ver, sha) if sha else None
                syn = opam_field(opam, "synopsis")
                if syn:
                    fixed = syn.strip().rstrip(".")
                    fixed = fixed[:1].upper() + fixed[1:]
                    r.update(old=f'synopsis: "{syn}"', new=f'synopsis: "{fixed}"', confidence="high")
                else:
                    r.update(note="Start the synopsis with a capital letter, without a final dot.", confidence="high")
            elif "warning 59" in text or ("Weak checksum" in text and "field url" in text):
                opam = src.opam_file(name, ver, sha) if sha else None
                cmd = checksum_command(url_src(opam))
                r.update(new='checksum: ["sha256=<hash>" "sha512=<hash>"]   (in the url section)',
                         note=(f"Compute it with `{cmd}` (or sha512sum)." if cmd else
                               "Download the archive and compute its checksum with sha256sum (or sha512sum)."),
                         confidence="high")
            elif "extra-files" in text or "Unexpected file in packages/" in text:
                opam = src.opam_file(name, ver, sha) if sha else None
                section = (opam or "").split("extra-files:", 1)[1] if "extra-files:" in (opam or "") else ""
                section = re.split(r"(?m)^[a-z][\w-]*:", section, 1)[0]      # up to the next field
                files = re.findall(r'\[\s*"([^"]+)"\s+"(?:md5|sha\d+)=', section)
                f0 = files[0] if files else "<file>"
                r.update(new=f'extra-source "{f0}" {{ src: "<URL where {f0} is published>" '
                             f'checksum: ["sha256=<hash>"] }}',
                         note="opam-repository no longer accepts files/: publish the file (in the upstream "
                              "repository, or as a release asset), refer to it with extra-source, and delete "
                              f"extra-files and packages/{name}/{name}.{ver}/files/."
                              + (f" Files: {', '.join(files)}." if files else ""), confidence="high")
            elif "Weak checksum" in text:
                r.update(note="Use SHA-256 or SHA-512 checksums (compute them with sha256sum or sha512sum).",
                         confidence="high")
            elif "error 60" in text:
                opam = src.opam_file(name, ver, sha) if sha else None
                cmd = checksum_command(url_src(opam))
                r.update(note="The upstream archive no longer matches the checksum (the tag moved, or the archive "
                              "was regenerated). " + (f"Recompute it with `{cmd}`" if cmd else
                                                      "Download the archive and compute its checksum again")
                              + ", and check with upstream that the release did not change.", confidence="high")
            elif "error 46" in text:
                r.update(note='Flagged "conf" but has url, build or install, while conf packages only check for system '
                              'packages. Remove the `conf` flag (and the `conf-` prefix) if it really builds '
                              'something, or drop url, build and install. A conf package that builds something '
                              'needs the maintainers\' agreement.', confidence="medium")
            elif "warning 41" in text:
                named = re.findall(r'"([\w+-]+)"', text)
                if name in named:
                    r.update(old=f"%{{{name}:VAR}}%", new="%{_:VAR}%   (in build/install/remove)",
                             confidence="high", note="The package refers to its own variables by name: use the `_` prefix.")
                else:
                    r.update(note=f"Add {', '.join(named)} to depends or depopts.", confidence="high")
            elif "No package source directory" in text:
                opam = src.opam_file(name, ver, sha) if sha else None
                if url_src(opam):
                    r.update(note="The package has a url, but its source could not be fetched or verified (see the "
                                  "other lint errors, such as a checksum mismatch).", confidence="medium")
                else:
                    r.update(note="Known false positive of opam-ci-check for packages without `url` (virtual "
                                  "packages): maintainers accept it.", confidence="high")
            elif "conf packages should always use" in text:
                opam = src.opam_file(name, ver, sha) if sha else None
                if re.search(r"depexts:[\s\S]*?npm-version", opam or ""):
                    r.update(note="The depexts are npm packages (`npm-version` filter, checked by "
                                  "opam-check-npm-deps), the usual convention for Melange bindings. This lint "
                                  "error is a false positive: nothing to change, maintainers accept it.",
                             confidence="high")
                else:
                    r.update(note="This package has `depexts` but is not a conf package: move them into a conf-* "
                                  "package, or ask the maintainers to accept it (they often do).", confidence="medium")
            else:
                r["note"] = ("The tool has no advice for this lint message." if level == "Error" else
                             "A warning, but the lint job fails on warnings too: follow the message.")
            if baseline and (name, text) in baseline[1]:
                prev = (r.get("note") or "").rstrip(".")
                r["note"] = ((prev + ". " if prev else "") + f"Already reported in the CI of #{baseline[0]} "
                             "(the previous release), which was merged.")
            # one entry for the same message and explanation on several packages
            same = next((o for o in out if not r.get("new") and not o.get("new") and o["error"] == r["error"]
                         and o.get("note") == r.get("note")), None)
            if same:
                same["failing"] += ", " + r["failing"]
                continue
            # one entry per (package, fix): several lint messages often share a fix
            if r.get("new") and any(o["failing"] == r["failing"] and o.get("new") == r["new"] for o in out):
                prev = next(o for o in out if o["failing"] == r["failing"] and o.get("new") == r["new"])
                prev["error"] += " | " + r["error"][:120]
                continue
            out.append(r)
    return out


def skipped_rules(data, src, cache_dir, pr_packages, bounds=None):
    """A PR package that installs nowhere: report the solver's conflict chain.
    When every conflict goes through a dependency whose upper bound the PR
    adds (`bounds`, from pr_added_bounds), that is the point of the change
    (an old release restricted to old compilers, say): explained as such."""
    out = []
    for t in pr_packages:
        own = [j for j in data["jobs"] if j["target"] == t and j["kind"] == "build"]
        if len(own) < 3 or any(j["category"] != "not-available" for j in own):
            continue
        pick = sorted(own, key=lambda j: (not j["variant"].startswith("compilers,"), j["variant"]))[-1:]
        chains, unknown, no_version = [], set(), set()
        for j in pick:
            try:
                txt = log_text(fetch(j["url"], cache_dir))
            except Exception:
                continue
            # opam writes the package with the constraint that pulled it in, if
            # any: "pkg = 1.0 -> …", "pkg >= 0.7 -> …" or "pkg -> …"; the line
            # after a chain gives the reason, such as "unknown package".
            for chain, why in re.findall(rf"^\s*- ({re.escape(t.split('.')[0])}(?: [<>=!]+ \S+)? -> .+)\n\s*(.*)$",
                                         txt, re.M):
                chains.append(chain)
                last = chain.split(" -> ")[-1]
                if why.strip() == "unknown package":
                    unknown.add(last.split(" ")[0])
                elif why.strip() == "no matching version" and " " in last and "|" not in last:
                    no_version.add(last)
        if not chains:
            continue
        firsts = {c.split(" -> ")[1] for c in chains if " -> " in c}
        r = {"variant": f"{t} [all build jobs skipped]", "failing": t, **fix_where("pr", "this PR"),
             "rule": "uninstallable",
             "error": "Not installable on any tested platform. Solver: " + "; ".join(sorted(set(chains))[:3])}
        added = (bounds or {}).get(t) or {}
        through = {f.split(" ")[0] for f in firsts}
        bounded = added and through and through <= set(added) and not unknown
        lines = "; ".join(f"`{added[d]}`" for d in sorted(through)) if bounded else ""
        # A bound that no version satisfies (`{>= "3.0" & < "2.0"}`) is a mistake, not the point of the change.
        from .suggest import VERSION_TERM, unsatisfiable       # (suggest imports this module)
        empty = sorted(d for d in through if bounded and "|" not in added[d]
                       and unsatisfiable(VERSION_TERM.findall(added[d])))
        if empty:
            r.update(confidence="high",
                     note=f"This PR adds {lines} to {t}, and no version of {', '.join(empty)} satisfies "
                          f"{'it' if len(empty) == 1 else 'them'}: a lower bound is above an upper bound. Fix "
                          f"the bound.")
        elif bounded:
            r.update(fix_where("none", "nothing (this PR's bound)"), rule="bounded-out", confidence="high",
                     note=f"This PR adds {lines} to {t}, which no tested platform satisfies, so CI cannot "
                          f"build it. That is expected when the bound is the point of the change.")
        elif unknown:
            names = ", ".join(f"`{u}`" for u in sorted(unknown))
            r.update(confidence="high",
                     note=f"{names} {'is' if len(unknown) == 1 else 'are'} not in opam-repository. Add "
                          f"{'it' if len(unknown) == 1 else 'them'} to this PR, or wait until "
                          f"{'it is' if len(unknown) == 1 else 'they are'} published.")
        elif no_version:
            dep, _, cstr = sorted(no_version)[0].partition(" ")
            r.update(confidence="high",
                     note=f"No version of `{dep}` in opam-repository satisfies `{cstr}`. Publish that version "
                          f"first (or in this PR), or relax the constraint if an existing version works.")
        elif len(firsts) == 1:
            first = firsts.pop()
            ends = sorted({c.split(" -> ")[2] for c in chains if c.count(" -> ") >= 2})
            r.update(confidence="high",
                     note=f"Every solver conflict goes through `{first}`"
                          + (f", which requires {' or '.join(ends)}" if ends else "")
                          + ". Relax or drop that constraint, after checking which versions of the dependency "
                            "really fail.")
        out.append(r)
    return out


def accepted_elsewhere(results, src, pr_packages, max_versions=3):
    """For a suggestion that restricts platforms (x-ci-accept-failures, or
    available on Windows), the other versions of the package that already
    list those platforms in x-ci-accept-failures: maintainers accepted them
    there, so the same is likely acceptable here."""
    cache = {}
    for r in results:
        new = r.get("new") or ""
        failing = r.get("failing") or ""
        name, ver = split_pkg(failing)
        if not ver or " " in failing:
            continue
        m = re.match(r"x-ci-accept-failures:\s*\[([^\]]*)\]", new)
        if m:
            wanted = set(re.findall(r'"([^"]+)"', m.group(1)))
        elif 'os != "win32"' in new:
            wanted = {"cygwin", "msys2"}
        else:
            continue
        if name not in cache:
            others = [v for v in reversed(src.versions(name) or []) if v != ver][:max_versions]
            cache[name] = {v: accepted_platforms(src.opam_file(name, v)) for v in others}
        found = {v: sorted(acc & wanted) for v, acc in cache[name].items() if acc & wanted}
        if not found:
            continue
        where = ", ".join(f"{name}.{v} ({', '.join(ps)})" for v, ps in sorted(found.items()))
        plats = sorted(set().union(*map(set, found.values())))
        r["note"] = ((r.get("note") or "").rstrip() + f" Other versions already accept failures there: {where}.").strip()
        if not m:
            r["alternative"] = (f'x-ci-accept-failures: [{" ".join(chr(34) + p + chr(34) for p in plats)}]'
                                f"   (as other versions of {name} do)")
