"""PR-level checks: pre-existing failures, opam-health-check, accepted failures, reruns, sources, the PR's own diff, lint, uninstallable packages."""

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

from .net import (  # noqa: F401
    BASE,
    REPO,
    fetch,
    gh_api,
)
from .ci import (  # noqa: F401
    CLASSIC_DISTRO,
    FAILING_CATS,
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
from .rules import (  # noqa: F401
    direct_deps,
)


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
            r.update(rule="accept-failures-ignored", fix_in="the CI environment (day10 builder)", confidence="high",
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


def health_summary(notes, src=None):
    """Entries comparing the failing reverse dependencies with opam-health-check
    (master): the ones that also fail there, and the ones that build there."""
    h, run = notes.get("health") or {}, notes.get("run")
    when = f" (run of {run})" if run else ""
    out = []

    def listing(d):
        return ", ".join(f"{rd} (OCaml {', '.join(sorted({v for v, _ in xs}))})" for rd, xs in sorted(d.items()))
    good = h.get("good") or {}
    for rd, xs in sorted(good.items()):
        # Broken by this PR: the usual fix in opam-repository is an upper
        # bound on the new release in the reverse dependency, in the same PR.
        targets = sorted({m.group(1) for _, v in xs for m in [re.search(r",([^,]+),revdeps,", v)] if m})
        target = targets[0] if targets else None
        via = sorted((notes.get("via") or {}).get(rd, ()))
        r = {"variant": f"{rd}, reverse dependency" + (f" of {target}" if target else ""),
             "rule": "health-check-new", "fix_in": "this PR", "confidence": "medium", "failing": rd,
             "error": (f"{rd} fails here, so {len(via)} reverse dependenc{'ies' if len(via) > 1 else 'y'} that "
                       f"build on master in opam-health-check{when} fail too ({', '.join(via[:6])}"
                       f"{' …' if len(via) > 6 else ''})" if via else
                       f"{rd} builds on master in opam-health-check{when} but fails here")
                      + f" (OCaml {', '.join(sorted({v for v, _ in xs}))})",
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
            else:
                old, new = None, f'conflicts: [ "{tn}" {{>= "{tv}"}} ]   (added to any existing `conflicts`)'
            r.update(dep=tn, old=old, new=new,
                     note=f"Probably broken by {target}. The usual fix in opam-repository is this upper bound in "
                          f"{rd}'s opam file, in the same PR (or a fixed release of {rn}). opam-health-check "
                          f"builds the newest version of each package, without tests.")
        out.append(r)
    bad = {**(h.get("bad") or {}), **(h.get("partial") or {})}
    if bad:
        out.append({"variant": f"{len(bad)} reverse dependenc{'ies' if len(bad) > 1 else 'y'}",
                    "rule": "health-check-preexisting", "fix_in": "the reverse dependencies (other packages)",
                    "confidence": "high", "failing": ", ".join(sorted(bad)),
                    "error": f"also fail on master in opam-health-check{when}: {listing(bad)}",
                    "note": "Pre-existing: these reverse dependencies fail without this PR (or one of their "
                            "dependencies does). Nothing to change in this PR for them.",
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
        for j in failing:
            if j["target"] != target or j["variant"] in per_job:
                continue
            b = idx.get((tuple(j["platform"]), j["kind"], j["revdep"]))
            if b and b["category"] in FAILING_CATS:
                pr = f"#{info['pr']}" if info.get("pr") else info["sha"][:12]
                per_job[j["variant"]] = f"{info['prev']} failed the same job in the CI of {pr}"
    # opam-health-check: master, newest version of each package, no tests, on
    # the classic builder's default platform.
    if notes is not None and base:
        d = gh_api(f"repos/{REPO}/compare/{base}...{sha}", cache_dir)
        if d:
            notes["added"] = {m.group(1) for f in d.get("files", []) if f.get("status") == "added"
                              for m in [re.match(r"packages/[^/]+/([^/]+)/opam$", f.get("filename", ""))] if m}
    if src is not None:
        cache, asked, newest = {}, set(), {}

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
            status, log = hc[0][d["ocaml"]]
            when = f"OCaml {d['ocaml']}, run of {hc[2]}" if hc[2] else f"OCaml {d['ocaml']}"
            if status == "bad":
                per_job[j["variant"]] = f"{rd} also fails to build on master in opam-health-check ({when})"
            elif status == "partial":
                per_job[j["variant"]] = (f"a dependency of {rd} also fails on master in opam-health-check "
                                         f"({when})")
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
                    f"{rd} builds on master in opam-health-check ({when}): this failure is probably caused "
                    f"by this PR.")
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
             "fix_in": f"{pkg} (upstream archive)", "experimental": j["experimental"],
             "error": (j["reason"] or "")[:200], "confidence": "medium"}
        if m:
            r.update(error=f"tar cannot create the symlink {m.group(1)} -> {m.group(2)} on {dims['distro']}",
                     confidence="high",
                     note=f"The source archive contains symlinks, which tar cannot create on {dims['distro']}. "
                          f"Replace them upstream by copies, or add `x-ci-accept-failures: [\"{dims['distro']}\"]` "
                          f"(or `available: os != \"win32\"`) to this release.")
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
        m = re.search(r"HTTP error code \d+|curl failed|Could not resolve", j["reason"] or "")
        counts[j["category"] + (f" ({m.group(0)})" if m else "")] += 1
    for r in transient:
        counts[r["error"]] += len(r.get("jobs") or [r["variant"]])
    variants = merge_jobs([j["variant"] for j in jobs], [v for r in transient for v in r.get("jobs") or [r["variant"]]])
    exp = sum(j["experimental"] for j in jobs)
    entry = {"variant": f"{len(variants)} job{'s' if len(variants) != 1 else ''}", "rule": "rerun", "fix_in": "CI (rerun)",
             "error": ", ".join(f"{n} {k}" for k, n in counts.most_common()),
             "confidence": "high", "jobs": variants,
             "note": "These jobs failed for reasons unrelated to the packages. Use \"Rebuild failed\" on the "
                     "CI page (opam-repository maintainers can do it)."
                     + (f" {exp} of them are on experimental platforms." if exp else "")}
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
        out.append({"variant": f"{pkg} [available changed by this PR]", "failing": pkg, "fix_in": "this PR",
                    "rule": "pr-relaxed-available",
                    "error": f"this PR changes `available` of {pkg} from `{old}` to "
                             f"`{new or '(removed)'}`; {pkg} now fails on {', '.join(plats)}",
                    "old": f"available: {new}" if new else None,
                    "new": f"available: {old}   (restore it, or narrow it to the platforms that build)",
                    "confidence": "high" if exact else "medium",
                    "jobs": [j["variant"] for j in failing],
                    "note": "The previous filter excluded each of these platforms." if exact else
                            "Check that the previous filter excluded these platforms."})
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
                mm = re.match(r"Error in ([\w+-]+)\.\S+?: (.*)$", m)
                if mm:
                    msgs.add(mm.groups())
        return info.get("pr"), msgs
    return None


def lint_rules(data, src, sha=None, cache_dir=".opam-ci-cache"):
    out = []
    baseline = None
    if any(j["kind"] == "lint" and j["status"] == "failed" for j in data["jobs"]):
        baseline = lint_baseline(data, cache_dir)
    for j in data["jobs"]:
        if j["kind"] != "lint" or j["status"] != "failed":
            continue
        if not any(re.match(r"Error in [\w+-]+\.\S+?: ", m) for m in j["messages"]):
            # The job failed before linting: the CI could not fetch or check out the PR.
            msg = next((m for m in j["messages"] if m.strip()), "") or "no message"
            git = re.search(r'"git"', msg) and re.search(r"status 128", msg)
            out.append({"variant": "(lint)", "fix_in": "CI (rerun)", "rule": "lint-ci-error",
                        "error": msg[:220], "url": j.get("url"),
                        "note": ("The lint job failed before linting anything"
                                 + (": git could not check out the PR's commit" if git else "")
                                 + ". Rerun it; if it fails again the same way, push the branch again.")})
            continue
        for msg in j["messages"]:
            m = re.match(r"Error in ([\w+-]+)\.(\S+?): (.*)$", msg)
            if not m:
                continue
            name, ver, text = m.groups()
            r = {"variant": "(lint)", "failing": f"{name}.{ver}", "fix_in": "this PR", "rule": "lint",
                 "error": text[:220]}
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
                for v in [v for v in reversed(src.versions(name)) if v != ver][:2]:
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
                u = url_src(opam) or "<archive URL>"
                r.update(new='checksum: ["sha256=<hash>" "sha512=<hash>"]   (in the url section)',
                         note=f"Compute it with: curl -sL {u} | sha256sum (or sha512sum).", confidence="high")
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
                u = url_src(opam) or "<archive URL>"
                r.update(note="The upstream archive no longer matches the checksum (the tag moved, or the archive "
                              f"was regenerated). Recompute it with curl -sL {u} | sha256sum, and check with "
                              "upstream that the release did not change.", confidence="high")
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
                r["note"] = "No rule for this lint message."
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


def skipped_rules(data, src, cache_dir, pr_packages):
    """A PR package that installs nowhere: report the solver's conflict chain."""
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
                elif why.strip() == "no matching version" and " " in last:
                    no_version.add(last)
        if not chains:
            continue
        firsts = {c.split(" -> ")[1] for c in chains if " -> " in c}
        r = {"variant": f"{t} [all build jobs skipped]", "failing": t, "fix_in": "this PR", "rule": "uninstallable",
             "error": "not installable on any tested platform; solver: " + "; ".join(sorted(set(chains))[:3])}
        if unknown:
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
            others = [v for v in reversed(src.versions(name)) if v != ver][:max_versions]
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
