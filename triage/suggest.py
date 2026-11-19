"""The `suggest` driver: runs every rule, merges, annotates and ranks the results."""

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
    fetch,
)
from .ci import (  # noqa: F401
    describe_job,
    excerpt,
    failure_key,
    job_dims,
    log_text,
    merge_jobs,
    split_pkg,
)
from .rules import (  # noqa: F401
    complete_test_output,
    lower_bounds_rule,
    suggest_for_job,
)
from .matrix import (  # noqa: F401
    collapse_systemic,
    matrix_rules,
)
from .context import (  # noqa: F401
    accepted_but_failing,
    health_summary,
    lint_rules,
    preexisting_failures,
    relaxed_available_rules,
    rerun_rules,
    skipped_rules,
    sources_rules,
)
from .llm import (  # noqa: F401
    apply_llm,
)


def suggest(sha, data, src, cache_dir, max_logs, match, llm=None):
    pr_packages = sorted({j["target"] for j in data["jobs"] if j["target"]})
    cats = {"build", "build-or-test", "solver", "dependency"}
    sel = [j for j in data["jobs"] if j["status"] == "failed" and j["category"] in cats
           and (not match or re.search(match, j["variant"]))]
    # One job per (package, revdep, kind, reason): the same error on many platforms is one problem.
    def group_key(j):
        # A dependency failure is the dependency's: one log per (cause, platform).
        return (("dependency", j["reason"], job_dims(j)["distro"]) if j["category"] == "dependency"
                else failure_key(j))
    # `merged` keeps every job behind each analysed one, so the report can say how many it stands for.
    merged, uniq = {}, []
    # Classic-builder logs first: they show the depext installation (versions).
    for j in sorted(sel, key=lambda j: (j["experimental"], j["platform"][:1] == ["day10"])):
        key = group_key(j)
        if key not in merged:
            merged[key] = []
            uniq.append(j)
        merged[key].append(j["variant"])
    if len(uniq) > max_logs:
        print(f"note: {len(uniq)} distinct failures, analysing the first {max_logs} (use --max)", file=sys.stderr)
        uniq = uniq[:max_logs]
    results, base = [], None
    for j in uniq:
        try:
            text = log_text(fetch(j["url"], cache_dir))
        except Exception as e:
            results.append({"variant": j["variant"], "note": f"Could not read the log: {e}",
                            "jobs": merged[group_key(j)]})
            continue
        base = base or excerpt(text).get("base")      # the master commit the PR was merged with
        r = suggest_for_job(src, j, text, pr_packages, sha)
        if r.get("rule") == "test-output":
            complete_test_output(src, r, j, text, data, cache_dir)
        lb = lower_bounds_rule(src, j, text, r, pr_packages, sha)
        r["_key"] = failure_key(j)
        r["jobs"] = merged[group_key(j)]
        results.append(r)
        if lb:
            lb.update(_key=failure_key(j), jobs=merged[group_key(j)])
            results.append(lb)
    failing_in_log = {r["_key"]: r["failing"] for r in results if r.get("failing")}
    # Jobs that failed because of the CI (tool crash, network) say nothing
    # about the package: leave them out of the pass/fail patterns.
    transient = {r["_key"] for r in results if r.get("rule") == "transient"}
    jobs = [j for j in data["jobs"] if not (j["status"] == "failed" and failure_key(j) in transient)]
    matrix = collapse_systemic(matrix_rules(jobs, results, match, set(pr_packages), failing_in_log))
    # A matrix suggestion explains the per-job failures that had no rule.
    explained = {k for m in matrix for k in m.get("_keys", [])}
    results = [r for r in results
               if r.get("keep") or not (r.get("_key") in explained and not r.get("new") and not r.get("candidates"))]
    if data.get("unfinished"):
        for m in matrix:
            m["note"] = ("CI is still running: this pattern comes from the jobs finished so far and may "
                         "change. " + (m.get("note") or ""))
    results += matrix
    accepted_but_failing(results, src, sha, set(pr_packages))
    new_notes = {}
    pre = preexisting_failures(data, sha, base, cache_dir, src, new_notes)
    for r in results:
        keys = r.get("_keys") or ([r["_key"]] if r.get("_key") else [])
        hints = {new_notes["new"][k] for k in keys if k in new_notes.get("new", {})}
        if hints and not any(k in pre for k in keys):
            r["new_on_master"] = sorted(hints)[0]
            r["note"] = sorted(hints)[0] + (" " + r["note"] if r.get("note") else "")
        why = {pre.get(k) for k in keys}
        if keys and None not in why:
            r["preexisting"] = (sorted(why)[0] if len(why) == 1 or not all(w.startswith("this PR only adds")
                                                                           for w in why)
                                else "this PR only adds upper bounds to these packages, so their dependencies "
                                     "are the same as on master")
            r["note"] = f"Pre-existing: {r['preexisting']}." + (" " + r["note"] if r.get("note") else "")
    results += health_summary(new_notes)
    for r in results:
        r.pop("_key", None)
        r.pop("_keys", None)
        r.pop("_distros", None)
        r.pop("keep", None)
    if llm:
        data["_sha"] = sha
        apply_llm(results, data, src, cache_dir, pr_packages, llm["limit"], llm["model"], llm["dry_run"])

    # Old releases failing on newer platforms: maintainers mark them
    # `x-maintained: false` so that the next archiving round removes them.
    for r in results:
        if r.get("rule") not in ("platform-matrix", "compiler-matrix", "missing-system-package", "missing-header",
                                 "newer-c-compiler"):
            continue
        name, ver = split_pkg(r.get("failing") or "")
        vs = src.versions(name) if name and ver else []
        if vs and ver in vs and vs[-1] != ver and not r.get("alternative"):
            r["alternative"] = (f"x-maintained: false  (old release, {name}.{vs[-1]} exists: the next "
                                f"archiving round can remove it)")

    # The same suggestion found on several platforms or through several
    # packages is reported once.
    seen, unique = {}, []
    for r in results:
        plat = r.pop("_plat", None)
        k = ((r.get("failing"), (r.get("new") or "").split("   ")[0]) if r.get("new")
             else (r.get("failing"), r["rule"], (r.get("note") or "").replace(plat or "\0", "{P}"))
             if r.get("rule") in PER_PACKAGE_RULES else None)
        if k and k in seen:
            first = seen[k]
            first["jobs"] = merge_jobs(first.get("jobs") or [first["variant"]], r.get("jobs") or [r["variant"]])
            if plat and plat not in first.get("_plats", []):
                # The same cause on another platform: one entry listing them all.
                first["_plats"].append(plat)
                head, _, tail = r["error"].partition("; ")
                if r["error"] not in first["error"]:
                    first["error"] += (", " + tail) if tail and first["error"].startswith(head + "; ") \
                        else "; " + r["error"]
            first.setdefault("also_seen_on", []).append(r["variant"])
            continue
        if k and plat:
            r["_plats"], r["_note"] = [plat], k[2]
        if k:
            seen[k] = r
        unique.append(r)
    for r in unique:
        plats, tmpl = r.pop("_plats", None), r.pop("_note", None)
        if plats and len(plats) > 1:
            r["note"] = tmpl.replace("{P}", ", ".join(plats))
    results = unique
    relaxed = relaxed_available_rules(data, sha, base, cache_dir, pr_packages)
    if relaxed:
        cause = ", ".join(r["failing"] for r in relaxed)
        for r in results:
            if r.get("rule", "").startswith("platform-matrix") and re.search(
                    r"Windows|freebsd|macos", r.get("error", "")):
                r["note"] = (f"CI tries these platforms only because this PR relaxed `available` of {cause} "
                             f"(see that suggestion). " + (r.get("note") or ""))
    results += relaxed
    results = rerun_rules(data, results, match)
    results += sources_rules(data, cache_dir, match)
    if not match or re.search(match, "(lint)"):
        results += lint_rules(data, src, sha, cache_dir)
    results += skipped_rules(data, src, cache_dir, pr_packages)
    return readable(merge_versions(merge_unrecognised(results)))


# Rules whose entries for several versions of one package are merged.
MERGE_VERSIONS = {"install-missing-files", "uninstallable", "newer-c-compiler", "system-library-version",
                  "needs-git-checkout", "install-outside-prefix", "test-output-ocaml", "missing-header",
                  "missing-tool", "missing-library", "missing-pkg-config", "sources"}


def merge_versions(results):
    """Several versions of one package with the same rule and no diff (a diff
    stays with its own opam file, for line comments): one entry, listing them."""
    out, first = [], {}
    for r in results:
        failing = r.get("failing") or ""
        name = split_pkg(failing)[0]
        if r.get("rule") not in MERGE_VERSIONS or r.get("new") or not name or " " in failing:
            out.append(r)
            continue
        k = (name, r["rule"], r.get("fix_in") == "this PR")
        f = first.get(k)
        if f is None:
            first[k] = r
            out.append(r)
            continue
        f["failing"] += ", " + failing
        f["jobs"] = merge_jobs(f.get("jobs") or [f["variant"]], r.get("jobs") or [r["variant"]])
        if r.get("fix_in") != "this PR":
            f["fix_in"] = f"{name} (other package)"
        if r.get("note") and r["note"] != f.get("_note0", f.get("note")):
            f.setdefault("_note0", f.get("note"))
            f.setdefault("_similar", []).append(failing)
    for r in out:
        r.pop("_note0", None)
        similar = r.pop("_similar", None)
        if similar:
            r["note"] = (r.get("note") or "").rstrip() + f" (The details differ slightly for {', '.join(similar)}.)"
    return out


def merge_unrecognised(results):
    """The failures no rule recognises, as one entry listing their jobs."""
    unk = [r for r in results if not (r.get("rule") or r.get("new") or r.get("candidates") or r.get("error")
                                      or r.get("llm_prompt"))]
    if not unk:
        return results
    jobs = merge_jobs([], [v for r in unk for v in (r.get("jobs") or [r["variant"]])])
    names = sorted({r["failing"] for r in unk if r.get("failing")})
    return [r for r in results if r not in unk] + [{
        "variant": f"{len(jobs)} job{'s' if len(jobs) != 1 else ''}", "rule": "unrecognised", "fix_in": "unknown", "confidence": "low",
        "failing": ", ".join(names[:6]) + (" ..." if len(names) > 6 else "") if names else None,
        "error": f"{len(jobs)} failed job{'s' if len(jobs) != 1 else ''} that no rule recognises",
        "note": "Read the logs of these jobs.", "jobs": jobs,
        "experimental": all(r.get("experimental") for r in unk)}]


RULE_TITLES = {
    "library-api": "API change in a dependency", "stdlib": "Standard library change",
    "safe-string": "Needs OCaml < 4.06 (safe-string)", "dune-lang": "dune too old",
    "missing-tool": "Missing tool", "missing-pkg-config": "Missing pkg-config file",
    "missing-library": "Missing library", "missing-header": "Missing C header",
    "missing-system-package": "System package not found", "install-missing-files": "Files not built",
    "test-output": "Test output differs", "test-output-ocaml": "Test output depends on the OCaml version",
    "compiler-matrix": "Fails on some OCaml versions", "platform-matrix": "Fails on some platforms",
    "platform-matrix-systemic": "Fails on some platforms, several packages",
    "compiler-matrix-systemic": "Fails on some OCaml versions, several packages",
    "builder-specific": "Fails only on the day10 builder",
    "builder-specific-systemic": "Fails only on the day10 builder, several packages",
    "pr-relaxed-available": "`available` relaxed by this PR", "lint": "Lint",
    "uninstallable": "Not installable anywhere", "newer-c-compiler": "Newer C compiler",
    "system-library-version": "System library version", "rerun": "Jobs to rerun",
    "sources": "Source archive cannot be unpacked", "accept-failures-ignored": "Accepted failure not ignored",
    "unrecognised": "Not recognised", "llm": "LLM diagnosis (unchecked)",
    "opam-same-version-reinstall": "opam reinstalls at the same version",
    "needs-git-checkout": "Build needs a git checkout", "install-outside-prefix": "Installs outside the switch",
    "unsupported-arch": "Architecture not supported", "platform-unsupported": "Platform not supported",
    "health-check-new": "Builds on master, fails here", "lower-bound": "Lower bound too low", "health-check-preexisting": "Also failing on master",
}


WHERE = {"this PR": "this PR", "unknown": "unknown", "CI (rerun)": "the CI",
         "the CI environment (day10 builder)": "the day10 builder", "the CI environment": "the CI"}


def readable(results):
    """Display fields for people: a title for the rule, the job in words,
    where the fix goes, and notes as sentences."""
    for r in results:
        if r.get("rule"):
            r["title"] = RULE_TITLES.get(r["rule"], r["rule"])
        if r.get("variant"):
            r["job"] = describe_job(r["variant"])
        f = r.get("fix_in") or ""
        r["where"] = (WHERE.get(f) or (f[:-len(" (other package)")] + ", another package"
                                       if f.endswith(" (other package)") else f)) if f else None
        for k in ("note", "error"):
            v = (r.get(k) or "").strip()
            if v and k == "note" and v[-1] not in ".)`!?":
                v += "."
            if v:
                r[k] = v
    return results


# Rules whose explanation does not depend on the platform: reported once.
PER_PACKAGE_RULES = {"newer-c-compiler", "system-library-version", "opam-same-version-reinstall", "lower-bound"}


WORKAROUND_RULES = {"platform-matrix", "missing-system-package"}


def classify(r):
    """'fix' (change in this PR), 'workaround' (restricts where it is built or
    tested), 'elsewhere' (another package, CI, systemic), or 'explanation'."""
    has_change = bool(r.get("new") or r.get("candidates"))
    if r.get("rule", "").endswith("-systemic") or (r.get("fix_in") and r.get("fix_in") != "this PR"):
        return "elsewhere"
    if not has_change:
        return "explanation"
    if r.get("rule") in WORKAROUND_RULES or "x-ci-accept-failures" in (r.get("new") or ""):
        return "workaround"
    return "fix"


def rank(results):
    order = {"fix": 0, "workaround": 1, "elsewhere": 2, "explanation": 3}
    conf = {"high": 0, "medium": 1, "low": 2}
    for r in results:
        r["class"] = classify(r)
    return sorted(results, key=lambda r: (order[r["class"]], bool(r.get("experimental")),
                                          conf.get((r.get("confidence") or "low").split(" ")[0], 3)))
