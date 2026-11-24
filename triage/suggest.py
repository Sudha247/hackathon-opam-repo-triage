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
import shutil
import subprocess
import tempfile
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
    opam_vcmp,
    split_pkg,
)
from .rules import (  # noqa: F401
    complete_test_output,
    conf_depext_platforms,
    log_info,
    lower_bounds_rule,
    previous_release_bound,
    suggest_for_job,
)
from .matrix import (  # noqa: F401
    collapse_systemic,
    matrix_rules,
)
from .context import (  # noqa: F401
    accepted_but_failing,
    analysis_rules,
    health_summary,
    lint_rules,
    preexisting_failures,
    relaxed_available_rules,
    rerun_rules,
    pr_added_bounds,
    skipped_rules,
    sources_rules,
    accepted_elsewhere,
)
from .llm import (  # noqa: F401
    apply_llm,
    llm_sig,
    llm_todo,
)
from .where import (  # noqa: F401
    fix_where,
    pr_or_other,
    where_kind,
    where_pkg,
)
from .words import (  # noqa: F401
    counted,
)


def group_key(j):
    """One job per (package, revdep, kind, reason): the same error on many
    platforms is one problem. A dependency failure is the dependency's: one
    log per (cause, platform)."""
    return (("dependency", j["reason"], job_dims(j)["distro"]) if j["category"] == "dependency"
            else failure_key(j))


def distinct_failures(jobs, match=None):
    """The failed jobs to analyse, grouped by `group_key`, in the order they
    are analysed (those beyond `--max` are not): non-experimental jobs first,
    then the PR's own packages before their reverse dependencies. In each
    group, the job whose log is read comes first: classic-builder logs first,
    as they show the depext installation (versions)."""
    cats = {"build", "build-or-test", "solver", "dependency"}
    sel = [j for j in jobs if j["status"] == "failed" and j["category"] in cats
           and (not match or re.search(match, j["variant"]))]
    groups = {}
    for j in sorted(sel, key=lambda j: (j["experimental"], bool(j["revdep"]), j["platform"][:1] == ["day10"])):
        groups.setdefault(group_key(j), []).append(j)
    return list(groups.values())


def suggest(sha, data, src, cache_dir, max_logs, match, llm=None):
    pr_packages = sorted({j["target"] for j in data["jobs"] if j["target"]})
    groups = distinct_failures(data["jobs"], match)
    if len(groups) > max_logs:
        print(f"note: {len(groups)} distinct failures, analysing the first {max_logs} (use --max)", file=sys.stderr)
    # `merged` keeps every job behind each analysed one, so the report can say how many it stands for.
    uniq = [g[0] for g in groups[:max_logs]]
    merged = {group_key(g[0]): [j["variant"] for j in g] for g in groups[:max_logs]}
    dropped = groups[max_logs:]          # to count the failures no suggestion explains
    results, base, prev_cache = [], None, {}
    for j in uniq:
        try:
            text = log_text(fetch(j["url"], cache_dir))
        except Exception as e:
            results.append({"variant": j["variant"], "note": f"Could not read the log: {e}",
                            "jobs": merged[group_key(j)]})
            continue
        base = base or excerpt(text).get("base")      # the master commit the PR was merged with
        try:
            r = suggest_for_job(src, j, text, pr_packages, sha)
            if r.get("rule") == "test-output":
                complete_test_output(src, r, j, text, data, cache_dir)
            lb = lower_bounds_rule(src, j, text, r, pr_packages, sha)
            info = log_info(text)
            r["_installed"] = info["installed"]
            pb = previous_release_bound(src, j, text, info, pr_packages, sha, prev_cache)
        except Exception as e:      # a bug in a rule: this job only, the others are still analysed
            results.append(rule_error(j, e, merged[group_key(j)]))
            continue
        if pb and not any(x.get("failing") == pb["failing"] and x.get("new") == pb["new"] for x in results + [r]):
            pb.update(_key=failure_key(j), jobs=merged[group_key(j)], keep=True)
            results.append(pb)
        r["_key"] = failure_key(j)
        r["jobs"] = merged[group_key(j)]
        if "_depexts" in r:
            conf_depext_platforms(r, {x["variant"]: x for x in data["jobs"]})
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
    # Distinct failures beyond `max_logs` that no matrix suggestion covers:
    # nothing says why they fail (the JSON output and the page say how many).
    data["unanalysed"] = sum(any(failure_key(j) not in explained for j in js) for js in dropped)
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
    if new_notes.get("health_stale"):
        data["health_stale"] = new_notes["health_stale"]     # reported in the JSON output and on the page
    elsewhere = drop_health_explained_elsewhere(new_notes, results, pr_packages)
    for r in results:
        keys = r.get("_keys") or ([r["_key"]] if r.get("_key") else [])
        hints = {new_notes["new"][k] for k in keys if k in new_notes.get("new", {})}
        if hints and not any(k in pre for k in keys) and id(r) not in elsewhere:
            r["new_on_master"] = sorted(hints)[0]
            r["note"] = sorted(hints)[0] + (" " + r["note"] if r.get("note") else "")
        why = {pre.get(k) for k in keys}
        if keys and None not in why:
            r["preexisting"] = (sorted(why)[0] if len(why) == 1 or not all(w.startswith("this PR only adds")
                                                                           for w in why)
                                else "this PR only adds upper bounds to these packages, so their dependencies "
                                     "are the same as on master")
            r["note"] = f"Pre-existing: {r['preexisting']}." + (" " + r["note"] if r.get("note") else "")
            if any(k in new_notes.get("unverified", ()) for k in keys):
                r["preexisting_unverified"] = True      # not counted as happening without the PR
    results += health_summary(new_notes, src)
    for r in results:
        r.pop("_key", None)
        r.pop("_keys", None)
        r.pop("_distros", None)
        r.pop("keep", None)
        r.pop("_depexts", None)
        r.pop("_check", None)
    if llm and data.get("unfinished") and not llm["dry_run"]:
        # The prompt shows the job's results on the other platforms, which
        # change as jobs finish: each analysis would pay again (the cache key
        # is the prompt). Asked once CI has finished.
        data["llm_unanswered"] = len({llm_sig(r) for r in llm_todo(results, data)})
        if data["llm_unanswered"]:
            print("note: CI is still running: the LLM is not asked until it finishes", file=sys.stderr)
    elif llm:
        data["_sha"] = sha
        data["llm_unanswered"] = apply_llm(results, data, src, cache_dir, pr_packages, llm["limit"], llm["model"],
                                           llm["dry_run"])
        for r in results:
            if r.get("rule") == "llm":
                # The answer's place comes as `fix_in` (see where.py): the rule's where_kind no longer holds.
                r.pop("where_kind", None)
                r.pop("where_pkg", None)

    # Old releases failing on newer platforms: maintainers mark them
    # `x-maintained: false` so that the next archiving round removes them.
    for r in results:
        if r.get("rule") not in ("platform-matrix", "compiler-matrix", "missing-system-package", "missing-header",
                                 "newer-c-compiler"):
            continue
        name, ver = split_pkg(r.get("failing") or "")
        vs = src.versions(name) if name and ver else []
        if vs and ver in vs and vs[-1] != ver and not r.get("alternative"):
            r["alternative"] = (f"x-maintained: false  (old release; {name}.{vs[-1]} exists, so the next "
                                f"archiving round can remove this one)")

    # The same suggestion found on several platforms or through several
    # packages is reported once.
    seen, unique = {}, []
    for r in results:
        plat = r.pop("_plat", None)
        r.pop("_unexplained", None)         # set by the rules, read by apply_llm
        k = ((r.get("failing"), (r.get("new") or "").split("   ")[0]) if r.get("new")
             else (r.get("failing"), r.get("advice")) if r.get("advice")
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
    results += analysis_rules(data, cache_dir, match)
    results += skipped_rules(data, src, cache_dir, pr_packages, pr_added_bounds(base, sha, cache_dir))
    check_changes(results, src, sha, set(pr_packages))
    accepted_elsewhere(results, src, set(pr_packages))
    for r in results:
        r.pop("_installed", None)
    return readable(merge_versions(merge_unrecognised(results)))


def rule_error(job, e, jobs):
    """The result for a job whose rules raised `e`."""
    return {"variant": job["variant"], "url": job["url"], "kind": job["kind"], "experimental": job["experimental"],
            "failing": job.get("revdep") or job.get("target"), "rule": "rule-error", **fix_where("unknown", "unknown"),
            "confidence": "low", "error": f"{type(e).__name__}: {str(e)[:300]}",
            "note": "The tool failed while analysing this log, which is a bug in the tool. Read the log.",
            "_key": failure_key(job), "jobs": jobs, "keep": True}


# A constraint on the package's own version (`>= "1.0"`), not a comparison
# of a variable (`os = "linux"`, `ocaml:version < "5.0"`): the operator
# comes first in the formula or after `{`, `&`, `|` or `(`.
VERSION_TERM = re.compile(r'(?:^|(?<=[{&|(]))\s*(<=|>=|!=|<|>|=)\s*"([^"]+)"')


def unsatisfiable(terms):
    """Do these version constraints (a conjunction) exclude every version?"""
    for lo_op, lo in terms:
        if lo_op not in (">=", ">", "="):
            continue
        for up_op, up in terms:
            if up_op not in ("<=", "<", "=") or (lo_op, lo) == (up_op, up):
                continue
            c = opam_vcmp(lo, up)
            if c > 0 or (c == 0 and (lo_op == ">" or up_op == "<")):
                return True
    return False


def allows(version, terms):
    ok = {"<": lambda c: c < 0, "<=": lambda c: c <= 0, ">": lambda c: c > 0, ">=": lambda c: c >= 0,
          "=": lambda c: c == 0, "!=": lambda c: c != 0}
    return all(ok[op](opam_vcmp(version, v)) for op, v in terms)


def lint_codes(text):
    """The warning and error codes `opam lint` reports for an opam file, or
    None without opam."""
    if not shutil.which("opam"):
        return None
    with tempfile.TemporaryDirectory() as d:
        path = os.path.join(d, "opam")
        with open(path, "w") as f:
            f.write(text)
        p = subprocess.run(["opam", "lint", "--strict", path], capture_output=True, text=True)
    return {f"{k} {n}" for k, n in re.findall(r"\b(warning|error)\s+(\d+):", p.stdout + p.stderr)}


def applied(text, r):
    """The opam file with the suggested change applied, when it can be placed."""
    new = (r.get("new") or "").split("   ")[0].strip()
    if r.get("old") and r["old"] in text:
        return text.replace(r["old"], new, 1)
    if re.match(r'"[\w+-]+"', new) and "new line in depends" in (r.get("new") or ""):
        return re.sub(r"(?m)^(depends:\s*\[)\s*$", lambda m: m.group(1) + "\n  " + new, text, count=1)
    return None


def check_changes(results, src, sha, pr_packages):
    """Check each suggested change before it is shown: a constraint that no
    version satisfies, a bound that still allows the version the failing job
    installed, or a change that `opam lint` objects to (when opam is
    installed) is moved to the note, as text to check."""
    for r in results:
        new = r.get("new") or ""
        m = re.match(r'\s*"([\w+-]+)"\s*\{([^}]*)\}', new)
        problem = None
        if m and "|" not in m.group(2):
            dep, terms = m.group(1), VERSION_TERM.findall(m.group(2))
            have = (r.get("_installed") or {}).get(dep)
            if terms and unsatisfiable(terms):
                problem = f"no version of {dep} satisfies it"
            elif (have and terms and "with-test" not in m.group(2) and allows(have, terms)
                  and r.get("rule") in ("library-api", "previous-release-bound", "lower-bound")):
                problem = f"it still allows {dep}.{have}, which the failing job installed"
        failing = r.get("failing") or ""
        name, ver = split_pkg(failing)
        if not problem and new and ver and " " not in failing and shutil.which("opam"):
            text = (src.opam_file(name, ver, sha) if failing in pr_packages else None) or src.opam_file(name, ver)
            after = applied(text, r) if text else None
            if after:
                before_codes, after_codes = lint_codes(text), lint_codes(after)
                if before_codes is not None and after_codes - before_codes:
                    problem = f"opam lint reports {', '.join(sorted(after_codes - before_codes))} on the result"
        if problem:
            change = new.split("   ")[0].strip()
            r["note"] = ((r.get("note") or "").rstrip() + f" Possible change, not shown as a diff because "
                         f"{problem}: `{change}`.").strip()
            for k in ("new", "old", "alternative"):
                r.pop(k, None)
            r["confidence"] = "low"
            r["checked"] = problem


# Rules whose entries for several versions of one package are merged.
MERGE_VERSIONS = {"install-missing-files", "uninstallable", "newer-c-compiler", "system-library-version",
                  "needs-git-checkout", "install-outside-prefix", "test-output-ocaml", "missing-header",
                  "missing-tool", "missing-library", "missing-pkg-config", "sources", "conf-no-depext"}


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
        k = (name, r["rule"], where_kind(r))
        f = first.get(k)
        if f is None:
            first[k] = r
            out.append(r)
            continue
        if failing not in f["failing"].split(", "):
            f["failing"] += ", " + failing
        f["jobs"] = merge_jobs(f.get("jobs") or [f["variant"]], r.get("jobs") or [r["variant"]])
        if where_kind(r) == "other-package":
            f.update(pr_or_other(name, False))
        if r.get("note") and r["note"] != f.get("_note0", f.get("note")):
            f.setdefault("_note0", f.get("note"))
            f.setdefault("_first", f["failing"].split(", ")[0])
            f.setdefault("_similar", []).append(failing)
    for r in out:
        r.pop("_note0", None)
        first = r.pop("_first", None)
        similar = [x for x in dict.fromkeys(r.pop("_similar", None) or []) if x != first]
        if similar:
            r["note"] = (r.get("note") or "").rstrip() + f" This note is for {first}; it differs for {', '.join(similar)}."
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
        "variant": counted(len(jobs), "job"), "rule": "unrecognised", **fix_where("unknown", "unknown"),
        "confidence": "low",
        "failing": ", ".join(names[:6]) + (" ..." if len(names) > 6 else "") if names else None,
        "error": f"{counted(len(jobs), 'failed job')} the tool cannot explain",
        "note": "Read its log." if len(jobs) == 1 else "Read their logs.", "jobs": jobs,
        "experimental": all(r.get("experimental") for r in unk)}]


RULE_TITLES = {
    "library-api": "API change in a dependency", "stdlib": "Standard library change",
    "safe-string": "Needs OCaml < 4.06 (safe-string)", "dune-lang": "dune too old",
    "missing-tool": "Missing tool", "missing-pkg-config": "Missing pkg-config file",
    "missing-library": "Missing library", "missing-header": "Missing C header",
    "missing-system-package": "System package not found", "install-missing-files": "Files not built",
    "test-output": "Test output differs", "test-output-ocaml": "Test output depends on the OCaml version",
    "compiler-matrix": "Fails on some OCaml versions", "platform-matrix": "Fails on some platforms",
    # The -systemic titles get the number of packages (see `title_of`).
    "platform-matrix-systemic": "Fails on some platforms",
    "compiler-matrix-systemic": "Fails on some OCaml versions",
    "builder-specific": "Fails only on the day10 builder",
    "builder-specific-systemic": "Fails only on the day10 builder",
    "pr-relaxed-available": "`available` relaxed by this PR", "lint": "opam lint error",
    "lint-ci-error": "Lint job failed (CI error)", "analysis": "CI could not analyse the PR",
    "uninstallable": "Not installable anywhere",
    "bounded-out": "This PR's bound excludes every tested platform", "newer-c-compiler": "Newer C compiler",
    "system-library-version": "System library version", "rerun": "Jobs to rerun",
    "sources": "Source archive cannot be unpacked",
    "accept-failures-ignored": "x-ci-accept-failures ignored by day10",
    "unrecognised": "Unexplained failures", "llm": "LLM diagnosis (unchecked)",
    "rule-error": "Analysis failed (tool error)",
    "opam-same-version-reinstall": "opam reinstalls at the same version",
    "needs-git-checkout": "Build needs a git checkout", "install-outside-prefix": "Installs outside the switch",
    "unsupported-arch": "Architecture not supported", "platform-unsupported": "Platform not supported",
    "health-check-new": "Broken by this PR (builds on master)", "lower-bound": "Lower bound too low",
    "link-error": "Link error", "broken-install": "Incomplete installation in the switch",
    "ocaml-runtime-api": "OCaml runtime function missing", "test-platform": "Fails on one compiler variant",
    "health-check-preexisting": "Also fails on master",
    "conf-no-depext": "No system package for this platform",
}


def title_of(r):
    """The title of a suggestion: its rule in words, with the number of
    packages for a pattern merged over several packages."""
    rule = r.get("rule")
    if not rule:
        return r.get("title") or "Note"
    t = RULE_TITLES.get(rule, rule)
    if rule == "lint" and r.get("lint_level") == "warning":
        t = "opam lint warning"
    m = re.match(r"(\d+) packages \[", r.get("variant") or "") if rule.endswith("-systemic") else None
    return f"{t} ({m.group(1)} packages)" if m else t


# Where the fix goes, in words, by `where_kind` (see where.py): "this PR",
# "{pkg} (another package)", "upstream", "the CI", "rerun" or "nothing to
# fix"; None when unknown.
WHERE = {"pr": "this PR", "upstream": "upstream", "ci": "the CI", "rerun": "rerun", "none": "nothing to fix"}
# Another package the analysis cannot name, by rule.
WHERE_UNNAMED = {"missing-system-package": "the dependency that declares these depexts (another package)",
                 "health-check-preexisting": "these reverse dependencies (other packages)"}


def where_text(r):
    """Where the fix of a suggestion goes, in words (see WHERE), or None."""
    k = where_kind(r)
    if k == "other-package":
        pkg = where_pkg(r)
        return f"{pkg} (another package)" if pkg else WHERE_UNNAMED.get(r.get("rule"), "another package")
    if k == "unknown" and (r.get("rule") or "").endswith("-systemic"):
        return "unknown: one cause shared by these packages"
    return WHERE.get(k)


def fix_label(where):
    """'fix in X', 'fix upstream', or the value alone when it is not a place
    ("rerun", "nothing to fix")."""
    if where in ("rerun", "nothing to fix"):
        return where
    return "fix upstream" if where == "upstream" else f"fix in {where}"


def readable(results):
    """Display fields for people: a title for the rule, the job in words,
    where the fix goes, and notes as sentences."""
    for r in results:
        if r.get("rule"):
            r["title"] = title_of(r)
        if r.get("variant"):
            r["job"] = describe_job(r["variant"])
        r["where_kind"], pkg = where_kind(r), where_pkg(r)
        if pkg:
            r["where_pkg"] = pkg
        else:
            r.pop("where_pkg", None)
        r["where"] = where_text(r)
        for k in ("note", "error"):
            v = (r.get(k) or "").strip()
            if v and k == "error" and v.endswith(":"):
                v = v.rstrip(":").rstrip()          # a reason with nothing after it ("x failed:")
            if v and k == "note" and v[-1] not in ".)`!?":
                v += "."
            if v:
                r[k] = v
    return results


# Rules whose explanation does not depend on the platform: reported once.
PER_PACKAGE_RULES = {"newer-c-compiler", "system-library-version", "opam-same-version-reinstall", "lower-bound"}


def drop_health_explained_elsewhere(notes, results, pr_packages):
    """Drop from `notes` (see preexisting_failures) the opam-health-check
    comparisons of the jobs whose log points at a dependency that is not a
    package of this PR: a library-api result bounding it. That dependency
    changed on master since opam-health-check's run, or is not in it: matrix
    0.2.0 broke console-eio while opam-health-check still saw it build, and
    the k7 PR (#31170) was blamed. Returns the ids of those results, whose
    note must not say that the PR probably causes the failure."""
    names = {split_pkg(p)[0] for p in pr_packages}
    hits = [r for r in results if r.get("rule") == "library-api" and r.get("new") and r.get("dep")
            and r["dep"] not in names]
    variants = {v for r in hits for v in (r.get("jobs") or [r.get("variant")]) if v}
    good = (notes.get("health") or {}).get("good") or {}
    for who in list(good):
        good[who] = {(o, v) for o, v in good[who] if v not in variants}
        if not good[who]:
            del good[who]
            (notes.get("via") or {}).pop(who, None)
    return {id(r) for r in hits}


WORKAROUND_RULES = {"platform-matrix", "missing-system-package"}


def classify(r):
    """'fix' (change in this PR), 'workaround' (skips platforms or tests),
    'elsewhere' (another package, upstream, the CI), or 'explanation' (no
    change, or the place of the fix is unknown). A change described in words
    (`advice`) counts as a change."""
    has_change = bool(r.get("new") or r.get("candidates") or r.get("advice"))
    k = where_kind(r)
    if r.get("rule") == "rule-error" or k == "unknown":
        return "explanation"
    if k != "pr":
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
