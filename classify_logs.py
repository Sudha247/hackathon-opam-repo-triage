#!/usr/bin/env python3
"""Classify and group opam-repository CI failures from logs downloaded by fetch_ci_logs.py.

Usage:
    python3 classify_logs.py                      # every ci-logs/<sha>/ directory
    python3 classify_logs.py ci-logs/<sha> ...    # specific runs
    python3 classify_logs.py --json report.json   # also write a combined report

For each failed job the log is classified with rules (no LLM):
  1. log-wide checks for failures outside a package build (CI infrastructure, lint, source
     download, system package installation, solver, timeout);
  2. the opam "#=== ERROR while <action> <pkg>" block is searched with build-error rules,
     most specific first;
  3. the matched line is normalised (versions, paths, numbers removed) into a signature, and
     jobs with the same (category, signature) form one group.

Groups whose signature came only from a generic rule, or from no rule at all, are marked
needs_llm=true; each group carries one representative excerpt that can be sent to an LLM.

Writes classification.json (per job) and groups.json (per group) next to each run's logs.
"""

import argparse
import json
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

# --------------------------------------------------------------------------------------
# Normalisation


def normalise(s):
    """Make an error line comparable across jobs: drop versions, paths, numbers, hashes."""
    s = s.strip().strip(".")
    s = re.sub(r"(?:[A-Za-z]:)?(?:[\\/][\w.@+~-]+){2,}[\\/]?", "<path>", s)  # unix + windows paths
    s = re.sub(r"\b[0-9a-f]{12,}\b", "<hash>", s)
    # package.version -> package   (conf-bison.2, plcairo.5.15.0.1, ocaml.4.14.4)
    s = re.sub(r"\b([a-z][\w-]*)\.v?\d[\w.~+-]*", r"\1", s)
    s = re.sub(r"-j \d+", "-j N", s)
    s = re.sub(r"\bline \d+(, characters \d+-\d+)?", "line N", s)
    s = re.sub(r"\s+", " ", s)
    return s


def clean(line):
    """Strip the prefixes opam/day10 put in front of build output ('# ', '- ', '#  | ')."""
    return re.sub(r"^(?:[#\-]\s?)?(?:\s*\|\s?)?", "", line.rstrip())


# --------------------------------------------------------------------------------------
# Rules. Each rule: (category, compiled regex, signature template, generic?)
# The template is formatted with the regex groups ({0} = whole match, {1}.. = groups).
# Order matters: earlier rules win.

def R(category, pattern, template, generic=False, flags=0):
    return (category, re.compile(pattern, flags), template, generic)


# Searched over the whole log, before the error block.
LOG_RULES = [
    R("ci_infrastructure", r"runc run failed|special device overlay does not exist|No space left on device"
      r"|Cannot allocate memory|Docker daemon|cannot connect to the Docker",
      "CI worker failure (container, mount or disk); retry the job"),
    R("system_package_unavailable", r"depends on the unavailable system package '([^']+)'",
      "System package(s) not available: {1}"),
    R("source_download", r"Failed to get sources of \S+: (.*)", "Failed to get sources: {1}"),
    R("source_download", r"Checksum mismatch|Bad checksum|checksum.*does not match", "Source checksum mismatch",
      flags=re.I),
    R("system_package_unavailable", r"These packages are still missing: (.*)", "System package(s) not available: {1}"),
    R("system_package_unavailable", r"No provider of '([^']+)' found", "System package(s) not available: {1}"),
    R("system_package_unavailable", r"Unable to locate package (\S+)", "System package(s) not available: {1}"),
    R("system_package_unavailable", r"No match for argument: (\S+)", "System package(s) not available: {1}"),
    R("system_package_unavailable", r"System package install failed with exit code (\d+)",
      "System package install failed (exit {1})", generic=True),
    R("solver", r"No solution for|The following dependencies couldn't be met|no solution on this variant",
      "No dependency solution"),
    R("timeout", r"Job failed:.*(?:[Tt]imeout|timed out)", "Job timed out"),
]

# Searched over the failing package's error block first, then the whole log.
BUILD_RULES = [
    # missing system tools / libraries (usually a missing depext)
    R("missing_system_dependency", r'"([^"]+)": command not found', "Missing tool: {1}"),
    R("missing_system_dependency", r"Program (\S+) not found in the tree or in PATH", "Missing tool: {1}"),
    R("missing_system_dependency", r"one of \[([^\]]+)\] is required", "Missing tool: {1}"),
    R("missing_system_dependency", r"(?:^|: )(\S+): (?:command )?not found$", "Missing tool: {1}"),
    R("missing_system_dependency", r"fatal error: (\S+?\.h(?:pp)?): No such file", "Missing C header: {1}"),
    R("missing_system_dependency", r"fatal error: '(\S+?\.h(?:pp)?)' file not found", "Missing C header: {1}"),
    R("missing_system_dependency", r"Package '?(\S+?)'?,? was not found in the pkg-config search path",
      "pkg-config package not found: {1}"),
    R("missing_system_dependency", r"No package '(\S+)' found", "pkg-config package not found: {1}"),
    R("missing_system_dependency", r"cannot find -l(\S+)", "Missing C library: -l{1}"),
    # OCaml side
    R("missing_ocaml_dependency", r'Library "([^"]+)" not found', 'OCaml library not found: "{1}"'),
    R("missing_ocaml_dependency", r"ocamlfind: Package (\S+) not found", "ocamlfind package not found: {1}"),
    R("ocaml_compile_error", r"Error: (Unbound (?:value|module|type constructor|constructor|record field|field|class|module type) \S+)",
      "{1}"),
    R("ocaml_compile_error", r"Error \((warning \d+ \[[^\]]+\])\)", "Warning as error: {1}"),
    R("ocaml_compile_error", r"Error \((alert \S+)\)", "Alert as error: {1}"),
    R("ocaml_compile_error", r"Error: (This expression has type|The value \S+ has type|Signature mismatch"
      r"|The implementation \S+ does not match|This (?:function|pattern|variant)[^.]*)", "{1}"),
    # C / C++ / linker
    R("c_compile_error", r":\d+(?::\d+)?: (?:fatal )?error: (.*)", "C/C++ error: {1}"),
    R("c_compile_error", r"undefined reference to [`'](\S+)'", "Linker: undefined reference to {1}"),
    # build environment problems
    R("sandbox_violation", r"(Read-only file system|Permission denied)", "Writes outside the opam prefix ({1})"),
    R("build_environment", r"fatal: not a git repository", "Build expects a git checkout"),
    # generic fallbacks (still grouped, but flagged for the LLM)
    R("ocaml_compile_error", r"^Error: (.*)", "OCaml error: {1}", generic=True),
    R("build_error", r"\berror: (.*)", "error: {1}", generic=True, flags=re.I),
    R("build_error", r"\*\*\* (.*)", "make: {1}", generic=True),
]

ERROR_BLOCK_RE = re.compile(r"^#=== ERROR while (\w+) (\S+) =+#")
BLOCK_END_RE = re.compile(r"^(?:#===|<><>|\[ERROR\]|\[WARNING\]|Job (?:succeeded|failed)|\d{4}-\d\d-\d\d \d\d:)")
LINT_RE = re.compile(r"^Error in (\S+?): (.*)")


def error_blocks(lines):
    """Yield (action, package, start_line, [lines]) for every opam error block."""
    i = 0
    while i < len(lines):
        m = ERROR_BLOCK_RE.match(lines[i])
        if not m:
            i += 1
            continue
        start, body = i, []
        i += 1
        while i < len(lines) and not BLOCK_END_RE.match(lines[i]) and len(body) < 2000:
            body.append(lines[i])
            i += 1
        yield m.group(1), m.group(2), start, body


def apply_rules(rules, lines):
    """First rule (in order) matching any line wins. Returns (rule, match, line_index) or None."""
    cleaned = [clean(l) for l in lines]
    for rule in rules:
        rx = rule[1]
        for idx, l in enumerate(cleaned):
            m = rx.search(l)
            if m:
                return rule, m, idx
    return None


def all_specific_signatures(lines, limit=5):
    """Every distinct specific (non-generic) build-rule signature found in these lines."""
    seen = []
    for l in map(clean, lines):
        for cat, rx, tmpl, generic in BUILD_RULES:
            if generic:
                continue
            m = rx.search(l)
            if m:
                s = normalise(tmpl.format(m.group(0), *m.groups()))
                if s not in seen:
                    seen.append(s)
                break
        if len(seen) >= limit:
            break
    return seen


def excerpt(lines, idx, before=8, after=6, width=300):
    lo, hi = max(0, idx - before), min(len(lines), idx + after + 1)
    return "\n".join(l[:width] for l in lines[lo:hi])


def pkg_name(p):
    return re.sub(r"\.v?\d[\w.~+-]*$", "", p or "")


# --------------------------------------------------------------------------------------


def classify_job(job, log_text):
    lines = log_text.splitlines()
    variant = job["variant"]
    parts = variant.split(",")
    tested = parts[-1] if len(parts) > 1 else None
    res = {
        "variant": variant,
        "platform": ",".join(parts[:-1]) if len(parts) > 1 else variant,
        "tested_package": tested,
        "failing_package": None,
        "relation": None,
        "category": None,
        "signature": None,
        "rule": None,
        "needs_llm": False,
        "ci_label": job.get("label"),
        "log_file": job.get("log_file"),
        "excerpt": "",
    }

    def done(category, signature, ex, rule="", generic=False):
        res.update(category=category, signature=normalise(signature), rule=rule,
                   needs_llm=generic, excerpt=ex)
        # Other specific errors in the same log (e.g. a missing header AND a missing OCaml library).
        scope = [l for b in blocks for l in b[3]] if blocks else lines
        res["also_seen"] = [s for s in all_specific_signatures(scope) if s != res["signature"]]
        if res["failing_package"] and tested:
            res["relation"] = "direct" if pkg_name(res["failing_package"]) == pkg_name(tested) else "dependency"
        return res

    blocks = []
    # Lint job: one group per distinct lint message.
    if variant == "(lint)" or variant.startswith("(lint"):
        msgs = [LINT_RE.match(l) for l in lines]
        msgs = [m for m in msgs if m]
        if msgs:
            sig = "; ".join(sorted({normalise(m.group(2)) for m in msgs}))
            res["failing_package"] = ", ".join(sorted({m.group(1) for m in msgs}))
            return done("lint", sig, "\n".join(m.group(0) for m in msgs[:20]), "lint")
        return done("lint", "lint failed", "\n".join(lines[-30:]), "lint", generic=True)

    blocks = list(error_blocks(lines))
    if blocks:
        res["failing_package"] = blocks[0][1]
    else:
        pkg = r"([a-z][\w+-]*\.[\w.~+-]+?)"  # name.version, so script text like ${x} never matches
        for pat in (r"\[ERROR\] (?:The compilation of |The installation of )?" + pkg + r"(?: failed|:)",
                    r"Failed to get sources of " + pkg + ":", r"dependencies failing:\s+" + pkg + r"\s*$",
                    r"Package " + pkg + r" depends on the unavailable"):
            m = re.search(pat, log_text, re.M)
            if m:
                res["failing_package"] = m.group(1)
                break

    # 1. log-wide failures that happen outside a package build
    hit = apply_rules(LOG_RULES, lines)
    if hit and hit[0][0] == "system_package_unavailable" and hit[0][3]:
        # prefer the specific "still missing: X" line if it exists anywhere
        better = apply_rules([r for r in LOG_RULES if r[0] == "system_package_unavailable" and not r[3]], lines)
        hit = better or hit
    if hit:
        (cat, _, tmpl, generic), m, idx = hit
        return done(cat, tmpl.format(m.group(0), *m.groups()), excerpt(lines, idx), cat, generic)

    # 2. build errors: the error block of the failing package first, then the whole log
    for _, pkg, start, body in blocks:
        hit = apply_rules(BUILD_RULES, body)
        if hit:
            (cat, _, tmpl, generic), m, idx = hit
            res["failing_package"] = pkg
            if generic:  # a specific match elsewhere in the log beats a generic one in the block
                spec = apply_rules([r for r in BUILD_RULES if not r[3]], lines)
                if spec:
                    (cat, _, tmpl, generic), m, idx2 = spec
                    return done(cat, tmpl.format(m.group(0), *m.groups()), excerpt(lines, idx2), cat, generic)
            return done(cat, tmpl.format(m.group(0), *m.groups()), excerpt(body, idx), cat, generic)

    hit = apply_rules(BUILD_RULES, lines)
    if hit:
        (cat, _, tmpl, generic), m, idx = hit
        return done(cat, tmpl.format(m.group(0), *m.groups()), excerpt(lines, idx), cat, generic)

    # 3. nothing matched: fall back to the CI's own one-line reason
    reason = re.sub(r"^.*?\(failed: ?|\)$", "", job.get("label", "")) or "unknown"
    tail = blocks[0][3][-40:] if blocks else lines[-40:]
    return done("unclassified", reason, "\n".join(l[:300] for l in tail), "fallback", generic=True)


def classify_run(run_dir):
    jobs_file = run_dir / "jobs.json"
    jobs = json.loads(jobs_file.read_text())
    out = []
    for job in jobs:
        if job.get("status") != "failed" or not job.get("log_file"):
            continue
        log = run_dir / job["log_file"]
        if not log.exists():
            print(f"warning: missing log {log}", file=sys.stderr)
            continue
        r = classify_job(job, log.read_text(errors="replace"))
        r["run"] = run_dir.name
        out.append(r)
    return out


def group(results):
    groups = defaultdict(list)
    for r in results:
        groups[(r["category"], r["signature"])].append(r)
    out = []
    for (cat, sig), rs in groups.items():
        rep = rs[0]
        out.append({
            "category": cat,
            "signature": sig,
            "count": len(rs),
            "needs_llm": any(r["needs_llm"] for r in rs),
            "failing_packages": sorted({pkg_name(r["failing_package"]) for r in rs if r["failing_package"]}),
            "tested_packages": sorted({r["tested_package"] for r in rs if r["tested_package"]}),
            "relations": dict(Counter(r["relation"] or "n/a" for r in rs)),
            "also_seen": dict(Counter(s for r in rs for s in r.get("also_seen", []))),
            "platforms": sorted({r["platform"] for r in rs}),
            "runs": sorted({r["run"] for r in rs}),
            "representative": {"run": rep["run"], "log_file": rep["log_file"], "variant": rep["variant"]},
            "excerpt": rep["excerpt"],
        })
    out.sort(key=lambda g: (-g["count"], g["category"], g["signature"]))
    return out


def print_report(title, results, groups, verbose):
    print(f"\n=== {title}")
    if not results:
        print("  no failed jobs")
        return
    by_cat = Counter(r["category"] for r in results)
    rule_hits = sum(1 for r in results if not r["needs_llm"])
    print(f"  {len(results)} failed jobs -> {len(groups)} groups; "
          f"{rule_hits}/{len(results)} jobs classified by a specific rule, "
          f"{sum(1 for g in groups if g['needs_llm'])} group(s) need an LLM")
    print("  by category: " + ", ".join(f"{c}={n}" for c, n in by_cat.most_common()))
    for g in groups:
        flag = "  [LLM]" if g["needs_llm"] else ""
        pk = ", ".join(g["failing_packages"][:4]) + (" ..." if len(g["failing_packages"]) > 4 else "")
        print(f"  [{g['count']:>3}] {g['category']:<27} {g['signature'][:110]}{flag}")
        if g["also_seen"]:
            print("        also seen: " + "; ".join(f"{s} ({n})" for s, n in g["also_seen"].items())[:150])
        if verbose:
            print(f"        failing: {pk or '-'} | relation: {g['relations']} | platforms: {len(g['platforms'])}"
                  f" | e.g. {g['representative']['log_file']}")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("runs", nargs="*", help="ci-logs/<sha> directories (default: all under ci-logs/)")
    ap.add_argument("--logs-root", default="ci-logs", help="where fetch_ci_logs.py stored runs (default ci-logs)")
    ap.add_argument("--json", help="also write a combined report (all runs) to this file")
    ap.add_argument("--combined", action="store_true", help="group across all runs instead of per run")
    ap.add_argument("-v", "--verbose", action="store_true", help="show packages/platforms per group")
    args = ap.parse_args()

    runs = [Path(r) for r in args.runs] or sorted(p.parent for p in Path(args.logs_root).glob("*/jobs.json"))
    if not runs:
        sys.exit(f"no runs found (looked for {args.logs_root}/*/jobs.json); run fetch_ci_logs.py first")

    all_results = []
    for run in runs:
        if not (run / "jobs.json").exists():
            print(f"skipping {run}: no jobs.json", file=sys.stderr)
            continue
        results = classify_run(run)
        groups = group(results)
        (run / "classification.json").write_text(json.dumps(results, indent=2) + "\n")
        (run / "groups.json").write_text(json.dumps(groups, indent=2) + "\n")
        all_results += results
        if not args.combined:
            print_report(run.name, results, groups, args.verbose)

    all_groups = group(all_results)
    if args.combined:
        print_report(f"all runs ({len(runs)})", all_results, all_groups, args.verbose)
    if args.json:
        Path(args.json).write_text(json.dumps({"jobs": all_results, "groups": all_groups}, indent=2) + "\n")
        print(f"\ncombined report written to {args.json}")
    print("\nper-run classification.json and groups.json written next to the logs")


if __name__ == "__main__":
    main()
