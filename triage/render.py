"""Output: text, compare, PR comment, line-level review."""

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
    gh_api,
)
from .ci import (  # noqa: F401
    FAILING,
    describe_job,
    ocaml_version,
    opam_vcmp,
    platform_str,
    split_pkg,
)
from .suggest import (  # noqa: F401
    rank,
)


def render_summary(sha, data, show_all):
    jobs = data["jobs"]
    print(f"PR #{data['pr']}  commit {sha[:12]}  {BASE}{sha}")
    if data.get("unfinished"):
        print(f"CI still running: {data['unfinished']} of {len(jobs)} jobs not finished")
    counts = {}
    for j in jobs:
        counts[j["category"]] = counts.get(j["category"], 0) + 1
    print("jobs:", len(jobs), " ".join(f"{k}={v}" for k, v in sorted(counts.items())))
    noise = {"passed", "ignored", "not-available"}

    for j in jobs:
        if j["kind"] in ("lint", "analysis"):
            print(f"\n[{j['kind']}] {j['status']}" + (f": {j['reason']}" if j["reason"] else ""))
            for m in j["messages"]:
                print(f"    {m}")

    targets = sorted({j["target"] for j in jobs if j["target"]})
    for t in targets:
        tj = [j for j in jobs if j["target"] == t]
        own = [j for j in tj if j["kind"] != "revdeps" and (show_all or j["category"] not in noise)]
        rev = [j for j in tj if j["kind"] == "revdeps" and (show_all or j["category"] not in noise)]
        print(f"\n== {t} ==")
        if own:
            print("  own jobs:")
            for j in own:
                print(f"    {j['kind']:<13} {j['category']:<14} {platform_str(j):<45} {j['reason'] or ''}")
        else:
            print("  own jobs: all passed (or ignored / not available)")
        if rev:
            by_pkg = {}
            for j in rev:
                by_pkg.setdefault(j["revdep"], []).append(j)
            print(f"  revdeps with problems: {len(by_pkg)} packages")
            for pkg, js in sorted(by_pkg.items()):
                # Group identical reasons across platforms.
                by_reason = {}
                for j in js:
                    by_reason.setdefault((j["category"], j["reason"]), []).append(platform_str(j))
                for (cat, reason), plats in by_reason.items():
                    print(f"    {pkg:<32} {cat:<14} {reason or ''}")
                    print(f"    {'':<32} {'':<14} on: {', '.join(plats)}")


def compare(jobs, base_jobs):
    """Return one verdict per (revdep, ocaml version) that fails in `jobs`."""
    def index(js):
        idx = {}
        for j in js:
            v = ocaml_version(j)
            if j["kind"] == "revdeps" and v:
                idx.setdefault((j["revdep"], v), []).append(j)
        return idx

    new_idx, base_idx = index(jobs), index(base_jobs)
    out = []
    for (revdep, v), js in sorted(new_idx.items()):
        failing = [j for j in js if j["category"] in FAILING]
        if not failing:
            continue
        passing_here = [j for j in js if j["category"] == "passed"]
        base = base_idx.get((revdep, v), [])
        base_fail = [j for j in base if j["category"] in FAILING]
        base_pass = [j for j in base if j["category"] == "passed"]
        if base_fail:
            same = {j["reason"] for j in failing} & {j["reason"] for j in base_fail}
            verdict = "pre-existing" + (" (same error)" if same else " (different error)")
        elif passing_here:
            verdict = "builder-specific"   # fails on one builder, passes on another in the same run
        elif base_pass:
            verdict = "NEW"
        elif base:
            verdict = "unknown (baseline: " + ",".join(sorted({j["category"] for j in base})) + ")"
        else:
            verdict = "no-baseline"
        out.append({
            "revdep": revdep, "ocaml": v, "verdict": verdict,
            "new": [{"platform": "/".join(j["platform"]), "category": j["category"],
                     "reason": j["reason"], "url": j["url"]} for j in failing],
            "passes_on": ["/".join(j["platform"]) for j in passing_here],
            "baseline": [{"platform": "/".join(j["platform"]), "category": j["category"],
                          "reason": j["reason"], "url": j["url"]} for j in base],
        })
    return out


def render_compare(target, info, results, show_all=False):
    print(f"== {target} vs baseline {info['prev']}: PR #{info.get('pr')} commit {info['sha'][:12]}")
    order = ["NEW", "builder-specific", "no-baseline", "unknown", "pre-existing"]
    def rank(r):
        return next(i for i, k in enumerate(order) if r["verdict"].startswith(k))
    counts = {}
    for r in results:
        k = r["verdict"].split(" (")[0]
        counts[k] = counts.get(k, 0) + 1
    print("   " + "  ".join(f"{k}={v}" for k, v in sorted(counts.items(), key=lambda kv: order.index(kv[0]) if kv[0] in order else 99)))
    compact_header = False
    for r in sorted(results, key=lambda r: (rank(r), r["revdep"], r["ocaml"])):
        if r["verdict"].startswith("pre-existing") and not show_all:
            if not compact_header:
                print("\n  pre-existing (also failed with the previous version; --all for details):")
                compact_header = True
            print(f"    {r['revdep'] + ' / OCaml ' + r['ocaml']:<36} {r['new'][0]['reason'] or ''}"[:200])
            continue
        print(f"\n  [{r['verdict']}] {r['revdep']} on OCaml {r['ocaml']}")
        for n in r["new"]:
            print(f"      new  {n['platform']:<28} {n['category']:<13} {n['reason'] or ''}")
        if r["passes_on"]:
            print(f"      but passes on: {', '.join(r['passes_on'])}")
        for b in r["baseline"]:
            print(f"      base {b['platform']:<28} {b['category']:<13} {b['reason'] or ''}")


def jobs_note(r):
    """' (N failed jobs)' when a suggestion stands for several jobs, else ''."""
    n = len(r.get("jobs") or [])
    return f" ({n} failed jobs)" if n > 1 else ""


def opam_path(pkg):
    name, ver = split_pkg(pkg or "")
    return f"packages/{name}/{name}.{ver}/opam" if name and ver else None


def diff_block(r):
    lines = []
    if r.get("old"):
        lines.append(f"- {r['old']}")
    new = (r.get("new") or "").split("   ")
    lines.append(f"+ {new[0]}")
    hint = " ".join(x.strip() for x in new[1:] if x.strip())
    return "```diff\n" + "\n".join(lines) + "\n```" + (f"\n<sub>{hint}</sub>" if hint else "")


def evidence_line(r):
    ev = r.get("evidence")
    if not ev:
        return r.get("note") or ""
    def vkey_of(k):
        return functools.cmp_to_key(opam_vcmp)(k.split(" ")[1] if " " in k else k)
    parts = [f"{k.replace(' (installed in failing job)', ' (used by CI)')}: {'yes' if v else 'no'}"
             for k, v in sorted(ev.items(), key=lambda kv: vkey_of(kv[0])) if v is not None]
    return f"`{r.get('error', '')}`: present in " + ", ".join(parts)


def render_comment(sha, data, results, placed=0, placed_lines=0):
    """A draft PR comment (Markdown) from ranked suggestions. With `placed`,
    the fixes already attached to lines by `review` are left out."""
    by = collections.defaultdict(list)
    for r in results:
        if not r.get("_placed"):
            by[r["class"]].append(r)
    out = ["### Automated triage of the opam-repo-ci failures", "",
           f"From the [CI results]({BASE}{sha}) for commit `{sha[:12]}`. Nothing was rebuilt: these "
           "suggestions come from the CI logs, the documentation of each package version on ocaml.org, and "
           "the opam files. Please check them before applying them.", ""]
    if data.get("unfinished"):
        out += [f"**CI is still running** ({data['unfinished']} of {len(data['jobs'])} jobs not finished): "
                "these are the failures so far.", ""]
    fixes = by["fix"]
    if placed:
        out += [f"{placed} suggested change{'s are' if placed > 1 else ' is'} attached to "
                f"{placed_lines} line{'s' if placed_lines > 1 else ''} of the diff "
                "(\"Commit suggestion\" applies each one).", ""]
    if fixes:
        out += [f"#### {'Other suggested' if placed else 'Suggested'} changes ({len(fixes)})", ""]
        for r in fixes:
            path = opam_path(r.get("failing"))
            out.append(f"**`{path or r.get('failing')}`**: {r.get('title') or r.get('rule')} "
                       f"({r.get('confidence')} confidence){jobs_note(r)}")
            if r.get("error"):
                out.append(f"> {r['error'][:300]}")
            if r.get("candidates"):
                out.append(f"Candidates ({r.get('compared_with', '')}):")
                out.append("```diff\n" + "\n".join("+ " + c["new"].split("   ")[0] for c in r["candidates"]) + "\n```")
            else:
                out.append(diff_block(r))
                if r.get("alternative"):
                    alt = re.split(r"\s{2,}(?=\()", r["alternative"], maxsplit=1)
                    out.append(f"Or `{alt[0].strip()}`" + (f" {alt[1].strip()}" if len(alt) > 1 else ""))
            ev = evidence_line(r)
            if ev:
                out.append(f"<sub>{ev[:400]}</sub>")
            out.append("")
    elif not placed:
        out += ["No change to this PR's opam files could be derived automatically.", ""]

    def details(title, rs, fmt):
        if not rs:
            return []
        body = ["<details>", f"<summary>{title} ({len(rs)})</summary>", ""]
        for r in rs:
            body += fmt(r) + [""]
        return body + ["</details>", ""]

    def head(r, where=False):
        exp = " (experimental platform)" if r.get("experimental") else ""
        what = [r.get("title") or r.get("rule")] + ([f"fix in {r.get('where') or r.get('fix_in')}"] if where else [])
        if r.get("failing"):
            return f"- **{r['failing']}**{exp} ({'; '.join(what)}){jobs_note(r)}"
        return f"- **{what[0]}**{exp} ({'; '.join([r.get('job') or r['variant']] + what[1:])})"

    def job_links(r, n=10):
        # For the entries whose content is the list of jobs (reruns, failures no rule recognises).
        if r.get("rule") not in ("rerun", "unrecognised"):
            return ""
        js = r.get("jobs") or []
        links = [f"[{describe_job(v)}]({BASE}{sha}/variant/{v})" for v in js[:n]]
        return (f"<br>Jobs: {', '.join(links)}" + (f" and {len(js) - n} more" if len(js) > n else "")) if js else ""

    out += details("Workarounds: restrict where this PR is built or tested", by["workaround"], lambda r: [
        f"{head(r)}: {(r.get('error') or '')[:220]}",
        "  " + diff_block(r).replace("\n", "\n  ")])
    out += details("Problems in other packages or in the CI (nothing to change in this PR)", by["elsewhere"],
                   lambda r: [
        f"{head(r, where=True)}: {(r.get('error') or '')[:200]}"
        + (f"<br>Suggested change: `{(r.get('new') or '').split('   ')[0]}`" if r.get("new") else "")
        + (f"<br>{r['note'][:300]}" if r.get("note") else "") + job_links(r)])
    out += details("Other findings (no change suggested)", by["explanation"], lambda r: [
        f"{head(r)}: {(r.get('error') or '')[:150]}"
        + (f"<br>{r['note'][:300]}" if r.get("note") else "")
        + (f" ([log]({r['url']}))" if r.get("url") else "") + job_links(r)])
    out.append("<sub>Generated by opam_repo_ci_triage.py</sub>")
    return "\n".join(out)


def commentable_lines(patch):
    """Line numbers (new side) a review comment can be attached to."""
    lines, new = set(), 0
    for l in (patch or "").splitlines():
        m = re.match(r"@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@", l)
        if m:
            new = int(m.group(1))
            continue
        if l.startswith("-"):
            continue
        if l.startswith(("+", " ")):
            lines.add(new)
            new += 1
    return lines


def line_edits(r, text):
    """[(line number, replacement)] for a fix on the file `text`, or []."""
    lines = (text or "").splitlines()
    new = (r.get("new") or "").split("   ")[0].strip()
    indent = lambda l: re.match(r"\s*", l).group(0)
    if r.get("rule") == "lint" and new.startswith("%{_:"):          # warning 41: every %{pkg:...}%
        name = split_pkg(r.get("failing"))[0]
        return [(i + 1, l.replace(f"%{{{name}:", "%{_:")) for i, l in enumerate(lines) if f"%{{{name}:" in l]
    if not new or "<" in new.split('"')[0] or "<hash>" in new or "<URL" in new or new.startswith(("in ", "x-ci-", "available:")):
        return []
    if r.get("old"):
        old = r["old"].strip()
        for i, l in enumerate(lines):
            if l.strip() == old:
                return [(i + 1, indent(l) + new)]
        return []
    if new.startswith('"'):                                        # new dependency: after the last one
        start = next((i for i, l in enumerate(lines) if l.startswith("depends:")), None)
        if start is None:
            return []
        end = next((i for i in range(start + 1, len(lines)) if lines[i].strip() == "]"), None)
        last = end - 1 if end else None
        if last is None or last <= start:
            return []
        return [(last + 1, lines[last] + "\n" + indent(lines[last]) + new)]
    m = re.match(r"([a-z][\w-]*):", new)
    if m:                                                          # new field: after a related one
        for f in ("homepage", "bug-reports", "maintainer", "synopsis"):
            for i, l in enumerate(lines):
                if l.startswith(f + ":") and l.count("[") == l.count("]"):
                    return [(i + 1, l + "\n" + new)]
    return []


def build_review(sha, data, results, src):
    """A GitHub pull-request review: line comments with suggestion blocks for
    the fixes that can be placed on the PR's diff, the rest in the body."""
    pr = data.get("pr")
    head = gh_api(f"repos/{REPO}/pulls/{pr}", cache=False) if pr else None
    if not head:
        sys.exit("cannot build a review: unknown PR (pass --pr N)")
    if head["head"]["sha"] != sha:
        print(f"note: the CI results are for {sha[:12]} but the PR head is {head['head']['sha'][:12]}: "
              "line comments are skipped (they would point at the wrong lines)", file=sys.stderr)
    files = {f["filename"]: f for f in (gh_api(f"repos/{REPO}/pulls/{pr}/files?per_page=100", cache=False) or [])}
    comments = []
    for r in results:
        if r.get("class") != "fix" or head["head"]["sha"] != sha:
            continue
        path = opam_path(r.get("failing"))
        if path not in files:
            continue
        ok = commentable_lines(files[path].get("patch"))
        text = src.raw(f"https://raw.githubusercontent.com/{REPO}/{sha}/{path}")
        edits = [(n, rep) for n, rep in line_edits(r, text) if n in ok]
        if not edits:
            continue
        r["_placed"] = True
        why = f"**{r.get('rule')}** ({r.get('confidence')} confidence)"
        if r.get("error"):
            why += f": {r['error'][:200]}"
        ev = evidence_line(r)
        for n, replacement in edits:
            comments.append({"path": path, "line": n, "side": "RIGHT",
                             "body": f"{why}\n```suggestion\n{replacement}\n```" + (f"\n<sub>{ev[:300]}</sub>" if ev else "")})
    body = render_comment(sha, data, results, placed=sum(1 for r in results if r.get("_placed")),
                          placed_lines=len(comments))
    return {"commit_id": sha, "event": "COMMENT", "body": body, "comments": comments}


def render_suggestions(results):
    for r in results:
        print("=" * 100)
        print(f"{r.get('title') or r.get('rule') or 'Note'}: {r.get('job') or r['variant']}")
        if r.get("rule") in ("rerun", "unrecognised"):
            for v in r.get("jobs") or []:
                print(f"  job             : {describe_job(v)}")
        elif len(r.get("jobs") or []) > 1:
            others = [v for v in r["jobs"] if v != r["variant"]]
            print(f"  same failure on : {len(r['jobs'])} failed jobs; also "
                  + ", ".join(describe_job(v) for v in others[:5])
                  + (f", ... ({len(others) - 5} more)" if len(others) > 5 else ""))
        if r.get("failing"):
            print(f"  failing package : {r['failing']}   fix in: {r.get('where') or r.get('fix_in')}"
                  + ("   (experimental platform: does not block)" if r.get("experimental") else ""))
        if r.get("error"):
            print(f"  error           : {r['error']}")
        if r.get("source"):
            print(f"  looked up in    : {r['source']}")
        if r.get("evidence"):
            print("  evidence        : " + ", ".join(f"{k}={'yes' if v else ('no' if v is False else '?')}" for k, v in r["evidence"].items()))
        for t in r.get("tried", []):
            print(f"  tried           : {t}")
        if r.get("new"):
            label = {"workaround": "workaround", "elsewhere": "change elsewhere"}.get(r.get("class"), "suggested change")
            print(f"  {label} ({r.get('confidence')} confidence):")
            if r.get("old"):
                print(f"    - {r['old']}")
            print(f"    + {r['new']}")
            if r.get("alternative"):
                print(f"    or {r['alternative']}")
        if r.get("candidates"):
            print(f"  candidates      : {r.get('confidence')}")
        for c in r.get("candidates", []):
            if "first_after" in c:
                print(f"    {c['dep']}: {c['failing']} installed; first release after {r.get('compared_with')} is {c['first_after']}")
            else:
                print(f"    {c['dep']}: {c['passing']} passes on {r.get('compared_with')}, {c['failing']} fails")
            if c.get("old"):
                print(f"    - {c['old']}")
            print(f"    + {c['new']}")
        if r.get("also_newer"):
            print(f"  also released after it (not ranked): {', '.join(r['also_newer'])}")
        if r.get("note"):
            print(f"  note            : {r['note']}")
        if r.get("llm_prompt"):
            print("  LLM prompt (dry run):\n    " + r["llm_prompt"].replace("\n", "\n    "))
