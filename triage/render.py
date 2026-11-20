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
    fix_label,
    rank,
    title_of,
    where_text,
)
from .words import (  # noqa: F401
    counted,
    short_date,
)
from .md import (  # noqa: F401
    SHA,
    ci_job_url,
    flat,
    md_code,
    md_fence,
    md_text,
    safe_ci_url,
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
            print(f"  reverse dependencies with problems: {len(by_pkg)}")
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


def conf_word(r):
    """'low' for a confidence of 'low (dependencies released after …)'."""
    return str(r.get("confidence") or "").split(" ")[0]


def jobs_note(r):
    """'N failed jobs' when a suggestion stands for several jobs, else ''."""
    n = len(r.get("jobs") or [])
    return f"{n} failed jobs" if n > 1 else ""


def cut(s, limit):
    """`s` on one line, cut to `limit` characters at the end of a sentence
    when there is one in the second half, else with "…" (by md_text)."""
    s = flat(s)
    if len(s) <= limit:
        return s
    ends = [m.end() for m in re.finditer(r"[.!?](?=\s)", s[:limit + 1])]
    return s[:ends[-1]] if ends and ends[-1] > limit // 2 else s


RELEASE_DATE = re.compile(r"release date of (\S+) \((\d{4}-\d\d-\d\d)\)$")


def compared_text(r, year=None):
    """What the candidates of a suggestion are compared with, in words:
    'the release date of passage.0.1.7 (17 Sep 2025)'."""
    cw = r.get("compared_with") or "?"
    m = RELEASE_DATE.match(cw)
    return f"the release date of {m.group(1)} ({short_date(m.group(2), year)})" if m else cw


def candidate_text(r, c, year=None):
    """One candidate of a suggestion, in words: what CI used and what passes."""
    if "first_after" in c:
        m = RELEASE_DATE.match(r.get("compared_with") or "")
        after = f"{m.group(1)} ({short_date(m.group(2), year)})" if m else compared_text(r, year)
        return f"CI used {c.get('failing')}; {c.get('first_after')} is the first release after {after}"
    return f"passes with {c.get('passing')} on {compared_text(r, year)}, fails with {c.get('failing')}"


def evidence_text(ev):
    """'ca-certs-nss 3.103 no, 3.113.1 yes, 3.101-1 (used by CI) no': whether
    each version defines the identifier, sorted by version, the package
    named once."""
    def vkey_of(k):
        return functools.cmp_to_key(opam_vcmp)(k.split(" ")[1] if " " in k else k)
    parts, name = [], None
    for k, v in sorted(ev.items(), key=lambda kv: vkey_of(kv[0])):
        if v is None:
            continue
        k = k.replace(" (installed in failing job)", " (used by CI)")
        pkg, _, rest = k.partition(" ")
        parts.append(f"{rest if pkg == name and rest else k} {'yes' if v else 'no'}")
        name = pkg
    return ", ".join(parts)


def confidence_text(r):
    """'possible causes (low confidence): dependencies released after …' for
    the candidates of a suggestion."""
    m = re.match(r"(\w+)(?: \((.*)\))?$", str(r.get("confidence") or ""))
    word, why = (m.group(1), m.group(2)) if m else (r.get("confidence"), None)
    return f"possible causes ({word} confidence)" + (f": {why}" if why else "")


def mentions_day10(results):
    """Whether the output names the day10 builder (then it is explained once)."""
    return any("day10" in str(r.get(k) or "") for r in results
               for k in ("job", "variant", "error", "note", "title", "jobs"))


DAY10 = ("The day10 builder is the newer of opam-repo-ci's two builders; the other one is the main builder.")


def opam_path(pkg):
    name, ver = split_pkg(pkg or "")
    return f"packages/{name}/{name}.{ver}/opam" if name and ver else None


def diff_block(r):
    """The change of a suggestion as a fenced diff (Markdown). `old` and `new`
    are untrusted (they quote opam files, logs or an LLM answer): only ever
    shown in the code block, whose fence no line of them can close."""
    new = r.get("new") or ""
    if "\n" in new.strip():                  # several lines (an LLM answer): no hint
        new_lines, hint = new.strip("\n").splitlines(), ""
    else:
        parts = new.split("   ")
        new_lines, hint = [parts[0]], " ".join(x.strip() for x in parts[1:] if x.strip())
    lines = ["- " + l for l in (r.get("old") or "").strip("\n").splitlines()] + ["+ " + l for l in new_lines]
    return md_fence("\n".join(lines), "diff") + (f"\n<sub>{md_text(hint)}</sub>" if hint else "")


def evidence_line(r, limit=600):
    """Which versions define the identifier (or the note), as Markdown."""
    ev = r.get("evidence")
    if not ev:
        return md_text(cut(r.get("note"), limit), limit)
    return "Defined in each version: " + md_text(evidence_text(ev), limit)


def changelog_details(c):
    """A folded changelog excerpt of a candidate, in a code block: its text is
    written by the dependency's authors, and in a code block GitHub renders no
    link, image or mention from it."""
    log = c["changelog"]
    text = log["text"] + ("\n…" if log.get("cut") else "")
    # The versions come from the changelog's headings: keep version-like ones only.
    versions = [v for v in log["versions"] if re.fullmatch(r"[\w.+~-]{1,40}", v)]
    # The summary is in an HTML block (no Markdown, no escaping): a package name or nothing.
    dep = c["dep"] if re.fullmatch(r"[\w+.-]{1,80}", c.get("dep") or "") else "a dependency"
    return ("<details><summary>" + f"Changelog of {dep}: {', '.join(versions)}" + "</summary>\n\n"
            + md_fence(text, "text") + "\n\n</details>")


def render_comment(sha, data, results, placed=0, placed_lines=0):
    """A draft PR comment (Markdown) from ranked suggestions. With `placed`,
    the fixes already attached to lines by `review` are left out.

    Every field of a suggestion is untrusted (it quotes CI logs, opam files,
    lint messages or an LLM answer): prose goes through `md_text`, code
    through `md_code` / `md_fence`, and links are only built from the CI
    base URL with a checked commit and variant."""
    by = collections.defaultdict(list)
    for r in results:
        if not r.get("_placed"):
            by[r["class"]].append(r)
    sha_ok = bool(SHA.fullmatch(sha or ""))
    ci = f"[CI results]({BASE}{sha})" if sha_ok else "CI results"
    out = ["### Automated triage of the opam-repo-ci failures", "",
           f"From the {ci} for commit {md_code(sha[:12]) if sha_ok else '?'}. Nothing was rebuilt: these "
           "suggestions come from the CI logs, the documentation of each package version on ocaml.org, and "
           "the opam files. Please check them before applying them."
           + (" " + DAY10 if mentions_day10(results) else ""), ""]
    if data.get("unfinished"):
        out += [f"**CI is still running**: {data['unfinished']} of {counted(len(data['jobs']), 'job')} "
                f"{'is' if data['unfinished'] == 1 else 'are'} not finished, so these are the failures so far.", ""]
    if data.get("unanalysed"):
        n = data["unanalysed"]
        out += [f"**{counted(n, 'more failure')} {'was' if n == 1 else 'were'} not analysed.** See the CI results.",
                ""]
    fixes = by["fix"]
    if placed:
        out += [f"{placed} suggested change{'s are' if placed > 1 else ' is'} attached to "
                f"{placed_lines} line{'s' if placed_lines > 1 else ''} of the diff "
                "(\"Commit suggestion\" applies each one).", ""]
    if fixes:
        out += [f"#### {'Other suggested' if placed else 'Suggested'} changes ({len(fixes)})", ""]
        for r in fixes:
            path = opam_path(r.get("failing"))
            meta = [md_text(title_of(r)), f"{md_text(conf_word(r))} confidence", jobs_note(r)]
            out.append(f"**{md_code(path or r.get('failing')) or '?'}**: " + " · ".join(x for x in meta if x))
            if r.get("error"):
                out.append(f"> {md_text(cut(r['error'], 300), 300)}")
            if r.get("candidates"):
                out.append(f"Possible causes, compared with {md_text(compared_text(r))}:")
                out.append(md_fence("\n".join("+ " + flat(c["new"].split("   ")[0]) for c in r["candidates"]), "diff"))
                for c in r["candidates"]:
                    if c.get("changelog"):
                        out.append(changelog_details(c))
            else:
                if r.get("new"):
                    out.append(diff_block(r))
                if r.get("alternative"):        # also without `new`: a change described in the note
                    alt = re.split(r"\s{2,}(?=\()", r["alternative"], maxsplit=1)
                    out.append(f"Or {md_code(alt[0])}" + (f" {md_text(alt[1])}" if len(alt) > 1 else ""))
            ev = evidence_line(r)
            if ev:
                out.append(f"<sub>{ev}</sub>")
            out.append("")
    elif by["workaround"] and not placed:
        out += ["No fix found. The workarounds below skip the failing platforms or tests.", ""]
    elif not placed:
        out += ["No change to this PR's opam files found.", ""]

    def details(title, rs, fmt):
        if not rs:
            return []
        body = ["<details>", f"<summary>{title} ({len(rs)})</summary>", ""]
        for r in rs:
            body += fmt(r) + [""]
        return body + ["</details>", ""]

    def head(r, where=False):
        # "- **pkg** · title · fix in X · 3 failed jobs · pre-existing · experimental platform"
        title = md_text(title_of(r))
        if r.get("failing"):
            parts = [f"**{md_text(r['failing'])}**", title]
        else:
            # The job, unless it is only a count ("4 jobs"), which the number of failed jobs says.
            job = r.get("job") or describe_job(r.get("variant"))
            parts = [f"**{title}**", "" if re.fullmatch(r"\d+ jobs?", job or "") else md_text(job)]
        w = where_text(r) if where and r.get("rule") != "rerun" else None     # "Jobs to rerun" says it
        if w and w != "this PR":
            parts.append(md_text(fix_label(w)))
        parts.append(jobs_note(r))
        if r.get("preexisting") and not r.get("preexisting_unverified"):
            parts.append("pre-existing")
        if r.get("experimental"):
            parts.append("experimental platform")
        return "- " + " · ".join(p for p in parts if p)

    def job_links(r, n=10):
        # For the entries whose content is the list of jobs (reruns, unexplained failures).
        if r.get("rule") not in ("rerun", "unrecognised"):
            return ""
        js = r.get("jobs") or []
        links = []
        for v in js[:n]:
            url = ci_job_url(BASE, sha, v)
            links.append(f"[{md_text(describe_job(v))}]({url})" if url else md_text(describe_job(v)))
        return (f"<br>Jobs: {', '.join(links)}" + (f" and {len(js) - n} more" if len(js) > n else "")) if js else ""

    def note(r):
        return f"<br>{md_text(cut(r['note'], 600), 600)}" if r.get("note") else ""

    def error(r):
        return f": {md_text(cut(r['error'], 300), 300)}" if r.get("error") else ""

    def log_link(r):
        url = safe_ci_url(r.get("url"))
        return f" ([log]({url}))" if url else ""

    out += details("Workarounds: skip some platforms or tests", by["workaround"], lambda r: [
        f"{head(r)}{error(r)}" + note(r)]
        + (["  " + diff_block(r).replace("\n", "\n  ")] if r.get("new") else []))
    out += details("Fix goes elsewhere", by["elsewhere"], lambda r: [
        f"{head(r, where=True)}{error(r)}"
        + (f"<br>Suggested change: {md_code((r.get('new') or '').split('   ')[0])}" if r.get("new") else "")
        + note(r) + job_links(r)])
    out += details("Other failures (no change suggested)", by["explanation"], lambda r: [
        f"{head(r)}{error(r)}" + note(r) + log_link(r) + job_links(r)])
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
        # An LLM answer is never a one-click suggestion (it stays in the summary,
        # as a diff to check), and a change is one opam line.
        if r.get("rule") == "llm" or any(c in (r.get(k) or "") for k in ("old", "new") for c in "\r\n"):
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
        why = f"**{md_text(title_of(r))}** · {md_text(conf_word(r))} confidence"
        if r.get("error"):
            why += f": {md_text(cut(r['error'], 300), 300)}"
        ev = evidence_line(r, 300)
        for n, replacement in edits:
            comments.append({"path": path, "line": n, "side": "RIGHT",
                             "body": f"{why}\n{md_fence(replacement, 'suggestion')}" + (f"\n<sub>{ev}</sub>" if ev else "")})
    body = render_comment(sha, data, results, placed=sum(1 for r in results if r.get("_placed")),
                          placed_lines=len(comments))
    return {"commit_id": sha, "event": "COMMENT", "body": body, "comments": comments}


def render_suggestions(results):
    if mentions_day10(results):
        print(DAY10)
    for r in results:
        print("=" * 100)
        # A lint error's job is "opam lint", and a pattern's "N packages": the title says it.
        print(title_of(r) + ("" if r.get("variant") == "(lint)" or r.get("rule", "").endswith("-systemic")
                             else f": {r.get('job') or describe_job(r['variant'])}"))
        jobs = r.get("jobs") or []
        if r.get("rule") in ("rerun", "unrecognised"):
            for v in jobs:
                print(f"  job             : {describe_job(v)}")
        elif len(jobs) > 1:
            others = [v for v in jobs if v != r["variant"]]
            # A pattern's variant is a label, not one of its jobs: then all are listed.
            label = (f"same failure on : {counted(len(others), 'other job')}: " if len(others) < len(jobs)
                     else f"failed jobs     : {len(jobs)}: ")
            print("  " + label + ", ".join(describe_job(v) for v in others[:5])
                  + (f" and {len(others) - 5} more" if len(others) > 5 else ""))
        w = where_text(r)
        if r.get("failing"):
            print(f"  failing package : {r['failing']}" + (f"   {fix_label(w)}" if w else "")
                  + ("   (experimental platform: does not block)" if r.get("experimental") else ""))
        if r.get("error"):
            print(f"  error           : {r['error']}")
        if r.get("source"):
            print(f"  looked up in    : {r['source']}")
        if r.get("evidence"):
            print(f"  defined in      : {evidence_text(r['evidence'])}")
        for t in r.get("tried", []):
            print(f"  tried           : {t}")
        if r.get("new"):
            label = {"workaround": "workaround", "elsewhere": "fix goes elsewhere"}.get(r.get("class"), "suggested change")
            print(f"  {label} ({conf_word(r)} confidence):")
            if r.get("old"):
                print(f"    - {r['old']}")
            print(f"    + {r['new']}")
            if r.get("alternative"):
                print(f"    or {r['alternative']}")
        elif r.get("alternative"):              # a change described in the note, and an alternative
            print(f"  alternative     : {r['alternative']}")
        if r.get("candidates"):
            print(f"  {confidence_text(r)}")
        for c in r.get("candidates", []):
            print(f"    {c['dep']}: {candidate_text(r, c)}")
            if c.get("old"):
                print(f"    - {c['old']}")
            print(f"    + {c['new']}")
            log = c.get("changelog")
            if log:
                lines = log["text"].splitlines()
                print(f"      changelog ({log.get('file') or '?'}, {', '.join(log['versions'])}), "
                      f"written by its authors:")
                for line in lines[:12]:
                    print(f"        | {line}")
                if len(lines) > 12 or log.get("cut"):
                    print("        | …")
        if r.get("also_newer"):
            print(f"  also released after it (not ranked): {', '.join(r['also_newer'])}")
        if r.get("note"):
            print(f"  note            : {r['note']}")
        if r.get("llm_prompt"):
            print("  LLM prompt (dry run):\n    " + r["llm_prompt"].replace("\n", "\n    "))
