#!/usr/bin/env python3
"""Render site/data.json (from triage_open_prs.py) into a single static page.

  python3 scripts/render_page.py [--data site/data.json] [--out site/index.html]

The page has no external resources (inline CSS and a little JS for the
filters). Everything that comes from GitHub, the CI logs or the suggestions is
untrusted and HTML-escaped; links are kept only when they are https URLs.
"""

import argparse
import collections
import html
import json
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, ROOT)
from opam_repo_ci_triage import BASE, RULE_TITLES, describe_job, render_comment  # noqa: E402
REPO_URL = "https://github.com/ocaml/opam-repository"
CLASSES = [("fix", "Changes to this PR", "change"),
           ("workaround", "Workarounds: restrict where this PR is built or tested", "workaround"),
           ("elsewhere", "Problems in other packages or in the CI", "elsewhere"),
           ("explanation", "Other findings, no change suggested", "other finding")]
CATEGORIES = [("ready", "Seems ready"), ("fix", "Has a fix for the PR"), ("blocked", "Blocked by other packages"),
              ("ci", "CI problems only"), ("other", "Needs a look"), ("error", "Not analysed"),
              ("merge", 'Labelled "consider for merge"')]
# Labels of ocaml/opam-repository: "consider for merge" is a filter, and a PR
# with one of NOT_READY_LABELS is never listed as ready, unless the label may
# be stale (the author acted after it was added, see triage_open_prs.py).
MERGE_LABEL, NO_MERGE_LABEL = "consider for merge", "do not merge"
NOT_READY_LABELS = {NO_MERGE_LABEL, "needs reporter action"}
# Rules whose failures come from the CI, not from the PR.
CI_SIDE = {"builder-specific", "builder-specific-systemic", "accept-failures-ignored", "rerun",
           "opam-same-version-reinstall", "health-check-preexisting", "lint-ci-error"}
# Job categories (from `summary`), in words.
JOB_CATEGORIES = {"build": "build failed", "build-or-test": "build or tests failed",
                  "dependency": "a dependency failed", "solver": "no solution", "network": "network error",
                  "cancelled": "cancelled", "sources": "sources not unpacked", "lint": "lint",
                  "not-available": "skipped (not installable there)", "ignored": "ignored (accepted failure)"}
NOISE = {"passed", "network", "cancelled", "not-available", "ignored"}


def e(s):
    return html.escape("" if s is None else str(s), quote=True)


def link(url, text, cls=None):
    """An <a> only for https URLs; plain escaped text otherwise."""
    c = f' class="{cls}"' if cls else ""
    if isinstance(url, str) and url.startswith("https://") and not re.search(r"[\s\"'<>`]", url):
        return f'<a href="{e(url)}"{c} rel="noopener noreferrer">{e(text)}</a>'
    return f"<span{c}>{e(text)}</span>"


def other_package(fix_in):
    fix_in = fix_in or ""
    return fix_in.endswith("(other package)") or fix_in.startswith("the dependency")


def categorise(pr):
    """Non-exclusive categories of a PR, and the suggestions grouped by class."""
    if pr.get("pending"):
        return {"error"}, {}
    res = pr.get("result")
    if res is None:
        return {"error"}, {}
    groups = collections.defaultdict(list)
    for r in res.get("suggestions", []):
        groups[r.get("class") or "explanation"].append(r)
    cats = set()
    sugg = res.get("suggestions", [])
    if groups["fix"] or groups["workaround"]:
        cats.add("fix")
    if any(other_package(r.get("fix_in")) for r in sugg):
        cats.add("blocked")
    if not cats and all(r.get("fix_in") != "this PR" for r in sugg):
        cats.add("ci")
    if not cats:
        cats.add("other")
    return cats, groups


def not_this_pr(r):
    """Whether a failure is not the PR's: it happens without the PR (pre-existing,
    or also failing in other open PRs), comes from the CI, or is on an
    experimental platform, whose failures do not block a PR."""
    return bool((r.get("preexisting") and not r.get("preexisting_unverified")) or r.get("also_failing_in")
                or r.get("experimental") or r.get("rule") in CI_SIDE)


def seems_ready(pr):
    """A PR whose CI fails, but no failure points at the PR. Its CI must have
    finished, and its failures must have been explained: a PR whose only
    "failures" are skipped jobs may not be installable anywhere."""
    res = pr.get("result")
    if pr.get("pending") or res is None or pr.get("ci_state") == "PENDING" or pr.get("draft"):
        return False
    if blocking_labels(pr) or pr.get("ci_unfinished") or res.get("ci_unfinished"):
        return False
    if res.get("unanalysed"):          # failures beyond `--max`: nothing says they are not the PR's
        return False
    sugg = res.get("suggestions", [])
    return bool(sugg) and all(not_this_pr(r) for r in sugg)


def pr_cats(pr):
    """The categories of a PR, for the badges and the filters."""
    cats = set(categorise(pr)[0])
    if seems_ready(pr):
        cats.add("ready")
    if MERGE_LABEL in (pr.get("labels") or []):
        cats.add("merge")
    return cats


def blocking_labels(pr):
    """The labels of NOT_READY_LABELS that still apply."""
    return NOT_READY_LABELS & set(pr.get("labels") or []) - set(pr.get("stale_labels") or [])


def label_badges(labels, stale=()):
    """The PR's labels, "consider for merge" first. A label the author may
    have answered since it was added says so."""
    def badge(lb):
        if lb in stale:
            return (f'<span class="badge b-label b-stale" title="The author pushed or commented after this label '
                    f'was added: it may be out of date">{e(lb)} (author active since)</span>')
        cls = {MERGE_LABEL: "b-merge", NO_MERGE_LABEL: "b-nomerge"}.get(lb, "b-label")
        return f'<span class="badge {cls}" title="Label">{e(lb)}</span>'
    return "".join(badge(lb) for lb in sorted(labels, key=lambda lb: lb != MERGE_LABEL))


def comment_markdown(pr):
    """The PR comment of the `comment` command, from the stored analysis. Low-
    confidence LLM answers are left out, as they are folded on the page."""
    res = pr.get("result") or {}
    results = [r for r in res.get("suggestions", []) if r.get("class")
               and not (r.get("rule") == "llm" and str(r.get("confidence") or "").startswith("low"))]
    total = (pr.get("jobs") or {}).get("total") or 0
    data = {"unfinished": pr.get("ci_unfinished") or 0, "jobs": [None] * total,
            "unanalysed": res.get("unanalysed") or 0}
    md = render_comment(pr.get("sha") or "", data, results)
    if seems_ready(pr):
        # After the paragraph on where the results come from.
        lines = md.split("\n")
        i = next((k for k, x in enumerate(lines) if x.startswith("From the [CI results]")), 1)
        lines[i + 1:i + 1] = ["", "**No failure seems to come from this PR**: each one also happens without it, "
                              "comes from the CI, or is on an experimental platform (details below)."]
        md = "\n".join(lines)
    return md


def copy_block(pr):
    """A button copying the Markdown comment, and the Markdown itself, folded."""
    n = int(pr["number"])
    try:
        md = comment_markdown(pr)
    except Exception as ex:          # a malformed entry must not break the page
        print(f"#{n}: no Markdown comment ({ex!r})", file=sys.stderr)
        return ""
    return (f'<div class="md"><button type="button" class="copy" data-for="md-{n}">'
            f'Copy as a PR comment (Markdown)</button>'
            f'<details class="mdv"><summary>Show the Markdown</summary>'
            f'<textarea id="md-{n}" readonly rows="14" spellcheck="false">{e(md)}</textarea></details></div>')


def failed_count(pr):
    failed = ((pr.get("jobs") or {}).get("failed") or {})
    return sum(v for k, v in failed.items() if k not in NOISE), sum(failed.values())


def job_link(sha, variant):
    return link(f"{BASE}{sha}/variant/{variant}" if sha else None, describe_job(variant))


def render_suggestion(r, sha=None):
    rule = r.get("rule")
    title = r.get("title") or RULE_TITLES.get(rule, rule) or "Note"
    out = ['<div class="sugg">', '<div class="sh">', f'<b>{e(title)}</b>']
    if r.get("failing"):
        out.append(f'<span class="pkg">{e(r["failing"])}</span>')
    if r.get("confidence"):
        conf = str(r["confidence"]).split(" ")[0]
        out.append(f'<span class="tag conf-{e(conf)}" title="{e(r["confidence"])}">{e(conf)} confidence</span>')
    if r.get("experimental"):
        out.append('<span class="tag" title="Failures on experimental platforms do not block the PR">'
                   'experimental platform</span>')
    out.append("</div>")
    meta = []
    jobs = r.get("jobs") or []
    lists_jobs = rule in ("rerun", "unrecognised")
    if r.get("variant") and r.get("url") and not lists_jobs:
        meta.append("Job: " + link(r.get("url"), r.get("job") or describe_job(r["variant"])))
    where = r.get("where") or r.get("fix_in")
    if where and where not in ("unknown",):
        meta.append("Fix in: " + e(where))
    if meta:
        out.append(f'<div class="meta">{" · ".join(meta)}</div>')
    if r.get("error"):
        out.append(f'<pre class="err">{e(r["error"])}</pre>')
    if r.get("new"):
        diff = []
        if r.get("old"):
            diff.append(f'<span class="del">- {e(r["old"])}</span>')
        diff.append(f'<span class="add">+ {e(r["new"])}</span>')
        if r.get("alternative"):
            diff.append(f'<span class="alt">or {e(r["alternative"])}</span>')
        out.append('<pre class="diff">' + "\n".join(diff) + "</pre>")
    if r.get("candidates"):
        out.append(f'<div class="meta">Dependencies that may have changed the output '
                   f'(compared with {e(r.get("compared_with") or "?")}):</div>')
        for c in r["candidates"]:
            if "first_after" in c:
                what = f'{c.get("failing")} installed; first release after the compared one: {c.get("first_after")}'
            else:
                what = f'{c.get("passing")} passes, {c.get("failing")} fails'
            diff = [f'<span class="cm"># {e(c.get("dep"))}: {e(what)}</span>']
            if c.get("old"):
                diff.append(f'<span class="del">- {e(c["old"])}</span>')
            if c.get("new"):
                diff.append(f'<span class="add">+ {e(c["new"])}</span>')
            out.append('<pre class="diff">' + "\n".join(diff) + "</pre>")
            log = c.get("changelog")
            if log:
                out.append(f'<details class="jobs"><summary>Changelog of {e(c.get("dep"))}: '
                           f'{e(", ".join(log.get("versions") or []))} ({e(log.get("file") or "?")}, written by '
                           f'its authors)</summary><pre class="err">{e(log.get("text"))}'
                           f'{chr(10) + "…" if log.get("cut") else ""}</pre></details>')
    if r.get("also_newer"):
        out.append(f'<div class="meta">Also released after it (not ranked): '
                   f'{e(", ".join(map(str, r["also_newer"])))}</div>')
    if r.get("evidence"):
        ev = []
        for k, v in r["evidence"].items():
            word, cls = ("yes", "yes") if v is True else (("no", "no") if v is False else ("?", "unk"))
            ev.append(f'<li>{e(k)}: <span class="{cls}">{word}</span></li>')
        src = f' (looked up in {e(r["source"])})' if r.get("source") else ""
        out.append(f'<div class="meta">Is it defined in each version?{src}</div><ul class="ev">{"".join(ev)}</ul>')
    elif r.get("source"):
        out.append(f'<div class="meta">Looked up in: {e(r["source"])}</div>')
    for t in r.get("tried") or []:
        out.append(f'<div class="meta">Tried: {e(t)}</div>')
    if r.get("note"):
        out.append(f'<div class="note">{e(r["note"])}</div>')
    if r.get("also_failing_in"):
        prs = r["also_failing_in"]
        out.append(f'<div class="note shared">Also failing in {len(prs)} other open PRs, so probably not '
                   f'caused by this one: '
                   + ", ".join(link(f"{REPO_URL}/pull/{int(n)}", f"#{int(n)}") for n in prs[:12])
                   + (f" and {len(prs) - 12} more" if len(prs) > 12 else "") + "</div>")
    if lists_jobs and jobs:
        out.append(f'<ul class="jobs">{"".join(f"<li>{job_link(sha, v)}</li>" for v in jobs)}</ul>')
    else:
        others = [v for v in jobs if v != r.get("variant")]
        if others:
            # A matrix suggestion's variant is a label, not one of its jobs: then list them all.
            what = (f'{len(others)} more job{"s" if len(others) != 1 else ""}' if len(others) < len(jobs)
                    else f'{len(jobs)} job{"s" if len(jobs) != 1 else ""}')
            out.append(f'<details class="jobs"><summary>Same failure on {what}</summary>'
                       f'<ul>{"".join(f"<li>{job_link(sha, v)}</li>" for v in others)}</ul></details>')
    out.append("</div>")
    html_ = "\n".join(out)
    if rule == "llm" and str(r.get("confidence") or "").startswith("low"):
        # Low-confidence LLM answers were never right in the evaluation: folded.
        html_ = (f'<details class="hint"><summary>Low-confidence LLM hint'
                 f'{": " + e(r["failing"]) if r.get("failing") else ""}</summary>{html_}</details>')
    return html_


READY_TITLE = (' title="No failure points at this PR: each one also happens without it (pre-existing, or in '
               'other open PRs), comes from the CI, or is on an experimental platform. Check the details '
               'before merging."')


def ready_section(data):
    """The PRs that seem ready: CI passes, or no failure points at the PR.
    Drafts and PRs with one of NOT_READY_LABELS are left out."""
    rows = []
    for p in data.get("passing") or []:
        if not p.get("draft") and not blocking_labels(p):
            rows.append((p, "CI passes"))
    for p in data.get("prs", []):
        if seems_ready(p):
            rows.append((p, None))
    # Those labelled "consider for merge" first, then the most recently updated.
    rows.sort(key=lambda pw: pw[0].get("updated") or "", reverse=True)
    rows.sort(key=lambda pw: MERGE_LABEL not in (pw[0].get("labels") or []))
    html_rows = []
    for p, why in rows:
        n = int(p["number"])
        why_html = e(why) if why else f'CI fails, not because of this PR (<a href="#pr-{n}">details</a>)'
        html_rows.append(f'<tr><td class="nw">{link(f"{REPO_URL}/pull/{n}", f"#{n}")}</td>'
                         f'<td>{e(p.get("title"))} {label_badges(p.get("labels") or [], p.get("stale_labels") or [])}</td>'
                         f'<td>{e(p.get("author") or "?")}</td><td>{why_html}</td>'
                         f'<td class="nw">{e((p.get("updated") or "")[:10])}</td></tr>')
    if not html_rows:
        return ""
    return ('<section id="ready"><h2>PRs that seem ready (' + str(len(html_rows)) + ')</h2>'
            '<p class="meta">Their CI passes, or no failure points at the PR. Drafts and PRs labelled '
            '"do not merge" or "needs reporter action" are left out, unless the author pushed or commented '
            'after the label was added. Only CI is checked here: reviews and discussions are not.</p>'
            + table(["PR", "Title", "Author", "CI", "Updated"], html_rows, shown=15) + "</section>")


def render_pr(pr):
    _, groups = categorise(pr)
    cats = pr_cats(pr)
    n = int(pr["number"])
    real, total = failed_count(pr)
    counts = [f'<span class="cnt c-{k}" title="{e(label)}">{len(groups[k])} {e(word)}'
              f'{"s" if len(groups[k]) != 1 and k != "elsewhere" else ""}</span>'
              for k, label, word in CLASSES if groups.get(k)]
    badges = "".join(f'<span class="badge b-{c}"{READY_TITLE if c == "ready" else ""}>{e(label)}</span>'
                     for c, label in CATEGORIES if c in cats and c != "merge")
    badges += label_badges(pr.get("labels") or [], pr.get("stale_labels") or [])
    if pr.get("ci_state") == "PENDING":
        left = pr.get("ci_unfinished")
        badges = (f'<span class="badge b-running" title="These are the failures so far; the page is updated '
                  f'when CI finishes">CI running{f": {e(left)} jobs left" if left else ""}</span>') + badges
    title = pr.get("title") or ""
    search = f"{n} {title} {pr.get('author') or ''}".lower()
    head = (f'<summary><span class="num">{link(f"{REPO_URL}/pull/{n}", f"#{n}")}</span>'
            f'<span class="title">{e(title)}{" <i>(draft)</i>" if pr.get("draft") else ""}</span>'
            f'<span class="row2">{badges}'
            f'<span class="cnt" title="Failed build and test jobs (network errors, cancelled and skipped '
            f'jobs are not counted)">{real} failed job{"s" if real != 1 else ""}</span>{"".join(counts)}'
            f'{link(pr.get("ci_url"), "CI results", "ci")}'
            f'<span class="date">{e((pr.get("updated") or "")[:10])}</span></span></summary>')
    failed = sorted(((pr.get("jobs") or {}).get("failed") or {}).items(), key=lambda kv: -kv[1])
    body = ['<div class="body">',
            f'<div class="meta">By {e(pr.get("author") or "?")} · commit {e((pr.get("sha") or "")[:12])}'
            f' · analysed {e((pr.get("analysed_at") or "never")[:16].replace("T", " "))} UTC'
            f' · jobs not passing: '
            f'{e(", ".join(f"{v} {JOB_CATEGORIES.get(k, k)}" for k, v in failed) or "?")}'
            "</div>"]
    since = pr.get("since_last_push")
    if since and (since.get("fixed") or since.get("new")):
        def items(xs):
            return ", ".join(e(x) for x in xs[:8]) + (f" and {len(xs) - 8} more" if len(xs) > 8 else "")
        parts = []
        if since.get("fixed"):
            parts.append(f'<span class="yes">{len(since["fixed"])} gone</span> ({items(since["fixed"])})')
        if since.get("new"):
            parts.append(f'<span class="no">{len(since["new"])} new</span> ({items(since["new"])})')
        parts.append(f'{e(since.get("still", 0))} unchanged')
        body.append(f'<p class="note since">Since the previous push '
                    f'({e((since.get("previous_sha") or "")[:12])}): ' + "; ".join(parts) + ".</p>")
    if pr.get("pending"):
        body.append('<p class="note">Not analysed yet: the run ran out of time. The next run continues.</p>')
    elif pr.get("result") is None:
        body.append(f'<p class="note">The analysis failed ({e(pr.get("attempts"))} attempts): '
                    f'<code>{e(pr.get("error"))}</code></p>')
    else:
        body.append(copy_block(pr))
        left = pr["result"].get("unanalysed")
        if left:
            body.append(f'<p class="note">{e(left)} more distinct failure{"s were" if left != 1 else " was"} not '
                        f'analysed (each run analyses a few per PR): see the CI results.</p>')
        if not pr["result"].get("suggestions"):
            body.append('<p class="note">No build or test failure to analyse.</p>')
        for k, label, _ in CLASSES:
            if groups.get(k):
                body.append(f'<h4 class="g-{k}">{e(label)} ({len(groups[k])})</h4>')
                body.extend(render_suggestion(r, pr.get("sha")) for r in groups[k])
    body.append("</div>")
    return (f'<details class="pr" id="pr-{n}" data-cats="{" ".join(sorted(cats))}" data-search="{e(search)}">'
            + head + "\n".join(body) + "</details>")


def table(head, rows, shown=25):
    """A table whose rows after the first `shown` are folded in a <details>."""
    th = "".join(f"<th>{e(h)}</th>" for h in head)
    def tab(rs):
        return (f'<div class="tw"><table><thead><tr>{th}</tr></thead><tbody>'
                + "".join(rs) + "</tbody></table></div>")
    out = tab(rows[:shown])
    if len(rows) > shown:
        out += f'<details class="more"><summary>{len(rows) - shown} more</summary>{tab(rows[shown:])}</details>'
    return out


def chronic(prs, min_prs=2):
    """Reverse dependencies (by package name) failing on several PRs, and other
    packages blamed by several PRs."""
    revdeps = collections.defaultdict(lambda: collections.defaultdict(dict))   # name -> PR -> version -> jobs
    blamed = collections.defaultdict(set)
    for pr in prs:
        n = int(pr["number"])
        for rd, jobs in ((pr.get("jobs") or {}).get("failing_revdeps") or {}).items():
            name, _, version = rd.partition(".")
            revdeps[name][n][version] = jobs
        for r in (pr.get("result") or {}).get("suggestions", []):
            if (r.get("fix_in") or "").endswith(" (other package)"):
                blamed[(r.get("fix_in") or "").replace(" (other package)", "")].add(n)
    rows = sorted(((rd, d) for rd, d in revdeps.items() if len(d) >= min_prs), key=lambda x: (-len(x[1]), x[0]))
    brows = sorted(((p, s) for p, s in blamed.items() if len(s) >= min_prs), key=lambda x: (-len(x[1]), x[0]))
    out = ['<section id="chronic"><h2>Failures shared by several PRs</h2>']
    out.append(f'<h3>Reverse dependencies failing on {min_prs} or more PRs ({len(rows)})</h3>')
    if rows:
        trs = []
        for name, d in rows:
            versions = sorted({v for vs in d.values() for v in vs})
            cells = " ".join(f'<span class="nw">{link(f"{REPO_URL}/pull/{n}", f"#{n}")}'
                              f' ({link(jobs[0].get("url"), jobs[0].get("category") or "job")})</span>'
                              for n, vs in sorted(d.items()) for jobs in [next(iter(vs.values()))])
            trs.append(f'<tr><td><b>{e(name)}</b><div class="meta">{e(", ".join(versions))}</div></td>'
                       f'<td>{len(d)}</td><td>{cells}</td></tr>')
        out.append(table(["Reverse dependency (failing versions)", "PRs", "PRs (one failing job each)"], trs))
    else:
        out.append('<p class="note">None among the analysed PRs.</p>')
    out.append(f'<h3>Other packages where the fix goes, for {min_prs} or more PRs ({len(brows)})</h3>')
    if brows:
        out.append(table(["Package", "PRs", "PRs"], [
            f'<tr><td><b>{e(p)}</b></td><td>{len(s)}</td><td>'
            + ", ".join(link(f"{REPO_URL}/pull/{n}", f"#{n}") for n in sorted(s)) + "</td></tr>"
            for p, s in brows]))
    else:
        out.append('<p class="note">None among the analysed PRs.</p>')
    out.append('<p class="meta">The first table counts the failed build and test jobs of reverse dependencies '
               '(network errors, cancelled, skipped and experimental jobs are left out). The second lists the '
               'packages that suggestions for several PRs point to.</p></section>')
    return "\n".join(out)


CSS = """
:root{--bg:#fbfbfa;--fg:#1d1f21;--mut:#5f6368;--card:#fff;--bd:#dadce0;--code:#f3f3f1;--acc:#0b57d0;
--add:#116329;--addbg:#dafbe1;--del:#82071e;--delbg:#ffebe9;--fix:#116329;--wk:#8a5a00;--else:#6639ba;--exp:#5f6368;--err:#b3261e}
@media (prefers-color-scheme:dark){:root{--bg:#121416;--fg:#e3e3e3;--mut:#9aa0a6;--card:#1c1f23;--bd:#3c4043;--code:#25292e;
--acc:#8ab4f8;--add:#7ee2a8;--addbg:#12361f;--del:#ffa198;--delbg:#3d1416;--fix:#7ee2a8;--wk:#f2cc60;--else:#d2a8ff;--exp:#9aa0a6;--err:#f28b82}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.45 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}
main{max-width:1100px;margin:0 auto;padding:16px}
a{color:var(--acc)}
h1{font-size:1.4rem;margin:.2em 0}h2{font-size:1.15rem;margin:1.5em 0 .5em}h3{font-size:1rem;margin:1.2em 0 .4em}
h4{font-size:.95rem;margin:1em 0 .4em}
.meta{color:var(--mut);font-size:.85rem;overflow-wrap:anywhere}
.stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(140px,1fr));gap:8px;margin:12px 0}
.stat{background:var(--card);border:1px solid var(--bd);border-radius:8px;padding:8px 12px}
.stat b{display:block;font-size:1.5rem}
.filters{display:flex;flex-wrap:wrap;gap:6px;margin:12px 0;align-items:center}
.filters button{font:inherit;font-size:.85rem;padding:5px 10px;border-radius:16px;border:1px solid var(--bd);
background:var(--card);color:var(--fg);cursor:pointer}
.filters button[aria-pressed=true]{background:var(--acc);color:var(--bg);border-color:var(--acc)}
.filters input{font:inherit;font-size:.9rem;padding:5px 10px;border-radius:16px;border:1px solid var(--bd);
background:var(--card);color:var(--fg);flex:1 1 180px;min-width:0}
details.pr{background:var(--card);border:1px solid var(--bd);border-radius:8px;margin:6px 0}
details.pr>summary{cursor:pointer;padding:8px 12px;display:flex;flex-wrap:wrap;gap:4px 10px;align-items:baseline;list-style:none}
details.pr>summary::-webkit-details-marker{display:none}
details.pr>summary::before{content:"\\25B8";color:var(--mut)}details.pr[open]>summary::before{content:"\\25BE"}
.num{font-weight:600}.title{flex:1 1 300px;min-width:0;overflow-wrap:anywhere}
.row2{display:flex;flex-wrap:wrap;gap:4px 8px;align-items:center;font-size:.8rem;width:100%;padding-left:1.1em}
.badge,.tag,.cnt{border-radius:10px;padding:1px 7px;font-size:.78rem;border:1px solid var(--bd);white-space:nowrap}
.b-fix{color:var(--fix);border-color:var(--fix)}.b-blocked{color:var(--else);border-color:var(--else)}
.b-ci{color:var(--wk);border-color:var(--wk)}.b-running{color:var(--acc);border-color:var(--acc)}.b-error{color:var(--err);border-color:var(--err)}
.c-fix{color:var(--fix)}.c-workaround{color:var(--wk)}.c-elsewhere{color:var(--else)}.c-explanation{color:var(--exp)}
.date{color:var(--mut);margin-left:auto}
.body{padding:0 12px 12px;border-top:1px solid var(--bd)}
h4.g-fix{color:var(--fix)}h4.g-workaround{color:var(--wk)}h4.g-elsewhere{color:var(--else)}h4.g-explanation{color:var(--exp)}
.sugg{border-left:3px solid var(--bd);padding:4px 0 4px 10px;margin:8px 0}
.sh{display:flex;flex-wrap:wrap;gap:6px;align-items:baseline;overflow-wrap:anywhere}
.conf-high{color:var(--fix)}.conf-medium{color:var(--wk)}.conf-low{color:var(--err)}
pre{background:var(--code);border-radius:6px;padding:6px 8px;margin:6px 0;overflow-x:auto;font-size:.82rem;
white-space:pre-wrap;overflow-wrap:anywhere}
pre.err{max-height:16em;overflow-y:auto}
.diff span{display:block}.add{color:var(--add);background:var(--addbg)}.del{color:var(--del);background:var(--delbg)}
.alt,.cm{color:var(--mut)}
.note{font-size:.88rem;overflow-wrap:anywhere}
.lead{max-width:70ch}.pkg{overflow-wrap:anywhere}
details.hint>summary{cursor:pointer;color:var(--mut);font-size:.85rem;margin:8px 0}
ul.jobs{margin:4px 0;padding-left:20px;font-size:.85rem}
dl.help dt{font-weight:600;margin-top:.6em}dl.help dd{margin:0 0 0 1.2em;font-size:.9rem;max-width:75ch}
details.jobs{color:var(--mut);font-size:.85rem;overflow-wrap:anywhere}
details.jobs>summary{cursor:pointer}details.jobs ul{margin:2px 0 4px;padding-left:20px}
ul.ev{margin:2px 0 6px;padding-left:1.4em;font-size:.85rem}.yes{color:var(--add)}.no{color:var(--del)}.unk{color:var(--mut)}
.cmd pre{margin-top:2px;white-space:pre}
.tw{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:.88rem}
th,td{border-bottom:1px solid var(--bd);padding:5px 8px;text-align:left;vertical-align:top}
td:last-child{overflow-wrap:anywhere}.nw{white-space:nowrap}
details.more>summary{cursor:pointer;color:var(--acc);margin:6px 0}
.hidden{display:none}
.b-ready{color:var(--fix);border-color:var(--fix);font-weight:600}.b-merge{color:var(--acc);border-color:var(--acc)}
.b-nomerge{color:var(--err);border-color:var(--err)}.b-label{color:var(--mut)}.b-stale{border-style:dashed}
.md{margin:8px 0}.md button{font:inherit;font-size:.85rem;padding:4px 10px;border-radius:6px;border:1px solid var(--bd);
background:var(--card);color:var(--fg);cursor:pointer}
details.mdv{display:inline-block;margin-left:8px;font-size:.85rem;color:var(--mut);vertical-align:top}
details.mdv>summary{cursor:pointer}
details.mdv[open]{display:block;margin:6px 0 0}
details.mdv textarea{width:100%;font:.8rem/1.4 ui-monospace,SFMono-Regular,Menlo,monospace;background:var(--code);
color:var(--fg);border:1px solid var(--bd);border-radius:6px;padding:6px 8px}
"""

JS = """
(function(){
  var cur='all', q='';
  var prs=[].slice.call(document.querySelectorAll('details.pr'));
  var btns=[].slice.call(document.querySelectorAll('.filters button[data-f]'));
  function apply(){
    var shown=0;
    prs.forEach(function(d){
      var ok=(cur==='all'||d.getAttribute('data-cats').split(' ').indexOf(cur)>=0)&&
             (!q||d.getAttribute('data-search').indexOf(q)>=0);
      d.classList.toggle('hidden',!ok); if(ok)shown++;
    });
    btns.forEach(function(b){b.setAttribute('aria-pressed',b.getAttribute('data-f')===cur?'true':'false');});
    document.getElementById('shown').textContent=shown+' shown';
  }
  btns.forEach(function(b){b.addEventListener('click',function(){cur=b.getAttribute('data-f');apply();});});
  document.getElementById('q').addEventListener('input',function(ev){q=ev.target.value.toLowerCase();apply();});
  document.getElementById('expand').addEventListener('click',function(){
    var open=!prs.some(function(d){return d.open&&!d.classList.contains('hidden');});
    prs.forEach(function(d){if(!d.classList.contains('hidden'))d.open=open;});
  });
  apply();
  [].forEach.call(document.querySelectorAll('button.copy'),function(b){
    var label=b.textContent;
    function done(ok){
      b.textContent=ok?'Copied':'Select the text below and copy it';
      setTimeout(function(){b.textContent=label;},2000);
    }
    b.addEventListener('click',function(){
      var t=document.getElementById(b.getAttribute('data-for'));
      function fallback(){
        var d=t.closest('details'); if(d)d.open=true;
        t.focus(); t.select();
        var ok=false; try{ok=document.execCommand('copy');}catch(e){}
        done(ok);
      }
      if(navigator.clipboard&&window.isSecureContext){
        navigator.clipboard.writeText(t.value).then(function(){done(true);},fallback);
      } else fallback();
    });
  });
})();
"""


def render(data):
    prs = data.get("prs", [])
    cats = {int(p["number"]): pr_cats(p) for p in prs}
    analysed = sum(1 for p in prs if p.get("result") is not None and not p.get("pending"))
    count = collections.Counter(c for s in cats.values() for c in s)
    gen = data.get("generated") or ""
    run = data.get("run") or {}
    passing = [p for p in data.get("passing") or [] if not p.get("draft")]
    stats = ([("PRs analysed", analysed)] + [(label, count[c]) for c, label in CATEGORIES
                                              if c not in ("error", "merge")]
             + [("CI passes (not on this list)", len(passing))])
    if count["error"]:
        stats.append(("Not analysed", count["error"]))
    limit = f" (limited to {e(data['limit'])})" if data.get("limit") else ""
    parts = [
        "<!DOCTYPE html>", '<html lang="en"><head><meta charset="utf-8">',
        '<meta name="viewport" content="width=device-width,initial-scale=1">',
        '<meta name="color-scheme" content="light dark">',
        "<title>opam-repository CI triage</title>",
        '<link rel="alternate" type="application/atom+xml" title="opam-repository CI triage" href="feed.xml">',
        f"<style>{CSS}</style></head><body><main>",
        "<h1>opam-repository CI triage</h1>",
        f'<p class="lead">The open pull requests of {link(REPO_URL, "ocaml/opam-repository")} whose CI '
        f'(opam-repo-ci) fails: {e(data.get("failing_prs"))} of {e(data.get("open_prs"))} open PRs{limit}'
        + (f', and {e(data.get("running_prs"))} whose CI is still running but already has failures'
           if data.get("running_prs") else "") + '. '
        f'For each one, the failures are read from the CI logs and matched against known causes, with a '
        f'suggested change when one can be derived. Nothing is rebuilt, so check a suggestion before '
        f'applying it.</p>'
        + (f'<p class="meta">{e(len(passing))} other open PRs (not drafts) pass CI: they are listed with '
           f'the PRs that <a href="#ready">seem ready</a>.</p>' if passing else "")
        + (f'<p class="meta">Failures that no rule recognises get a diagnosis from an LLM (Claude), shown as '
           f'"LLM diagnosis (unchecked)"; {e((data.get("llm") or {}).get("calls", 0))} new LLM calls in this '
           f'update.</p>' if (data.get("llm") or {}).get("enabled") else "")
        + f'<p class="meta">Updated {e(gen[:16].replace("T", " "))} UTC (<a href="feed.xml">feed</a>). Up to '
        f'{e(data.get("max_failures_per_pr"))} different failures are analysed per PR. In this update: '
        f'{e(run.get("analysed", 0))} PRs analysed, {e(run.get("unchanged", 0))} unchanged since the last one.'
        f'</p>',
        '<div class="stats">' + "".join(f'<div class="stat"><b>{e(v)}</b>{e(k)}</div>' for k, v in stats) + "</div>",
        '<nav class="filters" aria-label="Filters"><button data-f="all">All</button>'
        + "".join(f'<button data-f="{c}">{e(label)} ({count[c]})</button>' for c, label in CATEGORIES if count[c])
        + '<input id="q" type="search" placeholder="Search number, title, author" aria-label="Search">'
        '<button id="expand" type="button">Expand / collapse</button><span id="shown" class="meta"></span></nav>',
        ready_section(data),
        '<section id="prs">',
    ]
    parts.extend(render_pr(p) for p in prs)
    if not prs:
        parts.append('<p class="note">No open PR with a failing opam-ci status.</p>')
    parts.append("</section>")
    parts.append(chronic([p for p in prs if not p.get("pending")]))
    parts.append('''<section id="help"><h2>How to read this page</h2>
<dl class="help">
<dt>Seems ready</dt><dd>No failure points at the PR: each one also happens without it (pre-existing, or
failing in other open PRs too), comes from the CI, or is on an experimental platform. Together with the PRs
whose CI passes, they are listed under "PRs that seem ready". Only CI is checked: not the reviews, nor whether
a maintainer asked for changes.</dd>
<dt>Labels</dt><dd>The PR's labels on GitHub, set by the opam-repository maintainers. "consider for merge"
has a filter. A PR labelled "do not merge" is never listed as ready, nor one labelled "needs reporter action",
unless its author pushed or commented after the label was added: the label is then shown as "author active
since", since maintainers often leave it in place.</dd>
<dt>Copy as a PR comment</dt><dd>Copies a summary of the analysis, as Markdown, to paste in a comment on
the PR. Check it before posting: low-confidence LLM answers are left out, other suggestions are not
checked.</dd>
<dt>Has a fix for the PR</dt><dd>A change to the PR's opam files is suggested (a fix, or a restriction of the
platforms it is built or tested on).</dd>
<dt>Blocked by other packages</dt><dd>At least one failure comes from another package, which needs the fix.</dd>
<dt>CI problems only</dt><dd>Nothing points at the PR or at another package: network errors, cancelled jobs,
or problems of the CI machines.</dd>
<dt>Needs a look</dt><dd>Failures in the PR's packages without a suggested change.</dd>
<dt>Reverse dependency</dt><dd>A package that depends on a package of the PR. CI rebuilds them to check that
the PR does not break them.</dd>
<dt>day10 builder</dt><dd>opam-repo-ci builds packages with two systems: the classic builder, and the newer
day10 builder (the jobs named <code>day10,…</code>). When a job fails only on day10 while the classic
builder passes on the same system, the problem is in the CI setup, not in the package.</dd>
<dt>Pre-existing</dt><dd>The failure would happen without the PR: it also failed in the CI of the previous
release (the same job, with the same reason), the PR only adds an upper bound to that package, or opam-health-check (which builds every package on
master, without tests) sees it failing too. "Also failing in other open PRs" says the same from the other PRs
on this page.</dd>
<dt>LLM diagnosis (unchecked)</dt><dd>For failures that no rule recognises, an LLM reads the log and
suggests a cause. These answers are not checked: in a test on 25 of them, 15 were right, 7 plausible and 3
wrong. Changes to version bounds or depexts are shown as text to check, and low-confidence answers are
folded.</dd>
<dt>Experimental platform</dt><dd>FreeBSD, macOS and some other jobs are experimental: their failures do not
block a PR.</dd>
</dl>
<p class="meta">To post a suggestion as a line comment on a PR, review it first, then run
<code>python3 opam_repo_ci_triage.py review --pr NUMBER --post</code>
(from <a href="https://github.com/Sudha247/hackathon-opam-repo-triage" rel="noopener noreferrer">this
repository</a>).</p></section>''')
    parts.append(f"<script>{JS}</script></main></body></html>")
    return "\n".join(parts) + "\n"


def render_feed(data):
    """An Atom feed with one entry per analysed PR. An entry's id changes with
    the PR's head commit, so a feed reader shows each new analysis."""
    def x(v):
        return html.escape("" if v is None else str(v), quote=True)
    gen = data.get("generated") or ""
    out = ['<?xml version="1.0" encoding="utf-8"?>',
           '<feed xmlns="http://www.w3.org/2005/Atom">',
           "<title>opam-repository CI triage</title>",
           f"<id>urn:opam-repo-ci-triage:{x(data.get('repository'))}</id>",
           f"<updated>{x(gen)}</updated>",
           '<link rel="alternate" href="index.html"/>',
           "<author><name>opam-repo-ci triage</name></author>"]
    for pr in data.get("prs", []):
        if pr.get("pending") or pr.get("result") is None:
            continue
        n = int(pr["number"])
        _, groups = categorise(pr)
        cats = pr_cats(pr)
        lines = [", ".join(label for c, label in CATEGORIES if c in cats)]
        if pr.get("ci_state") == "PENDING":
            lines.append(f"CI still running ({pr.get('ci_unfinished') or '?'} jobs left).")
        since = pr.get("since_last_push")
        if since and (since.get("fixed") or since.get("new")):
            lines.append(f"Since the previous push: {len(since.get('fixed') or [])} gone, "
                         f"{len(since.get('new') or [])} new, {since.get('still', 0)} unchanged.")
        for k, label, _ in CLASSES:
            for r in groups.get(k, [])[:5]:
                title = r.get("title") or RULE_TITLES.get(r.get("rule"), r.get("rule")) or "Note"
                lines.append(f"{title}: {r.get('failing') or r.get('job') or ''}"
                             + (f" ({(r.get('new') or '').split('   ')[0]})" if r.get("new") else ""))
        updated = pr.get("analysed_at") or gen
        out += ["<entry>",
                f"<title>#{n} {x(pr.get('title'))}</title>",
                f"<id>{x(REPO_URL)}/pull/{n}#{x(pr.get('sha'))}</id>",
                f'<link rel="alternate" href="{x(REPO_URL)}/pull/{n}"/>',
                f'<link rel="related" href="index.html#pr-{n}"/>',
                f"<updated>{x(updated)}</updated>",
                f"<summary>{x(chr(10).join(lines))}</summary>",
                "</entry>"]
    out.append("</feed>")
    return "\n".join(out) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--data", default=os.path.join(ROOT, "site", "data.json"))
    ap.add_argument("--out", default=os.path.join(ROOT, "site", "index.html"))
    args = ap.parse_args()
    with open(args.data) as f:
        data = json.load(f)
    page = render(data)
    os.makedirs(os.path.dirname(os.path.abspath(args.out)), exist_ok=True)
    with open(args.out, "w") as f:
        f.write(page)
    with open(os.path.join(os.path.dirname(os.path.abspath(args.out)), "feed.xml"), "w") as f:
        f.write(render_feed(data))
    print(f"wrote {args.out} ({len(page.encode())} bytes, {len(data.get('prs', []))} PRs)")


if __name__ == "__main__":
    main()
