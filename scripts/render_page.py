#!/usr/bin/env python3
"""Render site/data.json (from triage_open_prs.py) into a single static page.

  python3 scripts/render_page.py [--data site/data.json] [--out site/index.html]

The page has no external resources (inline CSS and a little JS for the
filters). Everything that comes from GitHub, the CI logs or the suggestions is
untrusted and HTML-escaped; links are kept only when they are https URLs.

Each failing PR gets one verdict (see `verdict`), shown as a coloured word and
border, and a headline saying what the problem is (see `headline`).
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
from opam_repo_ci_triage import (  # noqa: E402
    BASE, RULE_TITLES, compared_text, describe_job, fix_label, render_comment, where_text)
from opam_repo_ci_triage import title_of as rule_title  # noqa: E402
REPO_URL = "https://github.com/ocaml/opam-repository"
# One verdict per PR: the first that applies, in this order (see `verdict`).
VERDICTS = [("ready", "Not this PR's fault"), ("change", "Change this PR"), ("blocked", "Blocked elsewhere"),
            ("ci", "CI only"), ("other", "Needs a look"), ("error", "Not analysed")]
VERDICT_LABEL = dict(VERDICTS)
VERDICT_HELP = {
    "ready": "CI fails, but none of the failures is this PR's fault: each one also fails without this PR, comes "
             "from the CI, or is on an experimental platform",
    "change": "A change to this PR's opam files is suggested: a fix, or a workaround that skips platforms or "
              "tests",
    "blocked": "At least one failure must be fixed in another package",
    "ci": "Nothing points at this PR or at another package: problems of the CI, network errors, cancelled jobs",
    "other": "Failures in this PR's packages, without a suggested change",
    "error": "The analysis is pending or failed"}
CHANGE_CLASSES = ("fix", "workaround")
# Labels of ocaml/opam-repository: "consider for merge" is a filter, and a PR
# with one of NOT_READY_LABELS is never listed as ready, unless the label may
# be stale (the author acted after it was added, see triage_open_prs.py).
MERGE_LABEL, NO_MERGE_LABEL = "consider for merge", "do not merge"
NOT_READY_LABELS = {NO_MERGE_LABEL, "needs reporter action"}
# Rules whose failures come from the CI, not from the PR.
CI_SIDE = {"builder-specific", "builder-specific-systemic", "accept-failures-ignored", "rerun",
           "opam-same-version-reinstall", "health-check-preexisting", "lint-ci-error",
           "bounded-out"}
# Job categories (from `summary`), in words.
JOB_CATEGORIES = {"build": "build failed", "build-or-test": "build or tests failed",
                  "dependency": "a dependency failed", "solver": "no solution", "network": "network error",
                  "cancelled": "cancelled", "sources": "sources not unpacked", "lint": "lint",
                  "not-available": "skipped (not installable there)", "ignored": "ignored (accepted failure)"}
NOISE = {"passed", "network", "cancelled", "not-available", "ignored"}
MONTHS = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split()


def e(s):
    return html.escape("" if s is None else str(s), quote=True)


def link(url, text, cls=None):
    """An <a> only for https URLs; plain escaped text otherwise."""
    c = f' class="{cls}"' if cls else ""
    if isinstance(url, str) and url.startswith("https://") and not re.search(r"[\s\"'<>`]", url):
        return f'<a href="{e(url)}"{c} rel="noopener noreferrer">{e(text)}</a>'
    return f"<span{c}>{e(text)}</span>"


def day(iso, year=None):
    """'2026-11-20T…' -> '20 Nov', with the year when it is not `year`."""
    m = re.match(r"(\d{4})-(\d\d)-(\d\d)", iso or "")
    if not m or not 1 <= int(m.group(2)) <= 12:
        return iso or ""
    out = f"{int(m.group(3))} {MONTHS[int(m.group(2)) - 1]}"
    return out if m.group(1) == year else f"{out} {m.group(1)}"


def shorten(s, n):
    s = " ".join(str(s or "").split())
    return s if len(s) <= n else s[:n - 1].rstrip() + "…"


def plural(n):
    return "" if n == 1 else "s"


def other_package(fix_in):
    fix_in = fix_in or ""
    return fix_in.endswith("(other package)") or fix_in.startswith("the dependency")


def groups(pr):
    """The suggestions of an analysed PR, by class."""
    g = collections.defaultdict(list)
    if not pr.get("pending"):
        for r in (pr.get("result") or {}).get("suggestions", []):
            g[r.get("class") or "explanation"].append(r)
    return g


def ci_running(pr):
    """Whether the PR's CI is still running: its opam-ci status is pending and
    the analysis did not find the CI page finished. The status stays pending
    for good when a job never starts (lint after a failed analysis)."""
    return pr.get("ci_state") == "PENDING" and pr.get("ci_unfinished") != 0


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
    if pr.get("pending") or res is None or ci_running(pr) or pr.get("draft"):
        return False
    if blocking_labels(pr) or pr.get("ci_unfinished") or res.get("ci_unfinished"):
        return False
    if res.get("unanalysed"):          # failures beyond `--max`: nothing says they are not the PR's
        return False
    sugg = res.get("suggestions", [])
    return bool(sugg) and all(not_this_pr(r) for r in sugg)


def verdict(pr):
    """The one verdict of a failing PR (a key of VERDICTS), the first that
    applies: not this PR's fault (`seems_ready`), a change to this PR is
    suggested, another package must be fixed, nothing points at this PR (CI
    only), or failures of this PR without a suggestion (needs a look)."""
    if pr.get("pending") or pr.get("result") is None:
        return "error"
    if seems_ready(pr):
        return "ready"
    sugg = pr["result"].get("suggestions", [])
    if any(r.get("class") in CHANGE_CLASSES for r in sugg):
        return "change"
    if any(other_package(r.get("fix_in")) for r in sugg):
        return "blocked"
    if all(r.get("fix_in") != "this PR" for r in sugg):
        return "ci"
    return "other"


def blocking_labels(pr):
    """The labels of NOT_READY_LABELS that still apply."""
    return NOT_READY_LABELS & set(pr.get("labels") or []) - set(pr.get("stale_labels") or [])


def is_merge(pr):
    return MERGE_LABEL in (pr.get("labels") or [])


def label_badges(labels, stale=()):
    """The PR's labels as small tags, "consider for merge" first. A label the
    author may have answered since it was added says so."""
    def badge(lb):
        if lb in stale:
            return (f' <span class="lbl stale" title="The author pushed or commented after this label was added: '
                    f'it may be out of date">{e(lb)} (author replied since)</span>')
        cls = {MERGE_LABEL: " merge", NO_MERGE_LABEL: " nomerge"}.get(lb, "")
        return f' <span class="lbl{cls}" title="GitHub label">{e(lb)}</span>'
    return "".join(badge(lb) for lb in sorted(labels, key=lambda lb: lb != MERGE_LABEL))


# --- Near-duplicate suggestions -------------------------------------------

CONF_RANK = {"high": 0, "medium": 1, "low": 2}


def conf_word(r):
    return str(r.get("confidence") or "").split(" ")[0]


def merge_key(r):
    """Suggestions with the same key say the same thing about different
    packages or jobs. LLM answers and ranked candidates are never merged."""
    if r.get("rule") == "llm" or r.get("candidates"):
        return None
    # Not the title: it follows from the rule (a merged pattern's title also counts its packages).
    return (r.get("class"), r.get("rule"), r.get("fix_in"), r.get("new"))


def merge_similar(rs):
    """`rs` with near-duplicates merged, in the order of their first one. A
    merged suggestion lists its packages (`packages`), the union of the jobs,
    the distinct errors and notes, and how many it merges (`merged`)."""
    order, by = [], {}
    for r in rs:
        k = merge_key(r)
        if k is None:
            order.append([r])
        elif k in by:
            by[k].append(r)
        else:
            by[k] = [r]
            order.append(by[k])
    return [combine(g) for g in order]


def combine(rs):
    if len(rs) == 1:
        return rs[0]

    def uniq(xs):
        return list(dict.fromkeys(x for x in xs if x))
    m = dict(rs[0])
    m["merged"] = len(rs)
    m["packages"] = uniq(r.get("failing") for r in rs)
    m["failing"] = ", ".join(m["packages"]) or None
    m["jobs"] = uniq(j for r in rs for j in (r.get("jobs") or [r.get("variant")]))
    m["variant"] = m["url"] = None        # the jobs are listed instead of the first one's
    m["errors"] = uniq(r.get("error") for r in rs)
    m["notes"] = uniq(r.get("note") for r in rs)
    m["error"] = m["note"] = None
    m["also_failing_in"] = sorted(set(n for r in rs for n in r.get("also_failing_in") or [])) or None
    m["experimental"] = all(r.get("experimental") for r in rs)
    m["preexisting"] = rs[0].get("preexisting") if all(r.get("preexisting") for r in rs) else None
    m["confidence"] = max((r.get("confidence") for r in rs), key=lambda c: CONF_RANK.get(str(c or "").split(" ")[0], 1))
    return m


def short_list(names, n=3):
    """Package names, versions of one package counted together."""
    by = {}
    for x in names:
        by.setdefault(x.split(".", 1)[0] if " " not in x else x, []).append(x)
    items = [vs[0] if len(vs) == 1 else f"{k} ({len(vs)} versions)" for k, vs in by.items()]
    return ", ".join(items[:n]) + (f" and {len(items) - n} more" if len(items) > n else "")


def packages_text(r, n=2):
    """The packages of a suggestion, short: a merged one lists them, others
    may have a comma-separated `failing`, cut with "..."."""
    names, cut = [], False
    for x in r.get("packages") or [r.get("failing")]:
        for y in str(x or "").split(", "):
            y = y.strip()
            if y.endswith(("...", "…")):
                cut, y = True, y.rstrip(".… ")
            if y and y not in names:
                names.append(y)
    if not names:
        return ""
    s = short_list(names, n)
    return s + " and more" if cut and " more" not in s else s


def blocked_by(sugg):
    """The other packages where a fix goes, in order. A fix_in that is not a
    package name ("the dependency declaring these depexts") is left out:
    the headline then names the finding instead."""
    out = []
    for r in sugg:
        f = r.get("fix_in") or ""
        if f.endswith(" (other package)"):
            name = f[:-len(" (other package)")]
            if name not in out:
                out.append(name)
    return out


def title_of(r):
    """The title of a suggestion, from its rule (stored titles may be older wordings)."""
    return rule_title(r)


# A suggested change written as an opam line: a quoted package, a field, a variable.
OPAM_LINE = re.compile(r'\s*("[\w+.@-]+"|[a-z][\w-]*:|extra-source\b|%\{)')


def headline(pr, v=None, as_html=True):
    """What the problem of a PR is, in a few words: the first change to this
    PR (package and new constraint), else the other packages to fix, else the
    first finding."""
    v = v or verdict(pr)
    t = e if as_html else str

    def code(s):
        return f"<code>{e(s)}</code>" if as_html else f"`{s}`"
    if v == "error":
        return t("not analysed yet; the next page update will cover it" if pr.get("pending")
                 else "the analysis failed")
    sugg = pr["result"].get("suggestions", [])
    if not sugg:
        return t("no build or test failure to analyse")
    g = groups(pr)
    parts = []
    if v == "change":
        items = merge_similar(g["fix"]) + merge_similar(g["workaround"])
        r = items[0]
        pkgs = packages_text(r)
        if r.get("new") and OPAM_LINE.match(r["new"]):
            s = code(shorten(r["new"].split("   ")[0], 48)) + (t(f" for {pkgs}") if pkgs else "")
        else:
            s = t(title_of(r) + (f" ({pkgs})" if pkgs else ""))
        if r.get("class") == "workaround":
            s += t(" (workaround)")
        if len(items) > 1:
            s += t(f" and {len(items) - 1} more change{'s' if len(items) > 2 else ''}")
        parts.append(s)
        others = blocked_by(sugg)
        if others:
            parts.append(t(f"also blocked by {short_list(others, 2)}"))
    elif v == "blocked" and blocked_by(sugg):
        others = blocked_by(sugg)
        parts.append(t(f"needs a fix in {short_list(others)}" if len(others) == 1
                       else f"needs fixes in {short_list(others)}"))
    elif v == "blocked":
        # The fix goes in a dependency the analysis cannot name (depexts declared
        # by a dependency, say): the first such finding.
        r = merge_similar([r for r in sugg if other_package(r.get("fix_in"))] or sugg)[0]
        pkgs = packages_text(r)
        parts.append(t(title_of(r) + (f" ({pkgs})" if pkgs else "")))
    else:
        pool = [r for r in sugg if r.get("fix_in") == "this PR"] if v == "other" else sugg
        pool = [r for r in pool if r.get("rule") != "llm"] or pool or sugg
        r = merge_similar(pool)[0]
        pkgs = packages_text(r)
        parts.append(t(title_of(r) + (f" ({pkgs})" if pkgs else "")))
    return " · ".join(parts)


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
        lines[i + 1:i + 1] = ["", "**None of the failures is this PR's fault**: each one also fails without this "
                              "PR, comes from the CI, or is on an experimental platform (details below)."]
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
    return (f'<button type="button" class="copy" data-for="md-{n}">Copy as a PR comment</button>'
            f'<details class="mdv"><summary>Show the Markdown</summary>'
            f'<textarea id="md-{n}" readonly rows="14" spellcheck="false" aria-label="PR comment, Markdown">'
            f'{e(md)}</textarea></details>')


def failed_count(pr):
    failed = ((pr.get("jobs") or {}).get("failed") or {})
    return sum(v for k, v in failed.items() if k not in NOISE), sum(failed.values())


SAFE_VARIANT = re.compile(r"[A-Za-z0-9._+~,-]{1,200}")
SAFE_SHA = re.compile(r"[0-9a-f]{7,40}")


def job_link(sha, variant):
    """A link to a job's log. Inside a PR, whose element carries the base URL
    of its commit's jobs (data-ci), only the variant is written and the
    script builds the href: a full URL per job made up most of the page."""
    if sha and SAFE_SHA.fullmatch(sha) and SAFE_VARIANT.fullmatch(variant or ""):
        return f'<a data-job="{e(variant)}">{e(describe_job(variant))}</a>'
    return e(describe_job(variant))


def where_of(r):
    """Where the fix goes, unless it is this PR (implied, or not meaningful
    for a failure without a change), in words (from `fix_in`, as stored
    `where` values may be older wordings)."""
    w = where_text(r)
    return None if w in (None, "", "this PR") else w


def conf_note(r):
    """The confidence, only when it is not high."""
    c = conf_word(r)
    return f"{c} confidence" if c and c != "high" else None


def suggestion_body(r, sha=None, full=True, year=None):
    """The details of a suggestion: job, error, change, candidates, notes and jobs."""
    rule = r.get("rule")
    out = []
    jobs = r.get("jobs") or []
    lists_jobs = rule in ("rerun", "unrecognised")
    meta = []
    if r.get("variant") and r.get("url") and not lists_jobs:
        meta.append("Job: " + link(r.get("url"), r.get("job") or describe_job(r["variant"])))
    if full and where_of(r):
        label = fix_label(where_of(r))
        meta.append(e(label[:1].upper() + label[1:]))
    if meta:
        out.append(f'<div class="meta">{" · ".join(meta)}</div>')
    errors = r.get("errors") or ([r["error"]] if r.get("error") else [])
    for err in errors[:2]:
        out.append(f'<pre class="err">{e(err)}</pre>')
    if len(errors) > 2:
        out.append(f'<div class="meta">and {len(errors) - 2} other error message{plural(len(errors) - 2)} '
                   f'(see the jobs)</div>')
    if r.get("new"):
        diff = []
        if r.get("old"):
            diff.append(f'<span class="del">- {e(r["old"])}</span>')
        diff.append(f'<span class="add">+ {e(r["new"])}</span>')
        if r.get("alternative"):
            diff.append(f'<span class="alt">or {e(r["alternative"])}</span>')
        out.append('<pre class="diff">' + "\n".join(diff) + "</pre>")
    elif r.get("alternative"):          # a change described in the note, and an alternative
        out.append(f'<pre class="diff"><span class="alt">or {e(r["alternative"])}</span></pre>')
    if r.get("candidates"):
        m = re.match(r"release date of (\S+) \((\d{4}-\d\d-\d\d)\)$", r.get("compared_with") or "")
        if m:
            what = (f"Dependencies newer than {m.group(1)}'s release ({day(m.group(2), year)}) that may have "
                    f"changed the output:")
        else:
            what = f"Dependencies that may have caused the failure, compared with {compared_text(r)}:"
        out.append(f'<div class="meta">{e(what)}</div>')
        for c in r["candidates"]:
            if "first_after" in c:
                what = f'CI used {c.get("failing")}; first release after it: {c.get("first_after")}'
            else:
                what = f'passes with {c.get("passing")}, fails with {c.get("failing")}'
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
    notes = r.get("notes") or ([r["note"]] if r.get("note") else [])
    for note in notes[:1]:
        out.append(f'<p class="note">{e(note)}</p>')
    if len(notes) > 1:
        out.append(f'<details class="jobs"><summary>{len(notes) - 1} similar note{plural(len(notes) - 1)} for the '
                   f'other packages</summary>' + "".join(f'<p class="note">{e(x)}</p>' for x in notes[1:])
                   + "</details>")
    if r.get("also_failing_in"):
        prs = r["also_failing_in"]
        out.append(f'<p class="note shared">Also fails in {len(prs)} other open PR{"s" if len(prs) != 1 else ""}, '
                   f'so probably not this PR\'s fault: '
                   + ", ".join(link(f"{REPO_URL}/pull/{int(n)}", f"#{int(n)}") for n in prs[:12])
                   + (f" and {len(prs) - 12} more" if len(prs) > 12 else "") + "</p>")
    if lists_jobs and jobs:
        out.append(f'<ul class="jobs">{"".join(f"<li>{job_link(sha, v)}</li>" for v in jobs)}</ul>')
    else:
        others = [v for v in jobs if v != r.get("variant")]
        if others:
            # A matrix suggestion's variant is a label, not one of its jobs: then list them all.
            what = (f'Same failure on {len(others)} other job{"s" if len(others) != 1 else ""}'
                    if len(others) < len(jobs) else f'All {len(jobs)} jobs' if len(jobs) > 1 else '1 job')
            out.append(f'<details class="jobs"><summary>{what}</summary>'
                       f'<ul>{"".join(f"<li>{job_link(sha, v)}</li>" for v in others)}</ul></details>')
    return "\n".join(out)


def low_llm(r):
    return r.get("rule") == "llm" and conf_word(r) == "low"


def render_suggestion(r, sha=None, year=None):
    """A suggested change, in full: title, package, tags, then the details."""
    head = [f'<b>{e(title_of(r))}</b>']
    if r.get("failing"):
        head.append(f'<span class="pkg">{e(r["failing"])}</span>')
    tags = []
    if r.get("class") == "workaround":
        tags.append('<span title="Skips platforms or tests">workaround</span>')
    if r.get("experimental"):
        tags.append('<span title="Failures on experimental platforms do not block the PR">experimental platform</span>')
    if conf_note(r):
        tags.append(f'<span title="{e(r["confidence"])}">{e(conf_note(r))}</span>')
    if tags:
        head.append(f'<span class="x">{" · ".join(tags)}</span>')
    html_ = (f'<div class="sugg c-{e(r.get("class") or "explanation")}"><div class="sh">{" · ".join(head)}</div>'
             + suggestion_body(r, sha, year=year) + "</div>")
    if low_llm(r):
        # Low-confidence LLM answers were never right in the evaluation: folded.
        html_ = (f'<details class="hint"><summary>LLM diagnosis (low confidence)'
                 f'{": " + e(r["failing"]) if r.get("failing") else ""}</summary>{html_}</details>')
    return html_


def render_item(r, sha=None, year=None):
    """A failure as one folded line: title · packages · failures, jobs, where
    to fix, confidence; the details inside."""
    title = "LLM diagnosis (low confidence)" if low_llm(r) else title_of(r)
    head = [f'<b>{e(title)}</b>']
    pkgs = packages_text(r)
    if pkgs:
        head.append(f'<span class="pkg">{e(pkgs)}</span>')
    x = []
    jobs = len(r.get("jobs") or [])
    if r.get("merged"):
        x.append(f'{r["merged"]} failures, {jobs} job{"s" if jobs != 1 else ""}')
    elif jobs > 1:
        x.append(f"{jobs} jobs")
    if where_of(r) and r.get("rule") != "rerun":          # "Jobs to rerun" says it
        x.append(fix_label(where_of(r)))
    if r.get("preexisting") and not r.get("preexisting_unverified"):
        x.append("pre-existing")
    if r.get("also_failing_in"):
        k = len(r["also_failing_in"])
        x.append(f"also in {k} other PR{'s' if k != 1 else ''}")
    if r.get("experimental"):
        x.append("experimental platform")
    if conf_note(r):
        x.append(conf_note(r))
    if x:
        head.append(f'<span class="x">{e(" · ".join(x))}</span>')
    return (f'<details class="it"><summary>{" · ".join(head)}</summary>'
            f'<div>{suggestion_body(r, sha, full=False, year=year)}</div></details>')


def ready_section(data, shown=5):
    """The PRs that seem ready: CI passes, or no failure points at the PR.
    Drafts and PRs with one of NOT_READY_LABELS are left out."""
    rows = []
    for p in data.get("passing") or []:
        if not p.get("draft") and not blocking_labels(p):
            rows.append((p, True))
    for p in data.get("prs", []):
        if seems_ready(p):
            rows.append((p, False))
    # Those labelled "consider for merge" first, then the most recently updated.
    rows.sort(key=lambda pw: pw[0].get("updated") or "", reverse=True)
    rows.sort(key=lambda pw: not is_merge(pw[0]))
    if not rows:
        return ""
    year = (data.get("generated") or "")[:4]
    items = []
    for p, passes in rows:
        n = int(p["number"])
        ci = "CI passes" if passes else f'fails, <a href="#pr-{n}">not this PR\'s fault</a>'
        items.append(f'<li>{link(f"{REPO_URL}/pull/{n}", f"#{n}", "num")}'
                     f'<span class="t">{e(p.get("title"))}'
                     f'{label_badges(p.get("labels") or [], p.get("stale_labels") or [])}</span>'
                     f'<span class="r">{e(p.get("author") or "?")} · {ci} · {e(day(p.get("updated"), year))}</span></li>')
    out = (f'<section id="ready"><h2>Ready as far as CI goes <span class="sub">({len(items)})</span></h2>'
           '<p class="sub">CI passes, or none of the failures is the PR\'s fault. Reviews and discussions are not '
           'checked '
           '(<a href="#h-ready">more</a>).</p><ul class="ready">' + "".join(items[:shown]) + "</ul>")
    if len(items) > shown:
        out += (f'<details class="more"><summary>{len(items) - shown} more</summary>'
                f'<ul class="ready">{"".join(items[shown:])}</ul></details>')
    return out + "</section>"


def render_pr(pr, year=None, max_failures=None):
    v = verdict(pr)
    g = groups(pr)
    n = int(pr["number"])
    real, _ = failed_count(pr)
    failed = sorted(((pr.get("jobs") or {}).get("failed") or {}).items(), key=lambda kv: -kv[1])
    title = pr.get("title") or ""
    pkgs = " ".join(dict.fromkeys(r.get("failing") or "" for rs in g.values() for r in rs))
    search = " ".join(f"{n} {title} {pr.get('author') or ''} {pkgs}".lower().split())
    line2 = [f'<b class="v" title="{e(VERDICT_HELP[v])}">{e(VERDICT_LABEL[v])}</b>', headline(pr, v)]
    if ci_running(pr):
        left = pr.get("ci_unfinished")
        line2.append(f'<span class="run" title="These are the failures so far; the page is updated when CI '
                     f'finishes">CI running{f", {e(left)} job{plural(left)} left" if left else ""}</span>')
    jobs_title = ", ".join(f"{k} {JOB_CATEGORIES.get(c, c)}" for c, k in failed if c not in NOISE)
    skipped = dict(failed).get("not-available", 0)
    if real == 0 and skipped:
        line2.append(f'<span title="Not installable on these platforms: the jobs did not run">{skipped} skipped '
                     f'job{"s" if skipped != 1 else ""}</span>')
    else:
        line2.append(f'<span title="Failed build and test jobs ({e(jobs_title or "none")}); network errors, '
                     f'cancelled and skipped jobs are not counted">{real} failed job{"s" if real != 1 else ""}'
                     f'</span>')
    head = (f'<summary>{link(f"{REPO_URL}/pull/{n}", f"#{n}", "num")}'
            f'<span class="ttl">{e(title)}{" <i>(draft)</i>" if pr.get("draft") else ""}'
            f'{label_badges(pr.get("labels") or [], pr.get("stale_labels") or [])}</span>'
            f'<span class="who">{e(pr.get("author") or "?")} · {e(day(pr.get("updated"), year))}</span>'
            f'<span class="l2">{" · ".join(line2)} {link(pr.get("ci_url"), "CI ↗", "ci")}</span></summary>')
    sha = pr.get("sha")
    bar = [link(pr.get("ci_url"), "CI results")]
    if not pr.get("pending") and pr.get("result") is not None:
        bar.append(copy_block(pr))
    bar.append(f'<span>commit {e((sha or "")[:12])} · analysed '
               f'{e((pr.get("analysed_at") or "never")[:16].replace("T", " "))} UTC</span>')
    if pr.get("last_error") and pr.get("result") is not None:
        # A newer analysis of the same commit failed: this result is the previous one.
        bar.append(f'<span title="{e(pr["last_error"])}">a newer analysis failed '
                   f'{e((pr.get("last_error_at") or "")[:16].replace("T", " "))} UTC; '
                   f'this result may be out of date</span>')
    body = ['<div class="body">', f'<div class="bar">{"".join(bar)}</div>']
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
        body.append('<p class="note">Not analysed yet; the next page update will cover it.</p>')
    elif pr.get("result") is None:
        body.append(f'<p class="note">The analysis failed ({e(pr.get("attempts"))} attempts): '
                    f'<code>{e(pr.get("error"))}</code></p>')
    else:
        left = pr["result"].get("unanalysed")
        if left:
            per = f" at most {e(max_failures)}" if max_failures else " a few"
            body.append(f'<p class="note">{e(left)} more failure{"s were" if left != 1 else " was"} not '
                        f'analysed: each page update analyses{per} per PR. See the CI results.</p>')
        if not pr["result"].get("suggestions"):
            body.append('<p class="note">No build or test failure to analyse.</p>')
        change = merge_similar(g["fix"]) + merge_similar(g["workaround"])
        if change:
            body.append(f'<h4 class="g-change">Change this PR ({len(change)})</h4>')
            body.extend(render_suggestion(r, sha, year) for r in change)
        elsewhere = merge_similar(g["elsewhere"])
        if elsewhere:
            body.append(f'<h4 class="g-else">Fix goes elsewhere ({len(elsewhere)})</h4>')
            body.extend(render_item(r, sha, year) for r in elsewhere)
        other = merge_similar(g["explanation"])
        if other:
            kinds = ", ".join(dict.fromkeys(title_of(r) for r in other))
            body.append(f'<details class="others"{" open" if v == "other" else ""}><summary>{len(other)} other '
                        f'failure{"s" if len(other) != 1 else ""}, no change suggested <span class="x">'
                        f'({e(shorten(kinds, 90))})</span></summary>'
                        + "".join(render_item(r, sha, year) for r in other) + "</details>")
    body.append("</div>")
    sha = pr.get("sha") or ""
    ci = f' data-ci="{e(BASE + sha)}/variant/"' if SAFE_SHA.fullmatch(sha) else ""
    return (f'<details class="pr v-{v}" id="pr-{n}" data-v="{v}"{" data-merge" if is_merge(pr) else ""}'
            f' data-search="{e(search)}"{ci}>' + head + "\n".join(body) + "</details>")


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
            def category(j):
                return JOB_CATEGORIES.get(j.get("category"), j.get("category") or "job")
            cells = " ".join(f'<span class="nw">{link(f"{REPO_URL}/pull/{n}", f"#{n}")}'
                              f' ({link(jobs[0].get("url"), category(jobs[0]))})</span>'
                              for n, vs in sorted(d.items()) for jobs in [next(iter(vs.values()))])
            trs.append(f'<tr><td><b>{e(name)}</b><div class="meta">{e(", ".join(versions))}</div></td>'
                       f'<td>{len(d)}</td><td>{cells}</td></tr>')
        out.append(table(["Reverse dependency (failing versions)", "PRs", "One failing job per PR"], trs))
    else:
        out.append('<p class="note">None among the analysed PRs.</p>')
    out.append(f'<h3>Packages to fix that block {min_prs} or more PRs ({len(brows)})</h3>')
    if brows:
        out.append(table(["Package", "PRs", "Which PRs"], [
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
--add:#116329;--addbg:#dafbe1;--del:#82071e;--delbg:#ffebe9;--err:#b3261e;
--ok:#116329;--chg:#b45309;--blk:#6639ba;--ci:#5f6368;--look:#b3261e;--na:#80868b}
@media (prefers-color-scheme:dark){:root{--bg:#121416;--fg:#e3e3e3;--mut:#9aa0a6;--card:#1c1f23;--bd:#3c4043;
--code:#25292e;--acc:#8ab4f8;--add:#7ee2a8;--addbg:#12361f;--del:#ffa198;--delbg:#3d1416;--err:#f28b82;
--ok:#7ee2a8;--chg:#f2b56b;--blk:#d2a8ff;--ci:#9aa0a6;--look:#f28b82;--na:#6f757b}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.45 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}
main{max-width:1000px;margin:0 auto;padding:16px}
a{color:var(--acc)}
header{display:flex;flex-wrap:wrap;align-items:baseline;gap:2px 16px}
h1{font-size:1.35rem;margin:.2em 0;flex:1 1 auto}
header nav{font-size:.85rem;color:var(--mut)}
.lead{margin:.2em 0 1em;max-width:75ch;color:var(--mut);font-size:.92rem}
h2{font-size:1.1rem;margin:1.4em 0 .3em}h3{font-size:1rem;margin:1.2em 0 .4em}
h4{font-size:.92rem;margin:14px 0 4px}
.sub{font-weight:400;font-size:.85rem;color:var(--mut);margin:0 0 .5em}
.meta{color:var(--mut);font-size:.85rem;overflow-wrap:anywhere}
ul.ready{list-style:none;margin:0;padding:0;border:1px solid var(--bd);border-radius:8px;background:var(--card)}
ul.ready li{display:flex;flex-wrap:wrap;gap:2px 10px;padding:6px 12px;border-top:1px solid var(--bd);align-items:baseline}
ul.ready li:first-child{border-top:0}
ul.ready .t{flex:1 1 12em;min-width:0;overflow-wrap:anywhere}
ul.ready .r{font-size:.85rem;color:var(--mut);overflow-wrap:anywhere}
.num{font-weight:600;white-space:nowrap}
.filters{display:flex;flex-wrap:wrap;gap:6px 10px;margin:6px 0 8px;align-items:center;background:var(--bg);padding:6px 0}
@media (min-width:700px){.filters{position:sticky;top:0;z-index:1}details.pr{scroll-margin-top:96px}}
.seg{display:flex;flex-wrap:wrap;border:1px solid var(--bd);border-radius:8px;overflow:hidden;background:var(--card)}
.seg button{font:inherit;font-size:.85rem;padding:5px 10px;border:0;border-right:1px solid var(--bd);background:var(--card);
color:var(--fg);cursor:pointer;white-space:nowrap}
.seg button:last-child{border-right:0}
.seg button[aria-pressed=true]{background:var(--fg);color:var(--bg)}
.seg i{display:inline-block;width:8px;height:8px;border-radius:50%;margin-right:5px;background:var(--v)}
.seg b{font-weight:400;opacity:.75;margin-left:3px}
.filters label{font-size:.85rem;color:var(--mut);white-space:nowrap}
.filters input[type=search]{font:inherit;font-size:.9rem;padding:5px 10px;border-radius:8px;border:1px solid var(--bd);
background:var(--card);color:var(--fg);flex:1 1 200px;min-width:0}
button:focus-visible,summary:focus-visible,input:focus-visible,a:focus-visible{outline:2px solid var(--acc);outline-offset:1px}
details.pr{background:var(--card);border:1px solid var(--bd);border-left:4px solid var(--v);border-radius:6px;margin:4px 0}
details.pr>summary{cursor:pointer;padding:6px 12px 6px 8px;list-style:none;display:grid;
grid-template-columns:1em auto 1fr auto;gap:0 8px;align-items:baseline}
details.pr>summary::-webkit-details-marker{display:none}
details.pr>summary::before{content:"\\25B8";color:var(--mut);font-size:.8rem}
details.pr[open]>summary::before{content:"\\25BE"}
.ttl{min-width:0;overflow-wrap:anywhere}
.who{font-size:.82rem;color:var(--mut);white-space:nowrap}
.l2{grid-column:3/5;font-size:.85rem;color:var(--mut);overflow-wrap:anywhere}
.v{font-weight:600;color:var(--v)}
.l2 code{font-size:.8rem;background:var(--code);padding:0 4px;border-radius:4px;color:var(--fg)}
.l2 .run{color:var(--acc)}
.l2 a.ci{white-space:nowrap;margin-left:4px}
.lbl{font-size:.75rem;color:var(--mut);border:1px solid var(--bd);border-radius:4px;padding:0 4px;white-space:nowrap}
.lbl.merge{color:var(--acc);border-color:var(--acc)}.lbl.nomerge{color:var(--err);border-color:var(--err)}
.lbl.stale{border-style:dashed}
.v-ready{--v:var(--ok)}.v-change{--v:var(--chg)}.v-blocked{--v:var(--blk)}.v-ci{--v:var(--ci)}
.v-other{--v:var(--look)}.v-error{--v:var(--na)}.v-all{--v:transparent}
details.pr.v-error{border-left-style:dashed}
@media (max-width:600px){details.pr>summary{grid-template-columns:1em auto 1fr}
.who{grid-column:3;grid-row:3}.l2{grid-column:3}}
.body{padding:4px 12px 12px;border-top:1px solid var(--bd)}
.bar{display:flex;flex-wrap:wrap;gap:6px 14px;align-items:center;font-size:.85rem;color:var(--mut);margin:8px 0}
.bar button{font:inherit;font-size:.85rem;padding:3px 10px;border-radius:6px;border:1px solid var(--bd);
background:var(--card);color:var(--fg);cursor:pointer}
details.mdv>summary{cursor:pointer;color:var(--acc)}
details.mdv[open]{order:9;flex-basis:100%}
details.mdv textarea{width:100%;margin-top:4px;font:.8rem/1.4 ui-monospace,SFMono-Regular,Menlo,monospace;
background:var(--code);color:var(--fg);border:1px solid var(--bd);border-radius:6px;padding:6px 8px}
h4.g-change{color:var(--chg)}h4.g-else{color:var(--blk)}
.sugg{border-left:3px solid var(--chg);padding:2px 0 2px 10px;margin:6px 0 12px}
.sh{overflow-wrap:anywhere}
.x{color:var(--mut);font-size:.85rem}
.pkg{overflow-wrap:anywhere}
details.it{margin:3px 0;font-size:.9rem;overflow-wrap:anywhere}
details.it>summary,details.others>summary,details.hint>summary{cursor:pointer}
details.it>div{padding:2px 0 6px 18px}
details.others{margin:10px 0 0;font-size:.9rem}
details.others>summary{color:var(--mut)}
details.others>details.it{margin-left:18px}
pre{background:var(--code);border-radius:6px;padding:6px 8px;margin:4px 0;overflow-x:auto;font-size:.8rem;
white-space:pre-wrap;overflow-wrap:anywhere}
pre.err{max-height:16em;overflow-y:auto}
.diff span{display:block}.add{color:var(--add);background:var(--addbg)}.del{color:var(--del);background:var(--delbg)}
.alt,.cm{color:var(--mut)}
.note{font-size:.88rem;margin:4px 0;overflow-wrap:anywhere}
details.hint>summary{color:var(--mut);font-size:.85rem;margin:8px 0}
ul.jobs{margin:4px 0;padding-left:20px;font-size:.85rem}
dl.help dt{font-weight:600;margin-top:.6em}dl.help dd{margin:0 0 0 1.2em;font-size:.9rem;max-width:75ch}
dl.help dt .v{margin-right:2px}
details.jobs{color:var(--mut);font-size:.85rem;overflow-wrap:anywhere}
details.jobs>summary{cursor:pointer}details.jobs ul{margin:2px 0 4px;padding-left:20px}
ul.ev{margin:2px 0 6px;padding-left:1.4em;font-size:.85rem}.yes{color:var(--add)}.no{color:var(--del)}.unk{color:var(--mut)}
.tw{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:.88rem}
th,td{border-bottom:1px solid var(--bd);padding:5px 8px;text-align:left;vertical-align:top}
td:last-child{overflow-wrap:anywhere}.nw{white-space:nowrap}
details.more>summary{cursor:pointer;color:var(--acc);margin:6px 0 6px 12px}
details.more>ul.ready{margin-top:4px}
.hidden{display:none}
"""

JS = """
(function(){
  // Job links carry only their variant: the href is the PR's data-ci + variant.
  [].forEach.call(document.querySelectorAll('a[data-job]'),function(a){
    var p=a.closest('[data-ci]'); if(!p)return;
    a.href=p.getAttribute('data-ci')+a.getAttribute('data-job');   // variants are checked: [A-Za-z0-9._+~,-]
    a.rel='noopener noreferrer';
  });
  var cur='all', q='', merge=false;
  var prs=[].slice.call(document.querySelectorAll('details.pr'));
  var btns=[].slice.call(document.querySelectorAll('.seg button[data-f]'));
  var mb=document.getElementById('merge'), shown=document.getElementById('shown');
  function apply(){
    var n=0;
    prs.forEach(function(d){
      var ok=(cur==='all'||d.getAttribute('data-v')===cur)&&(!merge||d.hasAttribute('data-merge'))&&
             (!q||d.getAttribute('data-search').indexOf(q)>=0);
      d.classList.toggle('hidden',!ok); if(ok)n++;
    });
    btns.forEach(function(b){b.setAttribute('aria-pressed',b.getAttribute('data-f')===cur?'true':'false');});
    if(shown)shown.textContent=(n===prs.length?'':n+' shown');
  }
  btns.forEach(function(b){b.addEventListener('click',function(){cur=b.getAttribute('data-f');apply();});});
  var qi=document.getElementById('q');
  if(qi)qi.addEventListener('input',function(ev){q=ev.target.value.toLowerCase().trim();apply();});
  if(mb)mb.addEventListener('change',function(){merge=mb.checked;apply();});
  apply();
  function openHash(){
    var d=location.hash&&document.getElementById(location.hash.slice(1));
    if(d&&d.tagName==='DETAILS'){d.classList.remove('hidden');d.open=true;}
  }
  window.addEventListener('hashchange',openHash); openHash();
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


def help_section(data):
    run = data.get("run") or {}
    llm = data.get("llm") or {}
    limit = f" The list was limited to {e(data['limit'])} PRs." if data.get("limit") else ""
    verdicts = "\n".join(f'<dt class="v-{k}"><span class="v">{e(label)}</span></dt><dd>{e(VERDICT_HELP[k])}.'
                         + {"ready": ' These PRs are listed under <b id="h-ready">"Ready as far as CI goes"</b>, '
                                     'with the PRs whose CI passes. Drafts and PRs labelled "do not merge" or "needs '
                                     'reporter action" are left out, unless the author pushed or commented after the '
                                     'label was added. Only CI is checked: not the reviews, nor whether a maintainer '
                                     'asked for changes.',
                            "change": ' Other failures may also need a fix elsewhere ("also blocked by").',
                            "blocked": " The fix goes in a dependency or a reverse dependency of the PR's packages.",
                            "error": " A PR not analysed yet is covered by the next page update."
                            }.get(k, "") + "</dd>" for k, label in VERDICTS)
    return f'''<section id="help"><h2>How to read this page</h2>
<p>Each failing PR gets one verdict, the first of these that applies. The row says what the problem is (the first
suggested change, the other packages to fix, or the first failure) and how many build and test jobs failed
(network errors, cancelled and skipped jobs are not counted). "CI running" means these are the failures so far.</p>
<dl class="help">
{verdicts}
</dl>
<dl class="help">
<dt>Change this PR, Fix goes elsewhere</dt><dd>In a PR's details, the suggested changes are shown in full;
the failures whose fix goes in another package, upstream or the CI are folded, one line each, with where the fix
goes. Similar failures of several packages are merged into one line. The confidence of a suggestion is shown
only when it is not high.</dd>
<dt>Labels</dt><dd>The PR's labels on GitHub, set by the opam-repository maintainers. "consider for merge"
has a filter. A label the author may have answered (they pushed or commented after it was added) says "author
replied since", since maintainers often leave it in place.</dd>
<dt>Copy as a PR comment</dt><dd>Copies a summary of the analysis, as Markdown, to paste in a comment on
the PR. Check it before posting: low-confidence LLM diagnoses are left out, other suggestions are not
checked.</dd>
<dt>Reverse dependency</dt><dd>A package that depends on a package of the PR. CI rebuilds them to check that
the PR does not break them.</dd>
<dt>day10 builder</dt><dd>opam-repo-ci builds packages with two builders: the main builder, and the newer
day10 builder (the jobs named <code>day10,…</code>). When a job fails only on day10 while the main
builder passes on the same system, the problem is in the CI setup, not in the package.</dd>
<dt>Pre-existing</dt><dd>The package fails without this PR too: it also failed in the CI of the previous
release (the same job, with the same reason), the PR only adds an upper bound to that package, or
opam-health-check (which builds every package on master, without tests) sees it failing too. "Also fails in
other open PRs" says the same from the other PRs on this page.</dd>
<dt>LLM diagnosis (unchecked)</dt><dd>For failures the tool cannot explain, an LLM (Claude) reads the log and
suggests a cause. These diagnoses are not checked: in a test on 25 of them, 15 were right, 7 plausible and 3
wrong. Changes to version bounds or depexts are shown as text to check, and low-confidence diagnoses are
folded, as "LLM diagnosis (low confidence)".</dd>
<dt>Experimental platform</dt><dd>FreeBSD, macOS and some other jobs are experimental: their failures do not
block a PR.</dd>
</dl>
<p class="meta">This update: {e(run.get("analysed", 0))} PRs analysed, {e(run.get("unchanged", 0))} unchanged since
the last one (a PR is analysed again when its head commit or its CI status changes). Up to
{e(data.get("max_failures_per_pr"))} different failures are analysed per PR.{limit}
{f"LLM diagnoses are enabled: {e(llm.get('calls', 0))} new LLM calls in this update." if llm.get("enabled")
 else "LLM diagnoses are not enabled."}</p>
<p class="meta">To post a suggestion as a line comment on a PR, review it first, then run
<code>python3 opam_repo_ci_triage.py review --pr NUMBER --post</code>
(from <a href="https://github.com/Sudha247/hackathon-opam-repo-triage" rel="noopener noreferrer">this
repository</a>).</p></section>'''


def render(data):
    prs = data.get("prs", [])
    verdicts = {int(p["number"]): verdict(p) for p in prs}
    count = collections.Counter(verdicts.values())
    gen = data.get("generated") or ""
    year = gen[:4]
    merge = sum(1 for p in prs if is_merge(p))
    failing, running = data.get("failing_prs"), data.get("running_prs")
    scope = (f'{e(len(prs))} of the {e(data.get("open_prs"))} open {link(REPO_URL, "ocaml/opam-repository")} PRs '
             f'have failing opam-repo-ci jobs'
             + (f' (for {e(running)} of them, CI is still running)' if running else "")
             + (f' (limited to {e(data["limit"])})' if data.get("limit") else "")
             + '. The failures are read from the CI logs and matched against known causes. Nothing is rebuilt, '
             'so check a suggestion before applying it.') if failing is not None else ""
    seg = ['<button type="button" class="v-all" data-f="all" aria-pressed="true">All<b>' + str(len(prs)) + '</b></button>']
    seg += [f'<button type="button" class="v-{k}" data-f="{k}" aria-pressed="false" title="{e(VERDICT_HELP[k])}">'
            f'<i></i>{e(label)}<b>{count[k]}</b></button>' for k, label in VERDICTS if count[k]]
    parts = [
        "<!DOCTYPE html>", '<html lang="en"><head><meta charset="utf-8">',
        '<meta name="viewport" content="width=device-width,initial-scale=1">',
        '<meta name="color-scheme" content="light dark">',
        "<title>opam-repository CI triage</title>",
        '<link rel="alternate" type="application/atom+xml" title="opam-repository CI triage" href="feed.xml">',
        f"<style>{CSS}</style></head><body><main>",
        '<header><h1>opam-repository CI triage</h1>'
        f'<nav>Updated {e(day(gen, year))} {e(gen[11:16])} UTC · <a href="feed.xml">Atom feed</a> · '
        '<a href="#help">How to read this page</a></nav></header>',
        f'<p class="lead">{scope}</p>' if scope else "",
        ready_section(data),
        f'<section id="failing"><h2>Failing PRs <span class="sub">({len(prs)})</span></h2>',
        '<nav class="filters" aria-label="Filters"><div class="seg" role="group" aria-label="Verdict">'
        + "".join(seg) + "</div>"
        + (f'<label><input type="checkbox" id="merge"> consider for merge ({merge})</label>' if merge else "")
        + '<input id="q" type="search" placeholder="Search number, title, author, package" aria-label="Search">'
        '<span id="shown" class="meta" aria-live="polite"></span></nav>',
        '<div id="prs">',
    ]
    parts.extend(render_pr(p, year, data.get("max_failures_per_pr")) for p in prs)
    if not prs:
        parts.append('<p class="note">No open PR has failing opam-repo-ci jobs.</p>')
    parts.append("</div></section>")
    parts.append(chronic([p for p in prs if not p.get("pending")]))
    parts.append(help_section(data))
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
        v = verdict(pr)
        g = groups(pr)
        lines = [f"{VERDICT_LABEL[v]}: {headline(pr, v, as_html=False)}"]
        if ci_running(pr):
            lines.append(f"CI still running ({pr.get('ci_unfinished') or '?'} jobs left).")
        since = pr.get("since_last_push")
        if since and (since.get("fixed") or since.get("new")):
            lines.append(f"Since the previous push: {len(since.get('fixed') or [])} gone, "
                         f"{len(since.get('new') or [])} new, {since.get('still', 0)} unchanged.")
        for k in ("fix", "workaround", "elsewhere", "explanation"):
            for r in merge_similar(g.get(k, []))[:5]:
                lines.append(f"{title_of(r)}: {r.get('failing') or r.get('job') or ''}"
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
