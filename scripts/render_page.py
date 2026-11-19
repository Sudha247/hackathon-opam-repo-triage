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

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REPO_URL = "https://github.com/ocaml/opam-repository"
CLASSES = [("fix", "Fixes for this PR"),
           ("workaround", "Workarounds (restrict where it is built or tested)"),
           ("elsewhere", "Elsewhere (another package, CI, systemic)"),
           ("explanation", "Explanations (no change suggested)")]
CATEGORIES = [("fix", "Fix for the PR itself"), ("blocked", "Blocked by other packages"),
              ("ci", "CI only"), ("other", "Needs a look"), ("error", "Not analysed")]
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


def failed_count(pr):
    failed = ((pr.get("jobs") or {}).get("failed") or {})
    return sum(v for k, v in failed.items() if k not in NOISE), sum(failed.values())


def render_suggestion(r):
    out = ['<div class="sugg">', '<div class="sh">']
    if r.get("failing"):
        out.append(f'<b>{e(r["failing"])}</b>')
    if r.get("rule"):
        out.append(f'<span class="tag">{e(r["rule"])}</span>')
    if r.get("confidence"):
        conf = str(r["confidence"]).split(" ")[0]
        out.append(f'<span class="tag conf-{e(conf)}" title="{e(r["confidence"])}">{e(conf)}</span>')
    if r.get("experimental"):
        out.append('<span class="tag">experimental platform</span>')
    out.append("</div>")
    meta = []
    if r.get("variant"):
        meta.append("job: " + link(r.get("url"), r["variant"]))
    if r.get("fix_in"):
        meta.append("fix goes in: " + e(r["fix_in"]))
    if meta:
        out.append(f'<div class="meta">{" · ".join(meta)}</div>')
    jobs = r.get("jobs") or []
    others = [v for v in jobs if v != r.get("variant")]
    if others:
        # A matrix suggestion's variant is a label, not one of its jobs: then list them all.
        what = (f'{len(others)} more job{"s" if len(others) != 1 else ""} ({len(jobs)} in total)'
                if len(others) < len(jobs) else f'{len(jobs)} job{"s" if len(jobs) != 1 else ""}')
        out.append(f'<details class="jobs"><summary>same failure on {what}</summary>'
                   f'<ul>{"".join(f"<li>{e(v)}</li>" for v in others)}</ul></details>')
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
        out.append(f'<div class="meta">candidates (compared with {e(r.get("compared_with") or "?")}):</div>')
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
    if r.get("also_newer"):
        out.append(f'<div class="meta">also released after it (not ranked): {e(", ".join(map(str, r["also_newer"])))}</div>')
    if r.get("evidence"):
        ev = []
        for k, v in r["evidence"].items():
            word, cls = ("yes", "yes") if v is True else (("no", "no") if v is False else ("?", "unk"))
            ev.append(f'<li>{e(k)}: <span class="{cls}">{word}</span></li>')
        src = f' (looked up in {e(r["source"])})' if r.get("source") else ""
        out.append(f'<div class="meta">evidence{src}:</div><ul class="ev">{"".join(ev)}</ul>')
    elif r.get("source"):
        out.append(f'<div class="meta">looked up in: {e(r["source"])}</div>')
    for t in r.get("tried") or []:
        out.append(f'<div class="meta">tried: {e(t)}</div>')
    if r.get("note"):
        out.append(f'<div class="note">{e(r["note"])}</div>')
    out.append("</div>")
    return "\n".join(out)


def render_pr(pr):
    cats, groups = categorise(pr)
    n = int(pr["number"])
    real, total = failed_count(pr)
    counts = [f'<span class="cnt c-{k}" title="{e(label)}">{len(groups[k])} {k}</span>'
              for k, label in CLASSES if groups.get(k)]
    badges = "".join(f'<span class="badge b-{c}">{e(label)}</span>' for c, label in CATEGORIES if c in cats)
    title = pr.get("title") or ""
    search = f"{n} {title} {pr.get('author') or ''}".lower()
    head = (f'<summary><span class="num">{link(f"{REPO_URL}/pull/{n}", f"#{n}")}</span>'
            f'<span class="title">{e(title)}{" <i>(draft)</i>" if pr.get("draft") else ""}</span>'
            f'<span class="row2">{badges}'
            f'<span class="cnt" title="failed jobs excluding network, cancelled and not-available">'
            f'{real} failed job{"s" if real != 1 else ""}</span>{"".join(counts)}'
            f'{link(pr.get("ci_url"), "CI run", "ci")}'
            f'<span class="date">{e((pr.get("updated") or "")[:10])}</span></span></summary>')
    body = ['<div class="body">',
            f'<div class="meta">by {e(pr.get("author") or "?")} · head {e((pr.get("sha") or "")[:12])}'
            f' · analysed {e((pr.get("analysed_at") or "never")[:16].replace("T", " "))}'
            f' · failed jobs by category: '
            f'{e(", ".join(f"{k} {v}" for k, v in sorted(((pr.get("jobs") or {}).get("failed") or {}).items())) or "?")}'
            "</div>"]
    if pr.get("pending"):
        body.append('<p class="note">Not analysed yet (time budget reached); the next run continues.</p>')
    elif pr.get("result") is None:
        body.append(f'<p class="note">The analysis failed ({e(pr.get("attempts"))} attempt(s)): '
                    f'<code>{e(pr.get("error"))}</code></p>')
    else:
        if not pr["result"].get("suggestions"):
            body.append('<p class="note">No failure analysed.</p>')
        for k, label in CLASSES:
            if groups.get(k):
                body.append(f'<h4 class="g-{k}">{e(label)} ({len(groups[k])})</h4>')
                body.extend(render_suggestion(r) for r in groups[k])
    body.append('<div class="cmd"><span class="meta">Line-level review, by hand (review it before posting):</span>'
                f'<pre><code>python3 opam_repo_ci_triage.py review --pr {n}</code></pre></div></div>')
    return (f'<details class="pr" data-cats="{" ".join(sorted(cats))}" data-search="{e(search)}">'
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
    out = ['<section id="chronic"><h2>Chronic failures</h2>']
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
    out.append(f'<h3>Other packages blamed in {min_prs} or more PRs ({len(brows)})</h3>')
    if brows:
        out.append(table(["Package", "PRs", "Blamed in"], [
            f'<tr><td><b>{e(p)}</b></td><td>{len(s)}</td><td>'
            + ", ".join(link(f"{REPO_URL}/pull/{n}", f"#{n}") for n in sorted(s)) + "</td></tr>"
            for p, s in brows]))
    else:
        out.append('<p class="note">None among the analysed PRs.</p>')
    out.append('<p class="meta">Failed build or test jobs of reverse dependencies (from the CI job list, '
               'excluding network, cancelled, not-available and experimental jobs); '
               '"blamed" means a suggestion says the fix goes in that package.</p></section>')
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
.b-ci{color:var(--wk);border-color:var(--wk)}.b-error{color:var(--err);border-color:var(--err)}
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
details.jobs{color:var(--mut);font-size:.85rem;overflow-wrap:anywhere}
details.jobs>summary{cursor:pointer}details.jobs ul{margin:2px 0 4px;padding-left:20px}
ul.ev{margin:2px 0 6px;padding-left:1.4em;font-size:.85rem}.yes{color:var(--add)}.no{color:var(--del)}.unk{color:var(--mut)}
.cmd pre{margin-top:2px;white-space:pre}
.tw{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:.88rem}
th,td{border-bottom:1px solid var(--bd);padding:5px 8px;text-align:left;vertical-align:top}
td:last-child{overflow-wrap:anywhere}.nw{white-space:nowrap}
details.more>summary{cursor:pointer;color:var(--acc);margin:6px 0}
.hidden{display:none}
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
})();
"""


def render(data):
    prs = data.get("prs", [])
    cats = {int(p["number"]): categorise(p)[0] for p in prs}
    analysed = sum(1 for p in prs if p.get("result") is not None and not p.get("pending"))
    count = collections.Counter(c for s in cats.values() for c in s)
    gen = data.get("generated") or ""
    run = data.get("run") or {}
    stats = [("PRs analysed", analysed), ("Fix for the PR itself", count["fix"]),
             ("Blocked by other packages", count["blocked"]), ("CI only", count["ci"]),
             ("Needs a look", count["other"])]
    if count["error"]:
        stats.append(("Not analysed", count["error"]))
    limit = f" (limited to {e(data['limit'])})" if data.get("limit") else ""
    parts = [
        "<!DOCTYPE html>", '<html lang="en"><head><meta charset="utf-8">',
        '<meta name="viewport" content="width=device-width,initial-scale=1">',
        '<meta name="color-scheme" content="light dark">',
        "<title>opam-repository CI triage</title>", f"<style>{CSS}</style></head><body><main>",
        "<h1>opam-repository CI triage</h1>",
        f'<p class="meta">Open PRs of {link(REPO_URL, "ocaml/opam-repository")} whose opam-ci status is '
        f'<b>failure</b>: {e(data.get("failing_prs"))} of {e(data.get("open_prs"))} open PRs{limit}. '
        f'Generated {e(gen[:16].replace("T", " "))} UTC by <code>opam_repo_ci_triage.py suggest</code> '
        f'(rules only, at most {e(data.get("max_failures_per_pr"))} distinct failures per PR). '
        f'This run: {e(run.get("analysed", 0))} analysed, {e(run.get("unchanged", 0))} unchanged, '
        f'{e(run.get("dropped", 0))} dropped. Suggestions are automatic: check them before acting.</p>',
        '<div class="stats">' + "".join(f'<div class="stat"><b>{e(v)}</b>{e(k)}</div>' for k, v in stats) + "</div>",
        '<nav class="filters" aria-label="Filters"><button data-f="all">All</button>'
        + "".join(f'<button data-f="{c}">{e(label)} ({count[c]})</button>' for c, label in CATEGORIES if count[c])
        + '<input id="q" type="search" placeholder="Search number, title, author" aria-label="Search">'
        '<button id="expand" type="button">Expand / collapse</button><span id="shown" class="meta"></span></nav>',
        '<section id="prs">',
    ]
    parts.extend(render_pr(p) for p in prs)
    if not prs:
        parts.append('<p class="note">No open PR with a failing opam-ci status.</p>')
    parts.append("</section>")
    parts.append(chronic([p for p in prs if not p.get("pending")]))
    parts.append('<p class="meta">Categories: <i>Fix for the PR itself</i>: a change to the PR\'s opam files '
                 '(fix or workaround); <i>Blocked by other packages</i>: a suggestion says the fix goes in another '
                 'package; <i>CI only</i>: nothing points at the PR or another package (network, systemic, CI '
                 'environment); <i>Needs a look</i>: failures in the PR without a suggested change.</p>')
    parts.append(f"<script>{JS}</script></main></body></html>")
    return "\n".join(parts) + "\n"


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
    print(f"wrote {args.out} ({len(page.encode())} bytes, {len(data.get('prs', []))} PRs)")


if __name__ == "__main__":
    main()
