#!/usr/bin/env python3
"""Group the LLM diagnoses of the triage page by error, to find the
recurring ones worth turning into rules.

  python3 scripts/llm_digest.py                          # the page's data.json, report on stdout
  python3 scripts/llm_digest.py llm-eval/*.json          # `suggest --json` outputs instead
  python3 scripts/llm_digest.py --issue OWNER/REPO       # also create or update the GitHub issue

Each LLM answer carries the log line that best names the error
(`log_error`). Answers are grouped by that line with paths, numbers and
identifiers masked, so that `Unbound value Conv.output` and `Unbound value
Conv.dimension` fall in the same group; a group seen in several PRs is a
candidate for a rule. A rule then replaces the LLM call for that error:
it is free, deterministic and tested, and the group disappears from the
next digest.

With --issue, the report becomes the body of one open issue titled
TITLE (created if needed, with `gh`; needs a token that can write issues).
"""

import argparse
import collections
import json
import os
import re
import subprocess
import sys
import urllib.request

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
from triage.md import md_code, md_text, safe_ci_url  # noqa: E402

PAGE_DATA = "https://sudha247.github.io/hackathon-opam-repo-triage/data.json"
TITLE = "LLM diagnoses to turn into rules"
MAX_BODY = 60000


def load(source):
    if source.startswith("https://"):
        req = urllib.request.Request(source, headers={"User-Agent": "opam-ci-triage"})
        with urllib.request.urlopen(req, timeout=60) as r:
            return json.load(r)
    with open(source) as f:
        return json.load(f)


def answers(sources):
    """(PR number, suggestion) for every LLM answer."""
    out = []
    for src in sources:
        d = load(src)
        if "prs" in d:                         # the page's data.json
            for p in d["prs"]:
                for r in (p.get("result") or {}).get("suggestions", []):
                    if r.get("rule") == "llm":
                        out.append((p.get("number"), r))
        else:                                  # one `suggest --json` output
            for r in d.get("suggestions", []):
                if r.get("rule") == "llm":
                    out.append((d.get("pr"), r))
    return out


def exact(line):
    """An error line without its locations and numbers."""
    s = re.sub(r'File "[^"]*", line \d+(, characters \d+-\d+)?:?', "", line or "")
    s = re.sub(r"(?:[\w.+-]*/)+[\w.+-]+", "<path>", s)              # paths
    s = re.sub(r"\b[\w.-]+\.(?:ml|mli|c|h|cpp|hpp|cc|rs|dune|opam):\d+(?::\d+)?", "<file>", s)
    s = re.sub(r"\d+", "N", s)
    return re.sub(r"\s+", " ", s).strip()


def masked(line):
    """The group key: also without identifiers and quoted names."""
    s = exact(line)
    s = re.sub(r"[`'\"‘’][^`'\"‘’\s]{1,60}[`'\"‘’]", "'…'", s)              # quoted names, not messages
    s = re.sub(r"\b[A-Z][\w']*(?:\.[\w']+)+\b", "M.x", s)           # Module.path.ident
    s = re.sub(r"\b(value|module|constructor|field|type constructor|library|Library|Package|package) [\w.'-]+",
               r"\1 x", s)
    return s


def pr_ref(n, page=None):
    """A reference to opam-repository PR `n` that GitHub does not turn into a
    cross-reference (a "mentioned this" event on the PR): a link to the PR on
    the triage page when it is known, else a code span."""
    n = str(n or "")
    if not n.isdigit():
        return "?"
    return f"[`#{n}`]({page}#pr-{n})" if page else md_code(f"ocaml/opam-repository#{n}")


def page_url(sources):
    """The triage page, when the data comes from its data.json."""
    for s in sources:
        m = re.fullmatch(r"(https://[A-Za-z0-9.-]+/(?:[A-Za-z0-9._~-]+/)*)data\.json", s)
        if m:
            return m.group(1)
    return None


def report(items, min_prs, page=None):
    """The digest (GitHub Markdown). Everything in an answer is untrusted (the
    model quotes CI logs and opam files): prose goes through `md_text`, log
    lines and changes are code spans."""
    groups = collections.defaultdict(list)
    for n, r in items:
        line = r.get("log_error") or r.get("error") or r.get("diagnosis") or ""
        groups[masked(line)].append((n, r, line))
    conf = collections.Counter(str(r.get("confidence") or "?").split(" ")[0] for _, r in items)
    prs = {n for n, _ in items}
    out = [f"{len(items)} LLM answers in {len(prs)} PRs "
           f"({md_text(', '.join(f'{v} {k}' for k, v in conf.most_common()))} confidence), in {len(groups)} groups "
           f"by error.", ""]
    multi = sorted(((k, v) for k, v in groups.items() if len({n for n, _, _ in v}) >= min_prs),
                   key=lambda kv: (-len({n for n, _, _ in kv[1]}), kv[0]))
    out.append(f"## Errors answered in {min_prs} or more PRs ({len(multi)})")
    out.append("")
    if not multi:
        out.append("None yet.")
    for key, v in multi:
        pr_list = sorted({n for n, _, _ in v if n})
        pkgs = sorted({str(r.get("failing") or "?") for _, r, _ in v})
        out += [f"### {md_code(key[:200]) or '?'}", "",
                f"- PRs: " + ", ".join(pr_ref(n, page) for n in pr_list),
                f"- Failing packages: {md_text(', '.join(pkgs[:12]))}" + (" …" if len(pkgs) > 12 else "")]
        lines = sorted({line for _, _, line in v})
        out.append("- Errors: " + "; ".join(md_code(l[:160]) for l in lines[:3]) + (" …" if len(lines) > 3 else ""))
        diags = collections.Counter((r.get("diagnosis") or r.get("error") or "", r.get("confidence") or "?",
                                     r.get("where") or r.get("fix_in") or "?") for _, r, _ in v)
        out.append("- Diagnoses:")
        for (dg, c, w), k in diags.most_common(3):
            out.append(f"  - ({md_text(c)} confidence, fix in {md_text(w)}{f', {k} times' if k > 1 else ''}) "
                       f"{md_text(dg, 400)}")
        changes = sorted({r.get("new") for _, r, _ in v if r.get("new")})
        if changes:
            out.append("- Suggested changes: " + "; ".join(md_code(c[:120]) for c in changes[:3]))
        logs = [u for u in (safe_ci_url(r.get("url")) for _, r, _ in v) if u][:2]
        if logs:
            out.append("- Logs: " + ", ".join(f"[log]({u})" for u in logs))
        out.append("")
    single = sorted((k, v) for k, v in groups.items() if len({n for n, _, _ in v}) < min_prs)
    if single:
        out += [f"## Answered in fewer PRs ({len(single)})", "",
                "<details><summary>Errors</summary>", ""]
        for key, v in single[:80]:
            n, r, _ = v[0]
            out.append(f"- {md_code(key[:160]) or '?'}: {pr_ref(n, page)} {md_text(r.get('failing'))}")
        out += ["", "</details>"]
    out += ["", "To turn a group into a rule: check the diagnosis on the logs, add the rule to `triage/` "
                "(most go in `known_errors` in `triage/rules.py`), and a regression case in "
                "`tests/test_triage.py`. The group then no longer reaches the LLM."]
    body = "\n".join(out)
    if len(body) <= MAX_BODY:
        return body
    # Cut at a line end: a cut line could leave a code span or a link open.
    return body[:body.rfind("\n", 0, MAX_BODY)] + "\n\n(truncated)"


def update_issue(repo, title, body):
    q = subprocess.run(["gh", "issue", "list", "-R", repo, "--state", "open", "--search", f'"{title}" in:title',
                        "--json", "number,title"], capture_output=True, text=True, check=True)
    found = next((i["number"] for i in json.loads(q.stdout) if i["title"] == title), None)
    cmd = (["gh", "issue", "edit", str(found), "-R", repo, "--body-file", "-"] if found
           else ["gh", "issue", "create", "-R", repo, "--title", title, "--body-file", "-"])
    p = subprocess.run(cmd, input=body, capture_output=True, text=True, check=True)
    print(p.stdout.strip() or f"updated issue #{found}", file=sys.stderr)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("sources", nargs="*", help=f"data.json or `suggest --json` files (default: {PAGE_DATA})")
    ap.add_argument("--min-prs", type=int, default=2, help="PRs an error must appear in to be a candidate")
    ap.add_argument("--issue", metavar="OWNER/REPO", help="create or update the digest issue there")
    ap.add_argument("--title", default=TITLE)
    args = ap.parse_args()
    items = answers(args.sources or [PAGE_DATA])
    if not items:
        print("no LLM answer in the data (is ANTHROPIC_API_KEY set for the page?)", file=sys.stderr)
        return
    body = report(items, args.min_prs, page_url(args.sources or [PAGE_DATA]))
    print(body)
    if args.issue:
        update_issue(args.issue, args.title, body)


if __name__ == "__main__":
    main()
