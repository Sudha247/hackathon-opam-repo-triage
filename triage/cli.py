"""Command line: triage opam-repo-ci failures of an ocaml/opam-repository PR."""

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
    NET,
    OFFLINE,
    OFFLINE_MISSES,
    REPO,
    fetch,
)
from .ci import (  # noqa: F401
    FAILING,
    excerpt,
    load_commit,
    log_text,
    parse_commit_page,
    resolve_sha,
)
from .sources import (  # noqa: F401
    Sources,
    find_baseline_commit,
)
from .words import (  # noqa: F401
    counted,
)
from .llm import (  # noqa: F401
    LLM_MODEL,
)
from .suggest import (  # noqa: F401
    rank,
    suggest,
)
from .render import (  # noqa: F401
    build_review,
    compare,
    render_comment,
    render_compare,
    render_suggestions,
    render_summary,
)


def main():
    ap = argparse.ArgumentParser(description="Triage opam-repo-ci failures of an ocaml/opam-repository PR.",
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("command", choices=["summary", "logs", "compare", "suggest", "comment", "review"])
    ap.add_argument("target", nargs="?", help="commit sha or opam.ci.ocaml.org URL")
    ap.add_argument("--pr", type=int, help="opam-repository PR number (uses gh to find the head sha)")
    ap.add_argument("--json", action="store_true", help="machine-readable output")
    ap.add_argument("--all", action="store_true", help="summary: also show passed/ignored/not-available jobs")
    ap.add_argument("--match", help="logs, suggest: only the jobs whose CI name (as in the job's URL) matches "
                                    "this regex")
    ap.add_argument("--category", action="append",
                    help="logs: only these categories (default: everything except network/cancelled/ignored/not-available)")
    ap.add_argument("--max", type=int, default=30,
                    help="logs: logs to fetch; suggest/comment: distinct failures to analyse (default: 30)")
    ap.add_argument("--llm", action="store_true",
                    help="suggest/comment: ask Claude about failures the tool cannot explain (needs `pip install "
                         "anthropic` and credentials; costs API tokens; responses are cached)")
    ap.add_argument("--llm-max", type=int, default=5, help="at most this many LLM calls (distinct failures)")
    ap.add_argument("--llm-model", default=LLM_MODEL, help=f"the Claude model for --llm (default: {LLM_MODEL})")
    ap.add_argument("--llm-dry-run", action="store_true", help="print the LLM prompts instead of calling the API")
    ap.add_argument("--post", action="store_true",
                    help="comment/review: post it on the PR with gh (default: print it)")
    ap.add_argument("--baseline", help="compare: baseline commit sha/URL (default: the PR of the previous version)")
    ap.add_argument("--cache-dir", default=".opam-ci-cache",
                    help="where CI pages, logs and lookups are cached (default: .opam-ci-cache)")
    ap.add_argument("--refresh", action="store_true",
                    help="fetch the commit page again, and the logs of its jobs when needed (after a rebuild)")
    args = ap.parse_args()

    sha = resolve_sha(args.target, args.pr)
    page, data = load_commit(sha, args.cache_dir, refresh=args.refresh)
    if not data["jobs"]:
        sys.exit("no jobs found (CI not started yet, or the page layout changed)")
    if data["unfinished"]:
        print(f"note: CI still running: {data['unfinished']} of {len(data['jobs'])} jobs not finished; "
              f"these are the failures so far", file=sys.stderr)

    if args.command in ("suggest", "comment", "review"):
        src = Sources(args.cache_dir)
        llm = ({"limit": args.llm_max, "model": args.llm_model, "dry_run": args.llm_dry_run}
               if args.llm or args.llm_dry_run else None)
        results = rank(suggest(sha, data, src, args.cache_dir, args.max, args.match, llm))
        if OFFLINE:
            print(f"offline cache misses: {len(OFFLINE_MISSES)}", file=sys.stderr)
            for m in OFFLINE_MISSES[:10]:
                print(f"  miss: {m}", file=sys.stderr)
        if args.command == "review":
            review = build_review(sha, data, results, src)
            if args.post:
                out = subprocess.run(["gh", "api", f"repos/{REPO}/pulls/{data['pr']}/reviews", "-X", "POST",
                                      "--input", "-"], input=json.dumps(review), capture_output=True, text=True)
                print(out.stdout.strip()[:300] or out.stderr.strip())
            else:
                print(json.dumps(review, indent=2))
            print(f"{counted(len(review['comments']), 'line comment')}; network requests: "
                  + ", ".join(f"{k}={v}" for k, v in NET.items()), file=sys.stderr)
            return
        if args.command == "comment":
            body = render_comment(sha, data, results)
            if args.post:
                if not data["pr"]:
                    sys.exit("cannot post: the CI page does not name a PR")
                out = subprocess.run(["gh", "pr", "comment", str(data["pr"]), "-R", REPO, "--body-file", "-"],
                                     input=body, capture_output=True, text=True)
                print(out.stdout.strip() or out.stderr.strip())
            else:
                print(body)
            print("network requests: " + ", ".join(f"{k}={v}" for k, v in NET.items()), file=sys.stderr)
            return
        if args.json:
            # incomplete: ocaml.org or another server failed during the analysis (see the counts).
            errors = {k: NET[k] for k in ("mcp-errors", "net-errors") if NET.get(k)}
            json.dump({"sha": sha, "pr": data["pr"], "ci_unfinished": data["unfinished"],
                       "ci_jobs": len(data["jobs"]), "unanalysed": data.get("unanalysed", 0),
                       "incomplete": bool(errors), "errors": errors,
                       "suggestions": results}, sys.stdout, indent=2)
            print()
        else:
            render_suggestions(results)
        print("network requests: " + ", ".join(f"{k}={v}" for k, v in NET.items()), file=sys.stderr)
        return

    if args.command == "compare":
        targets = sorted({j["target"] for j in data["jobs"]
                          if j["kind"] == "revdeps" and j["category"] in FAILING})
        report = []
        for t in targets:
            if args.baseline:
                info = {"prev": "(given)", "sha": resolve_sha(args.baseline, None), "pr": None}
            else:
                info = find_baseline_commit(t, args.cache_dir)
            if not info or "sha" not in info:
                print(f"== {t}: no previous version / baseline PR found", file=sys.stderr)
                continue
            try:
                base_data = parse_commit_page(fetch(BASE + info["sha"], args.cache_dir))
            except urllib.error.HTTPError as e:
                print(f"== {t}: baseline CI page for {info['prev']} ({info['sha'][:12]}) unavailable: {e}", file=sys.stderr)
                continue
            results = compare(data["jobs"], base_data["jobs"])
            results = [r for r in results if
                       any(j["target"] == t for j in data["jobs"] if j["revdep"] == r["revdep"])]
            report.append({"target": t, "baseline": info, "results": results})
            if not args.json:
                render_compare(t, info, results, args.all)
        if args.json:
            json.dump({"sha": sha, "pr": data["pr"], "report": report}, sys.stdout, indent=2)
            print()
        return

    if args.command == "summary":
        if args.json:
            json.dump({"sha": sha, **data}, sys.stdout, indent=2)
            print()
        else:
            render_summary(sha, data, args.all)
        return

    cats = set(args.category or ["build", "build-or-test", "dependency", "solver", "lint"])
    sel = [j for j in data["jobs"]
           if j["status"] == "failed" and (j["category"] in cats or j["kind"] == "lint")
           and (not args.match or re.search(args.match, j["variant"]))]
    if len(sel) > args.max:
        print(f"note: {len(sel)} jobs selected, fetching the first {args.max} (use --max)", file=sys.stderr)
        sel = sel[: args.max]

    def work(j):
        try:
            return {**j, "log": excerpt(log_text(fetch(j["url"], args.cache_dir)))}
        except Exception as e:  # keep going: one unreachable log must not hide the others
            return {**j, "log": {"fetch_error": str(e)}}

    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as ex:
        results = list(ex.map(work, sel))

    if args.json:
        json.dump({"sha": sha, "pr": data["pr"], "jobs": results}, sys.stdout, indent=2)
        print()
        return
    for r in results:
        lg = r["log"]
        print("=" * 100)
        builder = {"classic": "main"}.get(lg.get("builder"), lg.get("builder"))
        print(f"{r['variant']}  [{r['category']}] builder={builder} base={lg.get('base', '?')[:12]}")
        print(r["url"])
        if lg.get("job"):
            print("job:", lg["job"])
        for k in ("solver", "error_block"):
            if k in lg:
                print(lg[k])
        if "fetch_error" in lg:
            print("!! could not fetch log:", lg["fetch_error"])
            continue
        print("=> " + (lg.get("final") or lg.get("job_failed") or "?"))
