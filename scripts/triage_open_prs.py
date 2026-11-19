#!/usr/bin/env python3
"""Run `opam_repo_ci_triage.py suggest` on the open opam-repository PRs whose
opam-ci status is `failure`, and write the results to site/data.json.

  python3 scripts/triage_open_prs.py [--limit N] [--state state/prs.json] [--out site/data.json]

The open PRs and the opam-ci status of their head commit are listed with one
GitHub GraphQL query per 100 PRs (needs a token: GITHUB_TOKEN, GH_TOKEN or a
logged-in gh). A PR whose head sha is unchanged since the previous run is not
analysed again: its result comes from the state file. PRs that were closed,
or whose CI is no longer `failure`, are dropped. PRs are analysed one at a
time, to respect opam.ci.ocaml.org's rate limit; `--time-budget` stops
starting new ones after that many seconds, and the next run continues.
"""

import argparse
import collections
import datetime
import hashlib
import importlib.util
import json
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TOOL = os.path.join(ROOT, "opam_repo_ci_triage.py")
sys.path.insert(0, ROOT)
from opam_repo_ci_triage import BASE, USER_AGENT, github_token  # noqa: E402

OWNER, NAME = "ocaml", "opam-repository"
CONTEXT = "opam-ci"
MAX_ATTEMPTS = 3            # retries of a failed analysis of the same sha
# Failed-job categories (from `summary`) that are not about the package itself.
NOISE = {"passed", "network", "cancelled", "not-available", "ignored"}

QUERY = """
query($owner: String!, $name: String!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequests(states: OPEN, first: 100, after: $cursor,
                 orderBy: {field: UPDATED_AT, direction: DESC}) {
      pageInfo { hasNextPage endCursor }
      nodes {
        number title url isDraft updatedAt headRefOid
        author { login }
        commits(last: 1) { nodes { commit { oid status { contexts { context state targetUrl } } } } }
      }
    }
  }
}
"""


def now():
    return datetime.datetime.now(datetime.timezone.utc).replace(microsecond=0).isoformat()


def graphql(query, variables, token):
    req = urllib.request.Request(
        "https://api.github.com/graphql", data=json.dumps({"query": query, "variables": variables}).encode(),
        headers={"User-Agent": USER_AGENT, "Authorization": f"Bearer {token}",
                 "Content-Type": "application/json"})
    for attempt in range(3):
        try:
            with urllib.request.urlopen(req, timeout=60) as r:
                d = json.loads(r.read())
            if d.get("errors"):
                raise RuntimeError(f"GraphQL errors: {d['errors']}")
            return d["data"]
        except (urllib.error.URLError, TimeoutError) as e:
            if attempt == 2:
                raise
            print(f"GitHub API error ({e}), retrying", file=sys.stderr)
            time.sleep(10 * (attempt + 1))


def list_open_prs(token):
    """All open PRs with the opam-ci state of their head commit; (prs, number of requests)."""
    prs, cursor, calls = [], None, 0
    while True:
        calls += 1
        page = graphql(QUERY, {"owner": OWNER, "name": NAME, "cursor": cursor}, token)
        conn = page["repository"]["pullRequests"]
        for n in conn["nodes"]:
            ci = None
            commits = n["commits"]["nodes"]
            head = commits[0]["commit"] if commits else {}
            if head.get("oid") == n["headRefOid"]:
                for c in (head.get("status") or {}).get("contexts", []):
                    if c["context"] == CONTEXT:
                        ci = c
            prs.append({"number": n["number"], "title": n["title"], "url": n["url"], "draft": n["isDraft"],
                        "updated": n["updatedAt"], "sha": n["headRefOid"],
                        "author": (n.get("author") or {}).get("login"),
                        "ci_state": (ci or {}).get("state"), "ci_url": (ci or {}).get("targetUrl")})
        if not conn["pageInfo"]["hasNextPage"]:
            return prs, calls
        cursor = conn["pageInfo"]["endCursor"]


def run_tool(args, timeout):
    """Run the triage tool; (parsed JSON or None, error message or None, request counts)."""
    try:
        p = subprocess.run([sys.executable, TOOL] + args, cwd=ROOT, capture_output=True, text=True,
                           timeout=timeout)
    except subprocess.TimeoutExpired:
        return None, f"timeout after {timeout}s", {}
    net = {}
    m = re.search(r"network requests: (.*)", p.stderr)
    if m:
        net = {k: int(v) for k, v in re.findall(r"([\w-]+)=(\d+)", m.group(1))}
    if p.returncode != 0:
        lines = [x for x in p.stderr.strip().splitlines() if x.strip()]
        return None, (lines[-1] if lines else f"exit code {p.returncode}")[:500], net
    try:
        return json.loads(p.stdout), None, net
    except ValueError:
        return None, "unparsable JSON output", net


def jobs_overview(summary):
    """Failed-job counts by category, and the reverse dependencies that failed
    to build or test, from `summary --json` (which reads the cached CI page)."""
    failed = collections.Counter()
    revdeps = collections.defaultdict(list)
    for j in summary.get("jobs", []):
        if j.get("status") != "failed":
            continue
        failed[j.get("category") or "?"] += 1
        if j.get("revdep") and j.get("category") not in NOISE and not j.get("experimental"):
            revdeps[j["revdep"]].append({"variant": j.get("variant"), "url": j.get("url"),
                                         "category": j.get("category"), "target": j.get("target")})
    return {"total": len(summary.get("jobs", [])), "failed": dict(failed),
            "failing_revdeps": {k: v for k, v in sorted(revdeps.items())}}


def save_state(path, prs):
    os.makedirs(os.path.dirname(os.path.abspath(path)), exist_ok=True)
    with open(path + ".tmp", "w") as f:
        json.dump({"updated": now(), "prs": prs}, f)
    os.replace(path + ".tmp", path)


def llm_available():
    """The LLM fallback runs only with an API key and the anthropic package;
    without them the page is built from the rules alone."""
    return bool(os.environ.get("ANTHROPIC_API_KEY")) and importlib.util.find_spec("anthropic") is not None


def tool_version():
    with open(TOOL, "rb") as f:
        return hashlib.sha256(f.read()).hexdigest()[:12]


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--limit", type=int, help="only the N most recently updated failing PRs (for testing)")
    ap.add_argument("--state", default=os.path.join(ROOT, "state", "prs.json"))
    ap.add_argument("--out", default=os.path.join(ROOT, "site", "data.json"))
    ap.add_argument("--cache-dir", default=os.path.join(ROOT, ".opam-ci-cache"), help="the tool's cache")
    ap.add_argument("--max", type=int, default=6, help="suggest --max (distinct failures analysed per PR)")
    ap.add_argument("--timeout", type=int, default=600, help="per-PR timeout of the tool, in seconds")
    ap.add_argument("--time-budget", type=int, default=0,
                    help="stop starting new analyses after this many seconds (0: no limit)")
    ap.add_argument("--llm-max", type=int, default=2,
                    help="LLM calls per PR at most, for failures no rule recognises (needs ANTHROPIC_API_KEY)")
    ap.add_argument("--llm-budget", type=int, default=40,
                    help="LLM calls per run at most; PRs left without them get them in a later run")
    args = ap.parse_args()
    start = time.monotonic()

    token = github_token()
    if not token:
        sys.exit("a GitHub token is needed (GITHUB_TOKEN, GH_TOKEN or `gh auth login`): "
                 "the PR listing uses the GraphQL API")
    try:
        with open(args.state) as f:
            state = json.load(f)
    except (OSError, ValueError):
        state = {"prs": {}}
    old = state.get("prs", {})

    prs, list_calls = list_open_prs(token)
    failing = [p for p in prs if p["ci_state"] == "FAILURE"]
    print(f"{len(prs)} open PRs, {len(failing)} with {CONTEXT} = failure ({list_calls} GitHub API call(s))",
          file=sys.stderr)
    if args.limit is not None:
        failing = failing[:args.limit]

    llm = llm_available()
    llm_left = args.llm_budget if llm else 0
    print("LLM fallback: " + (f"on (at most {args.llm_budget} calls)" if llm else
                              "off (no ANTHROPIC_API_KEY, or the anthropic package is not installed)"),
          file=sys.stderr)
    version = tool_version()
    new_state, stats = {}, collections.Counter()
    net_total = collections.Counter()
    for p in failing:
        key = str(p["number"])
        prev = old.get(key)
        entry = dict(prev or {})
        entry.update({k: p[k] for k in ("number", "title", "url", "draft", "updated", "sha", "author")})
        entry["ci_url"] = p["ci_url"] or BASE + p["sha"]
        same = prev is not None and prev.get("sha") == p["sha"] and prev.get("tool_version") == version
        # With the LLM on, a PR analysed without it (key added later, or the
        # run's budget spent) is analysed again; the logs come from the cache.
        if same and llm and not prev.get("llm"):
            same = False
        if same and (prev.get("result") is not None or prev.get("attempts", 0) >= MAX_ATTEMPTS):
            stats["unchanged"] += 1
            new_state[key] = entry
            continue
        if args.time_budget and time.monotonic() - start > args.time_budget:
            stats["deferred"] += 1
            if not same:        # an older result would describe another commit
                for k in ("result", "error", "jobs", "analysed_at", "attempts", "tool_version"):
                    entry.pop(k, None)
            entry["pending"] = True
            new_state[key] = entry
            continue
        t0 = time.monotonic()
        attempts = (prev.get("attempts", 0) if same else 0) + 1
        use_llm = llm_left > 0
        extra = ["--llm", "--llm-max", str(min(args.llm_max, llm_left))] if use_llm else []
        result, err, net = run_tool(["suggest", p["sha"], "--json", "--max", str(args.max),
                                     "--cache-dir", args.cache_dir] + extra, args.timeout)
        net_total.update(net)
        llm_left -= net.get("llm", 0)
        summary, serr, _ = run_tool(["summary", p["sha"], "--json", "--cache-dir", args.cache_dir], 120)
        entry.update({"result": result, "error": err, "attempts": attempts, "tool_version": version,
                      "llm": use_llm,
                      "analysed_at": now(), "seconds": round(time.monotonic() - t0, 1),
                      "jobs": jobs_overview(summary) if summary else None})
        entry.pop("pending", None)
        stats["analysed" if result is not None else "failed"] += 1
        print(f"#{p['number']} {p['sha'][:12]} {'ok' if result is not None else 'ERROR: ' + err} "
              f"({entry['seconds']}s; requests: {', '.join(f'{k}={v}' for k, v in net.items()) or '?'})",
              file=sys.stderr)
        new_state[key] = entry
        save_state(args.state, {**old, **new_state})      # keep the progress if the run is killed

    stats["dropped"] = len(set(old) - set(new_state))
    save_state(args.state, new_state)

    data = {"generated": now(), "repository": f"{OWNER}/{NAME}", "open_prs": len(prs),
            "failing_prs": len([p for p in prs if p["ci_state"] == "FAILURE"]),
            "limit": args.limit, "max_failures_per_pr": args.max,
            "llm": {"enabled": llm, "calls": net_total.get("llm", 0)},
            "run": {"github_api_calls": list_calls, "tool_requests": dict(net_total), **stats},
            "prs": sorted(new_state.values(), key=lambda e: e["updated"], reverse=True)}
    os.makedirs(os.path.dirname(os.path.abspath(args.out)), exist_ok=True)
    with open(args.out, "w") as f:
        json.dump(data, f, indent=1)
    print(f"analysed {stats['analysed']}, failed {stats['failed']}, unchanged {stats['unchanged']}, "
          f"deferred {stats['deferred']}, dropped {stats['dropped']}; tool requests: "
          + (", ".join(f"{k}={v}" for k, v in net_total.items()) or "none") + f"; wrote {args.out}",
          file=sys.stderr)
    gh_out = os.environ.get("GITHUB_OUTPUT")
    if gh_out:
        with open(gh_out, "a") as f:
            f.write(f"changed={'true' if stats['analysed'] or stats['failed'] or stats['dropped'] else 'false'}\n")


if __name__ == "__main__":
    main()
