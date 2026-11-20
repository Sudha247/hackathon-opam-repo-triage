#!/usr/bin/env python3
"""Run `opam_repo_ci_triage.py suggest` on the open opam-repository PRs whose
opam-ci status is `failure`, and write the results to site/data.json.

  python3 scripts/triage_open_prs.py [--limit N] [--state state/prs.json] [--out site/data.json]

The open PRs and the opam-ci status of their head commit are listed with one
GitHub GraphQL query per 100 PRs (needs a token: GITHUB_TOKEN, GH_TOKEN or a
logged-in gh). A PR whose head sha and opam-ci status are unchanged since the
previous run is not analysed again: its result comes from the state file (after
a rebuild, the status changes and the PR is analysed from fresh CI results).
PRs that were closed, or whose CI is no longer `failure`, are dropped. PRs are
analysed one at a time, to respect opam.ci.ocaml.org's rate limit;
`--time-budget` stops starting new ones after that many seconds, and the next
run continues.
"""

import argparse
import collections
import datetime
import hashlib
import http.client
import importlib.util
import json
import os
import re
import subprocess
import sys
import tempfile
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
# Attempts expire a day after the last one: a timeout or a server that failed
# (ocaml.org, opam.ci.ocaml.org) may be gone by then.
ATTEMPTS_EXPIRE = 24 * 3600
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
        labels(first: 20) { nodes { name } }
        timelineItems(last: 30, itemTypes: [LABELED_EVENT, PULL_REQUEST_COMMIT, HEAD_REF_FORCE_PUSHED_EVENT,
                                            ISSUE_COMMENT]) {
          nodes {
            __typename
            ... on LabeledEvent { createdAt label { name } }
            ... on PullRequestCommit { commit { committedDate } }
            ... on HeadRefForcePushedEvent { createdAt }
            ... on IssueComment { createdAt author { login } }
          }
        }
        commits(last: 1) { nodes { commit { oid status { contexts { context state targetUrl createdAt } } } } }
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
            data = d.get("data")
            if d.get("errors"):
                # Partial errors (a node GitHub could not resolve): the data
                # is used when every top-level field is there.
                if not data or any(v is None for v in data.values()):
                    raise RuntimeError(f"GraphQL errors: {str(d['errors'])[:500]}")
                print(f"GitHub GraphQL errors, using the data returned: {str(d['errors'])[:300]}", file=sys.stderr)
            if not isinstance(data, dict):
                raise RuntimeError("GraphQL answer without data")
            return data
        except (OSError, http.client.HTTPException, ValueError, RuntimeError) as e:
            if attempt == 2:
                raise
            print(f"GitHub API error ({e}), retrying", file=sys.stderr)
            time.sleep(10 * (attempt + 1))


# Labels that ask the PR's author to act. Such a label is often left in place
# after the author has acted: then it may be stale.
AUTHOR_LABELS = ("needs reporter action",)


def stale_labels(node):
    """The labels of AUTHOR_LABELS the PR has, added before a new commit, a
    push or a comment by the PR's author. A label added before the last 30 timeline
    items is not known to be stale."""
    author = (node.get("author") or {}).get("login")
    items = ((node.get("timelineItems") or {}).get("nodes")) or []
    acts = []
    for i in items:
        t = i.get("__typename")
        if t == "PullRequestCommit":
            acts.append(((i.get("commit") or {}).get("committedDate")))
        elif t == "HeadRefForcePushedEvent":
            acts.append(i.get("createdAt"))
        elif t == "IssueComment" and author and (i.get("author") or {}).get("login") == author:
            acts.append(i.get("createdAt"))
    last_act = max((a for a in acts if a), default=None)
    current = {lb.get("name") for lb in ((node.get("labels") or {}).get("nodes") or [])}
    out = []
    for name in AUTHOR_LABELS:
        if name not in current:     # added, then removed
            continue
        added = [i.get("createdAt") for i in items
                 if i.get("__typename") == "LabeledEvent" and (i.get("label") or {}).get("name") == name]
        if added and last_act and last_act > max(added):
            out.append(name)
    return out


def list_open_prs(token):
    """All open PRs with the opam-ci state of their head commit; (prs, number of requests)."""
    prs, cursor, calls = [], None, 0
    while True:
        calls += 1
        page = graphql(QUERY, {"owner": OWNER, "name": NAME, "cursor": cursor}, token)
        conn = page["repository"]["pullRequests"]
        for n in conn["nodes"]:
            if not n:           # left out of a partial answer
                continue
            ci = None
            commits = (n.get("commits") or {}).get("nodes") or []
            head = commits[0]["commit"] if commits else {}
            if head.get("oid") == n["headRefOid"]:
                for c in (head.get("status") or {}).get("contexts", []):
                    if c["context"] == CONTEXT:
                        ci = c
            prs.append({"number": n["number"], "title": n["title"], "url": n["url"], "draft": n["isDraft"],
                        "updated": n["updatedAt"], "sha": n["headRefOid"],
                        "author": (n.get("author") or {}).get("login"),
                        "labels": [lb["name"] for lb in (n.get("labels") or {}).get("nodes") or []],
                        "stale_labels": stale_labels(n),
                        "ci_state": (ci or {}).get("state"), "ci_url": (ci or {}).get("targetUrl"),
                        "ci_created": (ci or {}).get("createdAt")})
        if not conn["pageInfo"]["hasNextPage"]:
            return prs, calls
        cursor = conn["pageInfo"]["endCursor"]


def attempts_used(prev, now_=None):
    """The analyses of `prev`'s commit made so far that count toward
    MAX_ATTEMPTS: none once ATTEMPTS_EXPIRE has passed since the last one."""
    if not prev:
        return 0
    try:
        last = datetime.datetime.fromisoformat(prev["analysed_at"]).timestamp()
    except (KeyError, TypeError, ValueError):
        last = None
    if last is not None and (now_ or time.time()) - last >= ATTEMPTS_EXPIRE:
        return 0
    return prev.get("attempts", 0)


def ci_changed(prev, p):
    """Whether the opam-ci status of the same commit changed since the
    previous analysis: another state, or a status posted again (a maintainer's
    "Rebuild failed", even one that finished within the hour). The CI page and
    the job logs are then fetched again (`--refresh`): the cache would give
    the results of the previous build."""
    if prev is None or prev.get("sha") != p["sha"]:
        return False
    newer = bool(p.get("ci_created") and prev.get("ci_created") and p["ci_created"] > prev["ci_created"])
    return newer or prev.get("ci_state") != p["ci_state"]


def reuse(prev, p, version, llm, now_=None):
    """Whether the previous analysis of PR `p` can be kept: (same, same_commit,
    mcp_retry). `same`: it describes the current state and need not be redone
    (a failed one is redone while attempts_used() < MAX_ATTEMPTS);
    `same_commit`: it has a result for the same commit, shown until it is
    redone; `mcp_retry`: it is redone because ocaml.org's MCP server (or
    another server) failed during it."""
    # A PR whose CI is running is analysed again on every run, until its CI finishes.
    same = (prev is not None and prev.get("sha") == p["sha"] and prev.get("tool_version") == version
            and not ci_changed(prev, p) and p["ci_state"] != "PENDING")
    same_commit = same and prev.get("result") is not None
    # With the LLM on, a PR analysed without it (key added later, or the
    # run's budget spent) is analysed again; the logs come from the cache.
    if same and llm and not prev.get("llm"):
        same = False
    # An analysis made while ocaml.org's MCP server failed lacks versions,
    # docs or health-check results: done again, a few times a day at most.
    mcp_retry = bool(same and (prev.get("mcp_errors") or prev.get("net_errors"))
                     and attempts_used(prev, now_) < MAX_ATTEMPTS)
    if mcp_retry:
        same = False
    return same, same_commit, mcp_retry


def keep_failed(prev, now_=None):
    """Whether a failed analysis (no result) of the same commit is kept
    rather than tried again."""
    return attempts_used(prev, now_) >= MAX_ATTEMPTS


def read_counts(path):
    """The request counts the tool wrote to `path` (OPAM_CI_NET_FILE), or {}."""
    try:
        with open(path) as f:
            d = json.load(f)
        return {k: v for k, v in d.items() if isinstance(v, int)} if isinstance(d, dict) else {}
    except (OSError, ValueError):
        return {}


def run_tool(args, timeout, tool=None):
    """Run the triage tool; (parsed JSON or None, error message or None, request
    counts). The counts come from a file the tool updates as it goes, so they
    are known after a timeout too (with "timeout": 1 added)."""
    fd, counts = tempfile.mkstemp(prefix="opam-ci-net-", suffix=".json")
    os.close(fd)
    try:
        try:
            p = subprocess.run([sys.executable, tool or TOOL] + args, cwd=ROOT, capture_output=True, text=True,
                               timeout=timeout, env={**os.environ, "OPAM_CI_NET_FILE": counts})
        except subprocess.TimeoutExpired:
            return None, f"timeout after {timeout}s", {**read_counts(counts), "timeout": 1}
        net = read_counts(counts)
    finally:
        os.remove(counts)
    m = re.search(r"network requests: (.*)", p.stderr)
    if m and not net:
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


SHARED_MIN = 3   # other open PRs a failure must also appear in to be marked


def mark_shared_failures(prs):
    """Mark the suggestions about reverse dependencies that also fail in at
    least SHARED_MIN other open PRs: such a failure is not caused by this PR.
    Uses the failing reverse dependencies of every PR (from `summary`). Every
    job of the suggestion must be a reverse-dependency job, and the same other
    PRs must fail on each of its reverse dependencies: three revdeps each
    failing in one other PR do not make a failure shared by three PRs."""
    where = collections.defaultdict(set)
    for e in prs.values():
        for rd in ((e.get("jobs") or {}).get("failing_revdeps") or {}):
            where[rd].add(int(e["number"]))
    for e in prs.values():
        n = int(e["number"])
        for r in (e.get("result") or {}).get("suggestions", []):
            found = [re.search(r"revdeps,([^,]+)", v or "") for v in (r.get("jobs") or [r.get("variant") or ""])]
            others = []
            if found and all(found):
                others = sorted(set.intersection(*(where[m.group(1)] for m in found)) - {n})
            if len(others) >= SHARED_MIN:
                r["also_failing_in"] = others
            else:
                r.pop("also_failing_in", None)


def since_last_push(prev, result):
    """What changed between the analysis of a PR's previous head and this
    one: the problems gone, the new ones, and how many remain. Problems are
    (rule, failing package) pairs, shown with their title (a new wording of
    a title is not a new problem); reruns and unrecognised failures are left
    out, since they come and go with CI."""
    def keys(res):
        return {(s.get("rule") or s.get("title") or "", s.get("failing") or s.get("job") or ""):
                s.get("title") or s.get("rule") or ""
                for s in (res or {}).get("suggestions", [])
                if s.get("rule") not in ("rerun", "unrecognised", "lint")}
    before, after = keys(prev.get("result")), keys(result)
    fmt = lambda ks, names: sorted(f"{names[k]}: {k[1]}" if k[1] else names[k] for k in ks)
    return {"previous_sha": prev.get("sha"), "fixed": fmt(before.keys() - after.keys(), before),
            "new": fmt(after.keys() - before.keys(), after), "still": len(before.keys() & after.keys())}


def llm_available():
    """The LLM fallback runs only with an API key and the anthropic package;
    without them the page is built from the rules alone."""
    return bool(os.environ.get("ANTHROPIC_API_KEY")) and importlib.util.find_spec("anthropic") is not None


def tool_version():
    """A hash of the tool's code (the entry point and the triage/ package):
    PRs are analysed again when it changes."""
    h = hashlib.sha256()
    pkg = os.path.join(ROOT, "triage")
    for path in [TOOL] + sorted(os.path.join(pkg, f) for f in os.listdir(pkg) if f.endswith(".py")):
        with open(path, "rb") as f:
            h.update(f.read())
    return h.hexdigest()[:12]


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
                    help="LLM calls per PR at most, for failures the tool cannot explain (needs ANTHROPIC_API_KEY)")
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
    # Failed CI, and CI still running (shown if some jobs have already failed).
    failing = [p for p in prs if p["ci_state"] in ("FAILURE", "PENDING")]
    print(f"{len(prs)} open PRs, {sum(p['ci_state'] == 'FAILURE' for p in prs)} with {CONTEXT} = failure, "
          f"{sum(p['ci_state'] == 'PENDING' for p in prs)} running ({list_calls} GitHub API call(s))",
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
        entry.update({k: p[k] for k in ("number", "title", "url", "draft", "updated", "sha", "author", "labels",
                                         "stale_labels")})
        entry["ci_url"] = p["ci_url"] or BASE + p["sha"]
        same, same_commit, mcp_retry = reuse(prev, p, version, llm)
        if same and (prev.get("result") is not None or keep_failed(prev)):
            stats["unchanged"] += 1
            new_state[key] = entry
            continue
        if args.time_budget and time.monotonic() - start > args.time_budget:
            stats["deferred"] += 1
            if same_commit:     # to redo (LLM, MCP errors): keep showing the result meanwhile
                new_state[key] = entry
                continue
            if not same:        # an older result would describe another commit
                for k in ("result", "error", "jobs", "analysed_at", "attempts", "tool_version"):
                    entry.pop(k, None)
            entry["pending"] = True
            new_state[key] = entry
            continue
        t0 = time.monotonic()
        summary = None
        # Jobs rebuilt since the previous analysis: the first run of the tool
        # fetches the CI page again and drops the cached logs of its jobs. Also
        # after an analysis made while CI was running: a job whose log was read
        # then may have been rebuilt since ("Rebuild failed" during the run).
        refresh = ["--refresh"] if ci_changed(prev, p) else []
        if refresh:
            stats["refreshed"] += 1
        if p["ci_state"] == "PENDING":
            # Only worth showing once some job has failed: `summary` reads the
            # CI page (not cached while CI runs) and gives the counts.
            summary, serr, _ = run_tool(["summary", p["sha"], "--json", "--cache-dir", args.cache_dir] + refresh,
                                        120)
            refresh = []
            failed = [j for j in (summary or {}).get("jobs", []) if j.get("status") == "failed"
                      and j.get("category") not in NOISE]
            if not failed:
                stats["running, no failure yet"] += 1
                continue
        attempts = (attempts_used(prev) if same or mcp_retry else 0) + 1
        use_llm = llm_left > 0
        extra = ["--llm", "--llm-max", str(min(args.llm_max, llm_left))] if use_llm else []
        result, err, net = run_tool(["suggest", p["sha"], "--json", "--max", str(args.max),
                                     "--cache-dir", args.cache_dir] + extra + refresh, args.timeout)
        net_total.update(net)
        llm_left -= net.get("llm", 0)
        if net.get("timeout") and use_llm:      # an LLM call may have been under way: count it
            llm_left -= 1
        if summary is None:
            summary, serr, _ = run_tool(["summary", p["sha"], "--json", "--cache-dir", args.cache_dir], 120)
        if prev and prev.get("sha") != p["sha"] and prev.get("result") is not None and result is not None:
            entry["since_last_push"] = since_last_push(prev, result)
        elif prev and prev.get("sha") != p["sha"]:
            entry.pop("since_last_push", None)      # no comparable previous result
        entry.update({"ci_state": p["ci_state"], "ci_created": p.get("ci_created"),
                      "ci_unfinished": (result or {}).get("ci_unfinished", 0)})
        entry.update({"result": result, "error": err, "attempts": attempts, "tool_version": version,
                      "llm": use_llm, "mcp_errors": net.get("mcp-errors", 0),
                      "net_errors": net.get("net-errors", 0) + net.get("timeout", 0),
                      "analysed_at": now(), "seconds": round(time.monotonic() - t0, 1),
                      "jobs": jobs_overview(summary) if summary else None})
        entry.pop("pending", None)
        stats["analysed" if result is not None else "failed"] += 1
        print(f"#{p['number']} {p['sha'][:12]} {'ok' if result is not None else 'ERROR: ' + err} "
              f"({entry['seconds']}s; requests: {', '.join(f'{k}={v}' for k, v in net.items()) or '?'})",
              file=sys.stderr)
        new_state[key] = entry
        save_state(args.state, {**old, **new_state})      # keep the progress if the run is killed

    mark_shared_failures(new_state)
    stats["dropped"] = len(set(old) - set(new_state))
    save_state(args.state, new_state)

    data = {"generated": now(), "repository": f"{OWNER}/{NAME}", "open_prs": len(prs),
            "failing_prs": len([p for p in prs if p["ci_state"] == "FAILURE"]),
            "running_prs": len([p for p in new_state.values() if p.get("ci_state") == "PENDING"]),
            "limit": args.limit, "max_failures_per_pr": args.max,
            "llm": {"enabled": llm, "calls": net_total.get("llm", 0)},
            "run": {"github_api_calls": list_calls, "tool_requests": dict(net_total), **stats},
            "prs": sorted(new_state.values(), key=lambda e: e["updated"], reverse=True),
            # PRs whose CI passes: nothing to analyse, listed so that the page
            # can say which PRs seem ready.
            "passing": [{k: p[k] for k in ("number", "title", "url", "draft", "updated", "author", "labels",
                                           "stale_labels")}
                        for p in prs if p["ci_state"] == "SUCCESS"]}
    os.makedirs(os.path.dirname(os.path.abspath(args.out)), exist_ok=True)
    with open(args.out, "w") as f:
        json.dump(data, f, indent=1)
    print(f"analysed {stats['analysed']}, failed {stats['failed']}, unchanged {stats['unchanged']}, "
          f"running without failure {stats['running, no failure yet']}, "
          f"deferred {stats['deferred']}, dropped {stats['dropped']}; tool requests: "
          + (", ".join(f"{k}={v}" for k, v in net_total.items()) or "none") + f"; wrote {args.out}",
          file=sys.stderr)
    gh_out = os.environ.get("GITHUB_OUTPUT")
    if gh_out:
        with open(gh_out, "a") as f:
            f.write(f"changed={'true' if stats['analysed'] or stats['failed'] or stats['dropped'] else 'false'}\n")


if __name__ == "__main__":
    main()
