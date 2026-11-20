#!/usr/bin/env python3
"""Evaluate `opam_repo_ci_triage.py suggest` on merged opam-repository PRs.

For each recently merged PR whose *initial* version (the commit before the
first force-push, or else the first commit) failed opam-ci and was then
changed before the merge:
  1. run `suggest` on the initial commit,
  2. diff the opam files between the initial and the merged version (the fix
     the author actually made),
  3. count a suggestion as a hit when the author changed the same dependency
     or field of the same package. A new upstream archive (checksum,
     x-commit-hash) is counted apart: it is not a change the tool suggests.

Usage:
  python3 evaluate_closed_prs.py [--since 2026-08-01] [--limit 150] [--json out.json]

Needs a logged-in `gh` (one GraphQL query per 50 PRs; for each candidate,
its list of files and one GraphQL query per 50 opam files). opam-repo-ci
deletes old runs, so only recent PRs can be used.
"""

import argparse
import difflib
import json
import re
import subprocess
import sys

REPO = "ocaml/opam-repository"
TOOL = "opam_repo_ci_triage.py"

QUERY = """
query($q: String!, $after: String) {
  search(query: $q, type: ISSUE, first: 50, after: $after) {
    pageInfo { hasNextPage endCursor }
    nodes { ... on PullRequest {
      number title mergedAt headRefOid
      commits(first: 1) { totalCount nodes { commit { oid status { contexts { context state } } } } }
      timelineItems(itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT], first: 1) {
        nodes { ... on HeadRefForcePushedEvent { beforeCommit { oid status { contexts { context state } } } } }
      }
    } }
  }
}"""


def gh(args, stdin=None):
    out = subprocess.run(["gh"] + args, input=stdin, capture_output=True, text=True)
    if out.returncode != 0:
        raise RuntimeError(out.stderr.strip()[:300])
    return json.loads(out.stdout)


def opam_ci_state(commit):
    for c in ((commit or {}).get("status") or {}).get("contexts", []):
        if c["context"] == "opam-ci":
            return c["state"].lower()      # GraphQL: FAILURE, REST: failure
    return None


def merged_prs(since, limit):
    q = f"repo:{REPO} is:pr is:merged merged:>={since}"
    after, prs = None, []
    while len(prs) < limit:
        args = ["api", "graphql", "-f", f"query={QUERY}", "-f", f"q={q}"]
        if after:
            args += ["-f", f"after={after}"]
        d = gh(args)["data"]["search"]
        prs += [n for n in d["nodes"] if n]
        if not d["pageInfo"]["hasNextPage"]:
            break
        after = d["pageInfo"]["endCursor"]
    return prs[:limit]


def initial_commit(pr):
    fp = pr["timelineItems"]["nodes"]
    if fp and fp[0].get("beforeCommit"):
        return fp[0]["beforeCommit"]
    nodes = pr["commits"]["nodes"]
    return nodes[0]["commit"] if nodes else None


def line_tokens(line):
    """What a changed opam line is about: a dependency name or a field name."""
    t = line.strip()
    m = re.match(r'"([\w+-]+)"', t)
    if m:
        return {"dep:" + m.group(1)}
    m = re.match(r"([a-z][\w-]*):", t)
    if m:
        return {"field:" + m.group(1)}
    toks = set()
    if "@runtest" in t or "with-test" in t:
        toks.add("field:runtest")
    if re.search(r"(sha256|sha512|md5)=", t):
        toks.add("field:checksum")
    return toks


def pr_files(number):
    """The files of a PR's merged version (relative to its own base, so not
    what master gained meanwhile)."""
    out = subprocess.run(["gh", "api", "--paginate", f"repos/{REPO}/pulls/{number}/files?per_page=100",
                          "--jq", ".[].filename"], capture_output=True, text=True)
    if out.returncode != 0:
        raise RuntimeError(out.stderr.strip()[:300])
    return out.stdout.split()


def blobs(commit, paths):
    """{path: text} of the files that exist at `commit` (GraphQL, 50 paths per query)."""
    out = {}
    for i in range(0, len(paths), 50):
        chunk = paths[i:i + 50]
        fields = " ".join(f"b{k}: object(expression: {json.dumps(commit + ':' + p)}) {{ ... on Blob {{ text }} }}"
                          for k, p in enumerate(chunk))
        q = f'query {{ repository(owner: "{REPO.split("/")[0]}", name: "{REPO.split("/")[1]}") {{ {fields} }} }}'
        repo = gh(["api", "graphql", "-f", f"query={q}"])["data"]["repository"]
        out.update({p: repo[f"b{k}"]["text"] for k, p in enumerate(chunk)
                    if repo.get(f"b{k}") and repo[f"b{k}"].get("text") is not None})
    return out


def actual_fix(number, initial, final):
    """{package.version: set(tokens)} changed in the PR's opam files between
    its initial and merged versions. Each file is compared as a whole: a
    compare of the two commits would go through their merge base, which for
    a PR rebased before its force-push shows the PR's files as added and
    master's changes as the author's."""
    paths = [f for f in pr_files(number) if re.match(r"packages/[^/]+/[^/]+/opam$", f)]
    old, new = blobs(initial, paths), blobs(final, paths)
    changed = {}
    for path in paths:
        if path not in old or path not in new:
            continue      # packages added/removed between the two versions are not fixes
        toks = set()
        for line in difflib.unified_diff(old[path].splitlines(), new[path].splitlines(), lineterm="", n=0):
            if line[:1] in "+-" and not line.startswith(("+++", "---")):
                toks |= line_tokens(line[1:])
        changed[path.split("/")[2]] = toks
    return changed


# A new upstream archive (a fix made upstream, then released again): not a
# change of the opam file that the tool could suggest, so not in the recall.
NEW_ARCHIVE = {"field:checksum", "field:x-commit-hash"}


def failing_packages(sha):
    """Packages of the PR that failed in that CI run: non-experimental build,
    test or solver failures, or lint errors."""
    out = subprocess.run([sys.executable, TOOL, "summary", sha, "--json"], capture_output=True, text=True)
    try:
        jobs = json.loads(out.stdout)["jobs"]
    except (ValueError, KeyError):
        return set()
    failed = {j["target"] for j in jobs if j["kind"] in ("build", "tests", "lower-bounds")
              and not j["experimental"] and j["category"] in ("build", "build-or-test", "dependency", "solver")}
    for j in jobs:
        for m in j.get("messages", []):
            mm = re.match(r"Error in (\S+?):", m)
            if mm:
                failed.add(mm.group(1))
    return failed


def suggestion_tokens(s):
    """What a suggestion would change: dependency or field tokens."""
    toks = set()
    if s.get("dep"):
        toks.add("dep:" + s["dep"])
    for c in s.get("candidates", []):
        toks.add("dep:" + c["dep"])
    alt = re.match(r'"?([\w+-]+)"?[: ]', s.get("alternative") or "")
    if alt:
        toks.add(("field:" if (s.get("alternative") or "").split(" ", 1)[0].endswith(":") else "dep:") + alt.group(1))
    new = s.get("new") or ""
    m = re.match(r'"([\w+-]+)"', new)
    if m:
        toks.add("dep:" + m.group(1))
    m = re.match(r"([a-z][\w-]*):", new)
    if m:
        toks.add("field:" + m.group(1))
    if "@runtest" in new:
        toks.add("field:runtest")
    if new.startswith("checksum"):
        toks.add("field:checksum")
    if new.startswith("extra-source"):
        toks |= {"field:extra-source", "field:extra-files"}
    if "%{_:" in new:
        toks |= {"field:build", "field:install", "field:remove"}
    return toks


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--since", default="2026-08-01")
    ap.add_argument("--limit", type=int, default=150)
    ap.add_argument("--max", type=int, default=6, help="passed to suggest")
    ap.add_argument("--json")
    args = ap.parse_args()

    prs = merged_prs(args.since, args.limit)
    cands = []
    for pr in prs:
        init = initial_commit(pr)
        if init and init["oid"] != pr["headRefOid"] and opam_ci_state(init) == "failure":
            cands.append((pr, init["oid"]))
    print(f"{len(prs)} merged PRs since {args.since}; {len(cands)} whose initial version failed opam-ci "
          f"and was changed before the merge", file=sys.stderr)

    rows, totals = [], {"scored": 0, "hit": 0, "fix_tokens": 0, "fix_covered": 0, "new_archive": 0}
    for pr, init in cands:
        fix = actual_fix(pr["number"], init, pr["headRefOid"])
        fix = {k: v for k, v in fix.items() if v}
        if not fix:
            continue        # the author changed nothing in opam files (e.g. CI rerun only)
        out = subprocess.run([sys.executable, TOOL, "suggest", init, "--max", str(args.max), "--json"],
                             capture_output=True, text=True)
        try:
            sugg = json.loads(out.stdout)["suggestions"]
        except (ValueError, KeyError):
            rows.append({"pr": pr["number"], "error": out.stderr.strip()[-200:]})
            continue
        scored = []
        for s in sugg:
            if s.get("fix_in") != "this PR" or not (s.get("new") or s.get("candidates")):
                continue
            pkg = s.get("failing")
            toks = suggestion_tokens(s)
            hit = bool(toks & fix.get(pkg, set()))
            scored.append({"package": pkg, "rule": s.get("rule"), "new": (s.get("new") or "")[:90],
                           "hit": hit, "tokens": sorted(toks)})
        # Recall: only the author's changes in packages that failed in the
        # initial run (rebases, version bumps and metadata edits elsewhere are
        # not fixes of a CI failure).
        failed = failing_packages(init)
        fix_failed = {p: ts - NEW_ARCHIVE for p, ts in fix.items() if p in failed}
        totals["new_archive"] += sum(1 for p, ts in fix.items() if p in failed and ts & NEW_ARCHIVE)
        fix_failed = {p: ts for p, ts in fix_failed.items() if ts}
        covered = {(p, t) for p, ts in fix_failed.items() for t in ts
                   if any(x["package"] == p and t in x["tokens"] for x in scored)}
        n_fix = sum(len(v) for v in fix_failed.values())
        totals["scored"] += len(scored)
        totals["hit"] += sum(x["hit"] for x in scored)
        totals["fix_tokens"] += n_fix
        totals["fix_covered"] += len(covered)
        rows.append({"pr": pr["number"], "title": pr["title"], "initial": init, "final": pr["headRefOid"],
                     "actual_fix": {k: sorted(v) for k, v in fix.items()},
                     "fix_in_failing_packages": {k: sorted(v) for k, v in fix_failed.items()}, "suggestions": scored,
                     "explanations": sum(1 for s in sugg if not (s.get("new") or s.get("candidates")))})

    for r in rows:
        if "error" in r:
            print(f"\n#{r['pr']}: suggest failed: {r['error']}")
            continue
        print(f"\n#{r['pr']} {r['title'][:70]}")
        for p, toks in r["actual_fix"].items():
            print(f"  author changed {p}: {', '.join(toks)}")
        for s in r["suggestions"]:
            print(f"  {'HIT ' if s['hit'] else 'miss'} [{s['rule']}] {s['package']}: {s['new']}")
        if not r["suggestions"]:
            print(f"  (no suggested change; {r['explanations']} explanations)")
    t = totals
    print(f"\nPRs evaluated: {len([r for r in rows if 'error' not in r])}")
    print(f"suggested changes to the PR's own files: {t['scored']}, matching the author's fix: {t['hit']}"
          + (f" ({100 * t['hit'] // t['scored']}%)" if t["scored"] else ""))
    print(f"author's changes in packages that failed (dependency/field per package): {t['fix_tokens']}, "
          f"anticipated: {t['fix_covered']}"
          + (f" ({100 * t['fix_covered'] // t['fix_tokens']}%)" if t["fix_tokens"] else ""))
    print(f"packages that failed and got a new upstream archive (not counted above): {t['new_archive']}")
    if args.json:
        with open(args.json, "w") as f:
            json.dump({"totals": t, "prs": rows}, f, indent=2)


if __name__ == "__main__":
    main()
