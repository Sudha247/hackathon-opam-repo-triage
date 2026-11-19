#!/usr/bin/env python3
"""Run `suggest --llm` on the PRs of the triage page that have failures no rule
recognises, and save each result for grading.

  .venv/bin/python scripts/llm_eval.py --first 5        # try 5 PRs, look at the token counts
  .venv/bin/python scripts/llm_eval.py                  # the rest (PRs already done are skipped)
  .venv/bin/python scripts/llm_eval.py --dry-run        # list the PRs and the number of prompts, no API call

Needs the anthropic package and ANTHROPIC_API_KEY (see the README). Each
distinct failure is one call to the model, at most --llm-max per PR; answers
are cached in the tool's cache directory, so running again costs nothing.
Results go to llm-eval/<PR>.json (and .err); a summary of the tokens used is
printed after each PR and at the end.
"""

import argparse
import json
import os
import subprocess
import sys
import urllib.request

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TOOL = os.path.join(ROOT, "opam_repo_ci_triage.py")
PAGE_DATA = "https://sudha247.github.io/hackathon-opam-repo-triage/data.json"


def load(source):
    if source.startswith("https://"):
        req = urllib.request.Request(source, headers={"User-Agent": "opam-ci-triage"})
        with urllib.request.urlopen(req, timeout=60) as r:
            return json.load(r)
    with open(source) as f:
        return json.load(f)


def unrecognised(pr):
    """Number of failures of a PR that no rule recognises, in the page's data."""
    n = 0
    for r in (pr.get("result") or {}).get("suggestions", []):
        if r.get("rule") == "unrecognised":
            n += 1
        elif not r.get("rule") and not r.get("new") and not r.get("candidates"):
            n += 1
    return n


def usage(result):
    calls = [r["llm_usage"] for r in result.get("suggestions", []) if r.get("llm_usage")]
    # Failures sharing one answer carry the same usage: count each answer once.
    uniq = {(u["input"], u["output"], u["model"]) for u in calls}
    return len(uniq), sum(u[0] for u in uniq), sum(u[1] for u in uniq)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--data", default=PAGE_DATA, help="the page's data.json (URL or file)")
    ap.add_argument("--out", default=os.path.join(ROOT, "llm-eval"))
    ap.add_argument("--first", type=int, help="only the first N selected PRs")
    ap.add_argument("--llm-max", type=int, default=2, help="LLM calls per PR at most")
    ap.add_argument("--max", type=int, help="distinct failures analysed per PR (default: the page's)")
    ap.add_argument("--dry-run", action="store_true", help="build the prompts but call no API")
    args = ap.parse_args()

    data = load(args.data)
    prs = [p for p in data.get("prs", []) if p.get("sha") and unrecognised(p)]
    prs.sort(key=lambda p: -unrecognised(p))
    if args.first:
        prs = prs[:args.first]
    os.makedirs(args.out, exist_ok=True)
    max_logs = str(args.max or data.get("max_failures_per_pr") or 6)
    total = [0, 0, 0]
    print(f"{len(prs)} PRs with failures no rule recognises (data from {data.get('generated', '?')})")
    for p in prs:
        n = int(p["number"])
        out = os.path.join(args.out, f"{n}.json")
        if os.path.exists(out) and not args.dry_run:
            with open(out) as f:
                res = json.load(f)
            note = "already done"
        else:
            cmd = [sys.executable, TOOL, "suggest", p["sha"], "--json", "--max", max_logs,
                   "--llm-max", str(args.llm_max), "--llm-dry-run" if args.dry_run else "--llm"]
            proc = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, timeout=1800)
            try:
                res = json.loads(proc.stdout)
            except json.JSONDecodeError:
                print(f"#{n}: failed: {proc.stderr.strip().splitlines()[-1:] or proc.returncode}")
                continue
            res["pr_title"] = p.get("title")
            if not args.dry_run:
                with open(out, "w") as f:
                    json.dump(res, f, indent=1)
                with open(out[:-5] + ".err", "w") as f:
                    f.write(proc.stderr)
            note = "dry run" if args.dry_run else proc.stderr.strip().splitlines()[-1] if proc.stderr.strip() else ""
        sugg = res.get("suggestions", [])
        if args.dry_run:
            print(f"#{n}: {sum(1 for r in sugg if r.get('llm_prompt'))} prompts  {p.get('title', '')[:60]}")
            continue
        calls, tin, tout = usage(res)
        total = [total[0] + calls, total[1] + tin, total[2] + tout]
        answered = sum(1 for r in sugg if r.get("rule") == "llm")
        print(f"#{n}: {answered} failures answered, {calls} new answers, {tin} input and {tout} output "
              f"tokens  ({note})")
    if not args.dry_run:
        print(f"total: {total[0]} answers, {total[1]} input tokens, {total[2]} output tokens "
              f"(answers from the cache count too)")


if __name__ == "__main__":
    main()
