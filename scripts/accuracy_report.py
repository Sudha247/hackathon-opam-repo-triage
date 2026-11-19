#!/usr/bin/env python3
"""Compare an `evaluate_closed_prs.py --json` result with the previous one,
and record it.

  python3 scripts/accuracy_report.py eval.json --history history/ [--commit SHA]

Reads history/latest.json (the previous evaluation, if any), prints a
Markdown report, appends a line to history/history.jsonl and replaces
history/latest.json with this evaluation.

Two comparisons:

- the totals (precision: suggested changes matching the author's fix;
  recall: the author's changes the suggestions anticipated). They depend on
  which PRs were merged recently, so they move from week to week;
- the PRs evaluated both times (same initial commit): a suggestion that
  matched the author's fix last time and does not any more is a regression
  of the tool, whatever the sample.

Exits with status 1 on a regression, or when precision drops by
--max-drop points or more (with at least --min-scored suggestions), so that
a scheduled job fails and GitHub notifies.
"""

import argparse
import datetime
import json
import os
import sys


def rates(t):
    p = 100 * t["hit"] / t["scored"] if t.get("scored") else None
    r = 100 * t["fix_covered"] / t["fix_tokens"] if t.get("fix_tokens") else None
    return p, r


def hits(row):
    """The suggestions of a PR that matched the author's fix, as comparable keys."""
    return {(s["package"], s["rule"], tuple(s["tokens"])) for s in row.get("suggestions", []) if s.get("hit")}


def pct(x):
    return "n/a" if x is None else f"{x:.0f}%"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("current", help="the JSON written by evaluate_closed_prs.py --json")
    ap.add_argument("--history", required=True, help="directory with history.jsonl and latest.json")
    ap.add_argument("--commit", default="", help="commit of the tool that was evaluated")
    ap.add_argument("--max-drop", type=float, default=15, help="precision drop (points) that fails")
    ap.add_argument("--min-scored", type=int, default=10, help="suggestions needed to compare precision")
    args = ap.parse_args()

    with open(args.current) as f:
        cur = json.load(f)
    os.makedirs(args.history, exist_ok=True)
    latest = os.path.join(args.history, "latest.json")
    prev = None
    if os.path.exists(latest):
        with open(latest) as f:
            prev = json.load(f)

    t = cur["totals"]
    p, r = rates(t)
    rows = [x for x in cur["prs"] if "error" not in x]
    errors = [x for x in cur["prs"] if "error" in x]
    out = ["## Accuracy of the triage tool on merged PRs", "",
           f"{len(rows)} PRs whose first version failed opam-ci and was fixed before the merge"
           + (f" ({len(errors)} could not be analysed)" if errors else "") + ".", "",
           "| | This run | Previous run |", "|---|---|---|"]
    pp = pr_ = None
    if prev:
        pp, pr_ = rates(prev["totals"])
    out.append(f"| Suggested changes matching the author's fix (precision) | {t['hit']}/{t['scored']} "
               f"({pct(p)}) | " + (f"{prev['totals']['hit']}/{prev['totals']['scored']} ({pct(pp)})"
                                   if prev else "none") + " |")
    out.append(f"| Author's changes anticipated (recall) | {t['fix_covered']}/{t['fix_tokens']} ({pct(r)}) | "
               + (f"{prev['totals']['fix_covered']}/{prev['totals']['fix_tokens']} ({pct(pr_)})"
                  if prev else "none") + " |")

    failed = []
    if prev:
        before = {(x["pr"], x.get("initial")): x for x in prev["prs"] if "error" not in x}
        lost, gained, common = [], [], 0
        for x in rows:
            y = before.get((x["pr"], x.get("initial")))
            if y is None:
                continue
            common += 1
            for k in sorted(hits(y) - hits(x)):
                lost.append((x["pr"], k))
            for k in sorted(hits(x) - hits(y)):
                gained.append((x["pr"], k))
        out += ["", f"PRs evaluated both times: {common}."]
        if gained:
            out.append("Suggestions that now match the author's fix: "
                       + ", ".join(f"#{n} {k[0]} ({k[1]})" for n, k in gained) + ".")
        if lost:
            out.append("**Regressions**, suggestions that matched the author's fix and no longer do: "
                       + ", ".join(f"#{n} {k[0]} ({k[1]})" for n, k in lost) + ".")
            failed.append(f"{len(lost)} regression(s) on PRs evaluated both times")
        if p is not None and pp is not None and t["scored"] >= args.min_scored and pp - p >= args.max_drop:
            failed.append(f"precision dropped from {pct(pp)} to {pct(p)}")
    for x in errors:
        out.append(f"- #{x['pr']}: the tool failed: `{x['error'][-150:]}`")
    if failed:
        out += ["", "**Failed:** " + "; ".join(failed) + "."]
    print("\n".join(out))

    record = {"date": datetime.date.today().isoformat(), "commit": args.commit, "prs": len(rows),
              "errors": len(errors), **t, "precision": p, "recall": r, "failed": failed}
    with open(os.path.join(args.history, "history.jsonl"), "a") as f:
        f.write(json.dumps(record) + "\n")
    with open(latest, "w") as f:
        json.dump(cur, f, indent=1)
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
