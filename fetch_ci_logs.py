#!/usr/bin/env python3
"""Download the individual job logs from an opam-repository CI (opam.ci.ocaml.org) commit page.

Usage:
    python3 fetch_ci_logs.py https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/<sha>

By default only jobs with status "failed" are fetched (skipped/passed jobs are ignored).
Logs are written as plain text to ./ci-logs/<sha>/, together with:
    jobs.json    - every job listed on the page (variant, status, label, url, log file, errors)
    summary.txt  - failing jobs grouped by root cause
"""

import argparse
import html
import json
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

BASE = "https://opam.ci.ocaml.org"
# The site returns 403 to the default urllib/curl user agent.
HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; opam-ci-log-fetcher)"}
# Shown (with HTTP 200) when the site is rate limiting.
BUSY_MARKER = "The site is busy"

JOB_RE = re.compile(
    r'<li class="(?P<status>[a-z_]+)"><a href="(?P<href>[^"]*/variant/[^"]*)">(?P<label>.*?)</a>',
    re.S,
)
# Log lines that indicate a CI worker problem rather than a package problem.
INFRA_PATTERNS = ("runc run failed", "special device overlay does not exist", "No space left on device")
COMMIT_URL_RE =re.compile(r"^/github/[^/]+/[^/]+/commit/[0-9a-f]{7,40}/?$")


def fetch(url, retries=6, backoff=3.0, timeout=60):
    """GET a URL, retrying on errors and on the site's 'busy' page."""
    last = None
    for attempt in range(retries):
        try:
            req = urllib.request.Request(url, headers=HEADERS)
            with urllib.request.urlopen(req, timeout=timeout) as resp:
                body = resp.read().decode("utf-8", errors="replace")
            if BUSY_MARKER not in body:
                return body
            last = "site busy (rate limited)"
        except (urllib.error.URLError, TimeoutError, ConnectionError) as e:
            last = str(e)
            if isinstance(e, urllib.error.HTTPError) and e.code == 404:
                break
        time.sleep(backoff * (attempt + 1))
    raise RuntimeError(f"failed to fetch {url}: {last}")


def html_to_text(page):
    """Turn a job page into plain text: keep the log, drop markup."""
    page = re.sub(r"<(script|style)\b.*?</\1>", "", page, flags=re.S | re.I)
    page = re.sub(r"<br\s*/?>", "\n", page, flags=re.I)
    return html.unescape(re.sub(r"<[^>]+>", "", page)).strip() + "\n"


def parse_jobs(page):
    jobs = {}
    for m in JOB_RE.finditer(page):
        href = html.unescape(m["href"])
        variant = urllib.parse.unquote(href.rsplit("/variant/", 1)[1])
        label = " ".join(html.unescape(re.sub(r"<[^>]+>", "", m["label"])).split())
        jobs.setdefault(variant, {"variant": variant, "status": m["status"], "label": label, "url": BASE + href})
    return list(jobs.values())


def safe_name(variant):
    return re.sub(r"[^A-Za-z0-9._,+-]", "_", variant) + ".log"


def extract_errors(text):
    """Pull out the interesting lines: opam [ERROR]s, lint errors, the final 'Job failed' line."""
    keep = []
    for line in text.splitlines():
        s = line.strip()
        if (
            s.startswith("[ERROR]")
            or s.startswith("#=== ERROR while")
            or s.startswith("Error in ")
            or "Job failed:" in s
            or "command not found" in s
            or s.startswith("[NOTE] Building ")
            or any(p in s for p in INFRA_PATTERNS)
        ) and s not in keep:
            keep.append(s)
    return keep


def root_cause(job):
    label = job["label"]
    m = re.search(r"\(failed: (.*)\)\s*$", label)
    cause = m.group(1) if m else label
    if "a dependency failed" in cause:
        errors = job.get("errors", [])
        for e in errors:
            dm = re.match(r"\[ERROR\] (\S+) failed", e) or re.match(r"#=== ERROR while \w+ (\S+)", e)
            if dm:
                return f"{dm.group(1)} failed: (via dependency)"
        if any(p in e for e in errors for p in INFRA_PATTERNS):
            building = [e.split()[-1] for e in errors if e.startswith("[NOTE] Building ")]
            where = f" while building {building[-1]}" if building else ""
            return f"CI infrastructure error{where} (container/mount failure; likely needs a retry)"
    return cause


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("url", help="opam.ci.ocaml.org commit summary page")
    ap.add_argument("-o", "--out", help="output directory (default: ci-logs/<sha>)")
    ap.add_argument("-s", "--status", action="append",
                    help="job status(es) to fetch, repeatable (default: failed). e.g. -s failed -s skipped")
    ap.add_argument("--all", action="store_true", help="fetch logs of every job regardless of status")
    ap.add_argument("-j", "--jobs", type=int, default=3, help="parallel downloads (default 3; site rate limits)")
    ap.add_argument("--force", action="store_true", help="re-download logs that already exist")
    args = ap.parse_args()

    parsed = urllib.parse.urlparse(args.url.strip())
    if parsed.netloc != "opam.ci.ocaml.org" or not COMMIT_URL_RE.match(parsed.path):
        sys.exit("expected a URL like https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/<sha>")
    sha = parsed.path.rstrip("/").rsplit("/", 1)[1]
    out = Path(args.out or Path("ci-logs") / sha)
    out.mkdir(parents=True, exist_ok=True)

    print(f"Fetching summary page for {sha} ...", file=sys.stderr)
    page = fetch(BASE + parsed.path.rstrip("/"))
    (out / "commit.html").write_text(page)
    jobs = parse_jobs(page)
    if not jobs:
        sys.exit("no jobs found on the page (format changed, or CI hasn't run yet?)")

    counts = defaultdict(int)
    for j in jobs:
        counts[j["status"]] += 1
    print("Jobs on page: " + ", ".join(f"{k}={v}" for k, v in sorted(counts.items())), file=sys.stderr)

    wanted = set(args.status or ["failed"])
    selected = [j for j in jobs if args.all or j["status"] in wanted]
    print(f"Downloading {len(selected)} log(s) to {out}/ ...", file=sys.stderr)

    def work(job):
        path = out / safe_name(job["variant"])
        job["log_file"] = path.name
        if path.exists() and not args.force:
            text = path.read_text()
        else:
            text = html_to_text(fetch(job["url"]))
            path.write_text(text)
        job["errors"] = extract_errors(text)
        return job

    failures = []
    with ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        futs = {pool.submit(work, j): j for j in selected}
        for i, f in enumerate(as_completed(futs), 1):
            try:
                f.result()
            except Exception as e:  # keep going; report at the end
                futs[f]["fetch_error"] = str(e)
                failures.append(futs[f])
            print(f"\r  {i}/{len(selected)}", end="", file=sys.stderr)
    print(file=sys.stderr)

    (out / "jobs.json").write_text(json.dumps(jobs, indent=2) + "\n")

    groups = defaultdict(list)
    for j in selected:
        if j["status"] == "failed":
            groups[root_cause(j)].append(j["variant"])
    lines = [f"CI summary for {args.url}", f"Jobs: " + ", ".join(f"{k}={v}" for k, v in sorted(counts.items())), ""]
    for cause, variants in sorted(groups.items(), key=lambda kv: -len(kv[1])):
        lines.append(f"[{len(variants)}] {cause}")
        lines += [f"    {v}" for v in sorted(variants)]
        lines.append("")
    (out / "summary.txt").write_text("\n".join(lines))
    print("\n".join(lines[:2] + [""] + [f"[{len(v)}] {c}" for c, v in sorted(groups.items(), key=lambda kv: -len(kv[1]))]))
    print(f"\nLogs, jobs.json and summary.txt written to {out}/")

    if failures:
        print(f"\n{len(failures)} log(s) could not be downloaded (re-run to retry):", file=sys.stderr)
        for j in failures:
            print(f"  {j['variant']}: {j['fetch_error']}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
