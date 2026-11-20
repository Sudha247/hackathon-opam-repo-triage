"""opam-repo-ci pages and logs: jobs, categories, platforms, log excerpts, job names in words."""

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
    REPO,
    cache_path,
    fetch,
    gh_api,
)


def unfinished_jobs(data):
    """Jobs still running or queued (status `active`, ...)."""
    return [j for j in data["jobs"] if j["status"] not in ("passed", "failed")]


def load_commit(sha, cache_dir, refresh=False):
    """The CI page of a commit, parsed. A page whose CI is still running is
    not kept in the cache (it changes until every job has finished), and one
    cached as running before is fetched again. With `refresh` (jobs were
    rebuilt), the page is fetched again and the cached logs of its jobs are
    dropped: a rebuilt job has the same URL but a new log. Each log is
    fetched again only when an analysis needs it."""
    url = BASE + sha
    before = NET["http"]
    page = fetch(url, cache_dir, refresh=refresh)
    data = parse_commit_page(page)
    if refresh:
        for j in data["jobs"]:
            try:
                os.remove(cache_path(j["url"], cache_dir))
            except OSError:
                pass
    if unfinished_jobs(data) and NET["http"] == before and not OFFLINE:
        page = fetch(url, cache_dir, refresh=True)
        data = parse_commit_page(page)
    data["unfinished"] = len(unfinished_jobs(data))
    # A test fixture is a snapshot: keep the page when recording one, even if
    # some jobs never finish (stuck experimental jobs on old commits).
    if data["unfinished"] and os.environ.get("OPAM_CI_RECORD") != "1":
        try:
            os.remove(cache_path(url, cache_dir))
        except OSError:
            pass
    return page, data


def resolve_sha(target, pr):
    if pr is not None:
        # One REST call (no token needed); the head can move, so not cached on disk.
        d = gh_api(f"repos/{REPO}/pulls/{pr}", cache=False)
        if d and d.get("head", {}).get("sha"):
            return d["head"]["sha"]
        # Fallback without the API: refs/pull/N/head (slow: lists every ref).
        NET["git"] += 1
        out = subprocess.run(["git", "ls-remote", f"https://github.com/{REPO}", f"refs/pull/{pr}/head"],
                             capture_output=True, text=True)
        if not out.stdout.strip():
            sys.exit(f"cannot resolve PR #{pr}: {out.stderr.strip()[:200]}")
        return out.stdout.split()[0]
    m = re.search(r"([0-9a-f]{40})", target or "")
    if not m:
        sys.exit(f"cannot find a commit sha in {target!r}")
    return m.group(1)


# --------------------------------------------------------------------------
# Commit page parsing
#
# Structure of the page:
#   <h2>Summary of errors</h2><ul class="statuses"> ... </ul>
#   <details><summary><b>Main results</b></summary><ul class="statuses"> ... </ul></details>
#   <details><summary><b>Reverse dependencies</b></summary><ul class="statuses"> ... </ul></details>
# Each node is <li class="none|passed|failed|skipped|..."> containing either a
# plain text label (a grouping node, e.g. "compilers", "4.14", "debian-13")
# or <a href=".../variant/<comma,separated,path>">label (status[: reason])</a>,
# optionally followed by a nested <ul class="statuses">.


class CommitPageParser(html.parser.HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.section = None          # "summary" | "main" | None
        self.stack = []              # one entry per open <li>: dict(label, cls, job)
        self.in_a = None             # href of the <a> being read
        self.text = []
        self.jobs = []               # (section, href, li_class, text, ancestors, messages)
        self.pr = None
        self._in_h2 = False
        self._in_summary_tag = False

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if tag == "h2":
            self._in_h2, self.text = True, []
        elif tag == "summary":
            self._in_summary_tag, self.text = True, []
        elif tag == "li" and self.section:
            self.stack.append({"cls": a.get("class"), "label": "", "job": None})
        elif tag == "ul" and self.section and self.stack:
            # Entering children: freeze the parent's label.
            top = self.stack[-1]
            top["label"] = top["label"].strip()
        elif tag == "a":
            href = a.get("href", "")
            if self.section and "/variant/" in href:
                self.in_a, self.text = href, []
            m = re.search(r"/pull/(\d+)$", href)
            if m:
                self.pr = int(m.group(1))

    def handle_endtag(self, tag):
        if tag == "h2" and self._in_h2:
            self._in_h2 = False
            if "".join(self.text).strip() == "Summary of errors":
                self.section = "summary"
        elif tag == "summary" and self._in_summary_tag:
            self._in_summary_tag = False
            # "Main results", "Reverse dependencies", ...
            self.section, self.stack = "main", []
        elif tag == "a" and self.in_a:
            text = " ".join("".join(self.text).split())
            ancestors = [e["label"] for e in self.stack[:-1]]
            cls = (self.stack[-1]["cls"] or "") if self.stack else ""
            self.jobs.append((self.section, self.in_a, cls, text, ancestors, []))
            if self.stack:
                self.stack[-1]["label"] = text
                self.stack[-1]["job"] = len(self.jobs) - 1
            self.in_a = None
        elif tag == "li" and self.section and self.stack:
            e = self.stack.pop()
            # A class-less <li> under a job is a message (e.g. lint errors).
            msg = " ".join(e["label"].split())
            if e["cls"] is None and e["job"] is None and msg:
                for anc in reversed(self.stack):
                    if anc["job"] is not None:
                        self.jobs[anc["job"]][5].append(msg)
                        break
        elif tag == "details" and self.section == "main":
            self.section = None

    def handle_data(self, data):
        if self.in_a or self._in_h2 or self._in_summary_tag:
            self.text.append(data)
        elif self.section and self.stack and not self.in_a:
            self.stack[-1]["label"] += data


LABEL_RE = re.compile(r"^(.*?) \((\w+)(?::\s*(.*))?\)$", re.S)


def split_variant(variant):
    """Split a variant path into (platform, target, kind, revdep).

    Examples:
      compilers,4.14,lwt.5.10.2,revdeps,osc.0.1.4
      day10,debian-13,x86_64,5.5,lwt.5.10.2,tests
      compilers,4.11,foo.1.0,lower-bounds
      distributions,centos-10-ocaml-4.14,foo.1.0
      (lint)
    """
    if variant.startswith("("):
        return [], None, variant.strip("()"), None
    parts = variant.split(",")
    if "revdeps" in parts:
        i = parts.index("revdeps")
        if i == len(parts) - 1:   # the job computing the list of revdeps
            return parts[: i - 1], parts[i - 1], "revdeps-list", None
        return parts[: i - 1], parts[i - 1], "revdeps", ",".join(parts[i + 1:])
    if parts[-1] in ("lower-bounds", "tests"):
        return parts[:-2], parts[-2], parts[-1], None
    return parts[:-1], parts[-1], "build", None


def categorize(status, reason):
    r = reason or ""
    if status == "passed":
        return "passed"
    if status != "failed":
        return status  # active, queued, ...
    if r.startswith("[SKIP] Failure ignored"):
        return "ignored"            # x-ci-accept-failures
    if r.startswith("[SKIP]"):
        return "not-available"      # package not installable on that variant
    if r == "Cancelled" or "Cancelled" in r[:20]:
        return "cancelled"
    if re.search(r"curl failed|HTTP error code|Could not resolve|Connection (refused|reset|timed out)", r):
        return "network"
    if "Failed to get sources" in r:
        return "sources"            # fetched but could not be extracted/verified (e.g. symlinks on msys2)
    if re.search(r"\bfailed to build\b|a dependency failed|^\s*\S+ failed:$", r):
        return "dependency"
    if re.search(r"No solution|conflict", r, re.I):
        return "solver"
    if re.search(r"failed at \".*(@runtest|test)", r):
        return "build-or-test"
    return "build"


def parse_commit_page(page):
    p = CommitPageParser()
    p.feed(page)
    summary_hrefs = {href for sec, href, *_ in p.jobs if sec == "summary"}
    jobs = []
    for sec, href, cls, text, ancestors, messages in p.jobs:
        if sec != "main":
            continue
        variant = urllib.parse.unquote(href.split("/variant/", 1)[1])
        m = LABEL_RE.match(text)
        label, status, reason = (m.group(1), m.group(2), m.group(3)) if m else (text, cls, None)
        platform, target, kind, revdep = split_variant(variant)
        jobs.append({
            "variant": variant,
            "url": urllib.parse.urljoin(BASE, href),
            "label": label,
            "status": status,
            "reason": reason.strip() if reason else None,
            "category": kind if kind in ("lint", "analysis") and status == "failed"
                        else categorize(status, reason.strip() if reason else None),
            "target": target,
            "kind": kind,
            "revdep": revdep,
            "platform": platform,
            "experimental": any("(experimental)" in a for a in ancestors),
            "in_summary": href in summary_hrefs,
            "messages": messages,
        })
    return {"pr": p.pr, "jobs": jobs}


# --------------------------------------------------------------------------
# Log excerpt extraction


# day10 prints opam's output indented by 8 spaces, and the build output with
# an extra "- " prefix; remove both so that day10 logs read like classic ones
# (the "#=== ERROR while ..." block, "-> installed ...", compiler errors).
DAY10_INDENT = re.compile(r"(?m)^ {8}(?:- (?=\S))?")


def log_text(page):
    m = re.search(r"<pre[^>]*>(.*?)</pre>", page, re.S)
    raw = m.group(1) if m else page
    text = html.unescape(re.sub(r"<[^>]+>", "", raw))
    if "Building with day10" in text or re.search(r"(?m)^ {8}#=== ERROR while ", text):
        text = DAY10_INDENT.sub("", text)
    return text


ERROR_LINE = re.compile(r"^# ?(Error\b|.*\b(error|Error):)")


def excerpt(text, max_output_lines=40):
    lines = text.splitlines()
    out = {}

    # 1. First "ERROR while compiling" block (opam repeats it in the error report).
    for i, l in enumerate(lines):
        if l.startswith("#=== ERROR while "):  # compiling, installing, fetching ...
            block = [l]
            for l2 in lines[i + 1:]:
                if not l2.strip() or l2.startswith("<><>"):
                    break
                block.append(l2)
            header = [b for b in block if not b.startswith("### output") and not re.match(r"# (env|output)-file", b)]
            try:
                k = block.index("### output ###")
                body = block[k + 1:]
                header = block[:k]
                if len(body) > max_output_lines:
                    cut = len(body) - max_output_lines
                    # Keep the first error too when it is before the tail: dune
                    # prints errors first and pages of warnings can follow.
                    first = next((i for i, b in enumerate(body[:cut]) if ERROR_LINE.search(b)), None)
                    if first is None:
                        body = [f"# [... {cut} lines elided ...]"] + body[-max_output_lines:]
                    else:
                        lo, hi = max(0, first - 5), min(first + 15, cut)
                        body = (([f"# [... {lo} lines elided ...]"] if lo else []) + body[lo:hi]
                                + ([f"# [... {cut - hi} lines elided ...]"] if cut > hi else [])
                                + body[-max_output_lines:])
                block = header + ["### output ###"] + body
            except ValueError:
                block = header
            out["error_block"] = "\n".join(block)
            break

    # 2. Solver failure.
    for i, l in enumerate(lines):
        if re.search(r"\[ERROR\] Package conflict|No solution|Sorry, no solution", l):
            out["solver"] = "\n".join(lines[i:i + 30])
            break

    # 3. "Log analysis" chosen message = last line of the log, after the timestamp.
    tail = [l for l in lines if re.match(r"^\d{4}-\d\d-\d\d \d\d:\d\d\.\d\d: ", l)]
    if tail:
        out["final"] = re.sub(r"^\S+ \S+ ", "", tail[-1])
    for l in lines:
        m = re.match(r"^\d{4}-\d\d-\d\d \d\d:\d\d\.\d\d: Job failed: (.*)", l)
        if m:
            out["job_failed"] = m.group(1)

    # 4. Builder and base commit info (useful to reproduce and compare).
    for l in lines:
        m = re.search(r"New job: (.*)", l)
        if m and "job" not in out:
            out["job"] = m.group(1).strip()
        m = re.search(r"git merge --no-edit ([0-9a-f]{40})|onto master ([0-9a-f]{40})", l)
        if m and "base" not in out:
            out["base"] = m.group(1) or m.group(2)
        if "Building with day10" in l:
            out["builder"] = "day10"
    out.setdefault("builder", "classic")
    return out


# --------------------------------------------------------------------------
# Rendering


def platform_str(job):
    p = "/".join(job["platform"]) or "-"
    return p + (" (exp)" if job["experimental"] else "")


def opam_vcmp(a, b):
    """Compare opam (Debian-like) version strings."""
    def order(c):
        if c == "~":
            return -1
        if c.isalpha():
            return ord(c)
        return ord(c) + 256

    def cmp_nondigit(x, y):
        for i in range(max(len(x), len(y))):
            ox = order(x[i]) if i < len(x) else 0
            oy = order(y[i]) if i < len(y) else 0
            if ox != oy:
                return -1 if ox < oy else 1
        return 0

    while a or b:
        ma = re.match(r"[^0-9]*", a).group(0)
        mb = re.match(r"[^0-9]*", b).group(0)
        c = cmp_nondigit(ma, mb)
        if c:
            return c
        a, b = a[len(ma):], b[len(mb):]
        da = re.match(r"[0-9]*", a).group(0)
        db = re.match(r"[0-9]*", b).group(0)
        na, nb = int(da or 0), int(db or 0)
        if na != nb:
            return -1 if na < nb else 1
        a, b = a[len(da):], b[len(db):]
    return 0


def split_pkg(full):
    name, _, version = full.partition(".")
    return name, version


def ocaml_version(job):
    p = job["platform"]
    if p[:1] == ["compilers"] and len(p) == 2:
        return p[1]
    if p[:1] == ["day10"] and len(p) == 4 and p[2] == "x86_64":
        return p[3]
    return None   # other platforms (distributions, extras, macos, ...) are not compared


FAILING = {"build", "build-or-test", "dependency", "solver"}


def failure_key(j):
    return (j["target"], j["revdep"], j["kind"], re.sub(r"[`\"']|-j \d+", "", j["reason"] or ""))


def merge_jobs(a, b):
    """Union of two lists of job variants, in order."""
    return list(dict.fromkeys((a or []) + (b or [])))


def describe_job(variant):
    """A CI job name in words: 'day10,archlinux,x86_64,5.5,pkg.1' ->
    'pkg.1 on archlinux x86_64, OCaml 5.5 (day10 builder)'."""
    parts = (variant or "").split(",")
    m = re.match(r"^(\S+) \[(build|tests|revdeps|lower-bounds)(?:, revdep of (\S+))?\]$", variant or "")
    if m:            # a pattern over several jobs (from matrix_rules)
        pkg, kind, target = m.groups()
        return (f"{pkg}, reverse dependency of {target}" if target
                else pkg + {"tests": " (tests)", "lower-bounds": " (lower bounds)"}.get(kind, ""))
    m = re.match(r"^(\S+) \[[^\]]*\]$", variant or "")
    if m:            # a label such as "pkg.1 [all build jobs skipped]": the package
        return m.group(1)
    if variant == "(lint)":
        return "opam lint"
    m = re.match(r"^(\d+ packages) \[", variant or "")
    if m:
        return m.group(1)
    builder = parts[0]
    if builder == "day10" and len(parts) >= 5:
        plat, rest = f"{parts[1]} {parts[2]}, OCaml {parts[3]} (day10 builder)", parts[4:]
    elif builder == "compilers" and len(parts) >= 3:
        plat, rest = f"OCaml {parts[1]}", parts[2:]
    elif builder in ("distributions", "extras", "freebsd", "macos") and len(parts) >= 3:
        m = re.match(r"^(?:(.+?)-)?ocaml-(\d+\.\d+)(?:-(.+))?$", parts[1])
        if m:
            pre, ver, post = m.groups()
            plat = ", ".join(x for x in [(pre or "").replace("opam-", "opam "), f"OCaml {ver}" +
                                         (f" {post}" if post and post not in ARCHES and post != "amd64" else "")] if x)
            if post in ARCHES or post == "amd64":
                plat = f"{pre} {post}, OCaml {ver}"
        else:
            plat = parts[1]
        rest = parts[2:]
    else:
        return variant
    pkg, extra = rest[0], rest[1:]
    if extra[:1] == ["revdeps"] and len(extra) >= 2:
        what = f"{extra[1]}, reverse dependency of {pkg}"
        extra = extra[2:]
    else:
        what = pkg
    what += "".join({"tests": " (tests)", "lower-bounds": " (lower bounds)"}.get(x, f" ({x})") for x in extra)
    return f"{what} on {plat}"


# --------------------------------------------------------------------------
# Rules on the job matrix (no fetch), on log text, on lint messages, and on
# packages that install nowhere.

ARCHES = ("x86_64", "amd64", "arm64", "aarch64", "riscv64", "s390x", "ppc64", "ppc64le", "x86_32", "i386", "arm32")


NEUTRAL = {"not-available", "ignored", "network", "cancelled", "sources"}


FAILING_CATS = {"build", "build-or-test", "dependency", "solver"}


# The classic builder's `compilers` jobs run on this image (FROM ocaml/opam:debian-13-ocaml-X).
CLASSIC_DISTRO = "debian-13"


def job_dims(job):
    """Platform of a CI job: OCaml version (X.Y), os (opam variable), distro, arch."""
    p = job["platform"]
    joined = ",".join(p)
    d = {"ocaml": None, "os": "linux", "distro": None, "arch": "x86_64"}
    m = re.search(r"ocaml-(\d+\.\d+)", joined)
    if m:
        d["ocaml"] = m.group(1)
    if p[:1] == ["compilers"] and len(p) >= 2:
        d["ocaml"] = p[1]
        d["distro"] = CLASSIC_DISTRO
    elif p[:1] == ["day10"] and len(p) >= 4:
        d["distro"], d["arch"], d["ocaml"] = p[1], p[2], p[3]
    elif p[:1] == ["distributions"] and len(p) >= 2:
        d["distro"] = re.sub(r"-ocaml-.*$", "", p[1])
    elif p[:1] == ["extras"] and len(p) >= 2 and p[1].split("-", 1)[0] in ARCHES:
        d["arch"] = p[1].split("-", 1)[0]
    elif p[:1] in (["freebsd"], ["macos"]) and len(p) >= 2:
        d["distro"] = re.sub(r"-ocaml-.*$", "", p[1])
        m = re.search(r"-(amd64|arm64|x86_64)$", p[1])
        if m:
            d["arch"] = m.group(1)
    dist = d["distro"] or ""
    if dist.startswith(("cygwin", "msys2")):
        d["os"] = "win32"
    elif dist.startswith("freebsd"):
        d["os"] = "freebsd"
    elif dist.startswith("macos"):
        d["os"] = "macos"
    return d


def vkey(v):
    return tuple(int(x) for x in re.findall(r"\d+", v))


def platform_words(job):
    """Where a job ran, in words: 'archlinux x86_64, OCaml 5.5 (day10 builder)'."""
    return describe_job(job["variant"]).rsplit(" on ", 1)[-1]


def failing_dep(reason):
    """For a dependency failure, the failing package named in the reason."""
    m = re.match(r"\s*([\w+-]+\.[^\s:]+)(?: [\w+-]+\.[^\s:]+)* failed", reason or "")
    return m.group(1) if m else None
