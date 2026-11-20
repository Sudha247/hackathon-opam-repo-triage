"""HTTP, GitHub API and git access, with their on-disk caches."""

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



BASE = "https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/"


NET = {"http": 0, "mcp": 0, "git": 0, "github-api": 0}   # requests actually sent (cache misses)


# OPAM_CI_OFFLINE=1 (used by the tests): everything must come from the cache;
# a cache miss is recorded in OFFLINE_MISSES and behaves like a failed request.
OFFLINE = os.environ.get("OPAM_CI_OFFLINE") == "1"


OFFLINE_MISSES = []


def offline_miss(what):
    OFFLINE_MISSES.append(what)
    return None


USER_AGENT = "opam-ci-triage"


# --------------------------------------------------------------------------
# Fetching


def cached_json(path, max_age=None):
    """The JSON stored at `path`, or in its gzip-compressed copy (test
    fixtures), or None if there is neither (or it is older than `max_age`
    seconds, except offline)."""
    for p, opener in ((path, open), (path + ".gz", gzip.open)):
        if os.path.exists(p):
            if max_age is not None and not OFFLINE and time.time() - os.path.getmtime(p) >= max_age:
                return None
            with opener(p, "rt", encoding="utf-8") as f:
                return {"value": json.load(f)}
    return None


def cache_path(url, cache_dir):
    return os.path.join(cache_dir, hashlib.sha256(url.encode()).hexdigest()[:32] + ".html")


def fetch(url, cache_dir, refresh=False):
    path = cache_path(url, cache_dir)
    if not refresh and os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            return f.read()
    if not refresh and os.path.exists(path + ".gz"):       # compressed test fixtures
        with gzip.open(path + ".gz", "rt", encoding="utf-8") as f:
            return f.read()
    if not refresh and os.path.exists(path + ".404"):      # known not to exist (e.g. a file in a tag)
        raise urllib.error.HTTPError(url, 404, "Not Found (cached)", None, None)
    if OFFLINE:
        offline_miss(url)
        raise RuntimeError(f"offline: {url} is not in the cache")
    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    NET["http"] += 1
    for attempt in range(6):
        try:
            with urllib.request.urlopen(req, timeout=120) as r:
                body = r.read().decode("utf-8", errors="replace")
            break
        except urllib.error.HTTPError as e:
            if e.code == 404:
                os.makedirs(cache_dir, exist_ok=True)
                open(path + ".404", "w").close()
            if e.code not in (429, 502, 503) or attempt == 5:
                raise
            delay = int(e.headers.get("Retry-After") or 0) or 2 ** (attempt + 1)
            time.sleep(min(delay, 60))
    if not body:
        raise RuntimeError(f"empty response for {url}")
    os.makedirs(cache_dir, exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        f.write(body)
    return body


# --------------------------------------------------------------------------
# Baseline comparison
#
# Cheap baseline: the opam-repo-ci results of the PR that published the
# previous version of the same package. Its revdeps jobs built the same revdep
# versions against the previous version, so a failure present there is not
# caused by the new PR. Costs a few GitHub API calls and one page fetch.

REPO = "ocaml/opam-repository"


def github_token():
    """A GitHub token if one is available (GITHUB_TOKEN/GH_TOKEN, or a logged-in
    gh CLI). None is fine: unauthenticated requests work, limited to 60/hour."""
    tok = os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN")
    if tok:
        return tok
    try:
        out = subprocess.run(["gh", "auth", "token"], capture_output=True, text=True, timeout=10)
        return out.stdout.strip() or None
    except (OSError, subprocess.TimeoutExpired):
        return None


_GH_CACHE = {}


def gh_api(path, cache_dir=".opam-ci-cache", cache=True):
    """GitHub REST API GET, token optional, cached on disk unless cache=False."""
    if path in _GH_CACHE:
        return _GH_CACHE[path]
    disk = os.path.join(cache_dir, "api-" + hashlib.sha256(path.encode()).hexdigest()[:24] + ".json")
    hit = cached_json(disk) if (cache or OFFLINE) else None
    if hit:
        _GH_CACHE[path] = hit["value"]
        return _GH_CACHE[path]
    if OFFLINE:
        return offline_miss("github-api " + path)
    NET["github-api"] += 1
    headers = {"User-Agent": USER_AGENT, "Accept": "application/vnd.github+json"}
    tok = github_token()
    if tok:
        headers["Authorization"] = f"Bearer {tok}"
    try:
        with urllib.request.urlopen(urllib.request.Request("https://api.github.com/" + path, headers=headers),
                                    timeout=60) as r:
            v = json.loads(r.read())
    except urllib.error.HTTPError as e:
        if e.code in (403, 429) and not tok:
            print("note: GitHub API rate limit reached (60/hour without a token); "
                  "set GITHUB_TOKEN to raise it", file=sys.stderr)
        return None
    if cache or os.environ.get("OPAM_CI_RECORD") == "1":     # recording fixtures keeps everything
        os.makedirs(cache_dir, exist_ok=True)
        with open(disk, "w") as f:
            json.dump(v, f)
    _GH_CACHE[path] = v
    return v


# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Bound suggestions
#
# Cheap rules only: CI logs, opam files and upstream git tags. Nothing is
# built. Upstream sources are read with plain git (no token, any host):
# `git ls-remote --tags`, then a shallow blob-less fetch of one tag gives the
# file list, and single files are read from raw.githubusercontent.com (GitHub)
# or fetched on demand by git (other hosts). Each suggestion carries its evidence.

# Upstream git URLs come from the dev-repo and src fields of opam files
# (untrusted): only plain https URLs reach git, after `--` where git takes
# it, and git may only use the https transport (no ssh://, file://, ext::).
SAFE_GIT_URL = re.compile(r"https://[A-Za-z0-9][A-Za-z0-9.-]*/[^\s]+")
GIT_PROTOCOLS = ["-c", "protocol.allow=never", "-c", "protocol.https.allow=always"]
GIT_TIMEOUT = 60
GIT_FETCH_TIMEOUT = 180          # a shallow blob-less fetch of a large repository


def safe_git_url(url):
    return bool(url) and SAFE_GIT_URL.fullmatch(url) is not None


def git(args, cwd=None, check=True):
    if args[0] in ("fetch", "ls-remote") and OFFLINE:
        offline_miss("git " + " ".join(args[:4]))
        return subprocess.CompletedProcess(args, 1, "", "offline")
    if args[0] in ("fetch", "ls-remote", "cat-file"):
        NET["git"] += 1
    env = {**os.environ, "GIT_TERMINAL_PROMPT": "0"}
    if OFFLINE:
        env["GIT_NO_LAZY_FETCH"] = "1"          # a partial clone must not fetch missing objects
    timeout = GIT_FETCH_TIMEOUT if args[0] == "fetch" else GIT_TIMEOUT
    try:
        out = subprocess.run(["git"] + GIT_PROTOCOLS + args, cwd=cwd, capture_output=True, text=True, env=env,
                             timeout=timeout)
    except subprocess.TimeoutExpired:
        out = subprocess.CompletedProcess(args, 124, "", f"timed out after {timeout} s")
    if check and out.returncode != 0:
        raise RuntimeError(f"git {' '.join(args[:3])}...: {out.stderr.strip()[:200]}")
    return out


def repo_url(dev_repo):
    """Normalise an opam dev-repo / src URL to a fetchable https git URL."""
    u = dev_repo.strip().removeprefix("git+")
    u = re.sub(r"^git://", "https://", u)
    u = re.sub(r"^git@([^:]+):", r"https://\1/", u)
    u = u.split("#", 1)[0]
    m = re.match(r"(https://github\.com/[^/]+/[^/]+?)(?:\.git)?(?:/.*)?$", u)
    if m:          # also archive URLs: https://github.com/o/r/releases/... -> repo
        return m.group(1)
    u = re.sub(r"/-/(archive|tags|tree)/.*$", "", u)   # GitLab archive URLs (nested groups kept)
    u = u.removesuffix("/")
    return u if safe_git_url(u) else None
