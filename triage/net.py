"""HTTP, GitHub API and git access, with their on-disk caches."""

import argparse
import collections
import concurrent.futures
import functools
import gzip
import hashlib
import html
import http.client
import html.parser
import json
import os
import re
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.parse
import urllib.request



BASE = "https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/"


class _Counts(dict):
    """Request counts. With OPAM_CI_NET_FILE set, they are also written to
    that file (JSON) on every change: a caller that kills the tool (a
    timeout) still knows what it did, LLM calls included."""

    def __setitem__(self, k, v):
        super().__setitem__(k, v)
        path = os.environ.get("OPAM_CI_NET_FILE")
        if path:
            try:
                write_atomic(path, json.dumps(self))
            except OSError:
                pass


# Requests actually sent (cache misses). "mcp-errors" and "net-errors" count
# the failed ones (transient: not cached); the analysis is then incomplete.
NET = _Counts({"http": 0, "mcp": 0, "git": 0, "github-api": 0})


# Network failures treated as transient: not cached, counted.
NET_ERRORS = (OSError, http.client.HTTPException)


def net_error():
    """Count a failed request (NET["net-errors"]): the analysis is incomplete,
    and the triage page analyses the PR again later."""
    NET["net-errors"] = NET.get("net-errors", 0) + 1


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


def write_atomic(path, data):
    """Write `data` (str) to `path` through a temporary file in the same
    directory, renamed over it: a killed process leaves the old file or none,
    never a truncated one. Nothing is written offline (the tests use their
    fixtures as the cache), except the request counts' file."""
    if OFFLINE and path != os.environ.get("OPAM_CI_NET_FILE"):
        return
    d = os.path.dirname(os.path.abspath(path))
    os.makedirs(d, exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=d, prefix=".tmp-")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            f.write(data)
        os.replace(tmp, path)
    except BaseException:
        try:
            os.remove(tmp)
        except OSError:
            pass
        raise


def fresh(path, max_age):
    """Is the file at `path` younger than `max_age` seconds (None: no limit;
    offline, everything is)?"""
    return max_age is None or OFFLINE or time.time() - os.path.getmtime(path) < max_age


def cached_json(path, max_age=None):
    """The JSON stored at `path`, or in its gzip-compressed copy (test
    fixtures), or None if there is neither (or it is older than `max_age`
    seconds, except offline, or unreadable)."""
    for p, opener in ((path, open), (path + ".gz", gzip.open)):
        if os.path.exists(p):
            if not fresh(p, max_age):
                return None
            try:
                with opener(p, "rt", encoding="utf-8") as f:
                    return {"value": json.load(f)}
            except (OSError, ValueError, EOFError):    # truncated or corrupt: a miss
                return None
    return None


def cache_path(url, cache_dir):
    return os.path.join(cache_dir, hashlib.sha256(url.encode()).hexdigest()[:32] + ".html")


# A page that did not exist (404) is fetched again after a day.
NOT_FOUND_MAX_AGE = 24 * 3600


def fetch(url, cache_dir, refresh=False, max_age=None):
    """The body of `url`, cached on disk (fetched again when the cached copy
    is older than `max_age` seconds, except offline)."""
    path = cache_path(url, cache_dir)
    if not refresh and os.path.exists(path) and fresh(path, max_age):
        with open(path, encoding="utf-8") as f:
            return f.read()
    if not refresh and os.path.exists(path + ".gz"):       # compressed test fixtures
        with gzip.open(path + ".gz", "rt", encoding="utf-8") as f:
            return f.read()
    if (not refresh and os.path.exists(path + ".404")      # known not to exist (e.g. a file in a tag)
            and fresh(path + ".404", min(max_age or NOT_FOUND_MAX_AGE, NOT_FOUND_MAX_AGE))):
        raise urllib.error.HTTPError(url, 404, "Not Found (cached)", None, None)
    if OFFLINE:
        offline_miss(url)
        raise RuntimeError(f"offline: {url} is not in the cache")
    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    NET["http"] += 1
    # A request that fails (after the retries) is counted in NET["net-errors"],
    # whatever the caller does with the exception: a log or a page that could
    # not be read makes the analysis incomplete. A 404 is an answer, not counted.
    try:
        for attempt in range(6):
            try:
                with urllib.request.urlopen(req, timeout=120) as r:
                    body = r.read().decode("utf-8", errors="replace")
                break
            except urllib.error.HTTPError as e:
                if e.code == 404:
                    write_atomic(path + ".404", "")
                if e.code not in (429, 502, 503) or attempt == 5:
                    raise
                delay = int(e.headers.get("Retry-After") or 0) or 2 ** (attempt + 1)
                time.sleep(min(delay, 60))
    except urllib.error.HTTPError as e:
        if e.code != 404:
            net_error()
        raise
    except NET_ERRORS:
        net_error()
        raise
    if not body:
        net_error()
        raise RuntimeError(f"empty response for {url}")
    write_atomic(path, body)
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
        if e.code != 404:
            net_error()
        return None
    except NET_ERRORS + (ValueError,):         # network error, unreadable answer: transient
        net_error()
        return None
    if cache or os.environ.get("OPAM_CI_RECORD") == "1":     # recording fixtures keeps everything
        write_atomic(disk, json.dumps(v))
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


# A fetch or ls-remote that failed because of the network or the server (counted
# in NET["net-errors"]: the tags or files are then unknown, not absent), as
# opposed to a clean negative answer: a missing tag ("couldn't find remote
# ref"), repository ("not found", 404) or a private one (credentials asked).
GIT_NET_FAILURE = re.compile(
    r"timed out|Could not resolve (host|proxy)|Failed to connect|Connection (refused|reset|timed out)|"
    r"returned error: (5\d\d|429)|RPC failed|early EOF|unexpected disconnect|hung up unexpectedly|"
    r"transfer closed|Recv failure|Empty reply|SSL|TLS|gnutls|Couldn't connect", re.I)


# Of those, the failures that a dead or misconfigured host gives every time
# (no DNS name, a bad certificate, nothing listening, no answer): the URL is
# not asked again for GIT_DOWN_MAX_AGE (see Sources.git_down).
GIT_UNREACHABLE = re.compile(
    r"timed out|Could not resolve host|Failed to connect|Couldn't connect|Connection (refused|timed out)|"
    r"SSL|TLS|gnutls|certificate", re.I)
GIT_DOWN_MAX_AGE = 24 * 3600


def safe_git_url(url):
    return bool(url) and SAFE_GIT_URL.fullmatch(url) is not None


def git(args, cwd=None, check=True, git_dir=None):
    """Run git. With `git_dir`, on that repository only (GIT_DIR): git never
    falls back to a repository found in a parent directory."""
    if args[0] in ("fetch", "ls-remote") and OFFLINE:
        offline_miss("git " + " ".join(args[:4]))
        return subprocess.CompletedProcess(args, 1, "", "offline")
    if args[0] in ("fetch", "ls-remote", "cat-file"):
        NET["git"] += 1
    env = {**os.environ, "GIT_TERMINAL_PROMPT": "0"}
    if git_dir:
        env["GIT_DIR"] = git_dir
    if OFFLINE:
        env["GIT_NO_LAZY_FETCH"] = "1"          # a partial clone must not fetch missing objects
    timeout = GIT_FETCH_TIMEOUT if args[0] == "fetch" else GIT_TIMEOUT
    try:
        out = subprocess.run(["git"] + GIT_PROTOCOLS + args, cwd=cwd, capture_output=True, text=True, env=env,
                             timeout=timeout)
    except subprocess.TimeoutExpired:
        out = subprocess.CompletedProcess(args, 124, "", f"timed out after {timeout} s")
    if args[0] in ("fetch", "ls-remote") and out.returncode != 0 and GIT_NET_FAILURE.search(out.stderr or ""):
        net_error()
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
