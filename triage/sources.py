"""Package metadata and sources: opam files, versions, ocaml.org docs (MCP), upstream git."""

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
import shutil
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

from .net import (  # noqa: F401
    NET,
    NET_ERRORS,
    OFFLINE,
    REPO,
    USER_AGENT,
    cached_json,
    fetch,
    gh_api,
    git,
    offline_miss,
    repo_url,
    write_atomic,
)
from .ci import (  # noqa: F401
    opam_vcmp,
    split_pkg,
)


def find_baseline_commit(target, cache_dir=".opam-ci-cache"):
    """For target 'lwt.5.10.2', return info about the PR that added the
    previous version on master, or None."""
    name, version = split_pkg(target)
    versions = Sources(cache_dir).versions(name) or []
    older = [v for v in versions if opam_vcmp(v, version) < 0]
    if not older:
        return None
    prev = max(older, key=functools.cmp_to_key(opam_vcmp))
    return {"prev": f"{name}.{prev}", **(find_publication(name, prev, cache_dir) or {})}


def find_publication(name, version, cache_dir=".opam-ci-cache"):
    """The PR that published name.version: {'pr': N, 'sha': head sha}, or None.
    Uses 2 GitHub REST calls (token optional)."""
    commits = gh_api(f"repos/{REPO}/commits?path=packages/{name}/{name}.{version}/opam&per_page=100",
                     cache_dir) or []
    if not commits:
        return None
    first = commits[-1]["sha"]          # oldest commit touching the file = the one adding it
    pulls = gh_api(f"repos/{REPO}/commits/{first}/pulls", cache_dir) or []
    pr = pulls[0] if pulls else None
    return {"pr": pr["number"] if pr else None, "sha": pr["head"]["sha"] if pr else first}


# Failed ocaml.org MCP calls in this process (see Sources.MCP_MAX_FAILURES).
MCP_FAILURES = {"n": 0}


class Sources:
    """Cached, token-free access to opam-repository and upstream repositories."""

    OPAM_REPO_URL = f"https://github.com/{REPO}"

    def __init__(self, cache_dir):
        self.cache_dir = cache_dir
        self.mem = {}

    def raw(self, url, max_age=None):
        """The body of `url`, or None if it does not exist or could not be
        fetched (a network error is counted in NET["net-errors"])."""
        if url in self.mem:
            return self.mem[url]
        try:
            v = fetch(url, self.cache_dir, max_age=max_age)
        except urllib.error.HTTPError as e:
            if e.code != 404:
                NET["net-errors"] = NET.get("net-errors", 0) + 1
            v = None
        except NET_ERRORS:
            NET["net-errors"] = NET.get("net-errors", 0) + 1
            v = None
        except RuntimeError:            # offline cache miss, empty answer
            v = None
        self.mem[url] = v
        return v

    # ---- git plumbing (bare, shallow, blob-less mirrors under cache_dir/git)

    # Touched on each use: the CI workflow deletes the mirrors not used for a month.
    GIT_STAMP = "opam-ci-last-used"

    def _gitdir(self, url):
        d = os.path.join(self.cache_dir, "git", hashlib.sha256(url.encode()).hexdigest()[:16])
        if ("gitdir", d) in self.mem:
            return d
        # A mirror whose files were deleted (cache pruning) is made again.
        valid = (os.path.isdir(d)
                 and git(["rev-parse", "--git-dir"], git_dir=d, check=False).returncode == 0
                 and git(["config", "--get", "remote.origin.url"], git_dir=d, check=False).stdout.strip() == url)
        if not valid and not OFFLINE:
            shutil.rmtree(d, ignore_errors=True)
            os.makedirs(d)
            git(["init", "-q", "--bare", d])
            git(["remote", "add", "origin", url], git_dir=d)
            git(["config", "remote.origin.promisor", "true"], git_dir=d)
            git(["config", "remote.origin.partialclonefilter", "blob:none"], git_dir=d)
            git(["config", "extensions.partialClone", "origin"], git_dir=d)
        write_atomic(os.path.join(d, self.GIT_STAMP), "")
        self.mem[("gitdir", d)] = True
        return d

    def _ensure(self, url, ref, is_tag):
        """Make `ref` (tag name or commit sha) available locally; return a rev."""
        key = ("ensure", url, ref)
        if key in self.mem:
            return self.mem[key]
        d = self._gitdir(url)
        rev = f"refs/tags/{ref}" if is_tag else ref
        if git(["rev-parse", "--verify", "-q", rev + "^{tree}"], git_dir=d, check=False).returncode != 0:
            spec = f"+refs/tags/{ref}:refs/tags/{ref}" if is_tag else ref
            r = git(["fetch", "-q", "--depth", "1", "--filter=blob:none", "origin", spec], git_dir=d, check=False)
            if r.returncode != 0:
                rev = None
        self.mem[key] = rev
        return rev

    def tags(self, url):
        key = ("tags", url)
        if key not in self.mem:
            disk = os.path.join(self.cache_dir, "tags-" + hashlib.sha256(url.encode()).hexdigest()[:16] + ".json")
            hit = cached_json(disk, max_age=86400)
            if hit and isinstance(hit["value"], list) and hit["value"]:
                tags = hit["value"]
            else:
                r = git(["ls-remote", "--tags", "--refs", url], check=False)
                tags = [l.split("refs/tags/", 1)[1] for l in r.stdout.splitlines() if "refs/tags/" in l]
                # A failed listing (network error, offline) is not kept on disk.
                if r.returncode == 0 and tags:
                    write_atomic(disk, json.dumps(tags))
            self.mem[key] = tags
        return self.mem[key]

    def tag_for(self, url, name, version):
        tags = set(self.tags(url))
        for c in (version, "v" + version, f"{name}-{version}", f"{name}.{version}",
                  f"{name}-v{version}", f"release-{version}", f"Release-{version}",
                  version.replace("~", "-"), "v" + version.replace("~", "-"),
                  version.replace("~", "_"), "v" + version.replace("~", "_")):
            if c in tags:
                return c
        return None

    def tree(self, url, ref, is_tag=True):
        key = ("tree", url, ref)
        if key not in self.mem:
            rev = self._ensure(url, ref, is_tag)
            paths = []
            if rev:
                r = git(["ls-tree", "-r", "--name-only", rev], git_dir=self._gitdir(url), check=False)
                paths = r.stdout.splitlines()
            self.mem[key] = paths
        return self.mem[key]

    def file_at(self, url, ref, path, is_tag=True):
        m = re.match(r"https://github\.com/([^/]+/[^/]+)$", url)
        if m:
            return self.raw(f"https://raw.githubusercontent.com/{m.group(1)}/{ref}/{path}")
        rev = self._ensure(url, ref, is_tag)
        if not rev:
            return None
        r = git(["cat-file", "-p", f"{rev}:{path}"], git_dir=self._gitdir(url), check=False)
        return r.stdout if r.returncode == 0 else None

    # ---- opam-repository

    def versions(self, name, ref="master"):
        """Versions of `name` in opam-repository, oldest first: from ocaml.org's
        ocaml_package_versions for master ([] if not a package; None if the
        MCP server did not answer, counted in NET["mcp-errors"]: callers must
        then not conclude that `name` is not a package), from a git listing
        for another ref."""
        if ref == "master":
            vs = self.mcp_versions(name)
            if vs is None:
                return None
            return sorted({v["version"] for v in vs if isinstance(v.get("version"), str)},
                          key=functools.cmp_to_key(opam_vcmp))
        return self._versions_git(name, ref)

    def _versions_git(self, name, ref="master"):
        if ref == "master":
            key = ("master-sha",)
            if key not in self.mem:
                r = git(["ls-remote", self.OPAM_REPO_URL, "refs/heads/master"], check=False)
                self.mem[key] = r.stdout.split()[0] if r.stdout else None
            ref = self.mem[key]
        if not ref:
            return []
        d = self._gitdir(self.OPAM_REPO_URL)
        rev = self._ensure(self.OPAM_REPO_URL, ref, is_tag=False)
        if not rev:
            return []
        r = git(["ls-tree", "--name-only", f"{rev}:packages/{name}"], git_dir=d, check=False)
        vs = [split_pkg(l)[1] for l in r.stdout.splitlines() if l.startswith(name + ".")]
        return sorted(vs, key=functools.cmp_to_key(opam_vcmp))

    # ---- ocaml.org documentation, through its MCP endpoint (JSON, plain-text
    # signatures; docs are built for every package version in opam-repository)

    MCP_URL = "https://ocaml.org/mcp"
    MCP_TIMEOUT = 20
    # After this many failed calls in a process, ocaml.org is not called
    # again: each skipped call counts as a failure (NET["mcp-errors"]).
    MCP_MAX_FAILURES = 3
    # The tool's own errors (an unknown package, a version without docs yet)
    # are asked again after a day: docs get built, a tool error may be transient.
    MCP_ERROR_MAX_AGE = 24 * 3600

    def mcp(self, tool, _max_age=None, **args):
        """Call an ocaml.org MCP tool; parsed JSON result, or None on error.
        Answers are cached on disk, including the tool's own errors (an unknown
        package, a version without docs), as None, for a day; with `_max_age`
        (seconds), an older answer is fetched again (for data that changes,
        like health-check results). A call that fails (network error, timeout,
        HTTP error, unreadable answer) is not cached and is counted in
        NET["mcp-errors"]: the analysis is then incomplete."""
        return self._mcp(tool, _max_age, args)[0]

    def _mcp(self, tool, max_age, args):
        """mcp(): (value, answered), `answered` False when the call failed
        (as opposed to the tool answering with an error)."""
        key = json.dumps([tool, args], sort_keys=True)
        if key in self.mem:
            return self.mem[key], True
        disk = os.path.join(self.cache_dir, "mcp-" + hashlib.sha256(key.encode()).hexdigest()[:24] + ".json")
        hit = cached_json(disk, max_age=max_age)
        if hit and hit["value"] is None:
            hit = cached_json(disk, max_age=min(max_age or self.MCP_ERROR_MAX_AGE, self.MCP_ERROR_MAX_AGE))
        if hit:
            v = hit["value"]
        elif OFFLINE:
            return offline_miss("mcp " + key), False
        elif MCP_FAILURES["n"] >= self.MCP_MAX_FAILURES:
            NET["mcp-errors"] = NET.get("mcp-errors", 0) + 1
            return None, False
        else:
            NET["mcp"] += 1
            body = json.dumps({"jsonrpc": "2.0", "id": 1, "method": "tools/call",
                               "params": {"name": tool, "arguments": args}}).encode()
            req = urllib.request.Request(self.MCP_URL, data=body, headers={
                "User-Agent": USER_AGENT, "Content-Type": "application/json",
                "Accept": "application/json, text/event-stream"})
            try:
                with urllib.request.urlopen(req, timeout=self.MCP_TIMEOUT) as r:
                    res = json.loads(r.read())["result"]
                if not isinstance(res, dict):
                    raise ValueError(f"no result: {res!r:.100}")
                v = None if res.get("isError") else json.loads(res["content"][0]["text"])
            except NET_ERRORS + (ValueError, KeyError, IndexError, TypeError, AttributeError) as e:
                NET["mcp-errors"] = NET.get("mcp-errors", 0) + 1
                MCP_FAILURES["n"] += 1
                if MCP_FAILURES["n"] == self.MCP_MAX_FAILURES:
                    print(f"note: ocaml.org's MCP server failed {MCP_FAILURES['n']} times (last: "
                          f"{type(e).__name__}: {str(e)[:100]}); not called again in this run", file=sys.stderr)
                return None, False                # transient: not cached
            write_atomic(disk, json.dumps(v))
        self.mem[key] = v
        return v, True

    # Version lists change with each release: their answers are kept a day.
    VERSIONS_MAX_AGE = 24 * 3600

    def mcp_versions(self, name):
        """[{version, published, statuses}] of `name`, newest first, from
        ocaml_package_versions (one call per 500 versions): [] if `name` is
        not a package, None if the server did not answer."""
        key = ("mcp-versions", name)
        if key not in self.mem:
            out, offset = [], 0
            while True:
                args = {"package": name, "limit": 500, **({"offset": offset} if offset else {})}
                d, answered = self._mcp("ocaml_package_versions", self.VERSIONS_MAX_AGE, args)
                if not answered:
                    out = None
                    break
                if not isinstance(d, dict) or not isinstance(d.get("versions"), list):
                    break                       # not a package (the tool's error)
                out += [v for v in d["versions"] if isinstance(v, dict)]
                if not d.get("truncated") or not d["versions"] or len(out) >= 5000:
                    break
                offset = len(out)
            self.mem[key] = out
        return self.mem[key]

    def changelog(self, pkg, since=None, until=None):
        """The sections of pkg's changelog for the versions after `since` up to
        `until`, from ocaml_package_changelog: {file, versions, text,
        truncated}, or None (no changelog, no section heading names a version,
        or no section in the range). The text is written by the package's
        authors: untrusted."""
        args = {"package": pkg, **({"since": since} if since else {}), **({"until": until} if until else {})}
        d = self.mcp("ocaml_package_changelog", **args)
        if not isinstance(d, dict) or not d.get("section_versions") or not isinstance(d.get("content"), str):
            return None
        return {"file": d.get("file"), "versions": [str(v) for v in d["section_versions"]],
                "text": d["content"], "truncated": bool(d.get("truncated"))}

    def doc_modules(self, pkg, version):
        """{TopModule: 'lib/TopModule'} for pkg.version, or None without docs."""
        d = self.mcp("ocaml_package_documentation", package=pkg, version=version)
        if not d or d.get("documentation_status") not in (None, "success"):
            return None
        mods = {}
        for m in [m for lib in d.get("libraries", []) for m in lib.get("modules", [])] + d.get("modules", []):
            if m.get("path"):
                mods.setdefault(m["name"], m["path"].removesuffix("/index.html"))
        return mods or None

    def doc_page(self, pkg, version, path):
        """Plain-text documentation (signatures) of one module page, or None."""
        d = self.mcp("ocaml_module_documentation", package=pkg, version=version, path=path + "/index.html")
        return d.get("content") if d else None

    def doc_present(self, pkg, version, mods, kind, ident):
        """Is `ident` (reached through module path `mods`) documented for
        pkg.version? None when ocaml.org has no docs for that version."""
        top0 = mods[0] if mods else ident
        cached = self.mem.get(("docpath", pkg, top0))
        if cached and mods:
            # Same module path as in another version: skip the package index.
            page = self.doc_page(pkg, version, "/".join([cached] + mods[1:]))
            if page is not None:
                return defines(page, kind, ident)
        tops = self.doc_modules(pkg, version)
        if tops is None:
            return None
        if kind == "module" and not mods:
            if ident in tops:
                return True
            # maybe wrapped: Wrapper.ident
            return any(defines(self.doc_page(pkg, version, b) or "", "module", ident)
                       for b in list(tops.values())[:3])
        if mods[0] in tops:
            base, rest = tops[mods[0]], mods[1:]
        else:   # reached through an open of a wrapper module (e.g. Term from Why3)
            base = next((f"{b}/{mods[0]}" for b in list(tops.values())[:3]
                         if self.doc_page(pkg, version, f"{b}/{mods[0]}")), None)
            if base is None:
                return None     # page missing: ocaml.org docs are sometimes partial
            rest = mods[1:]
        page = self.doc_page(pkg, version, "/".join([base] + rest))
        if page is None:
            return None
        self.mem[("docpath", pkg, top0)] = base
        return defines(page, kind, ident)

    def published(self, pkg, version):
        """Release date (YYYY-MM-DD) of pkg.version, from ocaml_package_versions,
        or None."""
        for v in self.mcp_versions(pkg) or []:
            if v.get("version") == version and re.fullmatch(r"\d{4}-\d\d-\d\d", str(v.get("published") or "")):
                return v["published"]
        return None

    # An opam file on master can be edited (bounds, x-maintained): fetched again after a day.
    MASTER_MAX_AGE = 24 * 3600

    def opam_file(self, name, version, ref="master"):
        return self.raw(f"https://raw.githubusercontent.com/{REPO}/{ref}/packages/{name}/{name}.{version}/opam",
                        max_age=self.MASTER_MAX_AGE if ref == "master" else None)

    def upstream(self, name, version, ref="master"):
        """Upstream git URL of a package, from its opam dev-repo (or url src)."""
        text = self.opam_file(name, version, ref) or self.opam_file(name, version)
        if not text:
            return None
        m = re.search(r'^dev-repo:\s*"([^"]+)"', text, re.M)
        if m:
            return repo_url(m.group(1))
        m = re.search(r'src:\s*"(https://(?:github|gitlab)\.com/[^"]+)"', text)
        return repo_url(m.group(1)) if m else None


OCAML_REPO = "https://github.com/ocaml/ocaml"


def ocaml_releases(src):
    rel = [t for t in src.tags(OCAML_REPO) if re.fullmatch(r"[45]\.\d+\.0", t)]
    rel = [t for t in rel if opam_vcmp(t, "4.08.0") >= 0]
    return sorted(rel, key=functools.cmp_to_key(opam_vcmp))


def module_files(paths, module):
    """Source files defining compilation unit `module` (also foo.lwt.ml variants)."""
    m = module.lower()
    out = []
    for p in paths:
        base = p.rsplit("/", 1)[-1]
        stem = base.split(".", 1)[0].lower()
        if stem == m and (base.endswith(".mli") or base.endswith(".ml")):
            out.append(p)
    out.sort(key=lambda p: (not p.endswith(".mli"), "/test" in p, len(p)))
    return out


def defines(text, kind, ident):
    i = re.escape(ident)
    pats = {
        "value": [rf"\b(val|external|let|and)\s+(rec\s+)?{i}\b"],
        "constructor": [rf"(\||=|\[)\s*{i}\b", rf"\b{i}\s+of\b"],
        # a record field starts a line or follows { or ; (not `val x :`, not `~x:`)
        "field": [rf"(?m)(^|[{{;])[ \t]*(mutable\s+)?{i}\s*:(?!:)"],
        "type": [rf"\btype\s+(nonrec\s+)?([^=]*\s)?{i}\b"],
        "module": [rf"\bmodule\s+(type\s+)?{i}\b"],
    }[kind]
    return any(re.search(p, text) for p in pats)


def present(src, repo, ref, path_mods, kind, ident, stdlib=False):
    """Is `ident` (of `kind`), reached through modules `path_mods`, defined at `ref`?
    Returns True/False, or None when the source cannot be located."""
    paths = src.tree(repo, ref)
    if not paths:
        return None
    if stdlib:
        paths = [p for p in paths if p.startswith("stdlib/")]
    if kind == "module" and not path_mods:
        return bool(module_files(paths, ident))
    # Try the innermost module that has its own file, then outer ones.
    for k in range(len(path_mods) - 1, -1, -1):
        files = module_files(paths, path_mods[k])
        if not files:
            continue
        inner = path_mods[k + 1:]
        for f in files[:3]:
            text = src.file_at(repo, ref, f) or ""
            if inner and not defines(text, "module", inner[0]):
                continue
            if kind == "module" or defines(text, kind, ident):
                return True
        return False
    return False


def bisect_presence(check, versions, bad, allowed=lambda v: True, cap=10):
    """`check(v)` -> True/False/None. The identifier is missing at version `bad`.
    Walk outwards from `bad` (history is not always monotonic): the first newer
    version that has it gives ('>=', v); otherwise the closest older version
    that has it gives ('<', version right after it). Only versions accepted by
    `allowed` (the package's other existing bounds) are considered."""
    evidence = {}

    def c(v):
        if v not in evidence:
            evidence[v] = check(v)
        return evidence[v]

    newer = [v for v in versions if opam_vcmp(v, bad) > 0 and allowed(v)]
    for v in newer[:cap]:
        if c(v):
            return (">=", v), evidence
    older = [v for v in versions if opam_vcmp(v, bad) < 0 and allowed(v)]
    for v in reversed(older[-cap:]):
        if c(v):
            later = [w for w in versions if opam_vcmp(w, v) > 0]
            return ("<", later[0] if later else bad), evidence
    return None, evidence


def existing_bounds(opam_text, dep):
    """[(op, version)] from a simple `"dep" {>= "a" & < "b"}` constraint."""
    m = re.search(r'^\s*"' + re.escape(dep) + r'"\s*\{([^}]*)\}', opam_text or "", re.M)
    if not m or "|" in m.group(1):
        return []
    return re.findall(r'(>=|<=|>|<|=)\s*"([^"]+)"', m.group(1))


def satisfies(v, bounds):
    ops = {">=": lambda c: c >= 0, ">": lambda c: c > 0, "<=": lambda c: c <= 0,
           "<": lambda c: c < 0, "=": lambda c: c == 0}
    return all(ops[op](opam_vcmp(v, b)) for op, b in bounds)


def norm(s):
    return re.sub(r"[-_]", "", s.lower())


def constraint_edit(opam_text, dep, op, version):
    """Return (old_line, new_line) for the dependency `dep` in `opam_text`."""
    m = re.search(r'^([ \t]*)"' + re.escape(dep) + r'"[ \t]*(\{([^}]*)\})?[^\n]*$', opam_text or "", re.M)
    new_term = f'{op} "{version}"'
    if not m:
        return None, f'"{dep}" {{{new_term}}}'
    old = m.group(0)
    filt = (m.group(3) or "").strip()
    if not filt:
        terms = []
    elif "|" in filt or "(" in filt:
        terms = [f"({filt})"]
    else:
        terms = [t.strip() for t in filt.split("&")]
        drop = r'(>=|>)\s*"' if op in (">=", ">") else r'(<=|<)\s*"'
        terms = [t for t in terms if not re.match(drop, t)]
    terms.append(new_term)
    return old.strip(), f'"{dep}" {{{" & ".join(terms)}}}'


def license_id(text):
    """A coarse SPDX guess from a license text."""
    t = re.sub(r"\s+", " ", text or "")
    if "Permission is hereby granted, free of charge" in t:
        return "MIT"
    if "Apache License" in t and "Version 2.0" in t:
        return "Apache-2.0"
    if "Redistribution and use in source and binary forms" in t:
        return "BSD-3-Clause" if re.search(r"Neither the name|names of its contributors", t) else "BSD-2-Clause"
    if "Permission to use, copy, modify, and/or distribute" in t or "Permission to use, copy, modify, and distribute" in t:
        return "ISC"
    if "GNU LESSER GENERAL PUBLIC LICENSE" in t:
        return "LGPL-2.1-or-later (check: -only vs -or-later, OCaml linking exception)" if "2.1" in t else "LGPL-3.0-or-later (check)"
    if "GNU GENERAL PUBLIC LICENSE" in t:
        return "GPL-3.0-or-later (check)" if "Version 3" in t else "GPL-2.0-or-later (check)"
    if "Mozilla Public License" in t and "2.0" in t:
        return "MPL-2.0"
    return None


def opam_field(text, field):
    m = re.search(r'^' + re.escape(field) + r':\s*"([^"]*)"', text or "", re.M)
    return m.group(1) if m else None


def url_src(text):
    m = re.search(r'^url\s*\{.*?\bsrc:\s*"([^"]+)"', text or "", re.M | re.S)
    return m.group(1) if m else None
