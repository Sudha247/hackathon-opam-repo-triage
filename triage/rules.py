"""Rules on one failed job's log."""

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
    fetch,
)
from .ci import (  # noqa: F401
    excerpt,
    job_dims,
    log_text,
    opam_vcmp,
    split_pkg,
)
from .where import fix_where, pr_or_other
from .sources import (  # noqa: F401
    API_HISTORY,
    BISECT_CAP,
    MODULE_FIND,
    bisect_presence,
    constraint_edit,
    defines,
    existing_bounds,
    history_has_module,
    history_presence,
    module_files,
    norm,
    ocaml_releases,
    present,
    satisfies,
)


def log_info(text):
    """Facts from a job log: failing package, installed versions, error lines."""
    info = {"installed": {}}
    for line in text.splitlines():
        m = re.search(r"-> installed ([\w+-]+)\.(\S+)", line)
        if m:
            info["installed"][m.group(1)] = m.group(2)
    for m in re.finditer(r'"([\w+-]+)\.([^"\s]+)"', "\n".join(l for l in text.splitlines() if "->" in l and '"' in l)):
        info["installed"].setdefault(m.group(1), m.group(2))   # day10 dependency graph
    m = re.search(r"#=== ERROR while (compiling|installing|removing) ([\w+-]+)\.(\S+?) =", text)
    if m:
        info["action"] = m.group(1)
        info["failing"] = (m.group(2), m.group(3))
    m = re.search(r"ocaml(?:-base)?-compiler\.(\d+\.\d+\.\d+)", text)
    if m:
        info["ocaml"] = m.group(1)
    blk = excerpt(text, max_output_lines=200).get("error_block", "")
    info["error_lines"] = [re.sub(r"^# ?", "", l) for l in blk.splitlines()]
    if not blk:
        # Older day10 logs have no "#=== ERROR while" block: the build output
        # (lines starting with "- ") comes before opam's "[ERROR] The
        # compilation of pkg failed at ..." line.
        m = re.search(r"(?m)^\[ERROR\] The (compilation|installation) of ([\w+-]+)\.(\S+?) failed at", text)
        if m:
            info["action"] = "compiling" if m.group(1) == "compilation" else "installing"
            info.setdefault("failing", (m.group(2), m.group(3)))
            before = text[:m.start()].splitlines()
            out = [l[2:] if l.startswith("- ") else l for l in before[-80:]]
            cut = max((i for i, l in enumerate(out) if re.match(r"\[(NOTE|ERROR)\]|Processing:", l)), default=-1)
            info["error_lines"] = out[cut + 1:]
            blk = "\n".join(info["error_lines"])
    info["test_diff"] = ".corrected" in blk or "git --no-pager diff" in blk
    info["base"] = excerpt(text).get("base")
    return info


UNBOUND_RES = [
    (re.compile(r"Unbound value [`\"']?([\w.']+)"), "value"),
    (re.compile(r"Unbound constructor [`\"']?([\w.']+)"), "constructor"),
    (re.compile(r"Unbound record field [`\"']?([\w.']+)"), "field"),
    (re.compile(r"Unbound type constructor [`\"']?([\w.']+)"), "type"),
    (re.compile(r"Unbound module type [`\"']?([\w.']+)"), "module-type"),
    (re.compile(r"Unbound class type [`\"']?([\w.']+)"), "class-type"),
    (re.compile(r"Unbound class (?!type )[`\"']?([\w.']+)"), "class"),
    (re.compile(r"Unbound module (?!type )[`\"']?([\w.']+)"), "module"),
]


# The standard library's sources, read for the releases ocaml.org's API history cannot tell.
STDLIB_SOURCES = "OCaml's stdlib .mli files"

# Dependencies not searched for the module of an unbound identifier (as in the
# guess from the dependencies), besides the conf-* packages: no OCaml module.
NOT_SEARCHED = ("ocaml", "dune", "ocamlfind")


def dep_names(opam_text, field):
    """The package names of an opam file's `depends` or `depopts` field, in order."""
    m = re.search(rf"(?m)^{field}:\s*\[([^\]]*)\]", opam_text or "")
    return re.findall(r'"([\w+-]+)"', re.sub(r"\{[^}]*\}", "", m.group(1))) if m else []


def module_find_note(found, top, installed, opam_text, fname):
    """What ocaml.org's module search (Sources.module_providers) said about
    module `top`, for the note of an unbound identifier that no dependency's
    history explains: "" when it gave no answer, or a partial one without a
    match."""
    if not found or (found["partial"] and not found["matches"]):
        return ""
    if found["matches"]:
        deps = set(dep_names(opam_text, "depends") + dep_names(opam_text, "depopts"))
        descs = []
        for m in found["matches"][:3]:
            p, what = m["package"], []
            if m["module"] != [top]:
                what.append("as " + ".".join(m["module"]))
            if m["library"]:
                what.append(f"library {m['library']}")
            if p == fname:
                what.append("the failing package")
            elif p not in installed:
                what.append("not installed in this job" if p in deps else f"not a dependency of {fname}")
            descs.append((f"{p} {m['version']}" if m["version"] else p) + (f" ({', '.join(what)})" if what else ""))
        more = len(found["matches"]) - len(descs)
        return (f"ocaml.org's docs have module {top} in " + "; ".join(descs)
                + (f"; and {more} more" if more > 0 else "") + ".")
    if found["distribution"]:
        return f"ocaml.org: {top} comes with the OCaml compiler, not with an opam package."
    no_docs = [p for p in found["no_docs"] if p in installed]
    return (f"ocaml.org's docs have no module {top} in the latest documented version of the dependencies"
            + (f" (without docs: {', '.join(no_docs[:5])}{', ...' if len(no_docs) > 5 else ''})" if no_docs else "")
            + ".")


def find_unbound(lines):
    text = "\n".join(lines)
    for rx, kind in UNBOUND_RES:
        m = rx.search(text)
        if m:
            parts = m.group(1).strip("`\"'.").split(".")
            return kind, parts[:-1], parts[-1]
    m = re.search(r"There is no field (\w+) within type [`\"']?([\w.]+)", text)
    if m:
        mods = m.group(2).split(".")[:-1]
        return "field", mods, m.group(1)
    return None


TOPLEVEL_LINE = re.compile(r"^[-+]\s*(val [\w'.]+ :|- : |type |module |exception |Line \d+, characters "
                           r"|Characters \d+-\d+|Error: |Warning \d+)")


def toplevel_diff(err):
    """Changed lines of a test diff, if most of them are OCaml toplevel output
    (mdx `# expr;;` answers, types, error locations)."""
    lines = [l for l in err if re.match(r"^[-+](?![-+]{2})", l)]
    hits = [l for l in lines if TOPLEVEL_LINE.match(l)]
    return hits if lines and len(hits) * 2 >= len(lines) else None


def suggest_for_job(src, job, text, pr_packages, pr_sha):
    info = log_info(text)
    dd = job_dims(job)
    out = {"variant": job["variant"], "url": job["url"], "kind": job["kind"], "experimental": job["experimental"]}
    if job["platform"][:1] == ["day10"]:
        out["_distros"] = [dd["distro"] or dd["os"]]   # see accepted_but_failing
    m = re.search(r"These packages are still missing: (.+)", text)
    if "failing" not in info and m:
        names = m.group(1).split()
        dims = job_dims(job)
        plat = dims["distro"] or ("debian-13" if dims["os"] == "linux" else dims["os"])
        owner = job["revdep"] or job["target"]
        is_conf = split_pkg(owner)[0].startswith("conf-")
        out.update(rule="missing-system-package", failing=owner,
                   **(pr_or_other(owner, owner in pr_packages) if is_conf
                      else fix_where("other-package", "the dependency declaring these depexts")),
                   error=f"System packages not available on {plat}: {', '.join(names)}",
                   new=f'x-ci-accept-failures: ["{plat}"]   (or correct the depext names for {plat})',
                   confidence="medium",
                   note="Either the depext name is wrong for this distribution, the package needs an extra "
                        "repository (EPEL, CRB, ports), or it does not exist there.")
        return out
    m = re.search(r"(Segmentation fault|Bus error|Illegal instruction)\s+(day10[\w-]*|obuilder[\w-]*)", text)
    if m:
        out.update(rule="transient", error=f"{m.group(1)} of the CI tool {m.group(2)}",
                   note="The CI tool crashed, not the package. Rerun the job.")
        return out
    m = re.search(r"runc run failed: [^\n]{0,120}", text)
    if m:
        out.update(rule="transient", error="The CI could not start the build container",
                   note="A CI infrastructure failure (the container did not start). Rerun the job.")
        return out
    m = re.search(r"HTTP error code (5\d\d|429)|Could not resolve host|Connection (?:reset|timed out)", text)
    if "failing" not in info and m:
        out.update(rule="transient", error=m.group(0), note="Network error. Rerun the job.")
        return out
    if "failing" not in info:
        out["note"] = "The log has no build error from opam: a dependency failed, or the network or the solver did."
        return out
    fname, fver = info["failing"]
    out["failing"] = f"{fname}.{fver}"
    in_pr = f"{fname}.{fver}" in pr_packages
    out.update(pr_or_other(f"{fname}.{fver}", in_pr))
    ref = pr_sha if in_pr else (info.get("base") or "master")
    opam_text = src.opam_file(fname, fver, ref) or src.opam_file(fname, fver)
    err = info["error_lines"]

    # opam reinstalls a package at the same version (a "recompile" in the
    # plan, e.g. after downgrading its dependencies) and removes it first; for
    # a package with a `remove:` field, the rebuild then runs in an empty
    # source directory ("No rule to make target", dune building nothing).
    # Seen with opam 2.6 on lower-bounds jobs; frama-c's own PR passed because
    # frama-c was pinned there.
    plan = re.search(rf"(?m)^\s*- recompile\s+{re.escape(fname)}\s+{re.escape(fver)}\s+(?!.*pinned).*$", text)
    removed = plan and re.search(rf"(?m)^-> removed\s+{re.escape(fname)}\.{re.escape(fver)}\s*$", text[plan.end():])
    if removed and re.search(r"(?m)^remove:", opam_text or ""):
        out.update(rule="opam-same-version-reinstall", confidence="high",
                   **fix_where("ci", "opam (the CI's opam version)"),
                   error=f"{fname}.{fver} is recompiled at the same version, removed, then fails to "
                         f"{'install' if info.get('action') == 'installing' else 'build'}",
                   note=f"opam rebuilds {fname}.{fver} at the same version and also removes it, which needs "
                        f"its sources (it has a `remove:` field). Since opam 2.6 (ocaml/opam#6440) both steps "
                        f"move the same source directory, so the one that runs second gets an empty directory. "
                        f"An opam bug, not a problem of the packages or their bounds: the job can be ignored. "
                        f"A pinned {fname} (as in its own PR) does not hit it.")
        return out

    # Install failures: the install command failed (its output is in the block),
    # or files listed for installation were never built because an earlier
    # build command failed without stopping the build.
    if info.get("action") == "installing" and any("couldn't be installed" in l for l in err):
        missing = [f for l in err for f in re.findall(r"/([^/\s]+) to /", l)]
        exts = collections.Counter(os.path.splitext(f)[1] or f for f in missing)
        cut = text.find("#=== ERROR while installing")
        earlier = [re.sub(r"^[-#] ?", "", l).strip() for l in text[:cut].splitlines()
                   if re.search(r"\bError\b|error:|Parse error|fatal error", l)
                   and not re.search(r"Error report|\[ERROR\] The (compilation|installation)", l)]
        out["install"] = ("Files listed for installation were not built: "
                          + ", ".join(f"{n} {x}" for x, n in exts.most_common()))
        if earlier:
            err = earlier[-15:]
            out["install"] += f"; earlier build error: {err[-1][:150]}"
        else:
            out.update(rule="install-missing-files", error=out.pop("install"), confidence="low",
                       note="The build does not produce these files on this platform (.cmxs files need native "
                            "dynlink, for example). Make them optional in the .install file or the build, or "
                            "restrict `available`.")
            return out

    # Rule: safe-string
    if re.search(r"has type\s+string\s+but an expression was expected of type\s*$", "\n".join(err), re.M) and \
       re.search(r"=?\s*bytes", "\n".join(err)):
        old, new = constraint_edit(opam_text, "ocaml", "<", "4.06")
        out.update(rule="safe-string", error="String used where bytes is expected (safe-string, default since OCaml 4.06)",
                   dep="ocaml", old=old, new=new, confidence="high")
        return out

    info["pr_packages"] = pr_packages
    info["_text"] = text
    lr = log_rules(src, err, fname, opam_text, info, job)
    if lr:
        out.update(lr)
        return out

    # In a test diff (cram/mdx), errors are part of the expected or actual
    # output: only the lines before the diff are compiler errors.
    pre_diff = err
    if info["test_diff"]:
        cut = next((i for i, l in enumerate(err) if re.match(r"(diff --git|--- |\S*git --no-pager diff)", l)), len(err))
        pre_diff = err[:cut]
    unb = find_unbound(pre_diff)
    if unb:
        kind, mods, ident = unb
        out["error"] = f"Unbound {kind.replace('-', ' ')} {'.'.join(mods + [ident])}"
        top = mods[0] if mods else (ident if kind == "module" else None)
        if top is None:
            out["note"] = "The identifier has no module path, so the tool cannot tell which package it comes from."
            out["_unexplained"] = True        # for the LLM fallback (popped by suggest)
            return out
        # Stdlib?
        rel = ocaml_releases(src)              # [] if the OCaml tags could not be listed
        latest, t414 = (rel[-1] if rel else None), "4.14.0"
        top_m = mods[1] if top == "Stdlib" and len(mods) > 1 else top
        def stdlib_mli(r, m):
            return src.raw(f"https://raw.githubusercontent.com/ocaml/ocaml/{r}/stdlib/{m.lower()}.mli")
        if (latest and stdlib_mli(latest, top_m)) or stdlib_mli(t414, top_m):
            if not rel:
                out["note"] = ("Standard library identifier, but the OCaml releases could not be listed "
                               "(git ls-remote failed): the tool could not check which releases have it.")
                return out
            inner = mods[2:] if top == "Stdlib" else mods[1:]
            # ocaml.org documents the standard library with the compiler:
            # ocaml-base-compiler up to 5.2, ocaml-compiler from 5.3 (where the
            # compiler's sources moved). One API-history call for each, with
            # the module path in full (Stdlib.List: "List" alone also names
            # compiler-libs modules there); the .mli of a release for what it
            # cannot tell (no docs, a failed call).
            path = ["Stdlib"] if kind == "module" and not mods else ["Stdlib", top_m] + inner
            split = "5.3.0"
            # Only the releases ocaml.org lists for each package: the tool rejects a
            # range that starts at a version it does not know (4.08.0 of
            # ocaml-base-compiler, say).
            def listed(pkg, rs):
                known = set(src.versions(pkg) or [])
                return [r for r in rs if r in known]
            older = listed("ocaml-base-compiler", [r for r in rel if opam_vcmp(r, split) < 0])
            newer = listed("ocaml-compiler", [r for r in rel if opam_vcmp(r, split) >= 0])
            hists = {"ocaml-base-compiler": older and src.api_history("ocaml-base-compiler", path, kind, ident, older),
                     "ocaml-compiler": newer and src.api_history("ocaml-compiler", path, kind, ident, newer)}
            used = set()

            def check(r):
                h = hists["ocaml-base-compiler" if opam_vcmp(r, split) < 0 else "ocaml-compiler"]
                p = history_presence(h or None, r)
                if p is True or p is False:
                    used.add(API_HISTORY)
                    return p
                used.add(STDLIB_SOURCES)
                text = stdlib_mli(r, top_m)
                if kind == "module" and not mods:
                    return text is not None
                if text is None:
                    return False
                if inner and not defines(text, "module", inner[0]):
                    return False
                return defines(text, kind, ident)
            bad = info.get("ocaml", "0")
            bad_rel = max([r for r in rel if opam_vcmp(r, bad) <= 0] or [rel[0]], key=lambda r: [int(x) for x in r.split(".")])
            res, ev = bisect_presence(check, rel, bad_rel)
            out["evidence"] = {f"ocaml {v}": r for v, r in sorted(ev.items(), key=lambda kv: [int(x) for x in kv[0].split(".")])}
            if used:
                out["source"] = ", ".join(sorted(used))
            if res:
                op, v = res
                v = ".".join(v.split(".")[:2])
                old, new = constraint_edit(opam_text, "ocaml", op, v)
                out.update(rule="stdlib", dep="ocaml", old=old, new=new, confidence="high")
                # The bound drops OCaml releases the package supported: some authors
                # keep them and do without the identifier (grenier 0.17, Float.log2).
                name = ".".join(mods + [ident])
                out["note"] = (f"`{name}` is new in OCaml {v}: to keep supporting older releases, do without it "
                               "instead." if op == ">=" else
                               f"`{name}` was removed in OCaml {v}: to support it, do without it instead.")
            else:
                out["note"] = "Standard library identifier, but no OCaml release adds or removes it."
            return out
        # Library: candidate packages whose name matches the top module.
        cands = [p for p in info["installed"] if norm(top).startswith(norm(p)) and len(norm(p)) >= 2]
        cands.sort(key=lambda p: -len(p))
        by_name = bool(cands)
        if not cands and opam_text:
            # The module may be reached through an `open` (e.g. Term from Why3).
            # Rank the direct dependencies by the module names in the error text.
            deps = re.findall(r'^\s*"([\w+-]+)"', opam_text.split("depends:", 1)[-1].split("]", 1)[0], re.M)
            deps = [d for d in deps if d in info["installed"] and d not in ("ocaml", "dune", "ocamlfind")]
            mentioned = {norm(w) for w in re.findall(r"\b([A-Z]\w+)\.", "\n".join(err))}
            cands = sorted(deps, key=lambda d: not any(m.startswith(norm(d)) for m in mentioned))
        # ocaml.org's module search: which of these packages and of the other
        # direct (and optional) dependencies installed document module `top`,
        # at any depth (Term in why3 is Why3.Term), in their latest documented
        # version. Those come first, with the module's full path, and need not
        # have it in the version installed (it may have appeared later);
        # the guesses above follow, for when the search does not answer or
        # its packages are not the cause.
        searched = [d for d in dict.fromkeys(cands + dep_names(opam_text, "depends") + dep_names(opam_text, "depopts"))
                    if d in info["installed"] and d not in NOT_SEARCHED and not d.startswith("conf-")]
        found = src.module_providers(top, searched) if searched else None
        chosen = {}                                  # package -> full path of `top` in it (None: several)
        for m in (found or {}).get("matches", []):
            if m["package"] in info["installed"]:
                chosen[m["package"]] = None if m["package"] in chosen else m["module"]
        cands = list(chosen) + [p for p in cands if p not in chosen]
        for pkg in cands[:5]:
            bad = info["installed"].get(pkg)
            if not bad:
                continue
            path = chosen.get(pkg)
            if path and mods:
                doc_mods = path + mods[1:]           # Term.t_loc in why3: ['Why3', 'Term']
            elif path:
                doc_mods = path[:-1]                 # a top-level module found in a wrapper: ['Why3']
            else:
                doc_mods = mods if mods else []      # full path, e.g. ['Ortools', 'Sat']
            git_path = mods[1:] if mods and norm(mods[0]).startswith(norm(pkg)) and len(mods) > 1 else mods
            used = {MODULE_FIND} if pkg in chosen else set()
            bounds = existing_bounds(opam_text, pkg)

            def allowed(v, b=bounds):
                return satisfies(v, [x for x in b if x[0] != "="])
            # ocaml.org's API history of the item: one call for every version
            # bisect_presence may check (not for a top-level module: the tool
            # tracks the items of a module).
            hist = None
            if doc_mods:
                vs = src.versions(pkg) or []
                window = ([bad] + [v for v in vs if opam_vcmp(v, bad) > 0 and allowed(v)][:BISECT_CAP]
                          + [v for v in vs if opam_vcmp(v, bad) < 0 and allowed(v)][-BISECT_CAP:])
                hist = src.api_history(pkg, doc_mods, kind, ident, window)

            def check(v, pkg=pkg, doc_mods=doc_mods, git_path=git_path, used=used, hist=hist):
                # 1. ocaml.org's API history (ocaml_module_changes), for the
                # versions it could tell
                h = history_presence(hist, v)
                if h in (True, False):
                    used.add(API_HISTORY)
                    return h
                # 2. ocaml.org docs for pkg.v (no git, no token, any upstream
                # host), unless the history says it has none
                if h != "no-docs":
                    r = src.doc_present(pkg, v, doc_mods, kind, ident)
                    if r is not None:
                        used.add("ocaml.org docs")
                        return r
                # 3. fallback: shallow blob-less git fetch of the matching tag
                repo = src.upstream(pkg, v) or src.upstream(pkg, bad)
                t = repo and src.tag_for(repo, pkg, v)
                if not t:
                    return None
                used.add(f"git {repo}")
                if kind == "module" and not mods:
                    return bool(module_files(src.tree(repo, t), ident))
                return present(src, repo, t, git_path or [top], kind, ident)

            if not by_name and pkg not in chosen:
                # Accept a dependency only if it actually provides that module.
                # (The history tells about the module path of the call: `top`
                # only when that path is `top` alone.)
                has_mod = history_has_module(hist, bad) if len(doc_mods) == 1 else None
                if has_mod is None:
                    has_mod = src.doc_present(pkg, bad, [], "module", top)
                if has_mod is None:
                    repo = src.upstream(pkg, bad)
                    t = repo and src.tag_for(repo, pkg, bad)
                    has_mod = bool(t and module_files(src.tree(repo, t), top))
                if not has_mod:
                    continue
            at_bad = check(bad)
            if at_bad is None:
                out.setdefault("tried", []).append(f"{pkg}.{bad}: no ocaml.org docs and no matching git tag")
                continue
            if at_bad:
                out.setdefault("tried", []).append(f"{pkg}.{bad} defines it ({', '.join(sorted(used))}): "
                                                   f"not a version problem of {pkg}")
                continue
            res, ev = bisect_presence(check, src.versions(pkg) or [], bad, allowed=allowed)
            out["evidence"] = {f"{pkg} {v}": r for v, r in sorted(ev.items(), key=lambda kv: functools.cmp_to_key(opam_vcmp)(kv[0]))}
            out["evidence"][f"{pkg} {bad} (installed in failing job)"] = False
            out["source"] = ", ".join(sorted(used))
            if res:
                op, v = res
                old, new = constraint_edit(opam_text, pkg, op, v)
                out.update(rule="library-api", dep=pkg, old=old, new=new, confidence="high")
                if f"{pkg}.{fver}" in pr_packages and src.upstream(fname, fver, ref) == src.upstream(pkg, fver, ref):
                    out["alternative"] = f'"{pkg}" {{= version}}  (same upstream repository and version)'
                return out
        if "note" not in out:
            out.update(note="The identifier was not found in the history of any dependency the tool checked.",
                       _unexplained=True)
        extra = module_find_note(found, top, info["installed"], opam_text, fname)
        if extra:
            out["note"] += " " + extra
        return out

    if info["test_diff"]:
        top = toplevel_diff(err)
        if top and info.get("ocaml"):
            # Expected toplevel output (types, values, error locations) printed
            # by an older compiler: the test depends on the OCaml version.
            mm = ".".join(info["ocaml"].split(".")[:2])
            m = re.search(r'^[ \t]*"ocaml"[ \t]*(?:\{([^}]*)\})?[^\n]*$', opam_text or "", re.M)
            term = f'< "{mm}" | !with-test'
            filt = (m.group(1) or "").strip() if m else ""
            prev = re.search(r'\(<\s*"[^"]+"\s*\|\s*!with-test\)', filt)
            if prev:                    # already disabled from a later version: lower it
                new = f'"ocaml" {{{filt.replace(prev.group(0), "(" + term + ")")}}}'
            elif filt:
                new = f'"ocaml" {{{filt} & ({term})}}'
            else:
                new = f'"ocaml" {{{term}}}'
            out.update(rule="test-output-ocaml", dep="ocaml", confidence="medium",
                       error=f"Toplevel output in the tests differs on OCaml {info['ocaml']}: {top[0][:100]}",
                       old=m.group(0).strip() if m else None,
                       new=new + f"   (keeps the package installable, disables its tests on OCaml >= {mm})",
                       alternative=f'"@runtest" {{with-test & ocaml:version < "{mm}"}}   (on the test command)',
                       note=f"{mm} is the oldest OCaml version that fails in this run; the output may have changed in "
                            f"an earlier release (5.4 prints shorter type paths, for example). Updating the "
                            f"expected output upstream fixes it for good.")
            return out
        out["rule"] = "test-output"
        out["error"] = "Test output differs (cram/mdx)"
        return out   # completed by the caller with a passing job's versions
    if out.get("install"):
        out.update(error=out.pop("install"), note="The tool does not recognise the earlier build error.",
                   _unexplained=True)
        return out
    out.update(note="Unrecognised error.", _unexplained=True)
    return out


def complete_test_output(src, r, job, text, data, cache_dir):
    """For a test-output diff: compare the failing job's installed versions with a
    passing job of the same package and suggest a with-test upper bound."""
    bad = log_info(text)
    fname, fver = bad["failing"]
    same = [k for k in data["jobs"] if k["target"] == job["target"] and k["revdep"] == job["revdep"]
            and k["kind"] == job["kind"] and k["category"] == "passed"]
    where = "this run"
    if not same:
        dated_candidates(src, r, bad, fname, fver)
        return
    ok = log_info(log_text(fetch(same[0]["url"], cache_dir)))
    opam_text = src.opam_file(fname, fver, bad.get("base") or "master") or ""
    deps = set(re.findall(r'^\s*"([\w+-]+)"', opam_text, re.M))
    diffs = []
    for p, v in bad["installed"].items():
        w = ok["installed"].get(p)
        if w and w != v and p in deps and p not in ("ocaml", "dune", "ocaml-base-compiler", "ocaml-compiler"):
            diffs.append((p, w, v))
    blob = "\n".join(bad["error_lines"]).lower()
    diffs.sort(key=lambda d: (d[0].lower() not in blob, d[0]))
    r["compared_with"] = "/".join(same[0]["platform"]) + f" in {where}"
    r["candidates"] = []
    for p, good_v, bad_v in diffs[:4]:
        if opam_vcmp(bad_v, good_v) > 0:
            vs = [v for v in src.versions(p) or [] if opam_vcmp(v, good_v) > 0]
            first = vs[0] if vs else bad_v
            # Test-only: keep the main constraint, add a separate with-test line.
            c = {"dep": p, "passing": good_v, "failing": bad_v, "old": None,
                 "new": f'"{p}" {{with-test & < "{first}"}}   (new line in depends)'}
            log = changelog_excerpt(src, p, good_v, first)
            if log:
                c["changelog"] = log
            r["candidates"].append(c)
    r["confidence"] = "low (dependencies whose version differs between a passing job and the failing one)"


# C/C++ errors that newer compilers introduced: code that built with an older
# GCC fails on recent distributions (debian-13, fedora, archlinux, ...).
NEWER_CC = [
    (re.compile(r"error: .*\[-W(implicit-int|implicit-function-declaration|incompatible-pointer-types|"
                r"int-conversion|return-mismatch|declaration-missing-parameter-type)\]"),
     "GCC 14 turned -W{0} into an error by default: the C code relies on pre-C99 rules",
     "Fix the C code upstream. For an old release, add a patch or `x-maintained: false`; "
     "adding -fpermissive or -Wno-error={0} to the C flags also works."),
    (re.compile(r"'u?int(?:8|16|32|64)_t' (?:does not name a type|was not declared in this scope|has not been declared)"
                r"|is defined in header '<cstdint>'"),
     "Since GCC 13, libstdc++ headers no longer include <cstdint> for you",
     "Add `#include <cstdint>` upstream, or a patch in the opam file for an old release."),
    (re.compile(r"(?:cannot use keyword|expected identifier or '\(' before) '(?:bool|true|false)'"
                r"|'bool' cannot be defined via 'typedef'"),
     "GCC 15 compiles C as C23 by default, where bool, true and false are keywords",
     "Fix the C code upstream, or build it with -std=gnu17."),
    (re.compile(r"error: .*\[-Werror=([\w-]+?)=?\]"),
     "The package builds with -Werror, and a newer compiler emits a new warning (-W{0})",
     "Drop -Werror from release builds upstream. For an old release, add a patch or `x-maintained: false`."),
]


def system_package_version(text, lib):
    """Version of the system package providing `lib`, from the depext
    installation lines of a log (apt, dnf/yum, apk, pacman, zypper)."""
    pats = [r"Setting up (?:lib)?{0}[\w.+-]*?(?:-dev)?:\w+ \(([^)]+)\)",          # apt
            r"(?m)^[-\s]*(?:lib)?{0}[\w+-]*?-devel\s+\w+\s+(\d\S*)\s",       # dnf table
            r"Installing\s*:\s*(?:lib)?{0}[\w+-]*?-devel-(\d[^\s]*)",
            r"Installing (?:lib)?{0}[\w+-]*? \(([^)]+)\)",                     # apk
            r"installing (?:lib)?{0} \(([^)]+)\)"]                              # pacman
    for name in dict.fromkeys([lib, re.sub(r"\d+$", "", lib)]):
        for pat in pats:
            m = re.search(pat.format(re.escape(name)), text, re.I)
            if m:
                return m.group(1)
    return None


def known_errors(text, full, fname, opam_text, info, dims, platform, deps):
    """Failures with a recognisable message and a known cause."""
    installed = info.get("installed") or {}
    m = re.search(r"no such function: unixepoch", text)
    if m:
        have = system_package_version(full, "sqlite3")
        return {"rule": "system-library-version", "keep": True, "confidence": "high", "_plat": platform,
                "error": f"sqlite3 >= 3.38 required (unixepoch); {platform} has {have or 'an older version'}",
                "note": f"The SQL uses `unixepoch()`, added in SQLite 3.38, and the system SQLite is older on "
                        f"{platform}. Exclude these distributions (`x-ci-accept-failures`, or `available` on "
                        f"os-distribution and os-version), or avoid `unixepoch()` upstream."}
    m = re.search(r"/lib/([\w+-]+)/META: No such file or directory", text)
    if m and m.group(1) != fname:
        pkg = m.group(1)
        return {"rule": "broken-install", "confidence": "medium", "keep": True, "error": m.group(0),
                "note": f"{pkg}'s directory in the switch has no META file: its installation was incomplete when "
                        f"{fname} was built (being removed or reinstalled by opam at the same time). This is not a "
                        f"missing dependency of {fname} (that gives `Library \"{pkg}\" not found`): look at what "
                        f"opam did with {pkg} in this job, or rerun it."}
    m = re.search(r"undefined reference to [`'‘]?([\w.$@]+)", text)
    if m and re.search(r"collect2|ld returned|linker command failed|/ld:", text):
        sym = m.group(1)
        hint = ("core's linux_ext needs `core_unix_gettid`, which jst-config only enables with some glibc "
                "versions: an old core/jst-config combination" if sym == "core_unix_gettid" else
                "the LAPACKE functions are missing from the system LAPACK: a depext providing LAPACKE (or a newer "
                "one) is needed" if sym.startswith("LAPACKE_") else
                f"the library that provides `{sym}` is missing, too old, or not linked")
        return {"rule": "link-error", "error": f"undefined reference to `{sym}`", "confidence": "medium",
                "keep": True, "note": f"Link failure on {platform}: {hint}."}
    m = re.search(r"unsupported option '(-m[\w=-]+)' for target '(arm64|aarch64)[^']*'", text)
    if m:
        return {"rule": "platform-unsupported", "confidence": "high", "keep": True,
                "error": f"The build passes the x86-only flag {m.group(1)} to the {m.group(2)} compiler",
                "note": "The C flags are x86-specific: upstream should only pass them on x86 (dune's "
                        "%{architecture}, or a configurator check). For this release, exclude arm64 with "
                        "`available: arch != \"arm64\"` if every arm64 build fails, or x-ci-accept-failures "
                        "for the platforms concerned."}
    m = re.search(r"execve\((\./[\w./-]+)\): No such file or directory", text)
    if m and dims["os"] == "freebsd":
        return {"rule": "platform-unsupported", "confidence": "medium", "keep": True,
                "error": f"{m.group(1)} cannot be run on FreeBSD", "note":
                f"{m.group(1)} exists, so its #! interpreter does not: usually `#!/bin/bash`, which FreeBSD does "
                f"not have. Upstream: use `#!/usr/bin/env bash` (or sh), or run it as `bash {m.group(1)}` in "
                f"the build; conf-bash provides bash."}
    if re.search(r"exception Stack overflow", text) and "flambda" in info.get("_variant", ""):
        return {"rule": "test-platform", "confidence": "medium", "keep": True,
                "error": "Stack overflow with flambda",
                "note": "Only the flambda compiler overflows: its inlining makes deep recursion use more stack. "
                        "Upstream can make the recursion tail-recursive or less deep; otherwise this extras "
                        "job's failure is usually accepted."}
    m = re.search(r"Could NOT find Python3?\b[^\n]*", text)
    if m:
        r = {"rule": "missing-tool", "error": m.group(0).strip()[:200], "confidence": "high"}
        if "conf-python-3" in deps:
            r["note"] = f"conf-python-3 is a dependency, but CMake cannot find Python on {platform}."
        else:
            r.update(dep="conf-python-3", old=None, new='"conf-python-3" {build}   (new line in depends)',
                     note="The CMake build needs a Python 3 interpreter.")
        return r
    if (re.search(r"fatal: not a git repository", text) and re.search(r"git (describe|rev-parse|log)", text)
            and not re.search(r"Error: (Program|Library) ", text)):     # another cause, with git noise
        return {"rule": "needs-git-checkout", "confidence": "high",
                "error": "The build runs git, but the release archive is not a git checkout",
                "note": "The build calls git (`git describe` or similar) to find the version. Release archives "
                        "have no .git directory, so this fails everywhere. Fix upstream: take the version from "
                        "the opam file (`%{version}%`) or from dune's `%%VERSION%%` substitution."}
    m = re.search(r"cannot create (?:regular file|directory) '?(/usr/[^':\s]*)'?: Read-only file system", text)
    if m:
        return {"rule": "install-outside-prefix", "confidence": "high",
                "error": f"The install step writes to {m.group(1)}, outside the opam switch",
                "note": "The install command ignores the switch's prefix. Pass the switch's directories to it "
                        "in the opam file, for example `PREFIX=%{prefix}%`, `BINDIR=%{bin}%` or "
                        "`LIBDIR=%{lib}%`, whichever variables the Makefile uses."}
    if re.search(r"No known cycle-counting instruction", text):
        arch = dims["arch"]
        fname = ".".join(info.get("failing") or (fname,))
        return {"rule": "unsupported-arch", "confidence": "high",
                "error": f"{fname} has no cycle counter for {arch}",
                "new": f'available: arch != "{arch}"   (in {fname}, combined with its existing `available`)',
                "old": None, "note": f"This version of {fname} does not support {arch}; newer versions do."}
    if re.search(r"Core_kernel shadows Unix|Core_kernel\.Unix", text) and re.search(r"Unbound (constructor|value)", text):
        return {"rule": "library-api", "confidence": "medium", "keep": True,
                "error": "Core_kernel shadows the Unix module: Unix constructors and values are not found",
                "note": f"{fname} opens Core_kernel, whose Unix module hides the standard one, then uses "
                        f"Unix. Fix upstream (use `Caml_unix` or `Stdlib.Unix`); if only an optional part "
                        f"(a depopt such as lwt) is affected, a `conflicts` entry for it avoids the build."}
    m = re.search(r"Unbound value Conv\.(\w+)", text)
    if m and "cmdliner" in installed and opam_vcmp(installed["cmdliner"], "2.0.0") >= 0:
        old, new = constraint_edit(opam_text, "cmdliner", "<", "2.0.0")
        return {"rule": "library-api", "dep": "cmdliner", "old": old, "new": new, "confidence": "high",
                "error": m.group(0),
                "note": f"`Arg.Conv.{m.group(1)}` does not exist in cmdliner {installed['cmdliner']}: the "
                        f"conversion API changed in cmdliner 2.0."}
    if re.search(r"checking for ar\.\.\. no", text) and dims["os"] == "win32":
        return {"rule": "platform-unsupported", "confidence": "medium",
                "error": f"The configure script of {fname} finds no `ar` on {platform}",
                "new": 'available: os != "win32"   (combined with any existing `available`)', "old": None,
                "note": f"{fname}'s configure script is not ported to Windows."}
    m = re.search(r"'NotForRedeclaration' is not a member of 'clang::Sema'|has no member named 'getOriginalNamespace'",
                  text)
    if m:
        clang = (re.search(r"libclang-(\d+)-dev", full) or [None, None])[1]
        return {"rule": "system-library-version", "keep": True, "confidence": "high", "_plat": platform,
                "error": m.group(0) + (f" (Clang {clang} installed)" if clang else ""),
                "note": f"{fname} uses Clang APIs that Clang 19 removed. A bound on conf-libclang does not "
                        f"choose the installed Clang (conf-libclang.N accepts any Clang up to N): it needs a "
                        f"conf-libclang version whose depexts install Clang 18 or older on {platform}, or an "
                        f"update to the newer Clang API."}
    if re.search(r"the expected type is\s+(?:# )?string option \* Parsetree\.(expression|pattern|core_type)", text):
        old, new = constraint_edit(opam_text, "ocaml", "<", "5.4")
        return {"rule": "stdlib", "dep": "ocaml", "old": old, "new": new, "confidence": "high",
                "error": "Pattern on Parsetree tuples: OCaml 5.4's labelled tuples changed their type",
                "note": "OCaml 5.4 added labelled tuples: the Parsetree's tuple nodes now hold "
                        "(label option * node) pairs. Code matching them directly needs OCaml < 5.4, or an "
                        "update (ppxlib's Ast versions abstract this)."}
    return None


def c_compiler_rule(text, platform):
    for rx, cause, fix in NEWER_CC:
        m = rx.search(text)
        if m:
            arg = m.group(1) if m.groups() else ""
            line = next((l for l in text.splitlines() if rx.search(l)), m.group(0))
            return {"rule": "newer-c-compiler", "error": line.strip()[:200], "confidence": "high",
                    "keep": True, "_plat": platform, "note": f"{cause.format(arg)} (seen on {platform}). {fix.format(arg)}"}
    return None


def log_rules(src, err, fname, opam_text, info, job):
    """Missing dune language version, tool, library or C header."""
    text = "\n".join(err)
    deps = set(re.findall(r'^\s*"([\w+-]+)"', (opam_text or "").split("depends:", 1)[-1].split("\n]", 1)[0], re.M))
    dims = job_dims(job)
    platform = dims["distro"] or ("debian (classic builder)" if dims["os"] == "linux" else dims["os"])
    m = re.search(r"implicit declaration of function ['‘]?(caml_\w+)", text)
    if m:
        ocaml = info.get("ocaml") or "this OCaml"
        return {"rule": "ocaml-runtime-api", "confidence": "medium", "keep": True,
                "error": f"The C stubs call {m.group(1)}, which OCaml {ocaml}'s runtime does not declare",
                "note": f"{m.group(1)} is not part of this OCaml's C API: it exists only in other OCaml versions, "
                        f"or in another compiler variant such as OxCaml. The package needs an `ocaml` constraint "
                        f"to the versions that have it, or a fix upstream (not a GCC problem, although GCC 14 "
                        f"reports it as an error)."}
    cc = c_compiler_rule(text, platform)
    if cc:
        return cc
    info["_variant"] = job["variant"]
    known = known_errors(text, info.get("_text") or text, fname, opam_text, info, dims, platform, deps)
    if known:
        return known
    m = re.search(r"maximum_version=(\d+)[\s\S]*?llvm_version=(\d[\w.]*)", text)
    if m and int(m.group(2).split(".")[0]) > int(m.group(1)):
        want, have = m.group(1), m.group(2)
        return {"rule": "system-library-version", "keep": True, "confidence": "high", "_plat": platform,
                "error": f"LLVM <= {want} required; {platform} has LLVM {have}",
                "note": f"{fname} needs LLVM {want} or older, but its depexts install the distribution's default "
                        f"LLVM, which is now {have.split('.')[0]} on {platform}. It needs versioned depexts there "
                        f"(llvm{want} and clang{want} compatibility packages, for example), or a new "
                        f"conf-libclang version for LLVM {have.split('.')[0]}."}
    m = re.search(r"No usable version of LLVM <= ([\d.x]+) found", text)
    if m:
        return {"rule": "system-library-version", "keep": True, "confidence": "medium", "error": m.group(0),
                "note": f"No LLVM found on {platform}: {fname} has no depext for it. Add one, or "
                        f"x-ci-accept-failures if LLVM is not packaged there."}
    m = (re.search(r"pkg-config (?:--atleast-version[= ](\S+)|--exact-version[= ](\S+)|--max-version[= ](\S+)) "
                   r"([\w.+-]+)\)?\s*\n(?:.*\n){0,2}?.*(?:exited with code|exit status) 1", text)
         or re.search(r"Requested '([\w.+-]+) (>=|<=|=) ([\w.~+-]+)' but version of .* is ([\w.~+-]+)", text))
    if m:
        if m.re.pattern.startswith("Requested"):
            lib, op, want, have = m.group(1), m.group(2), m.group(3), m.group(4)
        else:
            want = m.group(1) or m.group(2) or m.group(3)
            op = ">=" if m.group(1) else "=" if m.group(2) else "<="
            lib, have = m.group(4), system_package_version(info.get("_text", ""), m.group(4))
        too = "too old" if op == ">=" else "too new" if op == "<=" else "not the expected version"
        return {"rule": "system-library-version", "keep": True, "confidence": "high", "_plat": platform,
                "error": f"{lib} {op} {want} required; {platform} has {have or 'an older version'}",
                "note": f"The system {lib} is {too} on {platform}, and no depext can change that. Exclude "
                        f"these distributions (`x-ci-accept-failures`, or `available` on os-distribution and "
                        f"os-version), or relax the requirement upstream."}
    m = re.search(r"Version (\d+\.\d+) of the dune language is not supported", text)
    if m:
        old, new = constraint_edit(opam_text, "dune", ">=", m.group(1))
        return {"rule": "dune-lang", "error": m.group(0), "dep": "dune", "old": old, "new": new, "confidence": "high"}
    m = (re.search(r'Program "?([\w.+-]+)"? not found in the tree or in PATH', text)
         or re.search(r'"([\w.+-]+)": command not found', text)
         or re.search(r"([\w.+-]+): command not found", text)
         or re.search(r"Command not found: ([\w.+-]+)", text)
         or re.search(r"(?m)^\S*?([\w.+-]+): (?:command )?not found$", text))
    if m and (m.group(1).startswith("-") or (m.group(1) == "brew" and dims["os"] != "macos")
              or m.group(1).startswith("llvm-config")):     # configure scripts probing for llvm-config-N
        m = None   # an empty variable expanded as a command, or a probe for another platform's tool
    if m:
        tool = m.group(1)
        conf = f"conf-{tool}"
        r = {"rule": "missing-tool", "error": m.group(0)}
        family = re.split(r"[-\d]", dims["distro"] or "")[0]
        def field_text(name):
            if name + ":" not in (opam_text or ""):
                return ""
            rest = (opam_text or "").split(name + ":", 1)[1]
            return re.split(r"(?m)^[a-z][\w-]*:", rest, maxsplit=1)[0]
        depexts_text = field_text("depexts")
        accepted = re.findall(r'"([^"]+)"', field_text("x-ci-accept-failures"))
        declared = [pk.strip() for pk, flt in re.findall(r"\[([^\]]*)\]\s*\{([^}]*)\}", depexts_text)
                    if family and family in flt]
        if fname.startswith("conf-") and dims["distro"] and dims["distro"] in accepted:
            r.update(fix_where("ci", "the CI environment"),
                     note=f"{fname} lists {dims['distro']} in x-ci-accept-failures, so CI should have "
                          f"ignored this failure and the failures of the packages that depend on it.",
                     confidence="high")
        elif fname.startswith("conf-") and declared:
            r.update(fix_where("ci", "the CI environment"),
                     note=f"{fname} declares a depext for {family} ({', '.join(declared)}), but `{tool}` "
                          f"is missing on {platform}: the CI did not install it. Nothing to change in "
                          f"{fname}.", confidence="high")
        elif fname.startswith("conf-"):
            r.update(note=f"{fname} does not provide `{tool}` on {platform}: its depexts have no entry for that "
                          f"platform. Add one, unless the CI image should already have it.", confidence="medium")
        elif conf in deps:
            r.update(note=f"{conf} is a dependency, but `{tool}` is missing on {platform}: {conf} lacks a depext for it.",
                     confidence="medium")
        elif vs := src.versions(conf):
            r.update(dep=conf, old=None, new=f'"{conf}" {{build}}   (new line in depends)', confidence="medium")
        elif vs is None:
            r.update(note=f"`{tool}` is missing on {platform}. The tool could not check whether a {conf} package "
                          f"exists (ocaml.org did not answer): if it does, add it to depends; otherwise add a "
                          f"depext providing `{tool}`.", confidence="low")
        else:
            r.update(note=f"There is no {conf} package: add a depext providing `{tool}` on {platform}.", confidence="low")
        return r
    m = (re.search(r"Package (\S+) was not found in the pkg-config search path", text)
         or re.search(r"configure: error: ([\w.+-]+)\.pc not found", text))
    if m:
        conf = f"conf-{m.group(1)}"
        r = {"rule": "missing-pkg-config", "error": m.group(0)}
        if conf in deps:
            r.update(note=f"{conf} is a dependency, but its pkg-config file is missing on {platform}: {conf} lacks a depext for it.",
                     confidence="medium")
        elif vs := src.versions(conf):
            r.update(dep=conf, old=None, new=f'"{conf}"   (new line in depends)', confidence="medium")
        elif vs is None:
            r.update(note=f"{m.group(1)}.pc is missing on {platform}. The tool could not check whether a {conf} "
                          f"package exists (ocaml.org did not answer): if it does, add it to depends; otherwise "
                          f"add a depext providing {m.group(1)}.pc.", confidence="low")
        else:
            r.update(note=f"Add a depext providing {m.group(1)}.pc on {platform}.", confidence="low")
        return r
    m = re.search(r'Library "([\w.-]+)" not found', text) or re.search(r"Package `([\w.-]+)' not found", text)
    if m:
        lib = m.group(1)
        pkg = lib.split(".", 1)[0]
        in_tests = bool(re.search(r"(^|/)tests?/", text))
        r = {"rule": "missing-library", "error": m.group(0)}
        siblings = {split_pkg(p)[0]: split_pkg(p)[1] for p in (info.get("pr_packages") or [])}
        installed = info["installed"].get(pkg)
        if pkg in siblings and pkg in deps and pkg != fname and installed and installed != siblings[pkg]:
            # e.g. a lower-bounds job installed an older release of the sibling,
            # before the library was added (or made public) in the one of this PR.
            old, new = constraint_edit(opam_text, pkg, ">=", siblings[pkg])
            r.update(dep=pkg, old=old, new=new, confidence="medium",
                     note=f"{pkg}.{installed} was installed and does not provide `{lib}`; "
                          f"{pkg}.{siblings[pkg]} is released in this PR")
            if siblings[pkg] == (info.get("failing") or ("", ""))[1]:
                r["alternative"] = f'"{pkg}" {{= version}}  (released together, same version)'
        elif pkg in siblings and pkg not in deps and pkg != fname:
            r.update(note=f"`{lib}` comes from {pkg}, another package of this PR: the tests of {fname} use a sibling "
                          f"package. Restrict them (a dune `(package ...)` field on the test stanza, or no `@runtest` "
                          f"under `-p {fname}`) instead of adding a dependency, which would create a cycle.",
                     confidence="medium")
        elif pkg in deps or pkg == fname:
            r.update(note=f"{pkg} is already a dependency: `{lib}` may only exist in other versions of {pkg}, "
                          f"or be an optional sub-library.", confidence="low")
        elif vs := src.versions(pkg):
            filt = " {with-test}" if in_tests else ""
            r.update(dep=pkg, old=None, new=f'"{pkg}"{filt}   (new line in depends)', confidence="medium")
        elif vs is None:
            r.update(note=f"The tool could not check whether {pkg} is an opam package (ocaml.org did not "
                          f"answer): if it is, add it to depends; otherwise `{lib}` is probably an internal "
                          f"library missing from the release archive, or excluded by `-p {fname}`.",
                     confidence="low")
        else:
            r.update(note=f"`{lib}` is not an opam package. It is probably an internal library missing from the "
                          f"release archive, or excluded by `-p {fname}`.", confidence="medium")
        return r
    m = re.search(r"fatal error: ([\w/.+-]+): No such file or directory", text)
    if m:
        return {"rule": "missing-header", "error": m.group(0), "confidence": "low",
                "note": f"A system header is missing on {platform}. Add the depext (or conf-* dependency) that provides "
                        f"{m.group(1)}, or x-ci-accept-failures for the platforms without it."}
    return None


TRIVIAL_DEPS = {"ocaml", "dune", "ocamlfind", "odoc", "ocaml-base-compiler", "ocaml-compiler", "base-unix",
                "base-threads", "base-bytes", "conf-pkg-config", "dune-configurator"}


CHANGELOG_EXCERPT = 1500


def changelog_excerpt(src, dep, since, until):
    """The changelog sections of `dep` between two versions (after `since`, up
    to `until`), cut to CHANGELOG_EXCERPT characters, for a candidate of a
    test-output failure; None if ocaml.org has none. `until` is the first
    release after the passing one, whose changes the suggested bound excludes.
    ocaml.org escapes "<" and "&" in changelogs; the text is decoded here, and
    escaped again by each output (HTML page, Markdown code block)."""
    c = src.changelog(dep, since=since, until=until)
    if not c:
        return None
    text = html.unescape(c["text"]).strip()
    cut = len(text) > CHANGELOG_EXCERPT or c["truncated"]
    if len(text) > CHANGELOG_EXCERPT:
        text = text[:CHANGELOG_EXCERPT].rsplit("\n", 1)[0]
    return {"file": c["file"], "versions": c["versions"], "text": text, "cut": cut}


def dated_candidates(src, r, bad, fname, fver, max_lookups=8):
    """No passing job to compare with: dependencies whose installed version was
    released after the failing package itself are candidates for having changed
    its test output. Bound: before their first release after that date.
    Cost: one ocaml_package_versions call per dependency looked at (at most
    `max_lookups`, those mentioned in the error first; the binary search over
    release dates reuses it), and one ocaml_package_changelog call per
    candidate, for the changes between the two versions."""
    day = src.published(fname, fver)
    if not day:
        r["note"] = "No passing job to compare with, and no release date for the package."
        return
    opam_text = src.opam_file(fname, fver) or ""
    dep_lines = re.findall(r'^\s*"([\w+-]+)"([^\n]*)$', opam_text.split("depends:", 1)[-1].split("\n]", 1)[0], re.M)
    blob = "\n".join(bad["error_lines"]).lower()
    deps = [d for d, rest in dep_lines
            if d not in TRIVIAL_DEPS and d in bad["installed"]
            and not re.search(r"\b(build|with-doc|with-dev-setup)\b", rest)]
    deps.sort(key=lambda d: d.lower() not in blob)      # mentioned in the output first
    newer = []
    for d in deps[:max_lookups]:
        v = bad["installed"][d]
        dv = src.published(d, v)
        if dv and dv > day:
            newer.append((d.lower() not in blob, dv, d, v))
    newer.sort()
    cands = []
    for _, dv, d, v in newer[:3]:
        # first version released after `day`: binary search on release dates
        vs = [w for w in src.versions(d) or [] if opam_vcmp(w, v) <= 0]
        lo, hi = 0, len(vs) - 1                  # vs[hi] == v is after `day`
        while lo < hi:
            mid = (lo + hi) // 2
            dm = src.published(d, vs[mid])
            if dm and dm > day:
                hi = mid
            else:
                lo = mid + 1
        cands.append((d.lower() not in blob, dv, d, v, vs[lo] if vs else v, vs[lo - 1] if vs and lo else None))
    cands.sort(key=lambda c: (c[0], c[1]))
    r["compared_with"] = f"release date of {fname}.{fver} ({day})"
    r["candidates"] = []
    for _, _, d, v, first, before in cands:
        c = {"dep": d, "failing": v, "first_after": first, "old": None,
             "new": f'"{d}" {{with-test & < "{first}"}}   (new line in depends)'}
        log = changelog_excerpt(src, d, before, first)
        if log:
            c["changelog"] = log
        r["candidates"].append(c)
    r["also_newer"] = [d for _, _, d, _ in newer[3:]]
    r["confidence"] = f"low (dependencies released after {fname}.{fver}; check their changelogs)"
    if not cands:
        r["note"] = f"None of the {min(len(deps), max_lookups)} dependencies checked was released after {fname}.{fver} ({day})."


# opam's plan lines: "- downgrade dune 3.24.2 to 3.15.3 [required by miaou-core]".
PLAN_LINE = re.compile(r"^\s*- (install|downgrade|upgrade|recompile)\s+([\w+-]+)\s+(\S+)(?:\s+to\s+(\S+))?"
                       r"[^\[\n]*(?:\[(?:required by|uses) ([^\]]*)\])?", re.M)


def opam_plan(text):
    """({name: (from, to)} for the downgraded packages, {name: {packages requiring it}})."""
    downgraded, required_by = {}, collections.defaultdict(set)
    for m in PLAN_LINE.finditer(text):
        action, name, v1, v2, by = m.groups()
        if action == "downgrade" and v2:
            downgraded[name] = (v1, v2)
        if by and "required by" in m.group(0):
            required_by[name].update(p.strip() for p in by.replace("etc.", "").split(",") if p.strip())
    return downgraded, required_by


def direct_deps(opam_text):
    return re.findall(r'^\s*"([\w+-]+)"', (opam_text or "").split("depends:", 1)[-1].split("\n]", 1)[0], re.M)


def lower_bounds_rule(src, job, text, r, pr_packages, pr_sha):
    """A lower-bounds job: CI installs the PR's package with the oldest
    versions its constraints allow. Either a dependency of it was downgraded
    to a version that no longer builds (a new suggestion: raise the bound of
    the direct dependency that pulls it in), or the package itself fails and
    no rule explains why (r gets the downgraded direct dependencies as
    candidates, those named in the error first)."""
    if job["kind"] != "lower-bounds":
        return None
    info = log_info(text)
    if "failing" not in info:
        return None
    fname, fver = info["failing"]
    downgraded, required_by = opam_plan(text)
    siblings = {split_pkg(p)[0]: p for p in pr_packages}
    target = siblings.get(fname) or job["revdep"] or job["target"]
    tname, tver = split_pkg(target)
    opam = (src.opam_file(tname, tver, pr_sha) if target in pr_packages else None) or src.opam_file(tname, tver)
    deps = [d for d in direct_deps(opam) if d not in TRIVIAL_DEPS or d == "dune"]

    if fname != tname:
        # A: a dependency failed to build. Walk "required by" up to a direct
        # dependency of the package that was downgraded.
        seen, frontier, path = {fname}, [fname], {fname: None}
        found = None
        while frontier and not found:
            nxt = []
            for p in frontier:
                if p in deps and p in downgraded:
                    found = p
                    break
                for q in sorted(required_by.get(p, ())):
                    if q not in seen:
                        seen.add(q)
                        path[q] = p
                        nxt.append(q)
            frontier = nxt
        if not found:
            return None
        chain, p = [], found
        while p:
            chain.append(p)
            p = path[p]
        old, new = constraint_edit(opam, found, ">=", downgraded[found][0])
        via = f" (through {' -> '.join(chain[1:-1])})" if len(chain) > 2 else ""
        what = (f"{fname}.{fver}" if found == fname else f"{found}.{downgraded[found][1]}, which pulls in "
                f"{fname}.{fver}{via}")
        return {"variant": job["variant"], "url": job["url"], "kind": job["kind"], "experimental": job["experimental"],
                "rule": "lower-bound", "failing": target, **pr_or_other(target, target in pr_packages),
                "dep": found, "old": old, "new": new, "confidence": "medium",
                "error": f"The lower-bounds job installs {what}, which fails to build",
                "note": f"{found}.{downgraded[found][0]}, used in the other jobs, builds. A version in between "
                        f"may build too: this is the bound the CI results support."}

    # B: the package itself fails, and no rule explains the error.
    if r.get("rule") or r.get("new") or r.get("candidates"):
        return None
    err = "\n".join(info["error_lines"])
    mentioned = {norm(w) for w in re.findall(r"\b([A-Za-z][\w-]+)", err)}
    cands = [d for d in deps if d in downgraded]
    if not cands:
        return None
    cands.sort(key=lambda d: (norm(d) not in mentioned and not any(m.startswith(norm(d)) for m in mentioned
                                                                  if len(norm(d)) >= 3), d))
    out = []
    for d in cands[:5]:
        old, new = constraint_edit(opam, d, ">=", downgraded[d][0])
        out.append({"dep": d, "passing": downgraded[d][0], "failing": downgraded[d][1], "old": old, "new": new})
    named = [c["dep"] for c in out if norm(c["dep"]) in mentioned
             or any(m.startswith(norm(c["dep"])) for m in mentioned if len(norm(c["dep"])) >= 3)]
    r.update(rule="lower-bound", candidates=out, compared_with="the other jobs", confidence="low",
             failing=target, **pr_or_other(target, target in pr_packages),
             error=r.get("error") or next((l for l in info["error_lines"] if "Error" in l), "build error"),
             note=("The error names " + ", ".join(named) + ": most likely the lower bound to raise. "
                   if named else "")
             + "These direct dependencies are older in the lower-bounds job than in the other jobs, which pass.")
    return None


def upper_bounds(opam_text):
    """{dependency: (line, [(op, version)...])} for the dependencies with an
    upper bound (`<` or `<=`) in an opam file's depends."""
    out = {}
    block = (opam_text or "").split("depends:", 1)[-1].split("\n]", 1)[0]
    for m in re.finditer(r'^[ \t]*"([\w+-]+)"[ \t]*\{([^}]*)\}[^\n]*$', block, re.M):
        ups = re.findall(r'(<=?)\s*"([^"]+)"', m.group(2))
        if ups:
            out[m.group(1)] = (m.group(0).strip(), ups)
    return out


def error_mentions(info, dep):
    """Does the error itself (not the command that failed) name the package
    `dep`, or a module of it?"""
    lines = [l for l in info.get("error_lines", [])
             if not re.match(r"\s*(=== ERROR|context |path |command |exit-code |env-file |output-file |## output|"
                             r"\(cd _build|\[\.\.\.)", l) and "dune build" not in l]
    text = "\n".join(lines)
    if dep == "dune":       # named in many errors ("your dune file"): only a dune version problem counts
        return bool(re.search(r"dune language|lang dune|dune-project|version of dune", text, re.I))
    text = re.sub(r'File "[^"]*"', "", text)                       # dune's locations: `File "src/dune", line 3`
    text = re.sub(r"(?:[\w.+-]*/)+[\w.+-]+", "", text)              # paths
    words = {norm(w) for w in re.findall(r"[A-Za-z][\w-]+", text)}
    d = norm(dep)
    return d in words or (len(d) >= 4 and any(w.startswith(d) for w in words))


def previous_release_bound(src, job, text, info, pr_packages, pr_sha, cache):
    """A new release fails with a dependency version that the previous
    release excludes on master (maintainers often add bounds to old versions
    after a breakage, and a new release made from the upstream opam file does
    not have them): suggest the same bound, citing the previous release."""
    if "failing" not in info:
        return None
    fname, fver = info["failing"]
    pkg = f"{fname}.{fver}"
    if pkg not in pr_packages:
        return None
    if pkg not in cache:
        older = [v for v in src.versions(fname) or [] if opam_vcmp(v, fver) < 0]
        prev = max(older, key=functools.cmp_to_key(opam_vcmp)) if older else None
        cur = src.opam_file(fname, fver, pr_sha)
        prev_text = src.opam_file(fname, prev) if prev else None
        cache[pkg] = (prev, cur, upper_bounds(prev_text), upper_bounds(cur)) if prev and cur else None
    if not cache[pkg]:
        return None
    prev, cur, prev_up, cur_up = cache[pkg]
    cur_lines = {m.group(1): m.group(2) or "" for m in re.finditer(
        r'^[ \t]*"([\w+-]+)"[ \t]*(?:\{([^}]*)\})?', (cur or "").split("depends:", 1)[-1].split("\n]", 1)[0], re.M)}
    for dep, (line, ups) in sorted(prev_up.items()):
        if dep in cur_up:
            continue
        mentioned = set(re.findall(r'"([^"]+)"', cur_lines.get(dep, "")))
        lowers = re.findall(r'>=?\s*"([^"]+)"', cur_lines.get(dep, ""))
        if any(v in mentioned for _, v in ups) or any(opam_vcmp(lo, v) >= 0 for lo in lowers for _, v in ups):
            continue    # the new release changed this constraint on purpose (e.g. needs a newer API)
        have = info["installed"].get(dep)
        if not have or not error_mentions(info, dep):
            continue
        excluded = [(op, v) for op, v in ups if (opam_vcmp(have, v) >= 0 if op == "<" else opam_vcmp(have, v) > 0)]
        if not excluded:
            continue
        op, v = excluded[0]
        old, new = constraint_edit(cur, dep, op, v)
        return {"variant": job["variant"], "url": job["url"], "kind": job["kind"],
                "experimental": job["experimental"], "rule": "previous-release-bound", "failing": pkg,
                **fix_where("pr", "this PR"), "dep": dep, "old": old, "new": new, "confidence": "medium",
                "error": f"{dep}.{have} is installed; the previous release {fname}.{prev} excludes it on master "
                         f"(`{line}`)",
                "note": f"The previous release has this bound in opam-repository (often added after a breakage) "
                        f"and this one does not. If the new release really supports {dep}.{have}, the failure "
                        f"has another cause."}
    return None
