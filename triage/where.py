"""Where the fix of a suggestion goes, as data.

Each suggestion has `fix_in`, the place in words (shown, and stored by the
page), and `where_kind`, one of WHERE_KINDS, from which its class, the PR's
verdict and the page's grouping follow. `where_pkg` names the other package
when `where_kind` is "other-package" and the package is known.

Rules set the three together with `fix_where` or `pr_or_other`. Suggestions
stored before `where_kind` existed, and LLM answers (whose place comes as
`fix_in`), get it from `where_kind` / `where_pkg` below: the only place
that reads the words of `fix_in`.
"""

WHERE_KINDS = ("pr", "other-package", "upstream", "ci", "rerun", "none", "unknown")


def fix_where(kind, fix_in, pkg=None):
    """The fields saying where a fix goes, for dict.update: `kind` (one of
    WHERE_KINDS), the place in words, and the other package, if any."""
    assert kind in WHERE_KINDS, kind
    return {"fix_in": fix_in, "where_kind": kind, "where_pkg": pkg if kind == "other-package" else None}


def pr_or_other(pkg, in_pr):
    """The fix goes in this PR (`pkg` is one of its packages), or in `pkg`."""
    return fix_where("pr", "this PR") if in_pr else fix_where("other-package", f"{pkg} (other package)", pkg)


# The `fix_in` values written by the rules before `where_kind` existed.
LEGACY_KINDS = {
    "this PR": "pr", "unknown": "unknown", "CI (rerun)": "rerun",
    "the CI environment (day10 builder)": "ci", "the CI environment": "ci", "opam (the CI's opam version)": "ci",
    "upstream (the package's source)": "upstream", "nothing (this PR's bound)": "none",
    "the dependency declaring these depexts": "other-package",
    "the reverse dependencies (other packages)": "other-package",
    "the PR, a shared dependency, or the CI": "unknown",
}
OTHER_SUFFIX, UPSTREAM_SUFFIX = " (other package)", " (upstream archive)"


def _legacy(r):
    """(where_kind, where_pkg) of a suggestion without `where_kind`, from
    its `fix_in` (and `failing_dep`, the shared dependency of a pattern over
    several packages)."""
    f = r.get("fix_in") or ""
    if f == "the PR, a shared dependency, or the CI" and r.get("failing_dep"):
        return "other-package", r["failing_dep"]
    if f in LEGACY_KINDS:
        return LEGACY_KINDS[f], None
    if f.endswith(OTHER_SUFFIX):
        return "other-package", f[:-len(OTHER_SUFFIX)]
    if f.endswith(UPSTREAM_SUFFIX):
        return "upstream", None
    return "unknown", None


def where_kind(r):
    """Where the fix of suggestion `r` goes: one of WHERE_KINDS."""
    k = r.get("where_kind")
    return k if k in WHERE_KINDS else _legacy(r)[0]


def where_pkg(r):
    """The other package where the fix of `r` goes, or None."""
    if r.get("where_kind") in WHERE_KINDS:
        return r.get("where_pkg") if r["where_kind"] == "other-package" else None
    return _legacy(r)[1]
