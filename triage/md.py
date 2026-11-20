"""Untrusted text in GitHub Markdown (PR comments, reviews, the digest issue).

Text from CI logs, lint messages, opam files, changelogs and LLM answers
must not render links, images or HTML, notify people (@mentions) or create
cross-reference events (#N, owner/repo#N, GH-N, issue URLs), and must not
break out of the code blocks it is put in.

- `md_text` is for prose: one line; Markdown and HTML specials are
  backslash-escaped; a word that GitHub could turn into a mention, a
  reference or a link (it has `@`, `#`, `://`, `www.` or is `GH-N`) is put
  in a code span, where GitHub links nothing; `code` spans of the text
  itself stay code spans.
- `md_code` is an inline code span, `md_fence` a code block, each fenced by
  more backticks than any run in the text.

GitHub's mention, reference and autolink filters skip text in `code`, `pre`
and `a` elements, so a code span keeps the token readable and copyable
(unlike a zero-width joiner) and links nothing.
"""

import re

_SPECIAL = re.compile(r"([\\`*_\[\]<>|&])")
_LINKISH = re.compile(r"[@#]|://|\bwww\.|\bgh-\d", re.I)
_SPAN = re.compile(r"(?<!`)(`+)(?!`)(.+?)(?<!`)\1(?!`)")
_RUN = re.compile(r"`+")


def flat(s):
    """One line, with runs of whitespace (newlines too) as one space."""
    return re.sub(r"\s+", " ", str(s or "")).strip()


def md_code(s):
    """An inline code span of `s` (on one line)."""
    s = flat(s)
    if not s:
        return ""
    fence = "`" * (1 + max((len(m) for m in _RUN.findall(s)), default=0))
    pad = " " if s[0] == "`" or s[-1] == "`" else ""
    return f"{fence}{pad}{s}{pad}{fence}"


def md_fence(text, lang=""):
    """A fenced code block of `text`: a fence longer than any backtick run in
    it (at least 3), so that no line of `text` can close it."""
    text = str(text or "")
    fence = "`" * max(3, 1 + max((len(m) for m in _RUN.findall(text)), default=0))
    return f"{fence}{lang}\n{text}\n{fence}"


def _escape(s):
    return _SPECIAL.sub(r"\\\1", s)


def _word(w):
    if not _LINKISH.search(w):
        return _escape(w)
    # Punctuation around the token stays outside the code span: `#123`.
    lead, core, trail = re.fullmatch(r"([(\[\"']*)(.*?)([.,;:!?)\]\"']*)", w).groups()
    return _escape(lead) + md_code(core) + _escape(trail) if core else md_code(w)


def _words(s):
    return re.sub(r"\S+", lambda m: _word(m.group(0)), s)


def _join(pieces):
    out = ""
    for p in pieces:
        # Two code spans side by side would merge into other backtick runs,
        # and their content could be parsed as Markdown.
        if out.endswith("`") and p.startswith("`"):
            out += " "
        out += p
    return out


def md_text(s, limit=None):
    """Untrusted prose as one line of inert Markdown, cut to `limit` characters
    of the original text (before escaping, so no escape is cut in half)."""
    s = flat(s)
    if limit and len(s) > limit:
        s = s[:limit].rstrip() + "…"
    pieces, pos = [], 0
    for m in _SPAN.finditer(s):
        pieces += [_words(s[pos:m.start()]), md_code(m.group(2))]
        pos = m.end()
    pieces.append(_words(s[pos:]))
    return _join(p for p in pieces if p)


SHA = re.compile(r"[0-9a-f]{7,40}")
VARIANT = re.compile(r"[A-Za-z0-9._+~,-]+")
CI_URL = re.compile(r"https://opam\.ci\.ocaml\.org/[A-Za-z0-9._~,+%/=-]+")


def ci_job_url(base, sha, variant):
    """The URL of a CI job, or None if the commit or the variant has an
    unexpected shape (they are interpolated in a Markdown link)."""
    if SHA.fullmatch(sha or "") and VARIANT.fullmatch(variant or ""):
        return f"{base}{sha}/variant/{variant}"
    return None


def safe_ci_url(url):
    """`url` if it is an opam-repo-ci URL that can go in a Markdown link, else None."""
    return url if CI_URL.fullmatch(url or "") else None
