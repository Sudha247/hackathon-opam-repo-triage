"""Small helpers for the text people read: counts with plurals, dates."""

import datetime
import re

MONTHS = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split()


def counted(n, word, many=None):
    """'1 job', '2 jobs', '2 reverse dependencies': `n` and the noun, plural
    unless n is 1 (`many` for an irregular plural)."""
    if n == 1:
        return f"{n} {word}"
    if many is None:
        many = word[:-1] + "ies" if re.search(r"[^aeiou]y$", word) else word + "s"
    return f"{n} {many}"


def short_date(iso, year=None):
    """'2026-11-17…' -> '17 Nov', with the year when it is not `year` (by
    default the current year). Anything else is returned unchanged."""
    m = re.match(r"(\d{4})-(\d\d)-(\d\d)", iso or "")
    if not m or not 1 <= int(m.group(2)) <= 12:
        return iso or ""
    year = str(year or datetime.date.today().year)
    out = f"{int(m.group(3))} {MONTHS[int(m.group(2)) - 1]}"
    return out if m.group(1) == year else f"{out} {m.group(1)}"
