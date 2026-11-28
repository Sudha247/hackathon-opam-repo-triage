(** The scoring of evaluate-closed-prs: what a suggestion changes and what the
    author changed, as tokens ("dep:NAME" for a dependency, "field:NAME" for a
    field, "field:runtest" for the test command). A suggestion is a hit when its
    tokens meet the author's in the same package. *)

module S : Set.S with type elt = string

val version : int
(** The version of the scoring, in the totals: accuracy-report does not compare
    runs scored differently. 3 (27 November 2026): the review bot's
    [x-maintenance-intent] and the description are not fixes, packages with a
    new archive are left out, alternatives and fixes in another package of the
    PR are scored, merged [failing] lists are read. 4 (28 November 2026): an
    advice-only suggestion (no [new], candidate or [alternative]) is scored when
    its advice names the field it changes (see {!advice_tokens}). *)

val line_tokens : string -> S.t
(** What a changed opam line is about: a dependency name, a field name, the test
    command (a line with [@runtest] or [with-test]), a checksum. *)

val is_advice_only : Triage.Suggestion.t -> bool
(** An [advice] and no [new], candidate or [alternative]. *)

val advice_tokens : Triage.Suggestion.t -> S.t
(** The field an advice-only suggestion names, by the phrases of the rules that
    give one: "Add depexts …" (conf-no-depext) and "Add the missing depext …"
    (platform-matrix; its other way, excluding the platform, is not counted):
    [field:depexts]; "… the test command …" (test-needs-other-package: give the
    test command its directories or remove it): [field:runtest]; and the opam
    fields it names in backquotes ("exclude Windows there
    (`x-ci-accept-failures` or `available`)"), from a fixed list. Any other
    advice names no field: empty, and the suggestion is not scored. *)

val suggestion_tokens : Triage.Suggestion.t -> S.t
(** What a suggestion would change: its dependency, candidates, [alternative],
    [new], and {!advice_tokens}. *)

val has_change : Triage.Suggestion.t -> bool
(** Whether a suggestion is scored: a [new], a candidate, an [alternative], or
    an advice naming a field. *)
