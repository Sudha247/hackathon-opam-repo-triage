(** Suggestions in words: titles of the rules, where the fix goes, classes and
    ranking. *)

val title_of : Suggestion.t -> string
(** The title of a suggestion: its rule in words, with the number of packages
    for a pattern merged over several packages. *)

val where_text : Suggestion.t -> string option
(** Where the fix of a suggestion goes, in words, from its [where_kind] (see
    {!Where}): "this PR", "pkg (another package)", "upstream", "the CI", "rerun"
    or "nothing to fix"; None when unknown (but a pattern merged over several
    packages says so). *)

val fix_label : string -> string
(** "fix in X", "fix upstream", or the value of {!where_text} alone when it is
    not a place ("rerun", "nothing to fix"). *)

val readable : Suggestion.t list -> Suggestion.t list
(** Sets the members for people: a title for the rule, the job in words, where
    the fix goes, and the note and the error as sentences. *)

val per_package_rules : Rule.t list
(** The rules whose explanation does not depend on the platform: reported once
    per package. *)

val has_rule : Rule.t list -> Suggestion.t -> bool
(** Whether the rule of a suggestion is one of the list. *)

val is_systemic : Suggestion.t -> bool
(** Whether a suggestion is a pattern over several packages
    ({!Rule.is_systemic}). *)

val classify : Suggestion.t -> string
(** "fix" (a change in this PR), "workaround" (skips platforms or tests),
    "elsewhere" (another package, upstream, the CI), or "explanation" (no
    change, or the place of the fix is unknown). A change described in words
    ([advice]) counts as a change. *)

val rank : Suggestion.t list -> Suggestion.t list
(** Sets the class of each suggestion ({!classify}) and sorts them: fixes,
    workarounds, elsewhere, explanations; then non-experimental platforms first,
    then by confidence. *)
