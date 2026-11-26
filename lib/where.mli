(** Where the fix of a suggestion goes, as data.

    Each suggestion has [fix_in], the place in words (shown, and stored by the
    page), and [where_kind] (a {!Where_kind.t}), from which its class, the PR's
    verdict and the page's grouping follow. [where_pkg] names the other package
    when [where_kind] is "other-package" and the package is known. *)

val fix_where : ?pkg:string -> Where_kind.t -> string -> Suggestion.set list
(** The members saying where a fix goes, for {!Suggestion.update}: [where_kind],
    [fix_in] (the place in words), and [where_pkg] (the package if given and
    [where_kind] is [Other_package], else null). *)

val pr_or_other : string -> bool -> Suggestion.set list
(** [pr_or_other pkg in_pr]: the fix goes in this PR ([pkg] is one of its
    packages), or in [pkg]. *)

val where_kind : Suggestion.t -> Where_kind.t
(** Where the fix of a suggestion goes. Suggestions stored before [where_kind]
    existed, and LLM answers, get it from the words of [fix_in]. *)

val where_pkg : Suggestion.t -> string option
(** The other package where the fix of a suggestion goes, or None. *)

val tied_to_pr : Suggestion.t -> bool
(** Whether the analysis ties a failure to this PR: its fix goes in the PR, a
    rule says the PR's change causes it, or it suggests a change ([new]) made
    for this failure. *)
