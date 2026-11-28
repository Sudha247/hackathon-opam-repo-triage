(** Merging and checking suggestions, for {!Suggest}: one entry for the failures
    no rule recognises, one for several versions of a package, and checks of the
    suggested changes. *)

val rule_error : Ci.job -> exn -> string list -> Suggestion.t
(** [rule_error job e jobs]: the result for a job whose rules raised [e] (a bug
    in the tool), standing for [jobs]. *)

val check_changes :
  Suggestion.t list -> Sources.t -> string -> string list -> unit
(** [check_changes results src sha pr_packages]: check each suggested change
    before it is shown: a constraint that no version satisfies, a bound that
    still allows the version the failing job installed, or a change that
    [opam lint] objects to (when opam is installed) is moved to the note, as
    text to check. *)

val applied : string -> Suggestion.t -> string option
(** [applied opam_text r]: the opam file with the suggested change applied, when
    it can be placed. *)

val jobs_or_variant : Suggestion.t -> string list
(** The jobs a suggestion stands for, or its own job. *)

val merge_ci_failures : Suggestion.t list -> Suggestion.t list
(** The CI failures with the same error and note (the same broken image or
    crashing worker, under several packages): one entry, listing the packages
    and every job. *)

val merge_versions :
  ?plats:(string -> string list) -> Suggestion.t list -> Suggestion.t list
(** Several versions of one package with the same rule and no diff (a diff stays
    with its own opam file, for line comments): one entry, listing them. The
    same version with another note (from another platform) is merged only when
    the two differ by the platform names alone ([plats]: the names of a job's
    platform, by variant), naming every platform; otherwise it stays an entry of
    its own. *)

val merge_unrecognised : Suggestion.t list -> Suggestion.t list
(** The failures no rule recognises, as one entry listing their jobs. *)
