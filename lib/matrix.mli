(** Rules on the pass/fail pattern of the jobs of a package (no fetch): a
    failure on every older (or newer) OCaml version gives an [ocaml] bound, one
    only on Windows, one architecture or some distributions gives a platform
    restriction. *)

val matrix_rules :
  ?failing_in_log:(Ci.failure_key * string) list ->
  Ci.job list ->
  Suggestion.t list ->
  string option ->
  string list ->
  Suggestion.t list
(** [matrix_rules jobs earlier match_ pr_packages]: the suggestions from the
    pass/fail pattern of the jobs only ([match_]: the [--match] regular
    expression the jobs were selected with). [earlier]: the suggestions from the
    logs (their [key] is the failure they come from): next to a change from a
    log, the pattern adds only a workaround. [failing_in_log]: the package whose
    step failed in the log of each failure. *)

val collapse_systemic : ?threshold:int -> Suggestion.t list -> Suggestion.t list
(** The same matrix pattern on many packages (at least [threshold], 5 by
    default) is one cause (the PR, a shared dependency or the CI environment),
    not one fix per package: merge them. *)
