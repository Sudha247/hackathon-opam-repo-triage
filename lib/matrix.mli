(** Rules on the pass/fail pattern of the jobs of a package: a failure on every
    older (or newer) OCaml version gives an [ocaml] bound, one only on Windows,
    one architecture or some distributions gives a platform restriction, one
    only on the day10 builder where the main builder passes is the day10
    environment's. *)

type run = {
  day : int option;  (** the day the job ran (days since the epoch) *)
  base : string option;  (** the master commit it was merged onto *)
}
(** When and on what a job ran, from its log. *)

val twin_max_days : int
(** A main-builder job that passed more than this many days from a day10
    failure, on another master commit, says nothing about it. *)

val twin_confidence :
  (Ci.job -> run) option -> Ci.job -> Ci.job -> string option
(** [twin_confidence run j t]: how much the main builder's job [t], which passed
    on the distribution, architecture and OCaml version of the day10 failure
    [j], shows that [j] fails because of day10: "high" when both ran on the same
    master commit or within a day (one push), "medium" within {!twin_max_days},
    [None] for an older run or one [run] cannot date. Without [run], "high". *)

val matrix_rules :
  ?failing_in_log:(Ci.failure_key * string) list ->
  ?run:(Ci.job -> run) ->
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
    step failed in the log of each failure. [run]: when and on what a job ran
    (read from its log: one day10 job and one main-builder job per distribution
    with a day10 failure where the main builder passes); without it, the pattern
    trusts any passing main-builder job (see {!twin_confidence}). *)

val collapse_systemic : ?threshold:int -> Suggestion.t list -> Suggestion.t list
(** The same matrix pattern on many packages (at least [threshold], 5 by
    default) is one cause (the PR, a shared dependency or the CI environment),
    not one fix per package: merge them. *)
