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

val builder_distro : Ci.job -> bool * string option
(** Whether the job ran on day10 (else on the main builder), and its
    distribution: the image it ran in. *)

val per_builder : (Ci.job -> run) -> Ci.job -> run
(** [per_builder run]: [run] read once per builder (day10 or the main one) and
    distribution: the jobs of a push run together. *)

val stale_twins :
  run:(Ci.job -> run) ->
  among:Ci.job list ->
  Ci.job list ->
  string option ->
  Ci.job list
(** [stale_twins ~run ~among jobs match_]: the day10 failures of [among] whose
    distribution, architecture and OCaml version pass on the main builder (in
    [jobs], matching the regular expression [match_]), but in a run too far from
    theirs to vouch for them (see {!twin_confidence}): nothing explains them but
    their own log. *)

val matrix_rules :
  ?failing_in_log:(Ci.failure_key * string) list ->
  ?unknown:Ci.job list ->
  ?unsure:Ci.job list ->
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
    step failed in the log of each failure. [unknown]: the failed jobs left out
    of [jobs] (failures of the CI, whose outcome is not known): a platform
    pattern holds only if each of them is on the failing platforms or on a
    platform with a real pass. [unsure]: failed jobs of [jobs] whose log was not
    read, on a builder and distribution where the CI's image failed other jobs:
    unknown as well, unless a main-builder pass makes them builder-specific.
    [run]: when and on what a job ran (read from its log, once per builder and
    distribution: see {!per_builder}); without it, the pattern trusts any
    passing main-builder job (see {!twin_confidence}). *)

val error_line_re : Rx.t
(** A compiler's or a build's error line ("error:", "error[E0308]:", "Error:",
    "fatal error"), not opam's "[ERROR] The compilation of … failed". *)

val collapse_systemic : ?threshold:int -> Suggestion.t list -> Suggestion.t list
(** The same matrix pattern on many packages (at least [threshold], 5 by
    default) is one cause (the PR, a shared dependency or the CI environment),
    not one fix per package: merge them. *)
