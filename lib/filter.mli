(** opam filters on the platforms of CI jobs (the [available] field). *)

val platform_vars : Ci.job -> (string * string) list
(** The opam variables of a job's platform that [available] filters use. *)

val eval_filter : string option -> (string * string) list -> bool option
(** An opam filter on the variables [env]: [Some true], [Some false], or None
    when a variable not in [env] decides the result (or the filter is not
    understood). An empty filter is true. *)
