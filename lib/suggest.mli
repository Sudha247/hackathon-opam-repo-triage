(** The [suggest] driver: reads the logs of the distinct failures, runs every
    rule (log rules, job matrix, PR-level checks, the LLM fallback), merges,
    checks, annotates and ranks the results. *)

type llm_opts = {
  limit : int;  (** API calls at most *)
  model : string;
  dry_run : bool;  (** show the prompts instead of calling the API *)
}
(** The LLM fallback's options ([--llm]). *)

val suggest :
  string ->
  Ci.data ->
  Sources.t ->
  string ->
  int ->
  string option ->
  llm_opts option ->
  Suggestion.t list
(** [suggest sha data src cache_dir max_logs match_ llm]: the suggestions for a
    CI run, analysing at most [max_logs] distinct failures of the jobs whose
    variant matches [match_] (the others that nothing explains are counted in
    [data.unanalysed]). *)

val distinct_failures : Ci.job list -> string option -> Ci.job list list
(** [distinct_failures jobs match_]: the failed jobs to analyse, one group per
    distinct failure (the same error on many platforms is one problem; a
    dependency failure is the dependency's: one log per cause and platform), in
    the order they are analysed (those beyond [--max] are not): non-experimental
    jobs first, then the PR's own packages before their reverse dependencies. In
    each group, the job whose log is read comes first: classic-builder logs
    first, as they show the depext installation (versions). *)

val drop_health_explained_elsewhere :
  Context.notes -> Suggestion.t list -> string list -> Suggestion.t list
(** [drop_health_explained_elsewhere notes results pr_packages]: drop from
    [notes] (see {!Context.preexisting_failures}) the opam-health-check
    comparisons of the jobs whose log points at a dependency that is not a
    package of this PR: a library-api result bounding it. That dependency
    changed on master since opam-health-check's run, or is not in it: matrix
    0.2.0 broke console-eio while opam-health-check still saw it build, and the
    k7 PR (#31170) was blamed. Returns those results, whose note must not say
    that the PR probably causes the failure. *)

(** {1 Test seams} *)

val fetch_hook : (string -> string -> string) ref
(** How the logs are fetched. *)

val suggest_for_job_hook :
  (Sources.t -> Ci.job -> string -> string list -> string -> Suggestion.t) ref
(** {!Rules.suggest_for_job}. *)
