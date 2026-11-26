(** The commands of [opam-ci-triage]: triage the opam-repo-ci failures of an
    ocaml/opam-repository PR ([bin/opam_repo_ci_triage.ml] parses the command
    line). *)

type args = {
  command : string;  (** summary, logs, compare, suggest, comment or review *)
  target : string option;  (** a commit sha or an opam.ci.ocaml.org URL *)
  pr : int option;  (** [--pr]: the PR, whose head commit is looked up *)
  json : bool;
  all : bool;  (** summary: also the passed, ignored and not-available jobs *)
  match_ : string option;  (** [--match]: the jobs whose variant matches *)
  category : string list;  (** logs: the categories of the jobs *)
  max : int;  (** distinct failures analysed at most *)
  llm : bool;
  llm_max : int;  (** LLM calls at most *)
  llm_model : string;
  llm_dry_run : bool;  (** print the LLM prompts instead of calling the API *)
  post : bool;  (** comment, review: post it on the PR with gh *)
  baseline : string option;  (** compare: the baseline commit *)
  cache_dir : string;
  refresh : bool;
      (** fetch the CI page again, drop the cached logs of its jobs *)
}
(** The command line. *)

val main : args -> unit
(** Run a command: load the CI run of the target, then print its summary, logs,
    comparison, suggestions, PR comment or review. *)

val suggest_command : args -> string -> Ci.data -> unit
(** [suggest_command args sha data]: the commands that suggest fixes (suggest,
    comment, review) on a loaded CI run. *)
