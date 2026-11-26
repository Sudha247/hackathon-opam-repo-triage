(** The triage page's data: the state of each PR ([state/prs.json]) and the page
    data ([site/data.json]), as records with their reader and printer.

    [triage-open-prs] writes both, [render-page], [llm-digest] and [llm-eval]
    read the page data. The files are also read and written by older versions
    (the Python one among them): every reader takes any file stored so far and
    never fails. As in {!Triage.Suggestion}, a member that the JSON may hold as
    [null] is a {!field} (null and absent are printed as they were read), the
    others are options; a member of another name, or whose value has another
    type than its field's, goes to [extra] (and the field is absent), so that
    [to_json (of_json v)] has the members of [v] (in another order). *)

type 'a field = 'a Triage.Suggestion.field = Absent | Null | Set of 'a

val get : 'a field -> 'a option
(** The value, None when absent or null. *)

(** A failing job of a reverse dependency, as the PR's entry keeps it (from
    [summary --json]). *)
module Revdep_job : sig
  type t = {
    variant : string field;
    url : string field;
    category : Triage.Ci.Category.t field;
    target : string field;
    extra : (string * Triage.Json.t) list;
  }

  val of_json : Triage.Json.t -> t
  val to_json : t -> Triage.Json.t
end

(** The jobs of a PR's CI run, in short (from [summary --json]). *)
module Jobs : sig
  type t = {
    total : int option;  (** all the jobs *)
    failed : (Triage.Ci.Category.t * int) list option;
        (** the failed jobs by category, in the order first seen *)
    failing_revdeps : (string * Revdep_job.t list) list option;
        (** the failing reverse dependencies and their jobs, by name *)
    extra : (string * Triage.Json.t) list;
  }

  val of_json : Triage.Json.t -> t
  val to_json : t -> Triage.Json.t
end

(** The result of [suggest --json], as stored in a PR's entry. *)
module Result : sig
  type t = {
    sha : string option;
    pr : int field;  (** the PR the CI page names *)
    ci_unfinished : int option;  (** jobs still running *)
    ci_jobs : int option;
    unanalysed : int option;  (** failures beyond [--max] *)
    incomplete : bool option;  (** some lookups failed *)
    errors : (string * int) list option;  (** the failed lookups, by kind *)
    llm_unanswered : int option;
    health_stale : string option;
        (** the date of an opam-health-check run too old to compare with *)
    suggestions : Triage.Suggestion.t list option;
    extra : (string * Triage.Json.t) list;
  }

  val empty : t
  val of_json : Triage.Json.t -> t
  val to_json : t -> Triage.Json.t

  val suggestions : t -> Triage.Suggestion.t list
  (** The suggestions, or [[]]. *)
end

(** What changed since the analysis of the PR's previous head. *)
module Since_last_push : sig
  type t = {
    previous_sha : string field;
    fixed : string list option;  (** the problems gone *)
    new_ : string list option;  (** [new]: the new ones *)
    still : int option;  (** how many remain *)
    extra : (string * Triage.Json.t) list;
  }

  val of_json : Triage.Json.t -> t
  val to_json : t -> Triage.Json.t
end

(** A PR's entry: in the state, in the page data (failing PRs, and those whose
    CI passes, with fewer members), and as [triage-open-prs] lists the open PRs
    (the members that come from GitHub). See {!Triage_open_prs} for what the
    members mean to the runs. *)
module Entry : sig
  type t = {
    mutable number : int field;
    mutable title : string field;
    mutable url : string field;
    mutable draft : bool field;
    mutable updated : string field;
    mutable sha : string field;  (** the head commit *)
    mutable author : string field;
    mutable labels : string list field;
    mutable stale_labels : string list field;
        (** labels the author may have answered since *)
    mutable ci_url : string field;
    mutable pending : bool option;  (** left for a later run *)
    mutable since_last_push : Since_last_push.t option;
    mutable since_base : t option;
        (** the previous head's result, kept until this commit's lands: an entry
            with [sha] and the [result]'s suggestions *)
    mutable ci_state : string field;  (** opam-ci's status: "FAILURE"... *)
    mutable ci_created : string field;  (** the date of that status *)
    mutable ci_digest : string field;  (** the job statuses of a running CI *)
    mutable ci_unfinished : int field;
    mutable refresh_pending : bool option;
    mutable result : Result.t field;  (** null: the analysis failed *)
    mutable error : string field;
    mutable attempts : int option;
    mutable tool_version : string option;
    mutable llm : bool option;  (** every unexplained failure got its answer *)
    mutable llm_unanswered : int field;
    mutable llm_calls : int field;
    mutable mcp_errors : int option;
    mutable net_errors : int option;
    mutable git_unreachable : int option;
    mutable analysed_at : string option;
    mutable seconds : float option;
    mutable jobs : Jobs.t field;
    mutable last_error : string field;
        (** a failed redo of the analysis whose result is kept *)
    mutable last_error_at : string option;
    mutable last_error_ci_state : string field;
    mutable last_error_ci_created : string field;
    mutable last_error_refreshed : bool option;
    mutable ci_state_now : string field;
        (** opam-ci's status now, when the result kept is from another CI run *)
    mutable extra : (string * Triage.Json.t) list;
  }

  val create : unit -> t
  (** An entry without members. *)

  val copy : t -> t
  val of_json : Triage.Json.t -> t
  val to_json : t -> Triage.Json.t

  val remove : t -> string list -> unit
  (** Makes these members absent (in [extra] too). *)

  val number_opt : t -> int option
  (** The PR's number; a number written as a string is read too. *)

  val number : t -> int
  (** The same; fails without one. *)

  val str : string field -> string
  (** The string, or [""]. *)

  val list : 'a list field -> 'a list
  (** The list, or [[]]. *)

  val result : t -> Result.t option
  val has_result : t -> bool

  val suggestions : t -> Triage.Suggestion.t list
  (** The suggestions of the result, or [[]]. *)
end

(** What the LLM fallback did in a run. *)
module Llm_info : sig
  type t = {
    enabled : bool option;
    calls : int option;
    off : string field;  (** why it was turned off: "credentials", "outage" *)
    extra : (string * Triage.Json.t) list;
  }

  val empty : t
end

(** A run's counts. *)
module Run : sig
  type t = {
    github_api_calls : int option;
    tool_requests : (string * int) list option;
    counts : (string * int) list;
        (** the other integer members: PRs analysed, unchanged, dropped... *)
    extra : (string * Triage.Json.t) list;
  }

  val count : t -> string -> int option
end

type t = {
  generated : string option;
  repository : string option;
  open_prs : int option;
  failing_prs : int option;
  running_prs : int option;
  limit : int field;
  max_failures_per_pr : int field;
  llm : Llm_info.t option;
  run : Run.t option;
  prs : Entry.t list option;  (** the failing PRs *)
  passing : Entry.t list option;  (** the PRs whose CI passes *)
  extra : (string * Triage.Json.t) list;
}
(** The page data (site/data.json). *)

val of_json : Triage.Json.t -> t
val to_json : t -> Triage.Json.t

val prs : t -> Entry.t list
(** The failing PRs, or [[]]. *)

(** The state (state/prs.json): the entries by PR number. *)
module State : sig
  type t = {
    updated : string option;
    prs : (string * Entry.t) list option;
        (** an entry that is not an object reads as an empty one *)
    extra : (string * Triage.Json.t) list;
  }

  val of_json : Triage.Json.t -> t
  val to_json : t -> Triage.Json.t
end
