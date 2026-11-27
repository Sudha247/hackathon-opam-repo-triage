(** opam-repo-ci pages and logs: the jobs of a commit's CI run, their categories
    and platforms, log excerpts, job names in words.

    A job is named by its variant, the last part of its URL: e.g.
    [compilers,4.14,lwt.5.10.2,revdeps,osc.0.1.4] (the classic builder, OCaml
    4.14, building the reverse dependency osc.0.1.4 of lwt.5.10.2),
    [day10,debian-13,x86_64,5.5,lwt.5.10.2,tests] (the day10 builder),
    [compilers,4.11,foo.1.0,lower-bounds],
    [distributions,centos-10-ocaml-4.14,foo.1.0], [(lint)]. *)

(** The status of a job, as the page shows it. *)
module Status : sig
  type t =
    | Passed
    | Failed
    | Active
    | Aborted
    | Not_started  (** "not-started" *)
    | Other of string
        (** another status (the page's class of the job, possibly [""]); only
            {!of_string} makes one, so that [Other "failed"] never exists *)

  val to_string : t -> string
  (** The page's word: "passed", "failed", "active", "aborted", "not-started",
      or the other one. *)

  val of_string : string -> t
  (** The inverse of {!to_string}: [to_string (of_string s) = s]. *)
end

(** The kind of a job, from its variant. *)
module Kind : sig
  type t =
    | Build
    | Tests
    | Lower_bounds
    | Revdeps
    | Revdeps_list  (** the job listing the reverse dependencies *)
    | Lint
    | Analysis
    | Other of string  (** another job in parentheses, as [(lint)] is *)

  val to_string : t -> string
  (** "build", "tests", "lower-bounds", "revdeps", "revdeps-list", "lint",
      "analysis", or the other one. *)

  val of_string : string -> t
  (** The inverse of {!to_string}. *)
end

(** The category of a job: why a failed job failed, or the status of the others.
*)
module Category : sig
  type t =
    | Build
    | Build_or_test
    | Dependency
    | Solver
    | Network
    | Cancelled
    | Ignored  (** x-ci-accept-failures *)
    | Not_available  (** not installable there *)
    | Sources  (** the sources could not be extracted *)
    | Lint
    | Analysis
    | Status of Status.t  (** a job that did not fail: its status *)

  val to_string : t -> string
  (** "build", "build-or-test", "dependency", "solver", "network", "cancelled",
      "ignored", "not-available", "sources", "lint", "analysis", or the status.
  *)

  val of_string : string -> t
  (** The inverse of {!to_string}. *)

  val passed : t
  (** [Status Passed]. *)
end

type job = {
  variant : string;
  url : string;  (** its log's page *)
  label : string;  (** the page's label of the job *)
  status : Status.t;
  reason : string option;  (** after the status: [failed: reason] *)
  category : Category.t;
  target : string option;
      (** the PR's package the job builds, e.g. "lwt.5.10.2" *)
  kind : Kind.t;
  revdep : string option;  (** the reverse dependency a revdeps job builds *)
  platform : string list;
      (** the variant's parts before the target: ["compilers"; "4.14"],
          ["day10"; "debian-13"; "x86_64"; "5.5"], ... *)
  experimental : bool;  (** under an "(experimental)" node *)
  in_summary : bool;  (** listed in the page's "Summary of errors" *)
  messages : string list;  (** messages under the job (lint errors) *)
}
(** A job of the CI page. *)

type data = {
  pr : int option;  (** the PR the page names *)
  jobs : job list;
  mutable unfinished : int;  (** jobs still running or queued *)
  mutable unanalysed : int;
      (** distinct failures beyond [--max] that nothing explains *)
  mutable health_stale : string option;
      (** the date of an opam-health-check run too old to compare with *)
  mutable llm_unanswered : int option;
      (** failures left for the LLM that it did not answer *)
  mutable sha : string option;  (** the commit, once the LLM is asked *)
}
(** A CI run: the jobs of a commit page, and what the analysis adds. *)

val job_to_json : job -> Json.t
(** A job as [summary --json] prints it. *)

val data_fields : data -> (string * Json.t) list
(** The fields of a CI run as the summary prints them, after "sha". *)

(** {1 CI pages} *)

val parse_commit_page : string -> data
(** The jobs of a commit's CI page (its HTML). *)

val load_commit : ?refresh:bool -> string -> string -> string * data
(** [load_commit sha cache_dir]: the CI page of a commit, and its jobs. A page
    whose CI is still running is not kept in the cache (it changes until every
    job has finished), and one cached as running before is fetched again. With
    [refresh] (jobs were rebuilt), the page is fetched again and the cached logs
    of its jobs are dropped: a rebuilt job has the same URL but a new log. Each
    log is fetched again only when an analysis needs it. "refreshed" in the
    request counts says it was done (a page that cannot be fetched raises first,
    and the cache still holds the previous build's page and logs): the triage
    page reads it to know whether the next attempt must refresh again. *)

val resolve_sha : string option -> int option -> string
(** [resolve_sha target pr]: the commit to analyse: the head of PR [pr] (GitHub
    API, else [git ls-remote]), or the sha in [target] (a sha or a CI URL).
    Exits with an error when there is none. *)

val unfinished_jobs : job list -> job list
(** Jobs still running or queued (status [active], ...). An [aborted] job has
    finished. A [not-started] one waits for another job: it never starts when
    that job failed (lint after a failed analysis), so it is finished when the
    analysis failed or was aborted, or when no other job is running and some job
    failed. *)

(** {1 Logs} *)

val log_text : string -> string
(** The text of a job's log page. day10 logs are made to read like classic ones
    (without the indentation day10 adds to opam's output). *)

val failure_mark : Rx.t
(** A line of a log that marks a failure without the word "error": a test
    runner's [FAIL], a process killed by a signal, a test that timed out, make's
    [*** ... Error N]. *)

val excerpt :
  ?max_output_lines:int -> ?marks:bool -> string -> (string * string) list
(** The relevant parts of a log: [error_block] (the first
    ["#=== ERROR while ..."] block, its output cut to the last
    [max_output_lines] lines and the first error line before them, or with
    [marks] the first {!failure_mark} line when no error line comes before
    them), [solver], [final], [job_failed], [job], [base] (the master commit the
    PR was merged onto), [builder] (main or day10). *)

(** {1 Jobs} *)

val failing : Category.t list
(** The categories of the failures that the analysis looks into: build,
    build-or-test, dependency, solver. *)

val failing_cats : Category.t list
(** The same. *)

val neutral : Category.t list
(** The categories of failures that say nothing about the PR: not-available,
    ignored, network, cancelled, sources. *)

val failing_pkg : job -> string option
(** The package a job fails on: the reverse dependency it builds, if any, else
    its target. *)

val failing_dep : string option -> string option
(** For a dependency failure, the failing package named in the reason. *)

type failure_key = string option * string option * Kind.t * string
(** What makes two failures the same: target, reverse dependency, kind, and the
    reason (without quotes and [-j N]). *)

val failure_key : job -> failure_key

val merge_jobs : 'a list -> 'a list -> 'a list
(** Union of two lists of job variants, in order. *)

(** {1 Platforms} *)

type dims = {
  ocaml : string option;  (** the OCaml version, X.Y *)
  os : string;  (** the opam variable: "linux", "win32", "freebsd", "macos" *)
  distro : string option;
  arch : string;
}
(** The platform of a CI job. *)

val job_dims : job -> dims

val classic_distro : string
(** The distribution of the classic builder's [compilers] jobs (their image is
    [ocaml/opam:debian-13-ocaml-X]). *)

val platform_str : job -> string
(** The platform parts of a job, for the summary: "compilers/4.14", with "
    (exp)" for an experimental job. *)

val ocaml_version : job -> string option
(** The OCaml version of a [compilers] job or a day10 x86_64 one (the jobs the
    comparison with a baseline looks at). *)

(** {1 Versions} *)

val opam_vcmp : string -> string -> int
(** Compare opam (Debian-like) version strings. *)

val split_pkg : string -> string * string
(** "pkg.1.0" -> ("pkg", "1.0"). *)

val vkey : string -> int list
(** The numbers of a version, for sorting: "4.14.2" -> [4; 14; 2]. *)

(** {1 In words} *)

val describe_job : string -> string
(** A CI job name in words: ["day10,archlinux,x86_64,5.5,pkg.1"] ->
    ["pkg.1 on archlinux x86_64, OCaml 5.5 (day10 builder)"]. *)

val platform_words : job -> string
(** Where a job ran, in words: ["archlinux x86_64, OCaml 5.5 (day10 builder)"].
*)
