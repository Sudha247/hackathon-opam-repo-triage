(** PR-level checks, from the whole CI run rather than one job's log:
    pre-existing failures (the previous release's CI run, opam-health-check),
    lint, reruns, archives that cannot be unpacked, relaxed [available] filters,
    packages that install nowhere, the analysis job. *)

module SS : Set.S with type elt = string

module PS : Set.S with type elt = string * string
(** Pairs (OCaml version, job variant). *)

val platform_vars : Ci.job -> (string * string) list
(** {!Filter.platform_vars}. *)

val eval_filter : string option -> (string * string) list -> bool option
(** {!Filter.eval_filter}. *)

(** {1 Pre-existing failures} *)

type notes = {
  new_ : (Ci.failure_key, string) Hashtbl.t;
      (** "new": failures of reverse dependencies opam-health-check sees build
          on master, with the note to add *)
  mutable unverified : Ci.failure_key list;
      (** the release's own build failing at the same step as the previous
          release's: not compared *)
  good : (string, PS.t) Hashtbl.t;
      (** health.good: package -> the set of (ocaml version, job variant) *)
  bad : (string, PS.t) Hashtbl.t;  (** health.bad *)
  partial : (string, PS.t) Hashtbl.t;  (** health.partial *)
  via : (string, SS.t) Hashtbl.t;
      (** a failing dependency -> the reverse dependencies that fail through it
      *)
  mutable run : string option option;
      (** the date of the opam-health-check run used (set once) *)
  mutable added : SS.t option;  (** the packages the PR adds (None: unknown) *)
  mutable notes_health_stale : string option;
      (** the date of a run too old to be used *)
}
(** What {!preexisting_failures} notes for {!Suggest} and {!health_summary}. *)

val new_notes : unit -> notes

val preexisting_failures :
  ?src:Sources.t ->
  ?notes:notes ->
  Ci.data ->
  string ->
  string option ->
  string ->
  (Ci.failure_key * string) list
(** [preexisting_failures data sha base cache_dir]: [failure_key -> why] for the
    failing jobs that would fail without this PR: packages the PR only bounds,
    failures that the CI run of the previous release of a package also had (same
    platform, same job), and reverse dependencies that opam-health-check also
    sees failing on master ([base]: the master commit the PR was merged onto).
    Build failures that opam-health-check sees passing on master get a note in
    [notes] instead: they are probably caused by the PR. *)

val health_summary : ?src:Sources.t -> notes -> Suggestion.t list
(** Entries comparing the failing reverse dependencies with opam-health-check
    (master): the ones that also fail there, and the ones that build there. *)

val health_run_stale : ?today:string -> string option -> bool
(** Whether an opam-health-check run of a date (YYYY-MM-DD...) is older than a
    week (the service runs about daily: an older run is stuck, and describes an
    older master). An undated run is used; offline, the test fixtures are
    snapshots: never stale. [today] is "YYYY-MM-DD". *)

(** {1 Rules on the CI run} *)

val rerun_rules :
  ?match_:string -> Ci.data -> Suggestion.t list -> Suggestion.t list
(** Jobs that failed for reasons unrelated to the packages (network errors,
    cancelled jobs, CI tool crashes), and jobs that CI never ran to the end:
    aborted ones (the analysis included), and those waiting for another job that
    will never start (see {!Ci.unfinished_jobs}: their CI counts as finished).
    One entry listing what to rebuild. Jobs waiting for a failed analysis are
    left to the analysis rule. *)

val sources_rules : ?match_:string -> Ci.data -> string -> Suggestion.t list
(** [sources_rules data cache_dir]: source archives that were downloaded but
    could not be unpacked, one log per package (e.g. symlinks, which tar cannot
    create on msys2/cygwin). *)

val analysis_rules : ?match_:string -> Ci.data -> string -> Suggestion.t list
(** [analysis_rules data cache_dir]: a failed analysis job: opam-repo-ci could
    not work out which packages the PR changes, so no build job ran. Its reason
    comes from the CI page, or from the end of its log. *)

val lint_rules :
  ?src:Sources.t ->
  ?sha:string ->
  ?cache_dir:string ->
  Ci.data ->
  Suggestion.t list
(** The findings of a failed lint job: errors and warnings of this PR (with the
    fix when known), known false positives, errors the previous release's CI
    already reported; a lint job that failed with no finding is a CI error to
    rerun. [sha]: the PR's head, to read its opam files. *)

val pr_added_bounds :
  string option -> string -> string -> (string * (string * string) list) list
(** [pr_added_bounds base sha cache_dir]: [name.version -> [dep -> new line]]
    for the [depends] lines whose upper bound ([<], [<=]) the commit [sha] adds
    or lowers in an opam file it modifies, relative to master commit [base]: not
    a changed lower bound, nor [conflicts] or [depopts] (one GitHub compare
    call, cached). *)

val relaxed_available_rules :
  Ci.data ->
  string ->
  string option ->
  string ->
  string list ->
  Suggestion.t list
(** [relaxed_available_rules data sha base cache_dir pr_packages]: a PR that
    removes or relaxes the [available] filter of an existing version makes CI
    try it on platforms where it never ran: failures there are most likely
    caused by that change. Only the failures of that package (its own jobs, and
    those of its reverse dependencies) on platforms the old filter excluded and
    the new one allows count. *)

val skipped_rules :
  ?src:Sources.t ->
  ?bounds:(string * (string * string) list) list ->
  Ci.data ->
  string ->
  string list ->
  Suggestion.t list
(** [skipped_rules ~bounds data cache_dir pr_packages]: a PR package that
    installs nowhere: report the solver's conflict chain. When every conflict
    goes through a dependency whose upper bound the PR adds ([bounds], from
    {!pr_added_bounds}), that is the point of the change (an old release
    restricted to old compilers, say): explained as such, provided some
    published version of the dependency satisfies the bound. *)

(** {1 Platforms that accept failures} *)

val accepted_but_failing :
  Suggestion.t list -> Sources.t -> string -> string list -> unit
(** [accepted_but_failing results src sha pr_packages]: a failure on a platform
    the failing package already lists in x-ci-accept-failures: the classic
    builder ignores those, the day10 builder does not (cygwin, freebsd): nothing
    to change in the package. *)

val accepted_elsewhere :
  ?max_versions:int -> Suggestion.t list -> Sources.t -> string list -> unit
(** For a suggestion that restricts platforms (x-ci-accept-failures, or
    available on Windows), the other versions of the package (up to
    [max_versions], 3 by default) that already list those platforms in
    x-ci-accept-failures: maintainers accepted them there, so the same is likely
    acceptable here. *)

val checksum_command : string option -> string option
(** The command [curl -sL 'URL' | sha256sum] for the archive URL of a PR's opam
    file (untrusted: its [src:] can hold any character but a double quote), or
    None if the URL is not a plain https URL. *)

(** {1 Test seams} *)

val fetch_hook : (string -> string -> string) ref
val log_text_hook : (string -> string) ref
val parse_commit_page_hook : (string -> Ci.data) ref
val gh_api_hook : (string -> string -> Json.t option) ref
val find_baseline_commit_hook : (string -> string -> Dict.t option) ref

val opam_file_hook :
  (?ref:string -> Sources.t -> string -> string -> string option) ref

val versions_hook : (Sources.t -> string -> string list option) ref

val mcp_hook :
  (?max_age:float -> Sources.t -> string -> (string * Json.t) list -> Json.t)
  ref

val pr_opam_changes_hook :
  (string option -> string -> string -> (string * (string * string)) list) ref
(** [pr_opam_changes base sha cache_dir]:
    [name.version -> (old available, new available)] for the opam files the
    commit [sha] modifies relative to master commit [base] (one GitHub API
    call). *)
