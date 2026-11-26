(** Run [opam-ci-triage suggest] on the open opam-repository PRs whose opam-ci
    status is [failure], and write the results to site/data.json.

    {v
    triage-open-prs [--limit N] [--state state/prs.json] [--out site/data.json]
    v}

    The open PRs and the opam-ci status of their head commit are listed with one
    GitHub GraphQL query per 100 PRs (needs a token: GITHUB_TOKEN, GH_TOKEN or a
    logged-in gh). A PR whose head sha and opam-ci status are unchanged since
    the previous run is not analysed again: its result comes from the state file
    (after a rebuild, the status changes and the PR is analysed from fresh CI
    results). PRs that were closed, or whose CI is no longer [failure], are
    dropped. PRs are analysed one at a time, to respect opam.ci.ocaml.org's rate
    limit; [--time-budget] stops starting new ones after that many seconds, and
    the next run continues. *)

val main : ?argv:string array -> unit -> unit
(** Run [triage-open-prs] with the arguments [argv] (the command line's by
    default). *)

(** {1 Listing the open PRs} *)

val graphql : string -> Triage.Json.t -> string -> Triage.Json.t
(** [graphql query variables token]: the data of a query of GitHub's GraphQL API
    (partial errors are fine when every top-level field is there). Tried three
    times; then the last error is raised. *)

val list_open_prs : string -> Triage.Dict.t list * int
(** [list_open_prs token]: all open PRs with the opam-ci state of their head
    commit; (PRs, number of requests). *)

val stale_labels : Triage.Json.t -> string list
(** The labels a PR has that ask its author to act ("needs reporter action"),
    added before a new commit, a push or a comment by the PR's author: such a
    label is often left in place after the author has acted. A label added
    before the last 30 timeline items is not known to be stale. *)

(** {1 Whether to analyse a PR again} *)

val max_attempts : int
(** Analyses of the same commit that failed before it is given up. *)

val attempts_used : ?now_:float -> Triage.Dict.t option -> int
(** The analyses of the previous entry's commit made so far that count toward
    {!max_attempts}: none once some time has passed since the last one (for a
    failed attempt that kept an earlier result: since [last_error_at]). *)

val keep_failed : ?now_:float -> Triage.Dict.t option -> bool
(** Whether a failed analysis (no result) of the same commit is kept rather than
    tried again. *)

val ci_changed : Triage.Dict.t option -> Triage.Dict.t -> bool
(** [ci_changed prev p]: whether the opam-ci status of the same commit changed
    since the previous analysis: another state, or a status posted again (a
    maintainer's "Rebuild failed", even one that finished within the hour). The
    CI page and the job logs are then fetched again ([--refresh]): the cache
    would give the results of the previous build. A previous entry without the
    status's date (saved before dates were recorded, or never analysed) does not
    say which build it saw: changed, when the status has a date. *)

val needs_refresh : Triage.Dict.t option -> Triage.Dict.t -> bool
(** [needs_refresh prev p]: whether the analysis must fetch the CI page and the
    job logs again ([--refresh]) rather than read them from the cache: the
    status changed ({!ci_changed}), or nothing says which build the cache holds:
    no previous entry (a PR dropped when its CI passed, failing again after a
    rebuild, would get the first build's cached page) or another commit (free:
    its page is not cached), or a failed analysis that was to refresh it and
    failed before ([refresh_pending]). Not when a failed attempt already
    refreshed the cache for this CI run ([last_error_refreshed], set when the
    tool says it did: "refreshed" in its counts): the logs it read since come
    from this run, and dropping them again would cost the next attempt the same
    work. *)

val ci_running : Triage.Dict.t -> bool
(** Whether the CI of a state entry is running (see {!Render_page.ci_running}).
*)

val reuse :
  ?now_:float ->
  ?digest:string ->
  Triage.Dict.t option ->
  Triage.Dict.t ->
  string ->
  bool ->
  bool * bool * bool
(** [reuse prev p version llm]: whether the previous analysis [prev] of PR [p]
    can be kept: (same, same_commit, retry). [same]: it describes the current
    state and need not be redone (a failed one is redone while {!attempts_used}
    < {!max_attempts}); [same_commit]: it has a result for the same commit,
    shown until it is redone; [retry]: it is redone because ocaml.org's MCP
    server (or another server) failed during it, or because the previous attempt
    to redo it failed ([last_error]): the attempts add up. [version]: the tool's
    version (an analysis by another version is redone). [llm]: whether this run
    can still make LLM calls (false once its budget is spent: a PR analysed
    without the LLM is then not analysed again without it). [digest]: a digest
    of the CI page now, for a PR whose opam-ci status is pending (without it,
    such a PR is analysed again, unless its previous analysis failed). *)

(** {1 Running the tool} *)

type counts = (string * int) list
(** Request counts, as the tool reports them ({!Triage.Net.net}). *)

val count : counts -> string -> int
(** 0 when absent. *)

val run_tool :
  ?tool:string ->
  string list ->
  int ->
  Triage.Json.t option * string option * counts
(** [run_tool args timeout]: run the triage tool ([tool], by default
    [OPAM_CI_TRIAGE_TOOL] or the [opam-ci-triage] next to this executable);
    (parsed JSON or None, error message or None, request counts). The counts
    come from a file the tool updates as it goes, so they are known after a
    timeout too (with "timeout": 1 added). *)

(** {1 Failures shared with other PRs} *)

val job_platform : string option -> string option
(** The platform of a reverse-dependency job: its name before the tested package
    ("compilers,4.14," for "compilers,4.14,foo.1,revdeps,bar.2"), or None. *)

val mark_shared_failures : (string * Triage.Dict.t) list -> unit
(** Mark the suggestions about reverse dependencies that also fail in at least
    [shared_min] other open PRs: such a failure is not caused by this PR. Uses
    the failing reverse dependencies of every PR (from [summary]). Every job of
    the suggestion must be a reverse-dependency job, and the same other PRs must
    fail on each of its reverse dependencies: three revdeps each failing in one
    other PR do not make a failure shared by three PRs.

    Another PR counts only when its failing job is on the same platform (the
    job's name before the tested package: builder, distribution, architecture,
    OCaml version) as the job of this PR, tests another package (two releases of
    lwt break the same reverse dependencies: they do not vouch for each other)
    and fails the same way (its category, when known), and [shared_min] of those
    PRs must test different packages (the packages of their matching jobs,
    pairwise disjoint): two releases of lwt count as one piece of evidence for a
    third PR, and so do three PRs that each release dune, dune-configurator and
    dune-site. [also_failing_in] lists every PR that counts, not only those
    picked as independent. Never for a failure the analysis ties to this PR
    ([Where.tied_to_pr]: where_kind "pr", a rule saying the PR's change causes
    it, a change suggested for it): the evidence says it is this PR's; nor for
    jobs to rerun (cancelled or network errors, which [summary] does not list as
    failing revdeps). *)

(** {1 Test seams} *)

val sleep_hook : (float -> unit) ref
(** How retries wait. *)

val graphql_hook : (string -> Triage.Json.t -> string -> Triage.Json.t) ref
val list_open_prs_hook : (string -> Triage.Dict.t list * int) ref

val run_tool_hook :
  (string list -> int -> Triage.Json.t option * string option * counts) ref
(** {!run_tool} with the default tool. *)

val llm_available_hook : (unit -> bool) ref
(** Whether the LLM fallback can run (an API key is set); without it the page is
    built from the rules alone. *)

val tool_version_hook : (unit -> string) ref
(** A hash of the tool's code (the entry point and the library, computed at
    build time: see version/dune): PRs are analysed again when it changes. *)

val github_token_hook : (unit -> string option) ref
(** {!Triage.Net.github_token}. *)
