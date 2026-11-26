(** Render site/data.json (from triage_open_prs) into a single static page.

    {v render-page [--data site/data.json] [--out site/index.html] v}

    The page has no external resources (inline CSS and a little JS for the
    filters). Everything that comes from GitHub, the CI logs or the suggestions
    is untrusted and HTML-escaped; links are kept only when they are https URLs.

    Each failing PR gets one verdict (see {!verdict}), shown as a coloured word
    and border, and a headline saying what the problem is (see {!headline}).

    PRs and the page data are the JSON of data.json; suggestions are read as
    {!Triage.Suggestion.t}. *)

val main : unit -> unit
(** Run [render-page] with the command line's arguments: writes the page and its
    Atom feed (feed.xml, beside it). *)

val render : Triage.Json.t -> string
(** The page, from the page data. *)

val render_feed : Triage.Json.t -> string
(** An Atom feed with one entry per analysed PR. An entry's id changes with the
    PR's head commit, so a feed reader shows each new analysis. *)

val base : string
(** {!Triage.Net.base}. *)

(** {1 Verdicts} *)

val verdict : Triage.Json.t -> string
(** The one verdict of a failing PR, the first that applies: "error" (not
    analysed: no result, or its analysis is pending), "ready" (not this PR's
    fault: {!seems_ready}), "change" (a change to this PR is suggested),
    "blocked" (another package must be fixed), "ci" (on hold: no failure is this
    PR's but the PR is a draft, labelled or its CI still runs), "rerun" (jobs to
    rerun and nothing else of this PR), or "other" (needs a look: failures of
    this PR without a suggestion, of unknown cause, or not explained at all). *)

val verdict_label : string -> string
(** A verdict in words: "Not this PR's fault", "Change this PR", ... *)

val verdict_help : string -> string
(** What a verdict means, for the page's help. *)

val seems_ready : Triage.Json.t -> bool
(** A PR whose CI fails, but no failure points at the PR. Its CI must have
    finished, and its failures must have been explained: a PR whose only
    "failures" are skipped jobs may not be installable anywhere. *)

val not_this_pr : Triage.Suggestion.t -> bool
(** Whether a failure is not the PR's: it happens without the PR (pre-existing,
    or also failing in other open PRs), comes from the CI, or is on an
    experimental platform, whose failures do not block a PR. Failing in other
    PRs does not outweigh evidence that the failure is this PR's (it builds on
    master, say: see {!Triage_open_prs.mark_shared_failures}), nor counts for
    jobs to rerun (CI did not test them). *)

val ci_running : Triage.Json.t -> bool
(** Whether the PR's CI is still running: its opam-ci status is pending and the
    analysis did not find the CI page finished. The status stays pending for
    good when a job never starts (lint after a failed analysis). A result kept
    from an earlier CI run ([ci_state_now]: a rebuild whose analysis was
    deferred or failed) says nothing about the run now: running while the status
    now is pending. *)

(** {1 Parts of the page} *)

val headline : ?v:string -> ?as_html:bool -> Triage.Json.t -> string
(** What the problem of a PR is, in a few words: the first change to this PR
    (package and new constraint), else the other packages to fix, else the first
    finding. [v]: its verdict, if known. *)

val render_pr :
  ?year:string -> ?max_failures:Triage.Json.t -> Triage.Json.t -> string
(** A PR's section. [year]: the current year, left out of dates; [max_failures]:
    the failures analysed per PR, to say why some were not. *)

val render_suggestion :
  ?sha:string -> ?year:string -> Triage.Suggestion.t -> string
(** A suggested change, in full: title, package, tags, then the details. [sha]:
    the PR's head commit, for the links to job logs. *)

val render_item : ?sha:string -> ?year:string -> Triage.Suggestion.t -> string
(** A failure as one folded line: title, packages, failures, jobs, where to fix,
    confidence; the details inside. *)

val merge_similar : Triage.Suggestion.t list -> Triage.Suggestion.t list
(** Suggestions with near-duplicates merged, in the order of their first one. A
    merged suggestion lists its packages ([packages]), the union of the jobs,
    the distinct errors and notes, and how many it merges ([merged]). *)

val label_badges : string list -> string list -> string
(** [label_badges labels stale]: the PR's labels as small tags, "consider for
    merge" first. A label the author may have answered since it was added
    ([stale]) says so. *)

val job_link : string option -> string -> string
(** [job_link sha variant]: a link to a job's log. Inside a PR, whose element
    carries the base URL of its commit's jobs (data-ci), only the variant is
    written and the script builds the href: a full URL per job made up most of
    the page. *)

val ready_section : ?shown:int -> Triage.Json.t -> string
(** The PRs that seem ready: CI passes, or no failure points at the PR. Drafts
    and PRs with a label that holds them are left out. *)

val chronic : ?min_prs:int -> Triage.Json.t list -> string
(** Reverse dependencies (by package name) failing on several PRs (at least
    [min_prs]), and other packages blamed by several PRs. *)

val help_section : Triage.Json.t -> string
(** The page's help: the verdicts, how the PRs are analysed, and what the LLM
    fallback did in this update (its calls, whether it was turned off). *)

val comment_markdown : Triage.Json.t -> string
(** The PR comment of the [comment] command, from a PR's stored analysis.
    Low-confidence LLM answers are left out, as they are folded on the page. *)
