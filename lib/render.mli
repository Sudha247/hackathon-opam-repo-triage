(** Output of the commands: the summary, the comparison with a baseline,
    suggestions as text, the PR comment and the line-level review. *)

val render_summary : string -> Ci.data -> bool -> unit
(** [render_summary sha data show_all]: print the [summary] of a CI run: the job
    counts by category, the lint and analysis jobs with their messages, then for
    each PR package its own jobs and its reverse dependencies with problems,
    grouped by package and reason (the passed, ignored and not-available jobs
    only with [show_all]). *)

(** {1 Comparison with a baseline} *)

val compare : Ci.job list -> Ci.job list -> Json.t list
(** [compare jobs base_jobs]: one verdict per (reverse dependency, OCaml
    version) that fails in [jobs], against the CI run of the PR that published
    the previous version: NEW, pre-existing, builder-specific, no-baseline. *)

val render_compare : ?show_all:bool -> string -> Json.t -> Json.t list -> unit
(** [render_compare target info verdicts]: print the verdicts of {!compare} for
    [target], against the baseline [info] ({!Sources.find_baseline_commit}). *)

(** {1 Suggestions} *)

val render_suggestions : Suggestion.t list -> unit
(** Print the suggestions as text ([suggest]). *)

val conf_word : Suggestion.t -> string
(** The first word of a confidence: "low" for "low (dependencies released after
    ...)". *)

val compared_text : ?year:int -> Suggestion.t -> string
(** What the candidates of a suggestion are compared with, in words: "the
    release date of passage.0.1.7 (17 Sep 2025)". *)

val changelog_details : Suggestion.Candidate.t -> string
(** A folded changelog excerpt of a candidate, in a code block: its text is
    written by the dependency's authors, and in a code block GitHub renders no
    link, image or mention from it. *)

(** {1 PR comment and review}

    Every field of a suggestion is untrusted (it quotes CI logs, opam files,
    lint messages or an LLM answer): prose goes through {!Md.md_text}, code
    through {!Md.md_code} / {!Md.md_fence}, and links are only built from the CI
    base URL with a checked commit and variant. *)

val render_comment :
  ?placed:int ->
  ?placed_lines:int ->
  string ->
  Ci.data ->
  Suggestion.t list ->
  string
(** [render_comment sha data results]: a draft PR comment (Markdown) from ranked
    suggestions. The fixes that {!build_review} attached to lines of the diff
    (marked [placed]) are left out; [placed] and [placed_lines] count them and
    their lines, for the comment's text. *)

val render_comment_counts :
  ?placed:int ->
  ?placed_lines:int ->
  unfinished:int ->
  njobs:int ->
  unanalysed:int ->
  string ->
  Suggestion.t list ->
  string
(** {!render_comment} from the counts of a CI run rather than the run (the
    triage page has only its stored results). *)

val build_review :
  raw:(string -> string option) ->
  string ->
  Ci.data ->
  Suggestion.t list ->
  Json.t
(** [build_review ~raw sha data results]: a GitHub pull-request review: line
    comments with suggestion blocks for the fixes that can be placed on the PR's
    diff, the rest in the body. [raw url] reads a file ({!Sources.raw}: None
    when it cannot be read). Exits with an error without a PR. *)
