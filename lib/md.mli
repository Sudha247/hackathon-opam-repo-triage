(** Markdown for the PR comments and reviews: untrusted text (logs, opam files,
    package names) made inert. *)

val flat : string -> string
(** One line, with runs of whitespace (newlines too) as one space. *)

val md_code : string -> string
(** An inline code span of [s] (on one line). *)

val md_fence : ?lang:string -> string -> string
(** A fenced code block of [text]: a fence longer than any backtick run in it
    (at least 3), so that no line of [text] can close it. *)

val md_text : ?limit:int -> string -> string
(** Untrusted prose as one line of inert Markdown, cut to [limit] characters of
    the original text (before escaping, so no escape is cut in half). *)

val sha_re : string
(** A pattern matching an abbreviated or full commit sha. *)

val ci_job_url : string -> string -> string -> string option
(** The URL of a CI job, or None if the commit or the variant has an unexpected
    shape (they are interpolated in a Markdown link). *)

val safe_ci_url : string option -> string option
(** [url] if it is an opam-repo-ci URL that can go in a Markdown link, else
    None. *)
