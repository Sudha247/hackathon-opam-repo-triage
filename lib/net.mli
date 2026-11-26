(** HTTP, GitHub API and git access, with their on-disk caches.

    Every request is counted ({!net}): the commands print the counts, and the
    triage page reads them to know what an analysis cost and whether it is
    complete ("net-errors"). With [OPAM_CI_OFFLINE=1] (the tests), everything
    must come from the cache: a cache miss is recorded in {!offline_misses} and
    behaves like a failed request. *)

val base : string
(** The URL prefix of opam-repo-ci's commit pages: [base ^ sha] is the CI page
    of a commit of ocaml/opam-repository. *)

val repo : string
(** "ocaml/opam-repository". *)

val user_agent : string
(** The User-Agent of the tool's HTTP requests. *)

(** {1 Request counts} *)

val net : int Py.Odict.t
(** Request counts ("http", "mcp", "git", "github-api", then "net-errors",
    "llm", "refreshed", ... in the order they were first set). With
    [OPAM_CI_NET_FILE] set, they are also written to that file (JSON) on every
    change: a caller that kills the tool (a timeout) still knows what it did,
    LLM calls included. *)

val net_set : string -> int -> unit

val net_get : string -> int
(** 0 when not set. *)

val net_incr : string -> unit

(** {1 Offline mode} *)

val offline_ref : bool ref
(** Initially whether [OPAM_CI_OFFLINE=1]; the tests set it. *)

val is_offline : unit -> bool

val offline_misses : string list ref
(** What offline mode could not find in the cache, in order. *)

val offline_miss : string -> unit
(** Record a cache miss in offline mode. *)

(** {1 Cache files} *)

val write_atomic : string -> string -> unit
(** [write_atomic path data]: write [data] to [path] through a temporary file in
    the same directory, renamed over it: a killed process leaves the old file or
    none, never a truncated one. Nothing is written offline (the tests use their
    fixtures as the cache), except the request counts' file. *)

val fresh : string -> float option -> bool
(** [fresh path max_age]: is the file at [path] younger than [max_age] seconds
    (None: no limit; offline, everything is)? *)

val cached_json : ?max_age:float -> string -> Json.t option
(** The JSON stored at a path, or in its gzip-compressed copy (test fixtures),
    or None if there is neither (or it is older than [max_age] seconds, except
    offline, or unreadable). *)

val sha256_hex : string -> string

val cache_path : string -> string -> string
(** [cache_path url cache_dir]: where {!fetch} keeps the body of [url]. *)

(** {1 HTTP} *)

val fetch : ?refresh:bool -> ?max_age:float -> string -> string -> string
(** [fetch url cache_dir]: the body of [url], cached on disk (fetched again when
    the cached copy is older than [max_age] seconds, except offline, or with
    [refresh]). Retries on 429, 502 and 503; raises [Http.Http_error] for an
    HTTP error (a 404 is remembered for a day), [Http.Net_error], or [Failure]
    offline when the page is not in the cache. *)

(** {1 GitHub API} *)

val github_token : unit -> string option
(** A GitHub token if one is available (GITHUB_TOKEN/GH_TOKEN, or a logged-in gh
    CLI). None is fine: unauthenticated requests work, limited to 60/hour. *)

val gh_cache : (string, Json.t) Hashtbl.t
(** The answers of {!gh_api} already read in this process, by path. *)

val gh_api : ?cache_dir:string -> ?cache:bool -> string -> Json.t option
(** A GET of the GitHub REST API (a path such as
    ["repos/ocaml/opam-repository/pulls/31275"]), token optional, cached on disk
    unless [cache] is false. None when it fails (counted in "net-errors", except
    a 404). *)

(** {1 git}

    Upstream git URLs come from the dev-repo and src fields of opam files
    (untrusted): only plain https URLs reach git, after [--] where git takes it,
    and git may only use the https transport (no ssh://, file://, ext::). *)

val git_protocols : string list
(** The options of every git command that restrict it to https. *)

val safe_git_url : string option -> bool
(** Whether a URL may be given to git: a plain https URL. *)

val git_unreachable : string
(** A regular expression matching the errors of a git fetch or ls-remote that a
    dead or misconfigured host gives every time (no DNS name, a bad certificate,
    nothing listening, no answer): the URL is not asked again for
    {!git_down_max_age} seconds (see {!Sources}). *)

val git_down_max_age : float

val git :
  ?cwd:string -> ?check:bool -> ?git_dir:string -> string list -> Proc.result
(** Run git. With [git_dir], on that repository only (GIT_DIR): git never falls
    back to a repository found in a parent directory. A fetch or ls-remote that
    fails because of the network or the server counts in "net-errors" (the tags
    or files are then unknown, not absent). Raises [Failure] when [check] and
    git fails. *)

val repo_url : string -> string option
(** Normalise an opam dev-repo / src URL to a fetchable https git URL (None when
    it is not a safe one). *)

(** {1 Test seams} *)

val rename_hook : (string -> string -> unit) ref
(** How {!write_atomic} renames its temporary file. *)

val sleep_hook : (float -> unit) ref
(** How {!fetch} waits before a retry. *)
