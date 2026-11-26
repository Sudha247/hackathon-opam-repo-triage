(** opam files, versions, ocaml.org docs and upstream sources: cached,
    token-free access to opam-repository and upstream repositories.

    Upstream sources are read with plain git (no token, any host) in bare,
    shallow, blob-less mirrors under [cache_dir/git] ([git ls-remote --tags],
    then a fetch of one tag gives the file list); single files come from
    raw.githubusercontent.com (GitHub) or are fetched on demand by git (other
    hosts). ocaml.org's documentation, API history and package data come from
    its MCP endpoint (JSON; docs are built for every package version in
    opam-repository).

    Each function that reaches the network has a test seam ([*_hook]) that the
    tests replace. *)

type t
(** A source of data, with its cache directory and in-memory caches (one per
    analysis). *)

val create : string -> t
(** [create cache_dir]. *)

(** {1 Versions} *)

val max_version : string list -> string option
(** The highest version (opam order), None for none. *)

val min_version : string list -> string option

val satisfies : string -> (string * string) list -> bool
(** Whether a version satisfies all the bounds ([(op, version)]). *)

val existing_bounds : string option -> string -> (string * string) list
(** [existing_bounds opam_text dep]: [(op, version)] from a simple
    ["dep" {>= "a" & < "b"}] constraint of an opam file. *)

val constraint_edit :
  string option -> string -> string -> string -> string option * string
(** [constraint_edit opam_text dep op version]: (old line, new line) for the
    dependency [dep] in [opam_text], with the bound [op version] replacing a
    bound of the same direction (old line None: a new line). *)

(** {1 opam-repository} *)

val raw : ?max_age:float -> t -> string -> string option
(** The body of a URL, or None if it does not exist or could not be fetched
    (counted in "net-errors"). *)

val opam_file : ?ref:string -> t -> string -> string -> string option
(** [opam_file src name version]: the opam file of name.version in
    opam-repository at [ref] (a commit; master by default, fetched again after a
    day: bounds and x-maintained get edited there). *)

val versions : ?ref:string -> t -> string -> string list option
(** [versions src name]: the versions of [name] in opam-repository, oldest
    first: from ocaml.org's ocaml_package_versions for master ([] if not a
    package; None if the MCP server did not answer, counted in "mcp-errors":
    callers must then not conclude that [name] is not a package), from a git
    listing for another ref. *)

val versions_or_empty : ?ref:string -> t -> string -> string list
(** {!val-versions}, or []. *)

val published : t -> string -> string -> string option
(** [published src pkg version]: the release date (YYYY-MM-DD) of pkg.version,
    from ocaml_package_versions, or None. *)

val upstream : ?ref:string -> t -> string -> string -> string option
(** [upstream src name version]: the upstream git URL of a package, from its
    opam dev-repo (or url src). *)

val find_baseline_commit : ?cache_dir:string -> string -> Dict.t option
(** For a target such as "lwt.5.10.2", the PR that added the previous version on
    master: [{"prev", "pr", "sha"}] (2 GitHub REST calls), or None. Its CI run
    is the baseline the [compare] command uses. *)

(** {1 opam files} *)

val opam_field : string option -> string -> string option
(** [opam_field text field]: the value of a string field of an opam file. *)

val url_src : string option -> string option
(** The [src] of an opam file's [url] section. *)

val license_id : string option -> string option
(** A coarse SPDX guess from a license text. *)

(** {1 Upstream sources} *)

val tags : t -> string -> string list
(** [tags src url]: the tags of an upstream repository (kept a day on disk; none
    for an unsafe URL or a host that is down). *)

val tag_for : t -> string -> string -> string -> string option
(** [tag_for src url name version]: the tag of a release ("1.0", "v1.0",
    "name-1.0", ...). *)

val gitdir : t -> string -> string
(** The local mirror of an upstream repository, made if needed. Raises [Failure]
    for a URL that is not a plain https one. *)

val ensure : t -> string -> string -> bool -> string option
(** [ensure src url ref is_tag]: make [ref] (a tag name or a commit sha)
    available in the mirror; a rev for git, or None. *)

val tree : ?is_tag:bool -> t -> string -> string -> string list
(** [tree src url ref]: the files of the upstream repository at [ref]. *)

val file_at : ?is_tag:bool -> t -> string -> string -> string -> string option
(** [file_at src url ref path]: a file of the upstream repository at [ref]. *)

val module_files : string list -> string -> string list
(** [module_files paths modname]: the source files defining compilation unit
    [modname] (also foo.lwt.ml variants), the [.mli] first. *)

val defines : string -> string -> string -> bool
(** [defines text kind ident]: does the source [text] define [ident] of [kind]
    ("value", "constructor", "field", "type", "module", "module-type", "class",
    "class-type", "exception")? *)

val present :
  ?stdlib:bool ->
  t ->
  string ->
  string ->
  string list ->
  string ->
  string ->
  bool option
(** [present src repo ref path_mods kind ident]: is [ident] (of [kind]), reached
    through modules [path_mods], defined at [ref] of the upstream repository
    [repo]? None when the source cannot be located. With [stdlib] (the
    compiler's repository), only the files under [stdlib/]. *)

val ocaml_releases : t -> string list
(** The OCaml releases (X.Y.0 tags of ocaml/ocaml) from 4.08 on, in order. *)

(** {1 ocaml.org} *)

val mcp : ?max_age:float -> t -> string -> (string * Json.t) list -> Json.t
(** [mcp src tool args]: call an ocaml.org MCP tool; its parsed JSON result, or
    [`Null] on error. Answers are cached on disk, including the tool's own
    errors (an unknown package, a version without docs), as null, and
    documentation answers whose status is not success, for a day; with [max_age]
    (seconds), an older answer is fetched again (for data that changes, like
    health-check results). A call that fails (network error, timeout, HTTP
    error, unreadable answer) is not cached and is counted in "mcp-errors": the
    analysis is then incomplete. After {!mcp_max_failures} failed calls in a
    process, ocaml.org is not called again. *)

val mcp_url : string
val mcp_timeout : float

val mcp_max_failures : int
(** After this many failed calls in a process, ocaml.org is not called again:
    each skipped call counts as a failure ("mcp-errors"). *)

val mcp_failures : int ref
(** Failed ocaml.org MCP calls in this process. *)

val mcp_unavailable : (string, unit) Hashtbl.t
(** The tools (API history, module search) that ocaml.org's MCP server said it
    does not have ("unknown tool"): not asked again in this process; the callers
    have another way. *)

type changelog = {
  file : string option;  (** the changelog's file name *)
  cversions : string list;  (** the versions its sections name *)
  text : string;  (** written by the package's authors: untrusted *)
  truncated : bool;
}
(** Sections of a changelog. *)

val changelog :
  ?since:string -> ?until:string -> t -> string -> changelog option
(** [changelog ~since ~until src pkg]: the sections of pkg's changelog for the
    versions after [since] up to [until], from ocaml_package_changelog, or None
    (no changelog, no section heading names a version, or no section in the
    range). *)

val doc_modules : t -> string -> string -> (string * string) list option
(** [doc_modules src pkg version]: the top modules of the docs of pkg.version
    and their pages ([("TopModule", "lib/TopModule")]), or None without docs. *)

val doc_present :
  t -> string -> string -> string list -> string -> string -> bool option
(** [doc_present src pkg version mods kind ident]: is [ident] (reached through
    module path [mods]) documented for pkg.version? None when ocaml.org has no
    docs for that version. *)

type history = {
  versions : (string * string) list;  (** version -> the tool's status *)
  hmodule : string option;
      (** the module's full path, or None if no documented version has it *)
  partial : bool;
}
(** An API history (see {!api_history}). *)

val api_history :
  t ->
  string ->
  string list ->
  string ->
  string ->
  string list ->
  history option
(** [api_history src pkg mods kind ident versions]: whether the item [ident] (of
    [kind], in module path [mods] as written in the code) is in each of
    [versions] of [pkg], from ocaml.org's API history (ocaml_module_changes, one
    call for the range of [versions]). A status is the tool's: "present",
    "absent", "module-not-found" (documented, the module missing),
    "page-truncated", "no-docs", "docs-failed", or a failure
    ("docs-unavailable", "error", "skipped": the answer is partial, counted in
    "mcp-errors"); a version the tool did not check has none. See
    {!history_presence}. An item in no version checked (and of no other kind it
    may be: a constructor may be an exception's) is "absent" in each documented
    version ("found": false; older servers answer with an error instead, which
    gives no history). None when the tool gives no history: not deployed, a
    failed call (counted in "mcp-errors"), no module path (an unbound top-level
    module), its error (an unknown package or version; on older servers, the
    item in no documented version: then the callers look version by version,
    which also covers the versions without docs), and when the module path names
    several modules ("ambiguous_with") and the tool may have used another module
    than the code's: unless it chose the only one that has the item and could
    check them all ("resolved_by": "identifier", none "unchecked"). *)

type presence = Yes | No | No_docs | Unknown

val history_presence : history option -> string -> presence
(** From an {!api_history} answer: [Yes] or [No] when ocaml.org's docs of a
    version say whether it has the item (a module missing from a version makes
    the item absent there, if another version has it); [No_docs] when the
    version has no docs, or none of the versions has the module (the docs cannot
    tell: only the upstream sources can); [Unknown] when the tool could not tell
    (a page too large, docs-ci not read, a version not checked): the docs of
    that version may still be read another way. *)

val history_has_module : history option -> string -> bool option
(** From an {!api_history} answer: whether the module is documented in a version
    (Some true/false), or None when unknown. *)

val api_history_source : string
(** The [source] of a lookup answered by ocaml.org's ocaml_module_changes. *)

type find_match = {
  package : string;
  module_path : string list;
  mversion : string option;
  library : string option;
}
(** A module found by {!module_providers}. *)

type module_find = {
  matches : find_match list;
  checked : (string * string) list;  (** package -> status *)
  no_docs : string list;
  distribution : bool;
  fpartial : bool;
}
(** An answer of {!module_providers}. *)

val module_providers : t -> string -> string list -> module_find option
(** [module_providers src name packages]: which of [packages] (and of the
    packages named after the module) document a module [name] (a module path as
    written in the code), from ocaml.org's ocaml_module_find (one call; each
    candidate is searched in its latest documented version among its 5 newest,
    the module at any depth: "Term" finds why3's Why3.Term): the matches, in the
    tool's order (the packages given first, in their order), modules only;
    [checked]: package -> status ("match", "no-match", "no-docs", or a failure:
    "docs-unavailable", "error", "skipped"); [no_docs]: the candidates without
    docs in their newest versions; [distribution]: the tool says the module
    comes with the OCaml compiler (Unix, Str...); [fpartial]: some candidates
    could not be searched (counted in "mcp-errors", not cached). None when the
    tool gives no answer: not deployed, a failed call (counted in "mcp-errors"),
    or its error (no known package among the candidates, a name that is not a
    module path). *)

val module_find_source : string
(** The [source] of a lookup whose package ocaml.org's ocaml_module_find chose.
*)

(** {1 Bisection} *)

val bisect_cap : int
(** How many versions {!bisect_presence} checks on each side of the failing one.
*)

val bisect_presence :
  ?allowed:(string -> bool) ->
  ?cap:int ->
  (string -> bool option) ->
  string list ->
  string ->
  (string * string) option * (string * bool option) list
(** [bisect_presence check versions bad]: [check v] is Some true / Some false /
    None; the identifier is missing at version [bad]. Walk outwards from [bad]
    (history is not always monotonic): the first newer version that has it gives
    (">=", v); otherwise the closest older version that has it gives ("<", the
    version right after it). Only versions accepted by [allowed] (the package's
    other existing bounds) are considered. Returns the bound and the evidence
    (version -> answer, in the order checked). *)

val norm : string -> string
(** A name lowercased without "-" and "_", to compare package and module names.
*)

(** {1 Test seams} *)

val git_hook :
  (?cwd:string -> ?check:bool -> ?git_dir:string -> string list -> Proc.result)
  ref

val raw_hook : (?max_age:float -> t -> string -> string option) ref
val tags_hook : (t -> string -> string list) ref
val tag_for_hook : (t -> string -> string -> string -> string option) ref
val tree_hook : (?is_tag:bool -> t -> string -> string -> string list) ref

val file_at_hook :
  (?is_tag:bool -> t -> string -> string -> string -> string option) ref

val versions_hook : (?ref:string -> t -> string -> string list option) ref

val doc_modules_hook :
  (t -> string -> string -> (string * string) list option) ref

val doc_present_hook :
  (t -> string -> string -> string list -> string -> string -> bool option) ref

val api_history_hook :
  (t ->
  string ->
  string list ->
  string ->
  string ->
  string list ->
  history option)
  ref

val module_providers_hook :
  (t -> string -> string list -> module_find option) ref

val published_hook : (t -> string -> string -> string option) ref
val opam_file_hook : (?ref:string -> t -> string -> string -> string option) ref
val upstream_hook : (?ref:string -> t -> string -> string -> string option) ref
