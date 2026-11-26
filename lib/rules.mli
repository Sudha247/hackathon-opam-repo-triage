(** Rules on one failed job's log: from the log, the failing package's opam file
    and what ocaml.org and the upstream sources say, a suggestion of what to
    change and why. *)

val suggest_for_job :
  Sources.t -> Ci.job -> string -> string list -> string -> Suggestion.t
(** [suggest_for_job src job log pr_packages pr_sha]: the suggestion for one
    failed job, from its log; an "Unrecognised error" note when no rule applies.
*)

(** {1 Completing a suggestion with other jobs} *)

val complete_test_output :
  ?fetch:(string -> string -> string) ->
  Sources.t ->
  Suggestion.t ->
  Ci.job ->
  string ->
  Ci.data ->
  string ->
  unit
(** [complete_test_output src r job log data cache_dir]: for a test-output diff,
    compare the failing job's installed versions with a passing job of the same
    package and suggest a with-test upper bound ([fetch] reads the passing job's
    log). *)

val lower_bounds_rule :
  Sources.t ->
  Ci.job ->
  string ->
  Suggestion.t ->
  string list ->
  string ->
  Suggestion.t option
(** [lower_bounds_rule src job log r pr_packages pr_sha]: for a lower-bounds job
    (CI installs the PR's package with the oldest versions its constraints
    allow). Either a dependency of it was downgraded to a version that no longer
    builds (a new suggestion: raise the bound of the direct dependency that
    pulls it in), or the package itself fails and no rule explains why ([r] gets
    the downgraded direct dependencies as candidates, those named in the error
    first). *)

type prev_release
(** What {!previous_release_bound} learnt about the previous release of a
    package. *)

val previous_release_bound :
  Sources.t ->
  Ci.job ->
  Loginfo.info ->
  string list ->
  string ->
  (string, prev_release option) Hashtbl.t ->
  Suggestion.t option
(** [previous_release_bound src job info pr_packages pr_sha cache]: a new
    release fails with a dependency version that the previous release excludes
    on master (maintainers often add bounds to old versions after a breakage,
    and a new release made from the upstream opam file does not have them):
    suggest the same bound, citing the previous release. [cache] keeps what was
    learnt per package for the other jobs. *)

val conf_depext_platforms : Suggestion.t -> (string, Ci.job) Hashtbl.t -> unit
(** Complete a conf-no-depext result with the jobs behind it (variant -> job):
    the same error on other platforms, where a depext may apply (its check then
    failed for another reason). Names the platforms without one; the others are
    said apart. *)

(** {1 Single rules (for the tests)} *)

type ctx = {
  pr_packages : string list;
  full_text : string;  (** the whole log *)
  job_variant : string;
}
(** What the log rules know besides the log's {!Loginfo.info}. *)

val log_rules :
  Sources.t ->
  string list ->
  string ->
  string option ->
  Loginfo.info ->
  ctx ->
  Ci.job ->
  Suggestion.set list option
(** [log_rules src error_lines fname opam_text info ctx job]: the rules on the
    error of the failing package [fname] (its opam file [opam_text]): a missing
    dune language version, tool, library or C header, an OCaml runtime function
    this OCaml lacks, a newer C compiler, known messages. None when none
    applies. *)

val find_unbound : string list -> (string * string list * string) option
(** The unbound identifier of an error: (kind, module path, identifier). *)

val module_find_note :
  Sources.module_find option ->
  string ->
  Loginfo.info ->
  string option ->
  string ->
  string
(** [module_find_note found top info opam_text fname]: what ocaml.org's module
    search ({!Sources.module_providers}) said about module [top], for the note
    of an unbound identifier that no dependency's history explains: "" when it
    gave no answer, or a partial one without a match. *)

val depext_entries : string option -> (string list * string) list option
(** The entries of an opam file's [depexts] field, as (packages, filter) pairs
    (filter "" when there is none), or None without the field. *)

val conf_without_depext :
  string ->
  string option ->
  Loginfo.info ->
  ctx ->
  Ci.job ->
  string ->
  Suggestion.set list option
(** [conf_without_depext fname opam_text info ctx job platform]: a conf package
    whose check failed on a platform for which it declares no system package:
    nothing was installed for the check to find (conf-glib-2 on msys2, whose
    depexts cover Cygwin only, #31054). None when a depext applies there, or
    when the tool cannot tell (a filter on a variable it does not know for that
    platform). *)
