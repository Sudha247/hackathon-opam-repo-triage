(** The rules that recognise a failure: the [rule] of a {!Suggestion.t}.

    Each rule has a name, the string that [suggest --json] prints and the triage
    page stores ("library-api", "platform-matrix", ...). *)

type t =
  | Library_api  (** an API change in a dependency *)
  | Stdlib_change  (** "stdlib": a change of the standard library *)
  | Safe_string
  | Dune_lang
  | Missing_tool
  | Missing_pkg_config
  | Missing_library
  | Missing_header
  | Missing_system_package
  | Install_missing_files
  | Test_output
  | Test_output_ocaml
  | Compiler_matrix  (** fails on some OCaml versions *)
  | Platform_matrix  (** fails on some platforms *)
  | Builder_specific  (** fails only on the day10 builder *)
  | Compiler_matrix_systemic
      (** the [-systemic] rules: the same pattern over several packages *)
  | Platform_matrix_systemic
  | Builder_specific_systemic
  | Pr_relaxed_available
  | Lint
  | Lint_ci_error
  | Analysis
  | Uninstallable
  | Bounded_out
  | Newer_c_compiler
  | System_library_version
  | Rerun
  | Transient
      (** a network error or a cancelled job, before they are merged into
          {!Rerun} *)
  | Sources
  | Accept_failures_ignored
  | Unrecognised
  | Llm  (** an LLM answer *)
  | Rule_error  (** a rule crashed on the log *)
  | Opam_same_version_reinstall
  | Needs_git_checkout
  | Install_outside_prefix
  | Unsupported_arch
  | Platform_unsupported
  | Health_check_new
  | Health_check_preexisting
  | Lower_bound
  | Previous_release_bound
  | Link_error
  | Broken_install
  | Ocaml_runtime_api
  | Test_platform
  | Conf_no_depext
  | Ci_failure
      (** the CI failed, not the package: a crash of its tools or workers, a
          base image it could not build, a container that did not start *)
  | Test_timeout  (** a test ran past its own time limit *)
  | Test_needs_other_package
      (** [dune runtest -p pkg] needs a target of another package *)
  | Compiler_option
  | Experimental_only
      (** fails only on a compiler-option variant (flambda, no-flat-float-array,
          ...), passes on the plain compiler *)
  | Other of string
      (** a name no rule has (stored by another version): kept as it is. Only
          {!of_string} makes one, so that it never holds a known name. *)

val to_string : t -> string
(** The rule's name: "library-api", "stdlib", ... *)

val of_string : string -> t
(** The rule of a name; [to_string (of_string s) = s]. *)

val systemic : t -> t
(** The rule of the same pattern over several packages:
    [systemic Platform_matrix = Platform_matrix_systemic]. *)

val is_systemic : t -> bool
(** Whether the rule is a pattern over several packages (its name ends with
    "-systemic"). *)
