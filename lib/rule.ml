(* The rules that recognise a failure. See rule.mli. *)

type t =
  | Library_api
  | Stdlib_change
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
  | Compiler_matrix
  | Platform_matrix
  | Builder_specific
  | Compiler_matrix_systemic
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
  | Sources
  | Accept_failures_ignored
  | Unrecognised
  | Llm
  | Rule_error
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
  | Test_timeout
  | Other of string

let names =
  [
    (Library_api, "library-api");
    (Stdlib_change, "stdlib");
    (Safe_string, "safe-string");
    (Dune_lang, "dune-lang");
    (Missing_tool, "missing-tool");
    (Missing_pkg_config, "missing-pkg-config");
    (Missing_library, "missing-library");
    (Missing_header, "missing-header");
    (Missing_system_package, "missing-system-package");
    (Install_missing_files, "install-missing-files");
    (Test_output, "test-output");
    (Test_output_ocaml, "test-output-ocaml");
    (Compiler_matrix, "compiler-matrix");
    (Platform_matrix, "platform-matrix");
    (Builder_specific, "builder-specific");
    (Compiler_matrix_systemic, "compiler-matrix-systemic");
    (Platform_matrix_systemic, "platform-matrix-systemic");
    (Builder_specific_systemic, "builder-specific-systemic");
    (Pr_relaxed_available, "pr-relaxed-available");
    (Lint, "lint");
    (Lint_ci_error, "lint-ci-error");
    (Analysis, "analysis");
    (Uninstallable, "uninstallable");
    (Bounded_out, "bounded-out");
    (Newer_c_compiler, "newer-c-compiler");
    (System_library_version, "system-library-version");
    (Rerun, "rerun");
    (Transient, "transient");
    (Sources, "sources");
    (Accept_failures_ignored, "accept-failures-ignored");
    (Unrecognised, "unrecognised");
    (Llm, "llm");
    (Rule_error, "rule-error");
    (Opam_same_version_reinstall, "opam-same-version-reinstall");
    (Needs_git_checkout, "needs-git-checkout");
    (Install_outside_prefix, "install-outside-prefix");
    (Unsupported_arch, "unsupported-arch");
    (Platform_unsupported, "platform-unsupported");
    (Health_check_new, "health-check-new");
    (Health_check_preexisting, "health-check-preexisting");
    (Lower_bound, "lower-bound");
    (Previous_release_bound, "previous-release-bound");
    (Link_error, "link-error");
    (Broken_install, "broken-install");
    (Ocaml_runtime_api, "ocaml-runtime-api");
    (Test_platform, "test-platform");
    (Conf_no_depext, "conf-no-depext");
    (Ci_failure, "ci-failure");
    (Test_timeout, "test-timeout");
  ]

let to_string = function Other s -> s | r -> List.assoc r names

let of_string s =
  match List.find_opt (fun (_, n) -> n = s) names with
  | Some (r, _) -> r
  | None -> Other s

let systemic = function
  | Compiler_matrix -> Compiler_matrix_systemic
  | Platform_matrix -> Platform_matrix_systemic
  | Builder_specific -> Builder_specific_systemic
  | r -> of_string (to_string r ^ "-systemic")

let is_systemic = function
  | Compiler_matrix_systemic | Platform_matrix_systemic
  | Builder_specific_systemic ->
      true
  | Other s -> String.ends_with ~suffix:"-systemic" s
  | _ -> false
