open Suggestion.T

(* The title of a rule; None for the rules without one, named by their name. *)
let rule_title : Rule.t -> string option = function
  | Library_api -> Some "API change in a dependency"
  | Stdlib_change -> Some "Standard library change"
  | Safe_string -> Some "Needs OCaml < 4.06 (safe-string)"
  | Dune_lang -> Some "dune too old"
  | Missing_tool -> Some "Missing tool"
  | Missing_pkg_config -> Some "Missing pkg-config file"
  | Missing_library -> Some "Missing library"
  | Missing_header -> Some "Missing C header"
  | Missing_system_package -> Some "System package not found"
  | Install_missing_files -> Some "Files not built"
  | Test_output -> Some "Test output differs"
  | Test_output_ocaml -> Some "Test output depends on the OCaml version"
  (* The -systemic titles get the number of packages (see [title_of]). *)
  | Compiler_matrix | Compiler_matrix_systemic ->
      Some "Fails on some OCaml versions"
  | Platform_matrix | Platform_matrix_systemic -> Some "Fails on some platforms"
  | Builder_specific | Builder_specific_systemic ->
      Some "Fails only on the day10 builder"
  | Pr_relaxed_available -> Some "`available` relaxed by this PR"
  | Lint -> Some "opam lint error"
  | Lint_ci_error -> Some "Lint job failed (CI error)"
  | Analysis -> Some "CI could not analyse the PR"
  | Uninstallable -> Some "Not installable anywhere"
  | Bounded_out -> Some "This PR's bound excludes every tested platform"
  | Newer_c_compiler -> Some "Newer C compiler"
  | System_library_version -> Some "System library version"
  | Rerun -> Some "Jobs to rerun"
  | Sources -> Some "Source archive cannot be unpacked"
  | Accept_failures_ignored -> Some "x-ci-accept-failures ignored by day10"
  | Unrecognised -> Some "Unexplained failures"
  | Llm -> Some "LLM diagnosis (unchecked)"
  | Rule_error -> Some "Analysis failed (tool error)"
  | Opam_same_version_reinstall -> Some "opam reinstalls at the same version"
  | Needs_git_checkout -> Some "Build needs a git checkout"
  | Install_outside_prefix -> Some "Installs outside the switch"
  | Unsupported_arch -> Some "Architecture not supported"
  | Platform_unsupported -> Some "Platform not supported"
  | Health_check_new -> Some "Broken by this PR (builds on master)"
  | Lower_bound -> Some "Lower bound too low"
  | Link_error -> Some "Link error"
  | Broken_install -> Some "Incomplete installation in the switch"
  | Ocaml_runtime_api -> Some "OCaml runtime function missing"
  | Test_platform -> Some "Fails on one compiler variant"
  | Health_check_preexisting -> Some "Also fails on master"
  | Conf_no_depext -> Some "No system package for this platform"
  | Ci_failure -> Some "CI failure, not the package"
  | Test_timeout -> Some "Test timed out"
  | Test_needs_other_package -> Some "Tests need another package's targets"
  | Compiler_option -> Some "Fails only with a compiler option"
  | Experimental_only -> Some "Fails only on experimental platforms"
  | Transient | Previous_release_bound | Other _ -> None

let packages_re = Rx.v {|(\d+) packages \[|}

let title_of (r : Suggestion.t) =
  match r.rule with
  | None -> ( match r.title with Some t when t <> "" -> t | _ -> "Note")
  | Some rule -> (
      let t =
        match rule_title rule with Some t -> t | None -> Rule.to_string rule
      in
      let t =
        if rule = Lint && r.lint_level = Some "warning" then "opam lint warning"
        else t
      in
      let m =
        if Rule.is_systemic rule then
          Rx.match_ packages_re (Suggestion.str r.Suggestion.variant)
        else None
      in
      match m with
      | Some m -> Printf.sprintf "%s (%s packages)" t (Rx.group m 1)
      | None -> t)

(* Where the fix goes, in words, by [where_kind] (see where.ml): "this PR",
   "{pkg} (another package)", "upstream", "the CI", "rerun" or "nothing to
   fix"; None when unknown. *)
(* Another package the analysis cannot name, by rule. *)
let where_unnamed : Rule.t option -> string = function
  | Some Missing_system_package ->
      "the dependency that declares these depexts (another package)"
  | Some Health_check_preexisting ->
      "these reverse dependencies (other packages)"
  | _ -> "another package"

let is_systemic (r : Suggestion.t) =
  match r.rule with Some rule -> Rule.is_systemic rule | None -> false

let where_text r =
  match Where.where_kind r with
  | Other_package -> (
      match Where.where_pkg r with
      | Some pkg when pkg <> "" -> Some (pkg ^ " (another package)")
      | _ -> Some (where_unnamed r.rule))
  | Unknown when is_systemic r ->
      Some "unknown: one cause shared by these packages"
  | Unknown -> None
  | Pr -> Some "this PR"
  | Upstream -> Some "upstream"
  | Ci -> Some "the CI"
  | Rerun -> Some "rerun"
  | Nothing -> Some "nothing to fix"

let fix_label where =
  if where = "rerun" || where = "nothing to fix" then where
  else if where = "upstream" then "fix upstream"
  else "fix in " ^ where

let readable (results : Suggestion.t list) =
  List.iter
    (fun r ->
      if r.rule <> None then r.title <- Some (title_of r);
      if r.Suggestion.variant <> None then
        r.job <- Some (Ci.describe_job (Suggestion.str r.Suggestion.variant));
      (* Both from the suggestion as it is: once [where_kind] is set,
         [where_pkg] no longer derives the package from [fix_in] (LLM answers). *)
      let kind = Where.where_kind r and pkg = Where.where_pkg r in
      r.where_kind <- Some kind;
      r.where_pkg <-
        (match pkg with Some pkg when pkg <> "" -> Set pkg | _ -> Absent);
      r.where <- Suggestion.field_of_option (where_text r);
      (* The note and the error as sentences. *)
      let note = String.trim (Suggestion.str r.note) in
      if note <> "" then
        r.note <-
          Some
            (if String.contains ".)`!?" note.[String.length note - 1] then note
             else note ^ ".");
      let error = String.trim (Suggestion.str r.error) in
      if error <> "" then
        r.error <-
          Some
            (if String.ends_with ~suffix:":" error then
               Util.rstrip (Util.rstrip ~chars:":" error)
             else error))
    results;
  results

let per_package_rules : Rule.t list =
  [
    Newer_c_compiler;
    System_library_version;
    Opam_same_version_reinstall;
    Lower_bound;
  ]

let workaround_rules : Rule.t list = [ Platform_matrix; Missing_system_package ]

let has_rule rules (r : Suggestion.t) =
  match r.rule with Some rule -> List.mem rule rules | None -> false

let classify r =
  let has_change =
    Suggestion.is_set r.Suggestion.new_
    || Suggestion.list r.candidates <> []
    || r.advice <> None
  in
  let k = Where.where_kind r in
  if r.rule = Some Rule_error || k = Unknown then "explanation"
  else if k <> Pr then "elsewhere"
  else if not has_change then "explanation"
  else if
    has_rule workaround_rules r
    || Util.contains
         (Suggestion.str_field r.Suggestion.new_)
         "x-ci-accept-failures"
  then "workaround"
  else "fix"

let rank results =
  let order = function
    | "fix" -> 0
    | "workaround" -> 1
    | "elsewhere" -> 2
    | _ -> 3
  in
  let conf c =
    match c with "high" -> 0 | "medium" -> 1 | "low" -> 2 | _ -> 3
  in
  List.iter (fun r -> r.class_ <- Some (classify r)) results;
  Util.sort_by
    (fun r ->
      ( order (Suggestion.str r.class_),
        r.Suggestion.experimental = Some true,
        conf
          (List.hd
             (String.split_on_char ' '
                (match Suggestion.get r.confidence with
                | Some c when c <> "" -> c
                | _ -> "low"))) ))
    results
