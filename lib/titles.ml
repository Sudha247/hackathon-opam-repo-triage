open Suggestion.T

let rule_titles =
  [
    ("library-api", "API change in a dependency");
    ("stdlib", "Standard library change");
    ("safe-string", "Needs OCaml < 4.06 (safe-string)");
    ("dune-lang", "dune too old");
    ("missing-tool", "Missing tool");
    ("missing-pkg-config", "Missing pkg-config file");
    ("missing-library", "Missing library");
    ("missing-header", "Missing C header");
    ("missing-system-package", "System package not found");
    ("install-missing-files", "Files not built");
    ("test-output", "Test output differs");
    ("test-output-ocaml", "Test output depends on the OCaml version");
    ("compiler-matrix", "Fails on some OCaml versions");
    ("platform-matrix", "Fails on some platforms");
    (* The -systemic titles get the number of packages (see [title_of]). *)
    ("platform-matrix-systemic", "Fails on some platforms");
    ("compiler-matrix-systemic", "Fails on some OCaml versions");
    ("builder-specific", "Fails only on the day10 builder");
    ("builder-specific-systemic", "Fails only on the day10 builder");
    ("pr-relaxed-available", "`available` relaxed by this PR");
    ("lint", "opam lint error");
    ("lint-ci-error", "Lint job failed (CI error)");
    ("analysis", "CI could not analyse the PR");
    ("uninstallable", "Not installable anywhere");
    ("bounded-out", "This PR's bound excludes every tested platform");
    ("newer-c-compiler", "Newer C compiler");
    ("system-library-version", "System library version");
    ("rerun", "Jobs to rerun");
    ("sources", "Source archive cannot be unpacked");
    ("accept-failures-ignored", "x-ci-accept-failures ignored by day10");
    ("unrecognised", "Unexplained failures");
    ("llm", "LLM diagnosis (unchecked)");
    ("rule-error", "Analysis failed (tool error)");
    ("opam-same-version-reinstall", "opam reinstalls at the same version");
    ("needs-git-checkout", "Build needs a git checkout");
    ("install-outside-prefix", "Installs outside the switch");
    ("unsupported-arch", "Architecture not supported");
    ("platform-unsupported", "Platform not supported");
    ("health-check-new", "Broken by this PR (builds on master)");
    ("lower-bound", "Lower bound too low");
    ("link-error", "Link error");
    ("broken-install", "Incomplete installation in the switch");
    ("ocaml-runtime-api", "OCaml runtime function missing");
    ("test-platform", "Fails on one compiler variant");
    ("health-check-preexisting", "Also fails on master");
    ("conf-no-depext", "No system package for this platform");
  ]

let title_of (r : Suggestion.t) =
  match r.rule with
  | None | Some "" -> (
      match r.title with Some t when t <> "" -> t | _ -> "Note")
  | Some rule -> (
      let t = Option.value ~default:rule (List.assoc_opt rule rule_titles) in
      let t =
        if rule = "lint" && r.lint_level = Some "warning" then
          "opam lint warning"
        else t
      in
      let m =
        if String.ends_with ~suffix:"-systemic" rule then
          Rx.match_ {|(\d+) packages \[|} (Suggestion.str r.Suggestion.variant)
        else None
      in
      match m with
      | Some m -> Printf.sprintf "%s (%s packages)" t (Rx.group m 1)
      | None -> t)

(* Where the fix goes, in words, by [where_kind] (see where.ml): "this PR",
   "{pkg} (another package)", "upstream", "the CI", "rerun" or "nothing to
   fix"; None when unknown. *)
let where_words =
  [
    ("pr", "this PR");
    ("upstream", "upstream");
    ("ci", "the CI");
    ("rerun", "rerun");
    ("none", "nothing to fix");
  ]

(* Another package the analysis cannot name, by rule. *)
let where_unnamed =
  [
    ( "missing-system-package",
      "the dependency that declares these depexts (another package)" );
    ("health-check-preexisting", "these reverse dependencies (other packages)");
  ]

let where_text r =
  match Where.where_kind r with
  | "other-package" -> (
      match Where.where_pkg r with
      | Some pkg when pkg <> "" -> Some (pkg ^ " (another package)")
      | _ ->
          Some
            (Option.value ~default:"another package"
               (List.assoc_opt (Suggestion.str r.rule) where_unnamed)))
  | "unknown" when String.ends_with ~suffix:"-systemic" (Suggestion.str r.rule)
    ->
      Some "unknown: one cause shared by these packages"
  | k -> List.assoc_opt k where_words

let fix_label where =
  if where = "rerun" || where = "nothing to fix" then where
  else if where = "upstream" then "fix upstream"
  else "fix in " ^ where

let readable (results : Suggestion.t list) =
  List.iter
    (fun r ->
      if Suggestion.nonempty r.rule then r.title <- Some (title_of r);
      if Suggestion.nonempty r.Suggestion.variant then
        r.job <- Some (Ci.describe_job (Suggestion.str r.Suggestion.variant));
      (* Both from the suggestion as it is: once [where_kind] is set,
         [where_pkg] no longer derives the package from [fix_in] (LLM answers). *)
      let kind = Where.where_kind r and pkg = Where.where_pkg r in
      r.where_kind <- Some kind;
      r.where_pkg <-
        (match pkg with Some pkg when pkg <> "" -> Set pkg | _ -> Absent);
      r.where <- Suggestion.field_of_option (where_text r);
      (* The note and the error as sentences. *)
      let note = Py.strip (Suggestion.str r.note) in
      if note <> "" then
        r.note <-
          Some
            (if String.contains ".)`!?" note.[String.length note - 1] then note
             else note ^ ".");
      let error = Py.strip (Suggestion.str r.error) in
      if error <> "" then
        r.error <-
          Some
            (if String.ends_with ~suffix:":" error then
               Py.rstrip (Py.rstrip ~chars:":" error)
             else error))
    results;
  results

let per_package_rules =
  [
    "newer-c-compiler";
    "system-library-version";
    "opam-same-version-reinstall";
    "lower-bound";
  ]

let workaround_rules = [ "platform-matrix"; "missing-system-package" ]

let classify r =
  let has_change =
    Suggestion.nonempty_field r.Suggestion.new_
    || Suggestion.list r.candidates <> []
    || Suggestion.nonempty r.advice
  in
  let k = Where.where_kind r in
  if Suggestion.str r.rule = "rule-error" || k = "unknown" then "explanation"
  else if k <> "pr" then "elsewhere"
  else if not has_change then "explanation"
  else if
    List.mem (Suggestion.str r.rule) workaround_rules
    || Py.contains
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
  Py.sort_by
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
