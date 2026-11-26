(* Classify and group opam-repository CI failures from logs downloaded by fetch-ci-logs.

   Usage:
       classify-logs                      # every ci-logs/<sha>/ directory
       classify-logs ci-logs/<sha> ...    # specific runs
       classify-logs --json report.json   # also write a combined report

   For each failed job the log is classified with rules (no LLM):
     1. log-wide checks for failures outside a package build (CI
        infrastructure, lint, source download, system package installation,
        solver, timeout);
     2. the opam "#=== ERROR while <action> <pkg>" block is searched with
        build-error rules, most specific first;
     3. the matched line is normalised (versions, paths, numbers removed)
        into a signature, and jobs with the same (category, signature) form
        one group.

   Groups whose signature came only from a generic rule, or from no rule at
   all, are marked needs_llm=true; each group carries one representative
   excerpt that can be sent to an LLM.

   Writes classification.json (per job) and groups.json (per group) next to
   each run's logs. *)

open Triage

(* Python's \w, \s and \b see characters: see Rx_utf8. *)
module U = Rx_utf8

(* ------------------------------------------------------------------------ *)
(* Normalisation *)

let path_re = Rx.v {|(?:[A-Za-z]:)?(?:[\\/][\w.@+~-]+){2,}[\\/]?|}
let hash_re = Rx.v {|\b[0-9a-f]{12,}\b|}
let package_version_re = Rx.v {|\b([a-z][\w-]*)\.v?\d[\w.~+-]*|}
let jobs_flag_re = Rx.v {|-j \d+|}
let line_characters_re = Rx.v {|\bline \d+(, characters \d+-\d+)?|}
let spaces_re = Rx.v {|\s+|}

(** Make an error line comparable across jobs: drop versions, paths, numbers,
    hashes. *)
let normalise s =
  let s = Util.strip ~chars:"." (String.trim s) in
  let s =
    U.sub path_re "<path>" s
    (* unix + windows paths *)
  in
  let s = U.sub hash_re "<hash>" s in
  (* package.version -> package   (conf-bison.2, plcairo.5.15.0.1, ocaml.4.14.4) *)
  let s = U.sub package_version_re {|\1|} s in
  let s = U.sub jobs_flag_re "-j N" s in
  let s = U.sub line_characters_re "line N" s in
  U.sub spaces_re " " s

let line_prefix_re = Rx.v {|^(?:[#\-]\s?)?(?:\s*\|\s?)?|}

(** Strip the prefixes opam/day10 put in front of build output ('# ', '- ', '# |
    '). *)
let clean line = U.sub line_prefix_re "" (Util.rstrip line)

(* ------------------------------------------------------------------------ *)
(* Rules: (category, regex, flags, signature template, generic?). The
   template is formatted with the regex groups ({0} = whole match, {1}.. =
   groups). Order matters: earlier rules win. *)

type rule = {
  category : string;
  pattern : Rx.t;
  template : string;
  generic : bool;
}

let r ?(generic = false) ?(flags = []) category pattern template =
  { category; pattern = Rx.v ~flags pattern; template; generic }

(* Searched over the whole log, before the error block. *)
let log_rules =
  [
    r "ci_infrastructure"
      {|runc run failed|special device overlay does not exist|No space left on device|Cannot allocate memory|Docker daemon|cannot connect to the Docker|}
      "CI worker failure (container, mount or disk); retry the job";
    r "ci_infrastructure" {|index\.lock': File exists|}
      "CI worker failure (stale git lock in checkout); retry the job";
    r "timeout" {|Cancelling: Timeout \(([\d.]+) minutes\)|}
      "Job timed out after {1} minutes";
    r "system_package_unavailable"
      {|depends on the unavailable system package '([^']+)'|}
      "System package(s) not available: {1}";
    r "source_download" {|Failed to get sources of \S+: (.*)|}
      "Failed to get sources: {1}";
    r "source_download" ~flags:[ Rx.I ]
      {|Checksum mismatch|Bad checksum|checksum.*does not match|}
      "Source checksum mismatch";
    r "system_package_unavailable" {|These packages are still missing: (.*)|}
      "System package(s) not available: {1}";
    r "system_package_unavailable" {|No provider of '([^']+)' found|}
      "System package(s) not available: {1}";
    r "system_package_unavailable" {|Unable to locate package (\S+)|}
      "System package(s) not available: {1}";
    r "system_package_unavailable" {|No match for argument: (\S+)|}
      "System package(s) not available: {1}";
    r "system_package_unavailable"
      {|System package install failed with exit code (\d+)|}
      "System package install failed (exit {1})" ~generic:true;
    r "solver"
      {|No solution for|The following dependencies couldn't be met|no solution on this variant|}
      "No dependency solution";
    r "timeout" {|Job failed:.*(?:[Tt]imeout|timed out)|} "Job timed out";
  ]

(* Searched over the failing package's error block first, then the whole log. *)
let build_rules =
  [
    (* missing system tools / libraries (usually a missing depext) *)
    r "missing_system_dependency" {|"([^"]+)": command not found|}
      "Missing tool: {1}";
    r "missing_system_dependency"
      {|Program (\S+) not found in the tree or in PATH|} "Missing tool: {1}";
    r "missing_system_dependency" {|one of \[([^\]]+)\] is required|}
      "Missing tool: {1}";
    r "missing_system_dependency" {|(?:^|: )(\S+): (?:command )?not found$|}
      "Missing tool: {1}";
    r "missing_system_dependency"
      {|fatal error: (\S+?\.h(?:pp)?): No such file|} "Missing C header: {1}";
    r "missing_system_dependency"
      {|fatal error: '(\S+?\.h(?:pp)?)' file not found|} "Missing C header: {1}";
    r "missing_system_dependency"
      {|Package '?(\S+?)'?,? was not found in the pkg-config search path|}
      "pkg-config package not found: {1}";
    r "missing_system_dependency" {|No package '(\S+)' found|}
      "pkg-config package not found: {1}";
    r "missing_system_dependency" {|cannot find -l(\S+)|}
      "Missing C library: -l{1}";
    (* OCaml side *)
    r "missing_ocaml_dependency" {|Library "([^"]+)" not found|}
      {|OCaml library not found: "{1}"|};
    r "missing_ocaml_dependency" {|ocamlfind: Package (\S+) not found|}
      "ocamlfind package not found: {1}";
    r "ocaml_compile_error"
      {|Error: (Unbound (?:value|module|type constructor|constructor|record field|field|class|module type) \S+)|}
      "{1}";
    r "ocaml_compile_error" {|Error \((warning \d+ \[[^\]]+\])\)|}
      "Warning as error: {1}";
    r "ocaml_compile_error" {|Error \((alert \S+)\)|} "Alert as error: {1}";
    r "ocaml_compile_error"
      {|Error: (This expression has type|The value \S+ has type|Signature mismatch|The implementation \S+ does not match|This (?:function|pattern|variant)[^.]*)|}
      "{1}";
    (* C / C++ / linker *)
    r "c_compile_error" {|:\d+(?::\d+)?: (?:fatal )?error: (.*)|}
      "C/C++ error: {1}";
    r "c_compile_error" {|undefined reference to [`'](\S+)'|}
      "Linker: undefined reference to {1}";
    (* build environment problems *)
    r "sandbox_violation" {|(Read-only file system|Permission denied)|}
      "Writes outside the opam prefix ({1})";
    r "build_environment" {|fatal: not a git repository|}
      "Build expects a git checkout";
    (* generic fallbacks (still grouped, but flagged for the LLM) *)
    r "ocaml_compile_error" {|^Error: (.*)|} "OCaml error: {1}" ~generic:true;
    r "build_error" {|\berror: (.*)|} "error: {1}" ~generic:true ~flags:[ Rx.I ];
    r "build_error" {|\*\*\* (.*)|} "make: {1}" ~generic:true;
  ]

let error_block_re = Rx.v {|^#=== ERROR while (\w+) (\S+) =+#|}

let block_end_re =
  Rx.v
    {|^(?:#===|<><>|\[ERROR\]|\[WARNING\]|Job (?:succeeded|failed)|\d{4}-\d\d-\d\d \d\d:)|}

let lint_re = Rx.v {|^Error in (\S+?): (.*)|}
let placeholder_re = Rx.v {|\{(\d+)\}|}

(** [tmpl] with [{0}] standing for the whole match, [{1}] ... for its groups
    ("None" when a group did not take part). *)
let format tmpl m =
  Rx.subf placeholder_re
    (fun g ->
      Option.value ~default:"None"
        (U.group_opt m (int_of_string (Rx.group g 1))))
    tmpl

(** (action, package, start_line, lines) for every opam error block. *)
let error_blocks lines =
  let lines = Array.of_list lines in
  let n = Array.length lines in
  let out = ref [] in
  let i = ref 0 in
  while !i < n do
    match U.match_ error_block_re lines.(!i) with
    | None -> incr i
    | Some m ->
        let start = !i in
        let body = ref [] and len = ref 0 in
        incr i;
        while
          !i < n && (not (Rx.test_match block_end_re lines.(!i))) && !len < 2000
        do
          body := lines.(!i) :: !body;
          incr len;
          incr i
        done;
        out := (U.group m 1, U.group m 2, start, List.rev !body) :: !out
  done;
  List.rev !out

(** First rule (in order) matching any line wins: (rule, match, line index). *)
let apply_rules rules lines =
  let cleaned = List.map clean lines in
  List.find_map
    (fun rule ->
      List.find_mapi
        (fun idx l ->
          Option.map (fun m -> (rule, m, idx)) (U.search rule.pattern l))
        cleaned)
    rules

(** Every distinct specific (non-generic) build-rule signature found in these
    lines. *)
let all_specific_signatures ?(limit = 5) lines =
  let signature l =
    let l = clean l in
    Option.map
      (fun (rule, m) -> normalise (format rule.template m))
      (List.find_map
         (fun rule ->
           if rule.generic then None
           else Option.map (fun m -> (rule, m)) (U.search rule.pattern l))
         build_rules)
  in
  (* line by line, until [limit] signatures are found *)
  let rec scan seen = function
    | [] -> seen
    | l :: rest ->
        let seen =
          match signature l with
          | Some s when not (List.mem s seen) -> seen @ [ s ]
          | _ -> seen
        in
        if List.length seen >= limit then seen else scan seen rest
  in
  scan [] lines

let excerpt ?(before = 8) ?(after = 6) ?(width = 300) lines idx =
  let lo = max 0 (idx - before)
  and hi = min (List.length lines) (idx + after + 1) in
  String.concat "\n"
    (List.map
       (fun l -> Util.prefix_chars l width)
       (Util.sublist ~start:lo ~stop:hi lines))

let version_suffix_re = Rx.v {|\.v?\d[\w.~+-]*$|}
let pkg_name p = U.sub version_suffix_re "" p

(* ------------------------------------------------------------------------ *)

let job_type_hints =
  [
    ( "lower-bounds",
      "fails with the oldest dependency versions the opam file allows: a lower \
       bound is probably too loose" );
    ( "revdeps",
      "a package that depends on the PR's package fails: the PR may break \
       existing users" );
    ("tests", "fails when building/running the package's tests (with-test)");
  ]

(** Split a CI variant into (platform, job_type, pr_package, tested_package).

    day10,debian-13,x86_64,4.14,foo.1.0 -> build
    compilers,5.4,foo.1.0,lower-bounds -> lower-bounds
    extras,arm64-ocaml-5.5,foo.1.0,tests -> tests
    compilers,4.14,foo.1.0,revdeps,bar.2.0 -> revdeps (bar is built against foo)
*)
let parse_variant variant =
  let parts = String.split_on_char ',' variant in
  if List.length parts < 2 then (* (lint), (analysis) *)
    (variant, Util.strip ~chars:"()" variant, None, None)
  else begin
    let n = List.length parts in
    let job_type, revdep, parts =
      let last = List.nth parts (n - 1) in
      if last = "tests" || last = "lower-bounds" then
        (last, None, Util.take (n - 1) parts)
      else
        match List.find_index (( = ) "revdeps") parts with
        | Some i -> ("revdeps", List.nth_opt parts (i + 1), Util.take i parts)
        | None -> ("build", None, parts)
    in
    let k = List.length parts in
    let pr_package = List.nth parts (k - 1) in
    ( String.concat "," (Util.take (k - 1) parts),
      job_type,
      Some pr_package,
      Some (Option.value ~default:pr_package revdep) )
  end

let opt = Json.opt_string
let failed_re = Rx.v {|^.*?\(failed: ?|\)$|}

(* The lines naming the failing package (name.version, so that script text
   like ${x} never matches), when the log has no error block. *)
let failing_package_res =
  let pkg = {|([a-z][\w+-]*\.[\w.~+-]+?)|} in
  List.map (Rx.v ~flags:[ Rx.M ])
    [
      {|\[ERROR\] (?:The compilation of |The installation of )?|} ^ pkg
      ^ "(?: failed|:)";
      "Failed to get sources of " ^ pkg ^ ":";
      {|dependencies failing:\s+|} ^ pkg ^ {|\s*$|};
      "Package " ^ pkg ^ " depends on the unavailable";
    ]

let classify_job job log_text =
  let lines = Util.splitlines log_text in
  let variant = Dict.str job "variant" in
  let platform, job_type, pr_package, tested = parse_variant variant in
  let res =
    Dict.of_list
      [
        ("variant", `String variant);
        ("job_type", `String job_type);
        ("platform", `String platform);
        ("pr_package", opt pr_package);
        ("tested_package", opt tested);
        ("failing_package", `Null);
        ("relation", `Null);
        ("category", `Null);
        ("signature", `Null);
        ("rule", `Null);
        ("needs_llm", `Bool false);
        ("ci_label", Option.value ~default:`Null (Dict.get job "label"));
        ("log_file", Option.value ~default:`Null (Dict.get job "log_file"));
        ("excerpt", `String "");
      ]
  in
  let blocks = ref [] in
  let done_ ?(rule = "") ?(generic = false) category signature ex =
    Dict.update res
      [
        ("category", `String category);
        ("signature", `String (normalise signature));
        ("rule", `String rule);
        ("needs_llm", `Bool generic);
        ("excerpt", `String ex);
      ];
    (* Other specific errors in the same log (e.g. a missing header AND a missing OCaml library). *)
    let scope =
      if !blocks <> [] then List.concat_map (fun (_, _, _, b) -> b) !blocks
      else lines
    in
    let sg = Dict.str res "signature" in
    Dict.set res "also_seen"
      (Json.strings
         (List.filter (fun s -> s <> sg) (all_specific_signatures scope)));
    (match (Dict.str_opt res "failing_package", tested) with
    | Some f, Some t when f <> "" && t <> "" ->
        Dict.set_str res "relation"
          (if pkg_name f = pkg_name t then "direct" else "dependency")
    | _ -> ());
    res
  in
  let hit_done (rule, m, idx) ex_lines =
    done_ rule.category (format rule.template m) (excerpt ex_lines idx)
      ~rule:rule.category ~generic:rule.generic
  in
  (* Lint job: one group per distinct lint message. *)
  if variant = "(lint)" || String.starts_with ~prefix:"(lint" variant then begin
    let msgs = List.filter_map (fun l -> U.match_ lint_re l) lines in
    if msgs <> [] then begin
      let sg =
        String.concat "; "
          (List.sort_uniq compare
             (List.map (fun m -> normalise (U.group m 2)) msgs))
      in
      Dict.set_str res "failing_package"
        (String.concat ", "
           (List.sort_uniq compare (List.map (fun m -> U.group m 1) msgs)));
      done_ "lint" sg
        (String.concat "\n" (List.map U.group0 (Util.take 20 msgs)))
        ~rule:"lint"
    end
    else
      match apply_rules log_rules lines with
      | Some hit ->
          hit_done hit
            lines (* lint job that died before linting (e.g. CI fault) *)
      | None ->
          done_ "lint" "lint failed"
            (String.concat "\n" (Util.sublist ~start:(-30) lines))
            ~rule:"lint" ~generic:true
  end
  else begin
    blocks := error_blocks lines;
    (match !blocks with
    | (_, pkg, _, _) :: _ -> Dict.set_str res "failing_package" pkg
    | [] ->
        ignore
          (List.find_map
             (fun pat ->
               match U.search pat log_text with
               | Some m ->
                   Dict.set_str res "failing_package" (U.group m 1);
                   Some ()
               | None -> None)
             failing_package_res));
    (* 1. log-wide failures that happen outside a package build *)
    let hit = apply_rules log_rules lines in
    let hit =
      match hit with
      | Some (rule, _, _)
        when rule.category = "system_package_unavailable" && rule.generic -> (
          (* prefer the specific "still missing: X" line if it exists anywhere *)
          match
            apply_rules
              (List.filter
                 (fun r ->
                   r.category = "system_package_unavailable" && not r.generic)
                 log_rules)
              lines
          with
          | Some better -> Some better
          | None -> hit)
      | _ -> hit
    in
    match hit with
    | Some hit -> hit_done hit lines
    | None -> (
        (* 2. build errors: the error block of the failing package first, then the whole log *)
        let from_blocks =
          List.find_map
            (fun (_, pkg, _, body) ->
              match apply_rules build_rules body with
              | None -> None
              | Some ((rule, _, _) as h) ->
                  Dict.set_str res "failing_package" pkg;
                  if rule.generic then
                    (* a specific match elsewhere in the log beats a generic one in the block *)
                    match
                      apply_rules
                        (List.filter (fun r -> not r.generic) build_rules)
                        lines
                    with
                    | Some spec -> Some (hit_done spec lines)
                    | None -> Some (hit_done h body)
                  else Some (hit_done h body))
            !blocks
        in
        match from_blocks with
        | Some r -> r
        | None -> (
            match apply_rules build_rules lines with
            | Some hit -> hit_done hit lines
            | None ->
                (* 3. nothing matched: fall back to the CI's own one-line reason *)
                let reason =
                  match U.sub failed_re "" (Dict.str job "label") with
                  | "" -> "unknown"
                  | s -> s
                in
                let tail =
                  match !blocks with
                  | (_, _, _, b) :: _ -> Util.sublist ~start:(-40) b
                  | [] -> Util.sublist ~start:(-40) lines
                in
                done_ "unclassified" reason
                  (String.concat "\n"
                     (List.map (fun l -> Util.prefix_chars l 300) tail))
                  ~rule:"fallback" ~generic:true))
  end

let classify_run run_dir =
  let jobs =
    Json.to_list
      (Json.parse
         (In_channel.with_open_bin
            (Filename.concat run_dir "jobs.json")
            In_channel.input_all))
  in
  List.filter_map
    (fun job ->
      let job = Dict.of_json job in
      if
        Dict.str_opt job "status" <> Some "failed"
        || not (Dict.truthy job "log_file")
      then None
      else begin
        let log = Filename.concat run_dir (Dict.str job "log_file") in
        if not (Sys.file_exists log) then (
          Printf.eprintf "warning: missing log %s\n%!" log;
          None)
        else begin
          let r =
            classify_job job (In_channel.with_open_bin log In_channel.input_all)
          in
          Dict.set_str r "run" (Filename.basename run_dir);
          Some r
        end
      end)
    jobs

(* The number of occurrences of each element, in the order of first occurrence. *)
let counts l : Json.t =
  `Assoc
    (List.map
       (fun (k, v) -> (k, `Int (List.length v)))
       (Util.group_by Fun.id l))

let group results =
  let groups =
    Util.group_by
      (fun r ->
        (Dict.str r "job_type", Dict.str r "category", Dict.str r "signature"))
      results
  in
  let set k rs =
    Json.strings
      (List.sort_uniq compare
         (List.filter_map
            (fun r ->
              match Dict.str_opt r k with
              | Some s when s <> "" -> Some s
              | _ -> None)
            rs))
  in
  let out =
    List.map
      (fun ((job_type, cat, sg), rs) ->
        let rep = List.hd rs in
        `Assoc
          [
            ("job_type", `String job_type);
            ("category", `String cat);
            ("signature", `String sg);
            ("hint", opt (List.assoc_opt job_type job_type_hints));
            ("count", `Int (List.length rs));
            ("pr_packages", set "pr_package" rs);
            ( "needs_llm",
              `Bool
                (List.exists
                   (fun r -> Dict.get r "needs_llm" = Some (`Bool true))
                   rs) );
            ( "failing_packages",
              Json.strings
                (List.sort_uniq compare
                   (List.filter_map
                      (fun r ->
                        match Dict.str_opt r "failing_package" with
                        | Some s when s <> "" -> Some (pkg_name s)
                        | _ -> None)
                      rs)) );
            ("tested_packages", set "tested_package" rs);
            ( "relations",
              counts
                (List.map
                   (fun r ->
                     match Dict.str_opt r "relation" with
                     | Some s when s <> "" -> s
                     | _ -> "n/a")
                   rs) );
            ( "also_seen",
              counts (List.concat_map (fun r -> Dict.strs r "also_seen") rs) );
            ( "platforms",
              Json.strings
                (List.sort_uniq compare
                   (List.map (fun r -> Dict.str r "platform") rs)) );
            ( "runs",
              Json.strings
                (List.sort_uniq compare
                   (List.map (fun r -> Dict.str r "run") rs)) );
            ( "representative",
              `Assoc
                [
                  ("run", Option.value ~default:`Null (Dict.get rep "run"));
                  ( "log_file",
                    Option.value ~default:`Null (Dict.get rep "log_file") );
                  ( "variant",
                    Option.value ~default:`Null (Dict.get rep "variant") );
                ] );
            ("excerpt", Option.value ~default:`Null (Dict.get rep "excerpt"));
          ])
      groups
  in
  let key g =
    ( -Json.to_int (Json.member "count" g),
      Json.str "job_type" g,
      Json.str "category" g,
      Json.str "signature" g )
  in
  List.stable_sort (fun a b -> compare (key a) (key b)) out

let ljust w s =
  let n = Util.char_length s in
  if n >= w then s else s ^ String.make (w - n) ' '

let rjust w s =
  let n = Util.char_length s in
  if n >= w then s else String.make (w - n) ' ' ^ s

(* A dict of counts: "k=n, ...", "-" when empty. *)
let counts_text (v : Json.t) =
  match Json.to_assoc v with
  | [] -> "-"
  | kv ->
      String.concat ", "
        (List.map (fun (k, n) -> Printf.sprintf "%s=%d" k (Json.to_int n)) kv)

let print_report title results groups verbose =
  Printf.printf "\n=== %s\n" title;
  if results = [] then print_endline "  no failed jobs"
  else begin
    let by_cat =
      Util.counter (List.map (fun r -> Dict.str r "category") results)
    in
    let by_type =
      Util.counter (List.map (fun r -> Dict.str r "job_type") results)
    in
    let rule_hits =
      List.length
        (List.filter
           (fun r -> not (Dict.get r "needs_llm" = Some (`Bool true)))
           results)
    in
    Printf.printf
      "  %d failed jobs -> %d groups; %d/%d jobs classified by a specific \
       rule, %d group(s) need an LLM\n"
      (List.length results) (List.length groups) rule_hits (List.length results)
      (List.length (List.filter (fun g -> Json.bool "needs_llm" g) groups));
    let show l =
      String.concat ", " (List.map (fun (c, n) -> Printf.sprintf "%s=%d" c n) l)
    in
    print_endline ("  by job type: " ^ show by_type);
    print_endline ("  by category: " ^ show by_cat);
    List.iter
      (fun g ->
        let flag = if Json.bool "needs_llm" g then "  [LLM]" else "" in
        let fp = Json.to_str_list (Json.member "failing_packages" g) in
        let pk =
          String.concat ", " (Util.take 4 fp)
          ^ if List.length fp > 4 then " ..." else ""
        in
        Printf.printf "  [%s] %s %s %s%s\n"
          (rjust 3 (string_of_int (Json.to_int (Json.member "count" g))))
          (ljust 12 (Json.str "job_type" g))
          (ljust 27 (Json.str "category" g))
          (Util.prefix_chars (Json.str "signature" g) 100)
          flag;
        (match Json.str_opt "hint" g with
        | Some h when h <> "" -> Printf.printf "        hint: %s\n" h
        | _ -> ());
        let also = Json.assoc "also_seen" g in
        if also <> [] then
          print_endline
            ("        also seen: "
            ^ Util.prefix_chars
                (String.concat "; "
                   (List.map
                      (fun (s, n) -> Printf.sprintf "%s (%d)" s (Json.to_int n))
                      also))
                150);
        if verbose then
          Printf.printf
            "        failing: %s | relation: %s | platforms: %d | e.g. %s\n"
            (if pk = "" then "-" else pk)
            (counts_text (Json.member "relations" g))
            (List.length (Json.list "platforms" g))
            (match Json.member "log_file" (Json.member "representative" g) with
            | `String s -> s
            | _ -> "-"))
      groups
  end

let write path text =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

(* pathlib's normal form of a path (no trailing or doubled slashes, no "." parts). *)
let norm_path p =
  let abs = String.length p > 0 && p.[0] = '/' in
  let parts =
    List.filter (fun x -> x <> "" && x <> ".") (String.split_on_char '/' p)
  in
  match (if abs then "/" else "") ^ String.concat "/" parts with
  | "" -> "."
  | s -> s

let run ~runs ~logs_root ~json ~combined ~verbose () =
  let runs =
    if runs <> [] then List.map norm_path runs
    else
      List.filter_map
        (fun d ->
          let p = Filename.concat (norm_path logs_root) d in
          if Sys.file_exists (Filename.concat p "jobs.json") then Some p
          else None)
        (Util.listdir logs_root)
  in
  if runs = [] then
    Util.die
      (Printf.sprintf
         "no runs found (looked for %s/*/jobs.json); run fetch-ci-logs first"
         logs_root);
  let all_results = ref [] in
  List.iter
    (fun run ->
      if not (Sys.file_exists (Filename.concat run "jobs.json")) then
        Printf.eprintf "skipping %s: no jobs.json\n%!" run
      else begin
        let results = classify_run run in
        let groups = group results in
        write
          (Filename.concat run "classification.json")
          (Json.dumps ~indent:2 (`List (List.map Dict.to_json results)) ^ "\n");
        write
          (Filename.concat run "groups.json")
          (Json.dumps ~indent:2 (`List groups) ^ "\n");
        all_results := !all_results @ results;
        if not combined then
          print_report (Filename.basename run) results groups verbose
      end)
    runs;
  let all_groups = group !all_results in
  if combined then
    print_report
      (Printf.sprintf "all runs (%d)" (List.length runs))
      !all_results all_groups verbose;
  (match json with
  | Some path ->
      write path
        (Json.dumps ~indent:2
           (`Assoc
              [
                ("jobs", `List (List.map Dict.to_json !all_results));
                ("groups", `List all_groups);
              ])
        ^ "\n");
      Printf.printf "\ncombined report written to %s\n" path
  | None -> ());
  print_endline
    "\nper-run classification.json and groups.json written next to the logs"

let () =
  let open Cmdliner in
  let runs =
    Arg.(
      value & pos_all string []
      & info [] ~docv:"RUNS"
          ~doc:"the run directories (default: every run under the logs root)")
  in
  let logs_root =
    Arg.(
      value & opt string "ci-logs"
      & info [ "logs-root" ] ~docv:"LOGS_ROOT"
          ~doc:"where fetch-ci-logs put the logs")
  in
  let json =
    Arg.(
      value
      & opt (some string) None
      & info [ "json" ] ~docv:"JSON" ~doc:"write the results to this file")
  in
  let combined =
    Arg.(
      value & flag
      & info [ "combined" ]
          ~doc:"one report for all the runs, instead of one per run")
  in
  let verbose =
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc:"more detail")
  in
  let term =
    Term.(
      const (fun runs logs_root json combined verbose ->
          run ~runs ~logs_root ~json ~combined ~verbose ())
      $ runs $ logs_root $ json $ combined $ verbose)
  in
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "classify-logs"
             ~doc:"Classify and group the logs fetched by fetch-ci-logs.")
          term))
