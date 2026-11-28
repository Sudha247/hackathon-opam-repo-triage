open Suggestion.T
open Ci

let rule_error (job : job) e jobs =
  Suggestion.make
    ([
       `Variant job.variant;
       `Url job.url;
       `Kind job.kind;
       `Experimental job.experimental;
       `Failing (failing_pkg job);
       `Rule Rule.Rule_error;
     ]
    @ Where.fix_where Unknown "unknown"
    @ [
        `Confidence "low";
        `Error
          (Printf.sprintf "%s: %s" (Printexc.exn_slot_name e)
             (Util.prefix_chars (Util.exn_text e) 300));
        `Note
          "The tool failed while analysing this log, which is a bug in the \
           tool. Read the log.";
        `Key (failure_key job);
        `Jobs jobs;
        `Keep;
      ])

let warning_error_re = Rx.v {|\b(warning|error)\s+(\d+):|}

(** The warning and error codes [opam lint] reports for an opam file, or None
    without opam. *)
let lint_codes text =
  match Proc.which "opam" with
  | None -> None
  | Some _ ->
      let d = Filename.temp_dir "opam-lint" "" in
      Fun.protect ~finally:(fun () -> Util.rmtree d) @@ fun () ->
      let path = Filename.concat d "opam" in
      let oc = open_out_bin path in
      output_string oc text;
      close_out oc;
      let p =
        try Proc.run [ "opam"; "lint"; "--strict"; path ]
        with _ -> { Proc.code = 1; stdout = ""; stderr = "" }
      in
      Some
        (List.sort_uniq compare
           (List.map
              (function [ k; n ] -> k ^ " " ^ n | _ -> "")
              (Rx.findall_groups warning_error_re (p.stdout ^ p.stderr))))

let first_part r =
  String.trim
    (List.hd
       (Util.split_on ~maxsplit:1 "   "
          (Suggestion.str_field r.Suggestion.new_)))

let quoted_name_re = Rx.v {|"[\w+-]+"|}
let depends_re = Rx.v ~flags:[ Rx.M ] {|^(depends:\s*\[)\s*$|}

let applied text r =
  let new_ = first_part r in
  match Suggestion.get r.old with
  | Some old when old <> "" && Util.contains text old ->
      Some (Util.replace ~count:1 text old new_)
  | _ ->
      if
        Rx.test_match quoted_name_re new_
        && Util.contains
             (Suggestion.str_field r.Suggestion.new_)
             "new line in depends"
      then
        Some
          (Rx.subf ~count:1 depends_re
             (fun m -> Rx.group m 1 ^ "\n  " ^ new_)
             text)
      else None

let dependency_filter_re = Rx.v {|\s*"([\w+-]+)"\s*\{([^}]*)\}|}

let check_changes results src sha pr_packages =
  List.iter
    (fun r ->
      let new_ = Suggestion.str_field r.Suggestion.new_ in
      let problem =
        match Rx.match_ dependency_filter_re new_ with
        | Some m when not (String.contains (Rx.group m 2) '|') -> (
            let dep = Rx.group m 1
            and terms = Constraints.version_terms (Rx.group m 2) in
            let have =
              Option.bind r.Suggestion.installed (List.assoc_opt dep)
            in
            if terms <> [] && Constraints.unsatisfiable terms then
              Some (Printf.sprintf "no version of %s satisfies it" dep)
            else
              match have with
              | Some have
                when have <> "" && terms <> []
                     && (not (Util.contains (Rx.group m 2) "with-test"))
                     && Constraints.allows have terms
                     && Titles.has_rule
                          [ Library_api; Previous_release_bound; Lower_bound ]
                          r ->
                  Some
                    (Printf.sprintf
                       "it still allows %s.%s, which the failing job installed"
                       dep have)
              | _ -> None)
        | _ -> None
      in
      let failing = Suggestion.str_field r.Suggestion.failing in
      let name, ver = split_pkg failing in
      let problem =
        if
          problem = None && new_ <> "" && ver <> ""
          && (not (String.contains failing ' '))
          && Proc.which "opam" <> None
        then
          let text =
            match
              if List.mem failing pr_packages then
                Sources.opam_file ~ref:sha src name ver
              else None
            with
            | Some t when t <> "" -> Some t
            | _ -> Sources.opam_file src name ver
          in
          match text with
          | Some text when text <> "" -> (
              match applied text r with
              | Some after -> (
                  match (lint_codes text, lint_codes after) with
                  | Some before_codes, Some after_codes ->
                      let added =
                        List.filter
                          (fun c -> not (List.mem c before_codes))
                          after_codes
                      in
                      if added <> [] then
                        Some
                          (Printf.sprintf "opam lint reports %s on the result"
                             (String.concat ", " added))
                      else None
                  | _ -> None)
              | None -> None)
          | _ -> None
        else problem
      in
      match problem with
      | None -> ()
      | Some problem ->
          let change = first_part r in
          r.note <-
            Some
              (String.trim
                 (Util.rstrip (Suggestion.str r.note)
                 ^ Printf.sprintf
                     " Possible change, not shown as a diff because %s: `%s`."
                     problem change));
          r.new_ <- Absent;
          r.old <- Absent;
          r.alternative <- None;
          r.confidence <- Set "low";
          r.Suggestion.checked <- Some problem)
    results

(* Rules whose entries for several versions of one package are merged. *)
let merge_versions_rules : Rule.t list =
  [
    Install_missing_files;
    Uninstallable;
    Newer_c_compiler;
    System_library_version;
    Needs_git_checkout;
    Install_outside_prefix;
    Test_output_ocaml;
    Missing_header;
    Missing_tool;
    Missing_library;
    Missing_pkg_config;
    Sources;
    Conf_no_depext;
    Ci_failure;
  ]

let jobs_or_variant r =
  match r.Suggestion.jobs with
  | Some (_ :: _ as l) -> l
  | _ -> [ Suggestion.str r.Suggestion.variant ]

(* [s] with each name of [names] as a whole word (not in "cygwinports")
   replaced by [by]. *)
let replace_names names by s =
  let word c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  let names =
    List.sort (fun a b -> compare (String.length b) (String.length a)) names
  in
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i < n then
      match
        List.find_opt
          (fun x ->
            let k = String.length x in
            i + k <= n
            && String.sub s i k = x
            && (i = 0 || not (word s.[i - 1]))
            && (i + k = n || not (word s.[i + k])))
          names
      with
      | Some x ->
          Buffer.add_string b by;
          go (i + String.length x)
      | None ->
          Buffer.add_char b s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* A note without what [Suggest] put before the rule's: whether the failure
   is pre-existing or new on master (it differs from job to job, not from
   platform to platform). *)
let diagnosis_note (r : Suggestion.t) =
  let strip prefix n =
    if String.starts_with ~prefix n then
      Util.slice ~start:(String.length prefix) n
    else n
  in
  let n = Suggestion.str r.note in
  let n =
    match r.preexisting with
    | Some p -> strip (Printf.sprintf "Pre-existing: %s. " p) n
    | None -> n
  in
  match r.new_on_master with Some h -> strip (h ^ " ") n | None -> n

let merge_versions ?(plats = fun _ -> []) results =
  let first = Hashtbl.create 8 in
  let extra : (Suggestion.t * (string option * string * string list)) list ref =
    ref []
  in
  (* One version on several platforms whose texts differ only by the
     platform's name (conf-rust-2018 "does not provide `cargo` on cygwin" and
     "on msys2", #30751): one entry naming them all. Its error, note and
     advice with the names replaced by "{P}", and the names. *)
  let templates :
      (Suggestion.t
      * ((string option * string option * string option) * string list ref))
      list
      ref =
    ref []
  in
  let named r =
    let texts =
      List.filter_map Fun.id [ r.Suggestion.error; r.note; r.advice ]
    in
    List.filter
      (fun n -> List.exists (fun t -> replace_names [ n ] "" t <> t) texts)
      (Util.dedup (List.concat_map plats (jobs_or_variant r)))
  in
  let template names (r : Suggestion.t) =
    let t = Option.map (replace_names names "{P}") in
    (t r.error, t (Some (diagnosis_note r)), t r.advice)
  in
  (* For each merged entry: its first note, its first package, the others whose note differs. *)
  let out =
    List.filter
      (fun r ->
        let failing = Suggestion.str_field r.Suggestion.failing in
        let name = fst (split_pkg failing) in
        if
          (not (Titles.has_rule merge_versions_rules r))
          || Suggestion.is_set r.Suggestion.new_
          || name = ""
          || String.contains failing ' '
        then true
        else begin
          (* CI failures of different kinds (their notes) stay apart. *)
          let k =
            ( name,
              r.rule,
              Where.where_kind r,
              if r.rule = Some Ci_failure then r.note else None )
          in
          match Hashtbl.find_opt first k with
          | None ->
              Hashtbl.replace first k r;
              true
          | Some f
            when List.mem failing
                   (Util.split_on ", "
                      (Suggestion.str_field f.Suggestion.failing))
                 && Suggestion.nonempty r.note
                 && diagnosis_note r <> diagnosis_note f ->
              (* The same version, another note: from another platform. *)
              let tf, names =
                match List.assq_opt f !templates with
                | Some t -> t
                | None ->
                    let n = named f in
                    (template n f, ref n)
              in
              let nr = named r in
              if !names <> [] && nr <> [] && template nr r = tf then begin
                if not (List.mem_assq f !templates) then
                  templates := (f, (tf, names)) :: !templates;
                names := Util.dedup (!names @ nr);
                f.Suggestion.jobs <-
                  Some (merge_jobs (jobs_or_variant f) (jobs_or_variant r));
                false
              end
              else true (* a diagnosis of its own: not dropped *)
          | Some f ->
              if
                not
                  (List.mem failing
                     (Util.split_on ", "
                        (Suggestion.str_field f.Suggestion.failing)))
              then
                f.Suggestion.failing <-
                  Set
                    (Suggestion.str_field f.Suggestion.failing ^ ", " ^ failing);
              f.Suggestion.jobs <-
                Some (merge_jobs (jobs_or_variant f) (jobs_or_variant r));
              if Where.where_kind r = Other_package then
                Suggestion.update f (Where.pr_or_other name false);
              (let note0 =
                 match List.assq_opt f !extra with
                 | Some (n, _, _) -> n
                 | None -> f.note
               in
               if Suggestion.nonempty r.note && r.note <> note0 then begin
                 let n0, first_pkg, similar =
                   match List.assq_opt f !extra with
                   | Some e -> e
                   | None ->
                       ( note0,
                         List.hd
                           (Util.split_on ", "
                              (Suggestion.str_field f.Suggestion.failing)),
                         [] )
                 in
                 extra :=
                   (f, (n0, first_pkg, similar @ [ failing ]))
                   :: List.filter (fun (g, _) -> g != f) !extra
               end);
              false
        end)
      results
  in
  List.iter
    (fun r ->
      match List.assq_opt r !extra with
      | Some (_, first_pkg, similar) ->
          let similar =
            List.filter (fun x -> x <> first_pkg) (Util.dedup similar)
          in
          if similar <> [] then
            r.note <-
              Some
                (Util.rstrip (Suggestion.str r.note)
                ^ Printf.sprintf " This note is for %s; it differs for %s."
                    first_pkg
                    (String.concat ", " similar))
      | None -> ())
    out;
  List.iter
    (fun (r, ((error, note, advice), names)) ->
      let names = List.sort compare !names in
      if List.length names > 1 then begin
        let fill =
          Option.map (fun t ->
              let t =
                Util.replace t {|"{P}"|}
                  (String.concat " " (List.map (Printf.sprintf "%S") names))
              in
              let t =
                Util.replace
                  (Util.replace t "that platform has" "these platforms have")
                  "that platform" "these platforms"
              in
              Util.replace t "{P}" (String.concat ", " names))
        in
        let n = Suggestion.str r.note in
        let prefix =
          String.sub n 0 (String.length n - String.length (diagnosis_note r))
        in
        r.Suggestion.error <- fill error;
        r.note <- Option.map (fun t -> prefix ^ t) (fill note);
        r.advice <- fill advice
      end)
    !templates;
  out

let merge_ci_failures results =
  let first = Hashtbl.create 4 in
  List.filter
    (fun r ->
      if r.rule <> Some Ci_failure then true
      else
        let k = (r.Suggestion.error, r.note) in
        match Hashtbl.find_opt first k with
        | None ->
            Hashtbl.replace first k r;
            true
        | Some f ->
            let names = Util.split_on ", " (Suggestion.str_field f.failing) in
            let more =
              List.filter
                (fun x -> x <> "" && not (List.mem x names))
                (Util.split_on ", " (Suggestion.str_field r.failing))
            in
            if more <> [] then
              f.failing <- Set (String.concat ", " (names @ more));
            f.jobs <- Some (merge_jobs (jobs_or_variant f) (jobs_or_variant r));
            if r.experimental <> Some true then f.experimental <- r.experimental;
            false)
    results

let merge_unrecognised results =
  let unk =
    List.filter
      (fun r ->
        not
          (r.rule <> None
          || Suggestion.is_set r.Suggestion.new_
          || Suggestion.list r.candidates <> []
          || Suggestion.nonempty r.error
          || r.llm_prompt <> None))
      results
  in
  if unk = [] then results
  else begin
    let jobs = merge_jobs [] (List.concat_map jobs_or_variant unk) in
    let names =
      List.sort_uniq compare
        (List.filter_map
           (fun r ->
             match Suggestion.get r.Suggestion.failing with
             | Some f when f <> "" -> Some f
             | _ -> None)
           unk)
    in
    let n = List.length jobs in
    List.filter (fun r -> not (List.memq r unk)) results
    @ [
        Suggestion.make
          ([ `Variant (Words.counted n "job"); `Rule Rule.Unrecognised ]
          @ Where.fix_where Unknown "unknown"
          @ [
              `Confidence "low";
              `Failing
                (if names = [] then None
                 else
                   Some
                     (String.concat ", " (Util.take 6 names)
                     ^ if List.length names > 6 then " ..." else ""));
              `Error (Words.counted n "failed job" ^ " the tool cannot explain");
              `Note (if n = 1 then "Read its log." else "Read their logs.");
              `Jobs jobs;
              `Experimental
                (List.for_all
                   (fun r -> r.Suggestion.experimental = Some true)
                   unk);
            ]);
      ]
  end
