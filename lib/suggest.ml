open Suggestion.T
open Ci

let s x : Json.t = `String x

type llm_opts = { limit : int; model : string; dry_run : bool }
type group_key = Dep of string option * string option | Key of failure_key

(** One job per (package, revdep, kind, reason): the same error on many
    platforms is one problem. A dependency failure is the dependency's: one log
    per (cause, platform). *)
let group_key j =
  if j.category = "dependency" then Dep (j.reason, (job_dims j).distro)
  else Key (failure_key j)

let matches match_ variant =
  match match_ with None | Some "" -> true | Some m -> Rx.test m variant

let distinct_failures jobs match_ =
  let cats = [ "build"; "build-or-test"; "solver"; "dependency" ] in
  let sel =
    List.filter
      (fun j ->
        j.status = "failed" && List.mem j.category cats
        && matches match_ j.variant)
      jobs
  in
  let sorted =
    Util.sort_by
      (fun j ->
        ( j.experimental,
          (match j.revdep with Some r -> r <> "" | None -> false),
          match j.platform with "day10" :: _ -> true | _ -> false ))
      sel
  in
  List.map snd (Util.group_by group_key sorted)

let drop_health_explained_elsewhere (notes : Context.notes) results pr_packages
    =
  let names = List.map (fun p -> fst (split_pkg p)) pr_packages in
  let hits =
    List.filter
      (fun r ->
        Suggestion.str r.rule = "library-api"
        && Suggestion.nonempty_field r.Suggestion.new_
        && Suggestion.nonempty r.dep
        && not (List.mem (Suggestion.str r.dep) names))
      results
  in
  let variants =
    List.concat_map
      (fun r -> List.filter (fun v -> v <> "") (Checks.jobs_or_variant r))
      hits
  in
  let whos = Hashtbl.fold (fun k _ acc -> k :: acc) notes.good [] in
  List.iter
    (fun who ->
      let kept =
        Context.PS.filter
          (fun (_, v) -> not (List.mem v variants))
          (Hashtbl.find notes.good who)
      in
      if Context.PS.is_empty kept then begin
        Hashtbl.remove notes.good who;
        Hashtbl.remove notes.via who
      end
      else Hashtbl.replace notes.good who kept)
    whos;
  hits

(* Rules whose explanation does not depend on the platform: reported once. *)
let per_package_rules = Titles.per_package_rules

let keys_of (r : Suggestion.t) =
  match r.keys with _ :: _ as l -> l | [] -> Option.to_list r.key

(* Test seams. *)
let fetch_hook = ref (fun url cache_dir -> Net.fetch url cache_dir)
let suggest_for_job_hook = ref Rules.suggest_for_job

let suggest sha (data : data) src cache_dir max_logs match_ llm =
  let pr_packages =
    List.sort_uniq compare
      (List.filter_map
         (fun j ->
           match j.target with Some t when t <> "" -> Some t | _ -> None)
         data.jobs)
  in
  let groups = distinct_failures data.jobs match_ in
  if List.length groups > max_logs then
    Printf.eprintf
      "note: %d distinct failures, analysing the first %d (use --max)\n%!"
      (List.length groups) max_logs;
  (* [merged] keeps every job behind each analysed one, so the report can say how many it stands for. *)
  let analysed = Util.take max_logs groups in
  let uniq = List.map List.hd analysed in
  let merged =
    List.map
      (fun g -> (group_key (List.hd g), List.map (fun j -> j.variant) g))
      analysed
  in
  let merged_of j = List.assoc (group_key j) merged in
  let dropped = Util.drop max_logs groups in
  (* to count the failures no suggestion explains *)
  let results = ref [] and base = ref None in
  let prev_cache = Hashtbl.create 4 in
  let add r = results := !results @ [ r ] in
  let jobs_by_variant = Hashtbl.create 64 in
  List.iter (fun x -> Hashtbl.replace jobs_by_variant x.variant x) data.jobs;
  List.iter
    (fun j ->
      match log_text (!fetch_hook j.url cache_dir) with
      | exception e ->
          add
            (Suggestion.make
               [
                 `Variant j.variant;
                 `Note ("Could not read the log: " ^ Util.exn_text e);
                 `Jobs (merged_of j);
               ])
      | text -> (
          if !base = None then base := List.assoc_opt "base" (excerpt text);
          (* the master commit the PR was merged with *)
          match
            let r = !suggest_for_job_hook src j text pr_packages sha in
            if Suggestion.str r.rule = "test-output" then
              Rules.complete_test_output src r j text data cache_dir;
            let lb = Rules.lower_bounds_rule src j text r pr_packages sha in
            let info = Loginfo.log_info text in
            r.installed <- Some info.installed;
            let pb =
              Rules.previous_release_bound src j info pr_packages sha prev_cache
            in
            (r, lb, pb)
          with
          | exception ((Out_of_memory | Stack_overflow) as e) -> raise e
          | exception e ->
              add (Checks.rule_error j e (merged_of j))
              (* a bug in a rule: this job only, the others are still analysed *)
          | r, lb, pb -> (
              let key = failure_key j in
              (match pb with
              | Some pb
                when not
                       (List.exists
                          (fun x ->
                            Suggestion.get x.Suggestion.failing
                            = Suggestion.get pb.failing
                            && Suggestion.get x.new_ = Suggestion.get pb.new_)
                          (!results @ [ r ])) ->
                  Suggestion.update pb [ `Key key; `Jobs (merged_of j); `Keep ];
                  add pb
              | _ -> ());
              Suggestion.update r [ `Key key; `Jobs (merged_of j) ];
              if r.depexts <> None then
                Rules.conf_depext_platforms r jobs_by_variant;
              add r;
              match lb with
              | Some lb ->
                  Suggestion.update lb [ `Key key; `Jobs (merged_of j) ];
                  add lb
              | None -> ())))
    uniq;
  let results = !results in
  let failing_in_log =
    List.filter_map
      (fun r ->
        match (r.Suggestion.key, Suggestion.get r.Suggestion.failing) with
        | Some k, Some f when f <> "" -> Some (k, f)
        | _ -> None)
      results
  in
  (* Jobs that failed because of the CI (tool crash, network) say nothing
     about the package: leave them out of the pass/fail patterns. *)
  let transient =
    List.concat_map
      (fun r -> if Suggestion.str r.rule = "transient" then keys_of r else [])
      results
  in
  let jobs =
    List.filter
      (fun j -> not (j.status = "failed" && List.mem (failure_key j) transient))
      data.jobs
  in
  let matrix =
    Matrix.collapse_systemic
      (Matrix.matrix_rules ~failing_in_log jobs results match_ pr_packages)
  in
  (* A matrix suggestion explains the per-job failures that had no rule. *)
  let explained = List.concat_map (fun m -> m.Suggestion.keys) matrix in
  (* Distinct failures beyond [max_logs] that no matrix suggestion covers:
     nothing says why they fail (the JSON output and the page say how many). *)
  data.unanalysed <-
    List.length
      (List.filter
         (fun js ->
           List.exists (fun j -> not (List.mem (failure_key j) explained)) js)
         dropped);
  let results =
    List.filter
      (fun r ->
        r.Suggestion.keep
        || not
             ((match r.key with
                | Some k -> List.mem k explained
                | None -> false)
             && (not (Suggestion.nonempty_field r.Suggestion.new_))
             && Suggestion.list r.candidates = []))
      results
  in
  if data.unfinished > 0 then
    List.iter
      (fun m ->
        m.note <-
          Some
            ("CI is still running: this pattern comes from the jobs finished \
              so far and may change. " ^ Suggestion.str m.note))
      matrix;
  let results = results @ matrix in
  Context.accepted_but_failing results src sha pr_packages;
  let new_notes = Context.new_notes () in
  let pre =
    Context.preexisting_failures ~src ~notes:new_notes data sha !base cache_dir
  in
  (match new_notes.notes_health_stale with
  | Some d when d <> "" ->
      data.health_stale <- Some (`String d)
      (* reported in the JSON output and on the page *)
  | _ -> ());
  let elsewhere =
    drop_health_explained_elsewhere new_notes results pr_packages
  in
  List.iter
    (fun r ->
      let keys = keys_of r in
      let hints =
        List.sort_uniq compare
          (List.filter_map (fun k -> Hashtbl.find_opt new_notes.new_ k) keys)
      in
      let note_after x =
        x ^ match r.note with Some n when n <> "" -> " " ^ n | _ -> ""
      in
      (match hints with
      | h :: _
        when (not (List.exists (fun k -> List.mem_assoc k pre) keys))
             && not (List.memq r elsewhere) ->
          r.new_on_master <- Some h;
          r.note <- Some (note_after h)
      | _ -> ());
      let why = List.map (fun k -> List.assoc_opt k pre) keys in
      if keys <> [] && not (List.mem None why) then begin
        let why = List.sort_uniq compare (List.map Option.get why) in
        let p =
          if
            List.length why = 1
            || not
                 (List.for_all
                    (fun w -> String.starts_with ~prefix:"this PR only adds" w)
                    why)
          then List.hd why
          else
            "this PR only adds upper bounds to these packages, so their \
             dependencies are the same as on master"
        in
        r.preexisting <- Some p;
        r.note <- Some (note_after (Printf.sprintf "Pre-existing: %s." p));
        if List.exists (fun k -> List.mem k new_notes.unverified) keys then
          r.preexisting_unverified <-
            Some true (* not counted as happening without the PR *)
      end)
    results;
  let results = results @ Context.health_summary ~src new_notes in
  List.iter
    (fun (r : Suggestion.t) ->
      r.key <- None;
      r.keys <- [];
      r.distros <- [];
      r.keep <- false;
      r.depexts <- None;
      r.check <- None)
    results;
  (match llm with
  | Some l when data.unfinished > 0 && not l.dry_run ->
      (* The prompt shows the job's results on the other platforms, which
        change as jobs finish: each analysis would pay again (the cache key
        is the prompt). Asked once CI has finished. *)
      let n =
        List.length
          (Util.dedup (List.map Llm.llm_sig (Llm.llm_todo results data)))
      in
      data.llm_unanswered <- Some n;
      if n > 0 then
        prerr_endline
          "note: CI is still running: the LLM is not asked until it finishes"
  | Some l ->
      data.sha <- Some sha;
      data.llm_unanswered <-
        Some
          (Llm.apply_llm results data src cache_dir pr_packages l.limit l.model
             l.dry_run);
      List.iter
        (fun r ->
          if Suggestion.str r.rule = "llm" then begin
            (* The answer's place comes as [fix_in] (see where.ml): the rule's where_kind no longer holds. *)
            r.where_kind <- None;
            r.where_pkg <- Absent
          end)
        results
  | None -> ());

  (* Old releases failing on newer platforms: maintainers mark them
     [x-maintained: false] so that the next archiving round removes them. *)
  List.iter
    (fun r ->
      if
        List.mem (Suggestion.str r.rule)
          [
            "platform-matrix";
            "compiler-matrix";
            "missing-system-package";
            "missing-header";
            "newer-c-compiler";
          ]
      then begin
        let name, ver = split_pkg (Suggestion.str_field r.Suggestion.failing) in
        let vs =
          if name <> "" && ver <> "" then Sources.versions_or_empty src name
          else []
        in
        match Util.last_opt vs with
        | Some latest
          when List.mem ver vs && latest <> ver
               && not (Suggestion.nonempty r.alternative) ->
            r.alternative <-
              Some
                (Printf.sprintf
                   "x-maintained: false  (old release; %s.%s exists, so the \
                    next archiving round can remove this one)"
                   name latest)
        | _ -> ()
      end)
    results;

  (* The same suggestion found on several platforms or through several
     packages is reported once. *)
  let seen = Hashtbl.create 16 in
  let plats_of : (Suggestion.t * (string list ref * string option)) list ref =
    ref []
  in
  let unique =
    List.filter
      (fun r ->
        let plat = r.Suggestion.plat in
        r.plat <- None;
        r.unexplained <- false (* set by the rules, read by apply_llm *);
        let failing = Json.opt_string (Suggestion.get r.Suggestion.failing) in
        let k, tmpl =
          if Suggestion.nonempty_field r.Suggestion.new_ then
            ( Some
                (`List
                   [
                     `String "new";
                     failing;
                     s
                       (List.hd
                          (Util.split_on "   "
                             (Suggestion.str_field r.Suggestion.new_)));
                   ]),
              None )
          else if Suggestion.nonempty r.advice then
            ( Some
                (`List
                   [ `String "advice"; failing; `String (Option.get r.advice) ]),
              None )
          else if List.mem (Suggestion.str r.rule) per_package_rules then
            let t =
              Util.replace (Suggestion.str r.note)
                (match plat with Some p when p <> "" -> p | _ -> "\000")
                "{P}"
            in
            ( Some
                (`List
                   [ `String "rule"; failing; s (Suggestion.str r.rule); s t ]),
              Some t )
          else (None, None)
        in
        match k with
        | Some k when Hashtbl.mem seen k ->
            let first = Hashtbl.find seen k in
            first.Suggestion.jobs <-
              Some
                (merge_jobs
                   (Checks.jobs_or_variant first)
                   (Checks.jobs_or_variant r));
            (match plat with
            | Some plat when plat <> "" ->
                let plats =
                  match List.assq_opt first !plats_of with
                  | Some (p, _) -> p
                  | None ->
                      let p = ref [] in
                      plats_of := (first, (p, None)) :: !plats_of;
                      p
                in
                if not (List.mem plat !plats) then begin
                  plats := !plats @ [ plat ];
                  let rerr = Suggestion.str r.error
                  and ferr = Suggestion.str first.error in
                  let head, _, tail = Util.partition rerr "; " in
                  if not (Util.contains ferr rerr) then
                    first.error <-
                      Some
                        (ferr
                        ^
                        if
                          tail <> ""
                          && String.starts_with ~prefix:(head ^ "; ") ferr
                        then ", " ^ tail
                        else "; " ^ rerr)
                end
            | _ -> ());
            first.also_seen_on <-
              Some
                (Suggestion.list first.also_seen_on
                @ [ Suggestion.str r.Suggestion.variant ]);
            false
        | _ ->
            (match (k, plat) with
            | Some _, Some p when p <> "" ->
                plats_of := (r, (ref [ p ], tmpl)) :: !plats_of
            | _ -> ());
            (match k with Some k -> Hashtbl.replace seen k r | None -> ());
            true)
      results
  in
  List.iter
    (fun r ->
      match List.assq_opt r !plats_of with
      | Some (plats, Some tmpl) when List.length !plats > 1 ->
          r.note <- Some (Util.replace tmpl "{P}" (String.concat ", " !plats))
      | _ -> ())
    unique;
  let results = unique in
  let relaxed =
    Context.relaxed_available_rules data sha !base cache_dir pr_packages
  in
  if relaxed <> [] then begin
    let cause =
      String.concat ", "
        (List.map (fun r -> Suggestion.str_field r.Suggestion.failing) relaxed)
    in
    List.iter
      (fun r ->
        if
          String.starts_with ~prefix:"platform-matrix" (Suggestion.str r.rule)
          && Rx.test "Windows|freebsd|macos" (Suggestion.str r.error)
        then
          r.note <-
            Some
              (Printf.sprintf
                 "CI tries these platforms only because this PR relaxed \
                  `available` of %s (see that suggestion). %s"
                 cause (Suggestion.str r.note)))
      results
  end;
  let results = results @ relaxed in
  let results = Context.rerun_rules ?match_ data results in
  let results = results @ Context.sources_rules ?match_ data cache_dir in
  let results =
    if matches match_ "(lint)" then
      results @ Context.lint_rules ~src ~sha ~cache_dir data
    else results
  in
  let results = results @ Context.analysis_rules ?match_ data cache_dir in
  let results =
    results
    @ Context.skipped_rules ~src
        ~bounds:(Context.pr_added_bounds !base sha cache_dir)
        data cache_dir pr_packages
  in
  Checks.check_changes results src sha pr_packages;
  Context.accepted_elsewhere results src pr_packages;
  List.iter (fun (r : Suggestion.t) -> r.installed <- None) results;
  Titles.readable (Checks.merge_versions (Checks.merge_unrecognised results))
