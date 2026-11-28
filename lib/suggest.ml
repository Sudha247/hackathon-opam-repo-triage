open Suggestion.T
open Ci

let s x : Json.t = `String x

type llm_opts = { limit : int; model : string; dry_run : bool }
type group_key = Dep of string option * string option | Key of failure_key

(** One job per (package, revdep, kind, reason): the same error on many
    platforms is one problem. A dependency failure is the dependency's: one log
    per (cause, platform). *)
let group_key j =
  if j.category = Dependency then Dep (j.reason, (job_dims j).distro)
  else Key (failure_key j)

let matches match_ variant =
  match match_ with
  | None | Some "" -> true
  | Some m -> Rx.test (Rx.v m) variant

let distinct_failures jobs match_ =
  let cats = Category.[ Build; Build_or_test; Solver; Dependency ] in
  let sel =
    List.filter
      (fun j ->
        j.status = Failed && List.mem j.category cats
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
        r.rule = Some Library_api
        && (Suggestion.is_set r.Suggestion.new_
           (* or already bounded on master (dune's select, #26907) *)
           || Where.where_kind r = Rerun)
        && r.dep <> None
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
let windows_freebsd_macos_re = Rx.v "Windows|freebsd|macos"
let log_day_re = Rx.v ~flags:[ Rx.M ] {|^(\d{4}-\d\d-\d\d) \d\d:\d\d|}

(* What a result says about its job, without the job's platform: the same on
   two systems means one failure. For a failure no rule explains, its key
   error line, without numbers and directories (/usr/lib/llvm20/lib/ and
   /usr/local/llvm20/lib/ name the same missing library). *)
let path_dirs_re = Rx.v {|(?:/[^/\s'"`]+)+/|}
let digits_re = Rx.v {|\d+|}

let diagnosis j text (r : Suggestion.t) =
  let d = job_dims j in
  let plats =
    List.filter (fun p -> p <> "") [ Option.value ~default:"" d.distro; d.os ]
  in
  let norm s =
    List.fold_left (fun s p -> Util.replace s p "{P}") (Suggestion.str s) plats
  in
  ( r.rule,
    Suggestion.get r.failing,
    norm r.error,
    norm (Suggestion.get r.new_),
    norm r.advice,
    norm r.note,
    if r.unexplained then
      Rx.sub digits_re "N"
        (Rx.sub path_dirs_re "…/"
           (Option.value ~default:"" (Llm.key_error_line text)))
    else "" )

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
  (* When and on what a job ran, for the day10 failures that pass on the main
     builder (a log that cannot be read dates nothing). *)
  let run =
    Matrix.per_builder (fun j : Matrix.run ->
        match log_text (!fetch_hook j.url cache_dir) with
        | exception _ -> { day = None; base = None }
        | text ->
            {
              day =
                Option.bind (Rx.search log_day_re text) (fun m ->
                    Date.days_of_date (Rx.group m 1));
              base = List.assoc_opt "base" (excerpt text);
            })
  in
  (* The day10 failures sort last: most of them are day10's, and the pattern
     of passes says so (builder-specific). Not those whose main-builder twin
     passed in another run, months before: only their log explains them. Their
     groups are analysed beyond the limit, [max_logs] more at most (#24267:
     gnuplot missing on centos-9; #27642: llvm-17-dev on debian-12). The
     analysed groups keep their order. *)
  let analysed =
    if List.length groups <= max_logs then groups
    else begin
      let first = Util.take max_logs groups
      and rest = Util.drop max_logs groups in
      let stale =
        Matrix.stale_twins ~run ~among:(List.concat rest) data.jobs match_
      in
      let more =
        Util.take max_logs
          (List.filter (List.exists (fun j -> List.memq j stale)) rest)
      in
      if more <> [] then
        Printf.eprintf
          "note: also analysing %s that a main-builder pass from another run \
           does not explain\n\
           %!"
          (Words.counted (List.length more) "day10 failure");
      List.filter (fun g -> List.memq g first || List.memq g more) groups
    end
  in
  let dropped = List.filter (fun g -> not (List.memq g analysed)) groups in
  (* to count the failures no suggestion explains *)
  let results = ref [] and base = ref None and split_keys = ref [] in
  let prev_cache = Hashtbl.create 4 in
  let add r = results := !results @ [ r ] in
  let jobs_by_variant = Hashtbl.create 64 in
  List.iter (fun x -> Hashtbl.replace jobs_by_variant x.variant x) data.jobs;
  let variants js = List.map (fun j -> j.variant) js in
  let read j =
    match log_text (!fetch_hook j.url cache_dir) with
    | exception e -> Error e
    | text -> Ok text
  in
  (* The rules after the per-job ones: test output, bounds. *)
  let finish j text (r : Suggestion.t) =
    if r.rule = Some Test_output then
      Rules.complete_test_output src r j text data cache_dir;
    let lb = Rules.lower_bounds_rule src j text r pr_packages sha in
    let info = Loginfo.log_info text in
    r.installed <- Some info.installed;
    let pb =
      Rules.previous_release_bound src j info pr_packages sha prev_cache
    in
    (r, lb, pb)
  in
  (* [jobs]: every job the analysed one stands for, so the report can say how
     many. *)
  let add_results j jobs (r, lb, pb) =
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
        Suggestion.update pb [ `Key key; `Jobs jobs; `Keep ];
        add pb
    | _ -> ());
    Suggestion.update r [ `Key key; `Jobs jobs ];
    if r.depexts <> None then Rules.conf_depext_platforms r jobs_by_variant;
    add r;
    match lb with
    | Some lb ->
        Suggestion.update lb [ `Key key; `Jobs jobs ];
        add lb
    | None -> ()
  in
  (* A group is one failure key, read on its first job: the same reason on
     several systems may hide different failures (#29110: "LLVM 20 is not
     installed." on Alpine, Arch Linux, openSUSE and FreeBSD, whose llvm has no
     static libraries, and on Cygwin and MSYS2, which have no LLVM). The first
     job of each other OS in the group is read too; when its diagnosis
     differs, its OS's jobs are a result of their own. An unreadable log
     (offline: one not in the cache), or a rule that fails, leaves them with
     the first job. *)
  let os_of j = (job_dims j).os in
  let os_splits g diag =
    let oss = List.tl (Util.dedup (List.map os_of g)) in
    List.filter_map
      (fun o ->
        let js = List.filter (fun x -> os_of x = o) g in
        let j' = List.hd js in
        let path = Net.cache_path j'.url cache_dir in
        if
          Net.is_offline ()
          && not (Sys.file_exists path || Sys.file_exists (path ^ ".gz"))
        then
          None
          (* offline, a log not recorded is not missed: this read is optional *)
        else
          match read j' with
          | Error _ -> None
          | Ok text' -> (
              match !suggest_for_job_hook src j' text' pr_packages sha with
              | exception ((Out_of_memory | Stack_overflow) as e) -> raise e
              | exception _ -> None
              | r' ->
                  if diagnosis j' text' r' = diag then None
                  else Some (js, text', r')))
      oss
  in
  List.iter
    (fun g ->
      let j = List.hd g in
      match read j with
      | Error e ->
          add
            (Suggestion.make
               [
                 `Variant j.variant;
                 `Note ("Could not read the log: " ^ Util.exn_text e);
                 `Jobs (variants g);
               ])
      | Ok text -> (
          if !base = None then base := List.assoc_opt "base" (excerpt text);
          (* the master commit the PR was merged with *)
          (* The diagnosis compared with the other OSes': the per-job rules',
             before [finish] completes it (test output's candidates). *)
          match
            let r = !suggest_for_job_hook src j text pr_packages sha in
            let diag = diagnosis j text r in
            (diag, finish j text r)
          with
          | exception ((Out_of_memory | Stack_overflow) as e) -> raise e
          | exception e ->
              add (Checks.rule_error j e (variants g))
              (* a bug in a rule: this job only, the others are still analysed *)
          | diag, res ->
              let splits = os_splits g diag in
              let split = List.concat_map (fun (js, _, _) -> js) splits in
              if splits <> [] then split_keys := failure_key j :: !split_keys;
              add_results j
                (variants (List.filter (fun x -> not (List.memq x split)) g))
                res;
              List.iter
                (fun (js, text, r) ->
                  let j = List.hd js in
                  match finish j text r with
                  | exception ((Out_of_memory | Stack_overflow) as e) -> raise e
                  | exception e -> add (Checks.rule_error j e (variants js))
                  | res -> add_results j (variants js) res)
                splits))
    analysed;
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
  let ci = List.filter (Titles.has_rule [ Transient; Ci_failure ]) results in
  (* not a group split by OS (see [os_splits]): only its own jobs then *)
  let transient =
    List.filter
      (fun k -> not (List.mem k !split_keys))
      (List.concat_map keys_of ci)
  in
  (* ... and the jobs merged with them (the same dependency failure of other
     packages, on that distribution: the fedora-45 image of day10 without
     /usr/bin/env made them a pattern of their own) *)
  let ci_jobs = List.concat_map Checks.jobs_or_variant ci in
  let unknown, jobs =
    List.partition
      (fun j ->
        j.status = Failed
        && (List.mem (failure_key j) transient || List.mem j.variant ci_jobs))
      data.jobs
  in
  (* A broken image fails every job of its builder and distribution: the
     failures there whose log was not read may be the same (#29582: the
     fedora-45 image of day10). *)
  let broken =
    List.concat_map
      (fun r ->
        if r.Suggestion.broken_platform then
          List.filter_map
            (fun v ->
              Option.map Matrix.builder_distro
                (Hashtbl.find_opt jobs_by_variant v))
            (Checks.jobs_or_variant r)
        else [])
      ci
  in
  let unsure =
    List.filter
      (fun j ->
        let ((_, distro) as k) = Matrix.builder_distro j in
        distro <> None && List.mem k broken)
      (List.concat dropped)
  in
  let matrix =
    Matrix.collapse_systemic
      (Matrix.matrix_rules ~failing_in_log ~unknown ~unsure ~run jobs results
         match_ pr_packages)
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
  (* The results of a group split by OS: explained when the matrix
     suggestions list each of their jobs (not those it leaves out:
     experimental macOS jobs, #30296). *)
  let matrix_jobs = List.concat_map Checks.jobs_or_variant matrix in
  let results =
    List.filter
      (fun r ->
        r.Suggestion.keep
        || not
             ((match r.key with
                | Some k when List.mem k !split_keys ->
                    List.for_all
                      (fun v -> List.mem v matrix_jobs)
                      (Checks.jobs_or_variant r)
                | Some k -> List.mem k explained
                | None -> false)
             && (not (Suggestion.is_set r.Suggestion.new_))
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
      data.health_stale <- Some d
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
      r.check <- None;
      r.broken_platform <- false)
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
          if r.rule = Some Llm then begin
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
        Titles.has_rule
          [
            Platform_matrix;
            Compiler_matrix;
            Missing_system_package;
            Missing_header;
            Newer_c_compiler;
          ]
          r
      then begin
        let name, ver = split_pkg (Suggestion.str_field r.Suggestion.failing) in
        let vs =
          if name <> "" && ver <> "" then Sources.versions_or_empty src name
          else []
        in
        match Util.last_opt vs with
        | Some latest
          when List.mem ver vs && latest <> ver && r.alternative = None ->
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
          if Suggestion.is_set r.Suggestion.new_ then
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
          else if r.advice <> None then
            ( Some
                (`List
                   [ `String "advice"; failing; `String (Option.get r.advice) ]),
              None )
          else if Titles.has_rule per_package_rules r then
            let t =
              Util.replace (Suggestion.str r.note)
                (match plat with Some p when p <> "" -> p | _ -> "\000")
                "{P}"
            in
            ( Some
                (`List
                   [
                     `String "rule";
                     failing;
                     s (Option.fold ~none:"" ~some:Rule.to_string r.rule);
                     s t;
                   ]),
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
          Titles.has_rule [ Platform_matrix; Platform_matrix_systemic ] r
          && Rx.test windows_freebsd_macos_re (Suggestion.str r.error)
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
  let plats v =
    match Hashtbl.find_opt jobs_by_variant v with
    | Some j ->
        let d = job_dims j in
        List.filter
          (fun p -> p <> "")
          [ Option.value ~default:"" d.distro; d.os ]
    | None -> []
  in
  Context.accepted_elsewhere results src pr_packages;
  List.iter (fun (r : Suggestion.t) -> r.installed <- None) results;
  Titles.readable
    (Checks.merge_ci_failures
       (Checks.merge_versions ~plats (Checks.merge_unrecognised results)))
