open Ci

type args = {
  command : string;
  target : string option;
  pr : int option;
  json : bool;
  all : bool;
  match_ : string option;
  category : string list;
  max : int;
  llm : bool;
  llm_max : int;
  llm_model : string;
  llm_dry_run : bool;
  post : bool;
  baseline : string option;
  cache_dir : string;
  refresh : bool;
}

let print_json v =
  print_string (Json.dumps ~indent:2 v);
  print_newline ()

let counts_line () =
  String.concat ", "
    (List.map
       (fun (k, v) -> Printf.sprintf "%s=%d" k v)
       (Odict.to_list Net.net))

(** Run jobs on [n] threads, keeping the order of the results. *)
let parallel_map n f l =
  let a = Array.of_list l in
  let res = Array.make (Array.length a) None in
  let next = ref 0 and m = Mutex.create () in
  let worker () =
    let rec loop () =
      Mutex.lock m;
      let i = !next in
      incr next;
      Mutex.unlock m;
      if i < Array.length a then (
        res.(i) <- Some (f a.(i));
        loop ())
    in
    loop ()
  in
  let ts =
    List.init
      (max 1 (min n (Array.length a)))
      (fun _ -> Thread.create worker ())
  in
  List.iter Thread.join ts;
  Array.to_list (Array.map Option.get res)

let logs args sha (data : data) =
  let cats =
    if args.category = [] then
      Category.[ Build; Build_or_test; Dependency; Solver; Lint ]
    else List.map Category.of_string args.category
  in
  let sel =
    List.filter
      (fun j ->
        j.status = Failed
        && (List.mem j.category cats || j.kind = Lint)
        &&
        match args.match_ with
        | None | Some "" -> true
        | Some m -> Rx.test (Rx.v m) j.variant)
      data.jobs
  in
  let sel =
    if List.length sel > args.max then begin
      Printf.eprintf
        "note: %d jobs selected, fetching the first %d (use --max)\n%!"
        (List.length sel) args.max;
      Util.take args.max sel
    end
    else sel
  in
  let work j =
    try (j, `Ok (excerpt (log_text (Net.fetch j.url args.cache_dir))))
    with e -> (j, `Error (Util.exn_text e))
    (* keep going: one unreachable log must not hide the others *)
  in
  let results = parallel_map 2 work sel in
  let log_json = function
    | `Ok ex -> `Assoc (List.map (fun (k, v) -> (k, `String v)) ex)
    | `Error e -> `Assoc [ ("fetch_error", `String e) ]
  in
  if args.json then
    print_json
      (`Assoc
         [
           ("sha", `String sha);
           ("pr", match data.pr with Some n -> `Int n | None -> `Null);
           ( "jobs",
             `List
               (List.map
                  (fun (j, lg) ->
                    match job_to_json j with
                    | `Assoc kv -> `Assoc (kv @ [ ("log", log_json lg) ])
                    | v -> v)
                  results) );
         ])
  else
    List.iter
      (fun (j, lg) ->
        print_endline (String.make 100 '=');
        let lg =
          match lg with `Ok ex -> ex | `Error e -> [ ("fetch_error", e) ]
        in
        let g k = List.assoc_opt k lg in
        let builder =
          match g "builder" with
          | Some "classic" -> "main"
          | Some b -> b
          | None -> "None"
        in
        Printf.printf "%s  [%s] builder=%s base=%s\n" j.variant
          (Category.to_string j.category)
          builder
          (Util.prefix_chars (Option.value ~default:"?" (g "base")) 12);
        print_endline j.url;
        (match g "job" with
        | Some s when s <> "" -> print_endline ("job: " ^ s)
        | _ -> ());
        List.iter
          (fun k -> match g k with Some v -> print_endline v | None -> ())
          [ "solver"; "error_block" ];
        match g "fetch_error" with
        | Some e -> print_endline ("!! could not fetch log: " ^ e)
        | None ->
            print_endline
              ("=> "
              ^
              match g "final" with
              | Some f when f <> "" -> f
              | _ -> (
                  match g "job_failed" with
                  | Some f when f <> "" -> f
                  | _ -> "?")))
      results

let compare args sha (data : data) =
  let targets =
    List.sort_uniq compare
      (List.filter_map
         (fun (j : job) ->
           if j.kind = Revdeps && List.mem j.category failing then j.target
           else None)
         data.jobs)
  in
  let report =
    List.filter_map
      (fun t ->
        let info =
          match args.baseline with
          | Some b ->
              Some
                (Dict.of_list
                   [
                     ("prev", `String "(given)");
                     ("sha", `String (resolve_sha (Some b) None));
                     ("pr", `Null);
                   ])
          | None -> Sources.find_baseline_commit ~cache_dir:args.cache_dir t
        in
        match info with
        | Some info when Dict.mem info "sha" -> (
            let bsha = Dict.str info "sha" in
            match
              parse_commit_page (Net.fetch (Net.base ^ bsha) args.cache_dir)
            with
            | exception (Http.Http_error _ as e) ->
                Printf.eprintf
                  "== %s: baseline CI page for %s (%s) unavailable: %s\n%!" t
                  (Dict.str info "prev")
                  (Util.prefix_chars bsha 12)
                  (Http.error_text e);
                None
            | base_data ->
                let results = Render.compare data.jobs base_data.jobs in
                let results =
                  List.filter
                    (fun r ->
                      List.exists
                        (fun (j : job) -> j.target = Some t)
                        (List.filter
                           (fun (j : job) ->
                             Json.opt_string j.revdep = Json.member "revdep" r)
                           data.jobs))
                    results
                in
                if not args.json then
                  Render.render_compare ~show_all:args.all t (Dict.to_json info)
                    results;
                Some
                  (`Assoc
                     [
                       ("target", `String t);
                       ("baseline", Dict.to_json info);
                       ("results", `List results);
                     ]))
        | _ ->
            Printf.eprintf "== %s: no previous version / baseline PR found\n%!"
              t;
            None)
      targets
  in
  if args.json then
    print_json
      (`Assoc
         [
           ("sha", `String sha);
           ("pr", match data.pr with Some n -> `Int n | None -> `Null);
           ("report", `List report);
         ])

let suggest_command args sha (data : data) =
  let src = Sources.create args.cache_dir in
  let llm =
    if args.llm || args.llm_dry_run then
      Some
        {
          Suggest.limit = args.llm_max;
          model = args.llm_model;
          dry_run = args.llm_dry_run;
        }
    else None
  in
  let results =
    Titles.rank
      (Suggest.suggest sha data src args.cache_dir args.max args.match_ llm)
  in
  if Net.is_offline () then begin
    Printf.eprintf "offline cache misses: %d\n%!"
      (List.length !Net.offline_misses);
    List.iter
      (fun m -> Printf.eprintf "  miss: %s\n%!" m)
      (Util.take 10 !Net.offline_misses)
  end;
  let pr_num =
    match data.pr with Some n -> string_of_int n | None -> "None"
  in
  match args.command with
  | "review" ->
      let review =
        Render.build_review ~raw:(Sources.raw src) sha data results
      in
      if args.post then begin
        let out =
          Proc.run ~input:(Json.dumps review)
            [
              "gh";
              "api";
              Printf.sprintf "repos/%s/pulls/%s/reviews" Net.repo pr_num;
              "-X";
              "POST";
              "--input";
              "-";
            ]
        in
        print_endline
          (match Util.prefix_chars (String.trim out.stdout) 300 with
          | "" -> String.trim out.stderr
          | o -> o)
      end
      else print_json review;
      Printf.eprintf "%s; network requests: %s\n%!"
        (Words.counted
           (List.length (Json.list "comments" review))
           "line comment")
        (counts_line ())
  | "comment" ->
      let body = Render.render_comment sha data results in
      if args.post then begin
        if data.pr = None then
          Util.die "cannot post: the CI page does not name a PR";
        let out =
          Proc.run ~input:body
            [
              "gh"; "pr"; "comment"; pr_num; "-R"; Net.repo; "--body-file"; "-";
            ]
        in
        print_endline
          (match String.trim out.stdout with
          | "" -> String.trim out.stderr
          | o -> o)
      end
      else print_endline body;
      Printf.eprintf "network requests: %s\n%!" (counts_line ())
  | _ ->
      if args.json then begin
        (* incomplete: ocaml.org or another server failed during the analysis (see the counts).
         git-unreachable: a git host that failed earlier today, not asked again. *)
        let errors =
          List.filter_map
            (fun k ->
              match Net.net_get k with 0 -> None | v -> Some (k, `Int v))
            [ "mcp-errors"; "net-errors"; "git-unreachable" ]
        in
        print_json
          (`Assoc
             ([
                ("sha", `String sha);
                ("pr", match data.pr with Some n -> `Int n | None -> `Null);
                ("ci_unfinished", `Int data.unfinished);
                ("ci_jobs", `Int (List.length data.jobs));
                ("unanalysed", `Int data.unanalysed);
                ("incomplete", `Bool (errors <> []));
                ("errors", `Assoc errors);
              ]
             (* Unexplained failures left without an LLM answer (--llm: beyond --llm-max, an API
              error, or CI still running): the triage page asks again later. *)
             @ (if llm <> None then
                  [
                    ( "llm_unanswered",
                      `Int (Option.value ~default:0 data.llm_unanswered) );
                  ]
                else [])
             @ (match data.health_stale with
               | Some v when Json.truthy v -> [ ("health_stale", v) ]
               | _ -> [])
             @ [ ("suggestions", `List (List.map Suggestion.to_json results)) ]
             ))
      end
      else Render.render_suggestions results;
      Printf.eprintf "network requests: %s\n%!" (counts_line ())

let main args =
  let sha = resolve_sha args.target args.pr in
  let _page, data = load_commit ~refresh:args.refresh sha args.cache_dir in
  if data.jobs = [] then
    Util.die "no jobs found (CI not started yet, or the page layout changed)";
  if data.unfinished > 0 then
    Printf.eprintf
      "note: CI still running: %d of %d jobs not finished; these are the \
       failures so far\n\
       %!"
      data.unfinished (List.length data.jobs);
  match args.command with
  | "summary" ->
      if args.json then
        print_json (`Assoc (("sha", `String sha) :: data_fields data))
      else Render.render_summary sha data args.all
  | "logs" -> logs args sha data
  | "compare" -> compare args sha data
  | _ -> suggest_command args sha data
