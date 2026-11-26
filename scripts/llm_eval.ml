open Triage

let page_data =
  "https://sudha247.github.io/hackathon-opam-repo-triage/data.json"

(* The triage tool: $OPAM_CI_TRIAGE_TOOL, else opam-ci-triage next to this
   executable, else the dune build's, else opam-ci-triage on the PATH. *)
let tool () =
  match Util.getenv_nonempty "OPAM_CI_TRIAGE_TOOL" with
  | Some t -> t
  | None ->
      let dir = Filename.dirname (Util.abspath Sys.executable_name) in
      let near = Filename.concat dir "opam-ci-triage" in
      let dune = Filename.concat dir "../../bin/opam_repo_ci_triage.exe" in
      if Sys.file_exists near then near
      else if Sys.file_exists dune then dune
      else "opam-ci-triage"

(* The repository: the current directory (the tool's cache and llm-eval/ are there). *)
let root () = Sys.getcwd ()

let load source : Json.t =
  if String.starts_with ~prefix:"https://" source then
    Json.parse
      (Http.urlopen
         ~headers:[ ("User-Agent", "opam-ci-triage") ]
         ~timeout:60. source)
  else Json.parse (In_channel.with_open_bin source In_channel.input_all)

module E = Page_data.Entry

(** The suggestions of a [suggest --json] result. *)
let suggestions (result : Page_data.Result.t) =
  Page_data.Result.suggestions result

(** Number of failures of a PR that no rule recognises, in the page's data. *)
let unrecognised (pr : E.t) =
  List.length
    (List.filter
       (fun (r : Suggestion.t) ->
         r.rule = Some Unrecognised
         || not
              (r.rule <> None || Suggestion.is_set r.new_
              || Suggestion.list r.candidates <> []))
       (E.suggestions pr))

let usage result =
  let calls =
    List.filter_map
      (fun (r : Suggestion.t) ->
        match r.llm_usage with
        | Some (`Assoc _ as u) -> Some u
        | Some _ | None -> None (* null: a cached answer without it *))
      (suggestions result)
  in
  (* Failures sharing one answer carry the same usage: count each answer once. *)
  let uniq =
    Util.dedup
      (List.map
         (fun u ->
           (Json.member "input" u, Json.member "output" u, Json.member "model" u))
         calls)
  in
  ( List.length uniq,
    List.fold_left (fun a (i, _, _) -> a + Json.to_int i) 0 uniq,
    List.fold_left (fun a (_, o, _) -> a + Json.to_int o) 0 uniq )

let run ~data ~out ~first ~llm_max ~max_ ~dry_run () =
  let data = Page_data.of_json (load data) in
  let prs =
    List.filter
      (fun (p : E.t) -> E.str p.sha <> "" && unrecognised p > 0)
      (Page_data.prs data)
  in
  let prs =
    List.stable_sort (fun a b -> compare (unrecognised b) (unrecognised a)) prs
  in
  let prs =
    match first with Some n when n > 0 -> Util.take n prs | _ -> prs
  in
  Util.makedirs out;
  let max_logs =
    match max_ with
    | Some n when n > 0 -> string_of_int n
    | _ -> (
        match data.max_failures_per_pr with
        | Set n when n <> 0 -> string_of_int n
        | _ -> "6")
  in
  let total = ref (0, 0, 0) in
  Printf.printf "%d PRs with failures no rule recognises (data from %s)\n%!"
    (List.length prs)
    (Option.value ~default:"?" data.generated);
  List.iter
    (fun (p : E.t) ->
      let n = E.number p in
      let outf = Filename.concat out (Printf.sprintf "%d.json" n) in
      let res =
        if Sys.file_exists outf && not dry_run then
          Some
            ( Page_data.Result.of_json
                (Json.parse
                   (In_channel.with_open_bin outf In_channel.input_all)),
              "already done" )
        else begin
          let cmd =
            [
              tool ();
              "suggest";
              E.str p.sha;
              "--json";
              "--max";
              max_logs;
              "--llm-max";
              string_of_int llm_max;
              (if dry_run then "--llm-dry-run" else "--llm");
            ]
          in
          let proc = Proc.run ~cwd:(root ()) ~timeout:1800. cmd in
          match Json.parse_opt proc.stdout with
          | Some (`Assoc _ as v) ->
              let res = Page_data.Result.of_json v in
              (* the PR's title, for grading *)
              let res =
                {
                  res with
                  extra =
                    List.remove_assoc "pr_title" res.extra
                    @ [
                        ( "pr_title",
                          match p.title with Set t -> `String t | _ -> `Null );
                      ];
                }
              in
              if not dry_run then begin
                let oc = open_out_bin outf in
                output_string oc
                  (Json.dumps ~indent:1 (Page_data.Result.to_json res));
                close_out oc;
                let oc =
                  open_out_bin
                    (String.sub outf 0 (String.length outf - 5) ^ ".err")
                in
                output_string oc proc.stderr;
                close_out oc
              end;
              let note =
                if dry_run then "dry run"
                else
                  match Util.splitlines (String.trim proc.stderr) with
                  | [] -> ""
                  | l -> Util.last l
              in
              Some (res, note)
          | _ ->
              let last =
                match Util.splitlines (String.trim proc.stderr) with
                | [] -> string_of_int proc.code
                | l -> Util.last l
              in
              Printf.printf "#%d: failed: %s\n%!" n last;
              None
        end
      in
      match res with
      | None -> ()
      | Some (res, note) ->
          let sugg = suggestions res in
          if dry_run then
            Printf.printf "#%d: %d prompts  %s\n%!" n
              (List.length
                 (List.filter
                    (fun (r : Suggestion.t) -> r.llm_prompt <> None)
                    sugg))
              (Util.prefix_chars (E.str p.title) 60)
          else begin
            let calls, tin, tout = usage res in
            let a, b, c = !total in
            total := (a + calls, b + tin, c + tout);
            let answered =
              List.length
                (List.filter (fun (r : Suggestion.t) -> r.rule = Some Llm) sugg)
            in
            Printf.printf
              "#%d: %d failures answered, %d new answers, %d input and %d \
               output tokens  (%s)\n\
               %!"
              n answered calls tin tout note
          end)
    prs;
  if not dry_run then
    let a, b, c = !total in
    Printf.printf
      "total: %d answers, %d input tokens, %d output tokens (answers from the \
       cache count too)\n"
      a b c

let main () =
  let open Cmdliner in
  let data =
    Arg.(
      value & opt string page_data
      & info [ "data" ] ~docv:"DATA"
          ~doc:"the triage page data, a file or a URL")
  in
  let out =
    Arg.(
      value
      & opt string (Filename.concat (root ()) "llm-eval")
      & info [ "out" ] ~docv:"OUT" ~doc:"the directory of the results")
  in
  let first =
    Arg.(
      value
      & opt (some int) None
      & info [ "first" ] ~docv:"FIRST"
          ~doc:
            "only the first FIRST PRs (those with the most unrecognised \
             failures)")
  in
  let llm_max =
    Arg.(
      value & opt int 2
      & info [ "llm-max" ] ~docv:"LLM_MAX" ~doc:"LLM calls per PR at most")
  in
  let max_ =
    Arg.(
      value
      & opt (some int) None
      & info [ "max" ] ~docv:"MAX" ~doc:"suggest --max (default: the page's)")
  in
  let dry_run =
    Arg.(
      value & flag
      & info [ "dry-run" ]
          ~doc:"list the PRs and the number of prompts, no API call")
  in
  let term =
    Term.(
      const (fun data out first llm_max max_ dry_run ->
          run ~data ~out ~first ~llm_max ~max_ ~dry_run ())
      $ data $ out $ first $ llm_max $ max_ $ dry_run)
  in
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "llm-eval"
             ~doc:
               "Run suggest --llm on the PRs of the triage page that have \
                failures no rule recognises, and save each result for grading.")
          term))
