open Triage

(* The evaluation of [evaluate-closed-prs --json]: its totals and its rows,
   one per PR. *)

type totals = {
  hit : int option;
  scored : int option;
  fix_covered : int option;
  fix_tokens : int option;
  health_hit : int option;
  health_scored : int option;
  members : (string * Json.t) list;  (** every member, as read *)
}

let totals_of_json (v : Json.t) =
  let members = match v with `Assoc kv -> kv | _ -> [] in
  let int k =
    match List.assoc_opt k members with Some (`Int n) -> Some n | _ -> None
  in
  {
    hit = int "hit";
    scored = int "scored";
    fix_covered = int "fix_covered";
    fix_tokens = int "fix_tokens";
    health_hit = int "health_hit";
    health_scored = int "health_scored";
    members;
  }

type scored = {
  package : string;
  rule : string;
  hit : bool;
  tokens : string list;
}
(** A suggestion of a row, scored against the author's fix. *)

let scored_of_json (v : Json.t) =
  {
    package = Json.str "package" v;
    rule = Json.str "rule" v;
    hit = Json.member "hit" v = `Bool true;
    tokens = Json.to_str_list (Json.member "tokens" v);
  }

type row = {
  pr : int option;
  initial : string option;  (** the PR's first commit *)
  error : string option;  (** the tool failed on it *)
  suggestions : scored list;
  health_suggestions : scored list;  (** counted apart *)
}

let row_of_json (v : Json.t) =
  let scored k = List.map scored_of_json (Json.list k v) in
  {
    pr = (match Json.member "pr" v with `Int n -> Some n | _ -> None);
    initial = Json.str_opt "initial" v;
    error = (if Json.mem "error" v then Some (Json.str "error" v) else None);
    suggestions = scored "suggestions";
    health_suggestions = scored "health_suggestions";
  }

type evaluation = { totals : totals; prs : row list }

let evaluation_of_json (v : Json.t) =
  {
    totals = totals_of_json (Json.member "totals" v);
    prs = List.map row_of_json (Json.list "prs" v);
  }

let ratio a b =
  match b with
  | Some b when b <> 0 ->
      Some (100. *. float (Option.value ~default:0 a) /. float b)
  | _ -> None

let rates (t : totals) = (ratio t.hit t.scored, ratio t.fix_covered t.fix_tokens)

(** The suggestions of a PR that matched the author's fix, as comparable keys:
    the ones counted in the totals and the ones counted apart
    (opam-health-check), so that a suggestion moving from one to the other is
    not a regression. *)
let hits row =
  List.sort_uniq compare
    (List.filter_map
       (fun s -> if s.hit then Some (s.package, s.rule, s.tokens) else None)
       (row.suggestions @ row.health_suggestions))

let diff a b = List.filter (fun x -> not (List.mem x b)) a

(* A PR's number, "?" when unknown. *)
let pr_text = function Some n -> string_of_int n | None -> "?"
let pct = function None -> "n/a" | Some x -> Printf.sprintf "%.0f%%" x

(* A number of the totals, "?" when absent. *)
let s (v : Json.t) =
  match v with
  | `Int n -> string_of_int n
  | `Float f -> Json.repr_float f
  | `String s -> s
  | `Null -> "?"
  | v -> Json.dumps v

let run ~current ~history ~commit ~max_drop ~min_scored () =
  let cur_json =
    Json.parse (In_channel.with_open_bin current In_channel.input_all)
  in
  let cur = evaluation_of_json cur_json in
  Util.makedirs history;
  let latest = Filename.concat history "latest.json" in
  let prev =
    if Sys.file_exists latest then
      Some
        (evaluation_of_json
           (Json.parse (In_channel.with_open_bin latest In_channel.input_all)))
    else None
  in
  let t = cur.totals in
  let p, r = rates t in
  let rows = List.filter (fun x -> x.error = None) cur.prs in
  let errors = List.filter (fun x -> x.error <> None) cur.prs in
  let out = ref [] in
  let add l = out := List.rev_append l !out in
  add
    [
      "## Accuracy of the triage tool on merged PRs";
      "";
      Printf.sprintf
        "%d PRs whose first version failed opam-ci and was fixed before the \
         merge"
        (List.length rows)
      ^ (if errors <> [] then
           Printf.sprintf " (%d could not be analysed)" (List.length errors)
         else "")
      ^ ".";
      "";
      "| | This run | Previous run |";
      "|---|---|---|";
    ];
  let pt = Option.map (fun p -> p.totals) prev in
  let pp, pr_ = match pt with Some pt -> rates pt | None -> (None, None) in
  let g k = s (Option.value ~default:`Null (List.assoc_opt k t.members))
  and pg k =
    match pt with
    | Some pt -> s (Option.value ~default:`Null (List.assoc_opt k pt.members))
    | None -> ""
  in
  add
    [
      Printf.sprintf
        "| Suggested changes matching the author's fix (precision) | %s/%s \
         (%s) | "
        (g "hit") (g "scored") (pct p)
      ^ (if prev <> None then
           Printf.sprintf "%s/%s (%s)" (pg "hit") (pg "scored") (pct pp)
         else "none")
      ^ " |";
    ];
  add
    [
      Printf.sprintf "| Author's changes anticipated (recall) | %s/%s (%s) | "
        (g "fix_covered") (g "fix_tokens") (pct r)
      ^ (if prev <> None then
           Printf.sprintf "%s/%s (%s)" (pg "fix_covered") (pg "fix_tokens")
             (pct pr_)
         else "none")
      ^ " |";
    ];
  let has_health t = List.mem_assoc "health_scored" t.members in
  let prev_has_health =
    match pt with Some pt -> has_health pt | None -> false
  in
  if has_health t then
    add
      [
        Printf.sprintf
          "| Suggestions from today's opam-health-check (not counted above) | \
           %s/%s | "
          (g "health_hit") (g "health_scored")
        ^ (if prev <> None && prev_has_health then
             Printf.sprintf "%s/%s" (pg "health_hit") (pg "health_scored")
           else "none")
        ^ " |";
      ];
  (* The scoring of evaluate_closed_prs: "scoring" in the totals since 27
     November 2026 (3, then 4: see Scoring.version); before, 2 with the
     opam-health-check totals (20 November 2026), else 1. *)
  let scoring t =
    match List.assoc_opt "scoring" t.members with
    | Some (`Int n) -> n
    | _ -> if has_health t then 2 else 1
  in
  (* A break in the series since the scoring of 27 November 2026: the totals,
     and the hits of a PR, are not compared. *)
  let break =
    match pt with
    | Some pt -> scoring pt <> scoring t && max (scoring pt) (scoring t) >= 3
    | None -> false
  in
  if break then
    add
      [
        "";
        (let a = match pt with Some pt -> scoring pt | None -> 0
         and b = scoring t in
         let changes =
           List.filter_map
             (fun (v, c) ->
               if v > min a b && v <= max a b then Some c else None)
             [
               ( 3,
                 "since 27 November 2026 the review bot's x-maintenance-intent \
                  and description changes are not fixes, packages with a new \
                  archive are not scored, and alternatives and fixes placed in \
                  another package of the PR are" );
               ( 4,
                 "since 28 November 2026 a suggestion that only gives advice \
                  is scored when the advice names the field to change \
                  (depexts, the test command)" );
             ]
         in
         Printf.sprintf
           "The previous run was scored differently (scoring %d, this run %d; \
            %s): the totals are not comparable, and no check fails on them."
           a b
           (String.concat "; " changes));
      ];
  if prev <> None && has_health t && not prev_has_health then
    (* evaluate_closed_prs changed on 2026-11-20: each file of the PR compared
       between its two versions (rebased PRs), new archives and
       opam-health-check counted apart. *)
    add
      [
        "";
        "The previous run was scored the old way (before 20 November 2026): \
         the totals are not comparable.";
      ];
  let failed = ref [] in
  (match prev with
  | Some prev -> (
      let before =
        List.filter_map
          (fun x ->
            if x.error <> None then None else Some ((x.pr, x.initial), x))
          prev.prs
      in
      (* dict: the last row of a key wins *)
      let find k =
        List.fold_left
          (fun acc (k', x) -> if k' = k then Some x else acc)
          None before
      in
      let lost = ref [] and gained = ref [] and common = ref 0 in
      List.iter
        (fun x ->
          match find (x.pr, x.initial) with
          | None -> ()
          | Some y ->
              incr common;
              List.iter
                (fun k -> lost := !lost @ [ (x.pr, k) ])
                (diff (hits y) (hits x));
              List.iter
                (fun k -> gained := !gained @ [ (x.pr, k) ])
                (diff (hits x) (hits y)))
        rows;
      add [ ""; Printf.sprintf "PRs evaluated both times: %d." !common ];
      let show l =
        String.concat ", "
          (List.map
             (fun (n, (pkg, rule, _)) ->
               Printf.sprintf "#%s %s (%s)" (pr_text n) pkg rule)
             l)
      in
      if !gained <> [] then
        add
          [
            "Suggestions that now match the author's fix: " ^ show !gained ^ ".";
          ];
      if !lost <> [] && break then
        add
          [
            "Suggestions that matched the author's fix and no longer do (not \
             compared: the scoring changed): " ^ show !lost ^ ".";
          ]
      else if !lost <> [] then begin
        add
          [
            "**Regressions**, suggestions that matched the author's fix and no \
             longer do: " ^ show !lost ^ ".";
          ];
        failed :=
          !failed
          @ [
              Printf.sprintf "%d regression(s) on PRs evaluated both times"
                (List.length !lost);
            ]
      end;
      match (p, pp) with
      | Some p', Some pp'
        when (not break)
             && Option.value ~default:0 t.scored >= min_scored
             && pp' -. p' >= max_drop ->
          failed :=
            !failed
            @ [
                Printf.sprintf "precision dropped from %s to %s" (pct pp)
                  (pct p);
              ]
      | _ -> ())
  | None -> ());
  List.iter
    (fun x ->
      add
        [
          Printf.sprintf "- #%s: the tool failed: `%s`" (pr_text x.pr)
            (Util.suffix_chars (Option.value ~default:"" x.error) 150);
        ])
    errors;
  if !failed <> [] then
    add [ ""; "**Failed:** " ^ String.concat "; " !failed ^ "." ];
  print_endline (String.concat "\n" (List.rev !out));
  let fl = function Some f -> `Float f | None -> `Null in
  let record =
    Dict.of_list
      [
        ("date", `String (Date.today ()));
        ("commit", `String commit);
        ("prs", `Int (List.length rows));
        ("errors", `Int (List.length errors));
      ]
  in
  Dict.update record t.members;
  Dict.update record
    [ ("precision", fl p); ("recall", fl r); ("failed", Json.strings !failed) ];
  let oc =
    open_out_gen
      [ Open_append; Open_creat; Open_binary ]
      0o644
      (Filename.concat history "history.jsonl")
  in
  output_string oc (Json.dumps (Dict.to_json record) ^ "\n");
  close_out oc;
  let oc = open_out_bin latest in
  output_string oc (Json.dumps ~indent:1 cur_json);
  close_out oc;
  exit (if !failed <> [] then 1 else 0)

let main () =
  let open Cmdliner in
  let current =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"CURRENT" ~doc:"the result of evaluate-closed-prs --json")
  in
  let history =
    Arg.(
      required
      & opt (some string) None
      & info [ "history" ] ~docv:"HISTORY"
          ~doc:"the directory of the evaluations (latest.json, history.jsonl)")
  in
  let commit =
    Arg.(
      value & opt string ""
      & info [ "commit" ] ~docv:"COMMIT"
          ~doc:"the commit of the tool, recorded with the evaluation")
  in
  let max_drop =
    Arg.(
      value & opt float 15.
      & info [ "max-drop" ] ~docv:"MAX_DROP"
          ~doc:"a precision drop of this many points or more fails")
  in
  let min_scored =
    Arg.(
      value & opt int 10
      & info [ "min-scored" ] ~docv:"MIN_SCORED"
          ~doc:"the fewest scored suggestions for the precision check")
  in
  let term =
    Term.(
      const (fun current history commit max_drop min_scored ->
          run ~current ~history ~commit ~max_drop ~min_scored ())
      $ current $ history $ commit $ max_drop $ min_scored)
  in
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "accuracy-report"
             ~doc:
               "Compare an evaluate-closed-prs result with the previous one, \
                and record it. Exits with status 1 on a regression, or when \
                precision drops by MAX_DROP points or more.")
          term))
