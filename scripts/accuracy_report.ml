open Triage

let num (v : Json.t) = match Json.to_float_opt v with Some f -> f | None -> 0.

let rates t =
  let p =
    if Json.truthy (Json.member "scored" t) then
      Some (100. *. num (Json.member "hit" t) /. num (Json.member "scored" t))
    else None
  in
  let r =
    if Json.truthy (Json.member "fix_tokens" t) then
      Some
        (100.
        *. num (Json.member "fix_covered" t)
        /. num (Json.member "fix_tokens" t))
    else None
  in
  (p, r)

(** The suggestions of a PR that matched the author's fix, as comparable keys:
    the ones counted in the totals and the ones counted apart
    (opam-health-check), so that a suggestion moving from one to the other is
    not a regression. *)
let hits row =
  List.sort_uniq compare
    (List.filter_map
       (fun s ->
         if Json.truthy (Json.member "hit" s) then
           Some
             ( Json.str "package" s,
               Json.str "rule" s,
               Json.to_str_list (Json.member "tokens" s) )
         else None)
       (Json.list "suggestions" row @ Json.list "health_suggestions" row))

let diff a b = List.filter (fun x -> not (List.mem x b)) a
let pct = function None -> "n/a" | Some x -> Printf.sprintf "%.0f%%" x

(* Python's str() of a JSON number. *)
let s (v : Json.t) =
  match v with
  | `Int n -> string_of_int n
  | `Float f -> Py.repr_float f
  | `String s -> s
  | `Null -> "None"
  | v -> Json.dumps v

let run ~current ~history ~commit ~max_drop ~min_scored () =
  let cur = Json.parse (Py.read_file current) in
  Py.makedirs history;
  let latest = Filename.concat history "latest.json" in
  let prev =
    if Sys.file_exists latest then Some (Json.parse (Py.read_file latest))
    else None
  in
  let t = Json.member "totals" cur in
  let p, r = rates t in
  let rows =
    List.filter (fun x -> not (Json.mem "error" x)) (Json.list "prs" cur)
  in
  let errors =
    List.filter (fun x -> Json.mem "error" x) (Json.list "prs" cur)
  in
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
  let pt = Option.map (Json.member "totals") prev in
  let pp, pr_ = match pt with Some pt -> rates pt | None -> (None, None) in
  let g k = s (Json.member k t)
  and pg k = match pt with Some pt -> s (Json.member k pt) | None -> "" in
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
  let prev_has_health =
    match pt with Some pt -> Json.mem "health_scored" pt | None -> false
  in
  if Json.mem "health_scored" t then
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
  if prev <> None && Json.mem "health_scored" t && not prev_has_health then
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
            if Json.mem "error" x then None
            else Some ((Json.member "pr" x, Json.member "initial" x), x))
          (Json.list "prs" prev)
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
          match find (Json.member "pr" x, Json.member "initial" x) with
          | None -> ()
          | Some y ->
              incr common;
              List.iter
                (fun k -> lost := !lost @ [ (Json.member "pr" x, k) ])
                (diff (hits y) (hits x));
              List.iter
                (fun k -> gained := !gained @ [ (Json.member "pr" x, k) ])
                (diff (hits x) (hits y)))
        rows;
      add [ ""; Printf.sprintf "PRs evaluated both times: %d." !common ];
      let show l =
        String.concat ", "
          (List.map
             (fun (n, (pkg, rule, _)) ->
               Printf.sprintf "#%s %s (%s)" (s n) pkg rule)
             l)
      in
      if !gained <> [] then
        add
          [
            "Suggestions that now match the author's fix: " ^ show !gained ^ ".";
          ];
      if !lost <> [] then begin
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
        when Json.to_int (Json.member "scored" t) >= min_scored
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
          Printf.sprintf "- #%s: the tool failed: `%s`"
            (s (Json.member "pr" x))
            (Py.suffix (Json.str "error" x) 150);
        ])
    errors;
  if !failed <> [] then
    add [ ""; "**Failed:** " ^ String.concat "; " !failed ^ "." ];
  print_endline (String.concat "\n" (List.rev !out));
  let fl = function Some f -> `Float f | None -> `Null in
  let record =
    Dict.of_list
      [
        ("date", `String (Py.today ()));
        ("commit", `String commit);
        ("prs", `Int (List.length rows));
        ("errors", `Int (List.length errors));
      ]
  in
  Dict.update record (Json.to_assoc t);
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
  output_string oc (Json.dumps ~indent:1 cur);
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
