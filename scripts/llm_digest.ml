open Triage
open Suggestion.T

(* Python's \w, \s and \b see characters: see Rx_utf8. *)
module U = Rx_utf8

let page_data =
  "https://sudha247.github.io/hackathon-opam-repo-triage/data.json"

let title = "LLM diagnoses to turn into rules"
let max_body = 60000

let load source : Json.t =
  if String.starts_with ~prefix:"https://" source then
    Json.parse
      (Http.urlopen
         ~headers:[ ("User-Agent", "opam-ci-triage") ]
         ~timeout:60. source)
  else Json.parse (In_channel.with_open_bin source In_channel.input_all)

let llm_answers n sugg =
  List.filter_map
    (fun (r : Suggestion.t) -> if r.rule = Some Llm then Some (n, r) else None)
    sugg

(** (PR number, suggestion) for every LLM answer. *)
let answers sources =
  List.concat_map
    (fun src ->
      match load src with
      | `Assoc kv as d when List.mem_assoc "prs" kv ->
          (* the page's data.json *)
          List.concat_map
            (fun p ->
              llm_answers
                (Page_data.Entry.number_opt p)
                (Page_data.Entry.suggestions p))
            (Page_data.prs (Page_data.of_json d))
      | d ->
          (* one `suggest --json` output *)
          let res = Page_data.Result.of_json d in
          llm_answers (Suggestion.get res.pr) (Page_data.Result.suggestions res))
    sources

let file_line_characters_re =
  Rx.v {|File "[^"]*", line \d+(, characters \d+-\d+)?:?|}

let path_re = Rx.v {|(?:[\w.+-]*/)+[\w.+-]+|}

let ml_mli_cpp_re =
  Rx.v {|\b[\w.-]+\.(?:ml|mli|c|h|cpp|hpp|cc|rs|dune|opam):\d+(?::\d+)?|}

let number_re = Rx.v {|\b\d+\b|}
let spaces_re = Rx.v {|\s+|}

(** An error line without its locations and numbers. *)
let exact line =
  let s = U.sub file_line_characters_re "" line in
  let s =
    U.sub path_re "<path>" s
    (* paths *)
  in
  let s = U.sub ml_mli_cpp_re "<file>" s in
  let s =
    U.sub number_re "N" s
    (* numbers, not the digits of a name (collect2, msys2) *)
  in
  String.trim (U.sub spaces_re " " s)

(* Python's \s on characters: ASCII whitespace (with the separators
   0x1c-0x1f) and Unicode white space. *)
let is_space_cp u =
  (u >= 0x09 && u <= 0x0d)
  || u = 0x20
  || (u >= 0x1c && u <= 0x1f)
  || (u >= 0x80 && Rx_utf8.is_space u)

(* Python: re.sub(r"[`'\"‘’][^`'\"‘’\s]{1,60}[`'\"‘’]", "'…'", s), on
   characters (the quotes ‘ and ’ are not ASCII). *)
let mask_quoted s =
  let n = String.length s in
  let char_at i =
    let u, k = Json.utf8_at s i in
    (u, k)
  in
  let is_q u = u = 0x60 || u = 0x27 || u = 0x22 || u = 0x2018 || u = 0x2019 in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let u, k = char_at !i in
    let matched =
      if not (is_q u) then None
      else begin
        let j = ref (!i + k) and count = ref 0 and stop = ref false in
        while (not !stop) && !j < n && !count < 60 do
          let v, l = char_at !j in
          if is_q v || is_space_cp v then stop := true
          else (
            j := !j + l;
            incr count)
        done;
        if !count >= 1 && !j < n && is_q (fst (char_at !j)) then
          Some (!j + snd (char_at !j))
        else None
      end
    in
    match matched with
    | Some e ->
        Buffer.add_string b "'\xe2\x80\xa6'";
        i := e
    | None ->
        Buffer.add_string b (String.sub s !i k);
        i := !i + k
  done;
  Buffer.contents b

let masked_numbers_re = Rx.v {|N(?:\.N)+\b|}
let module_path = Rx.v {|\b[A-Z][\w']*(?:\.[\w']+)+\b|}

(* Python: re.sub(r"\b(?!N(?:\.N)+\b)[A-Z][\w']*(?:\.[\w']+)+\b", "M.x", s):
   Module.path.ident, not N.N. *)
let mask_paths s =
  let e, offs = U.prepare s in
  let b = Buffer.create (String.length s) in
  let rec go pos last =
    match Rx.search ~pos module_path e with
    | None -> Buffer.add_string b (String.sub s last (String.length s - last))
    | Some m ->
        let a = Rx.start m in
        if Rx.match_ ~pos:a masked_numbers_re e <> None then go (a + 1) last
        else begin
          Buffer.add_string b (String.sub s last (offs.(a) - last));
          Buffer.add_string b "M.x";
          go (max (Rx.end_ m) (a + 1)) offs.(Rx.end_ m)
        end
  in
  go 0 0;
  Buffer.contents b

let value_module_constructor_re =
  Rx.v {|\b(value|module|constructor|field|type constructor) [\w.'-]+|}

let masked line =
  let s = exact line in
  let s =
    mask_quoted s
    (* quoted names, not messages *)
  in
  let s = mask_paths s in
  (* (A library or package name is quoted in the errors, masked above: the word
     after "package" in a sentence is not a name.) *)
  U.sub value_module_constructor_re {|\1 x|} s

(* Python's str() of a JSON scalar. *)
let py_str (v : Json.t) =
  match v with
  | `String s -> s
  | `Int n -> string_of_int n
  | `Float f -> Json.repr_float f
  | `Bool true -> "True"
  | `Bool false -> "False"
  | `Null -> "None"
  | v -> Json.dumps v

(** A reference to opam-repository PR [n] that GitHub does not turn into a
    cross-reference (a "mentioned this" event on the PR): a link to the PR on
    the triage page when it is known, else a code span. *)
let pr_ref ?page (n : int option) =
  match n with
  | Some n when n > 0 -> (
      match page with
      | Some page -> Printf.sprintf "[`#%d`](%s#pr-%d)" n page n
      | None -> Md.md_code ("ocaml/opam-repository#" ^ string_of_int n))
  | _ -> "?"

let https_za_z0_re =
  Rx.v {|(https://[A-Za-z0-9.-]+/(?:[A-Za-z0-9._~-]+/)*)data\.json|}

(** The triage page, when the data comes from its data.json. *)
let page_url sources =
  List.find_map
    (fun s ->
      match Rx.fullmatch https_za_z0_re s with
      | Some m -> Some (Rx.group m 1)
      | None -> None)
    sources

(* The first of [values] that is a non-empty string. *)
let first_nonempty values =
  List.find_map (function Some s when s <> "" -> Some s | _ -> None) values

let distinct_prs v = List.length (Util.dedup (List.map (fun (n, _, _) -> n) v))

let report ?page items min_prs =
  (* One answer covers the failures that share its error, in several jobs or
     packages of a PR, and is repeated on each: counted once per PR. *)
  let seen = Hashtbl.create 16 in
  let items =
    List.filter
      (fun (n, r) ->
        let k =
          ( n,
            Suggestion.get r.Suggestion.failing,
            match r.diagnosis with
            | Some d when d <> "" -> Some d
            | _ -> r.error )
        in
        if Hashtbl.mem seen k then false
        else (
          Hashtbl.add seen k ();
          true))
      items
  in
  let groups =
    Util.group_by
      (fun (_, _, line) -> masked line)
      (List.map
         (fun (n, r) ->
           ( n,
             r,
             Option.value ~default:""
               (first_nonempty
                  [ Suggestion.get r.log_error; r.error; r.diagnosis ]) ))
         items)
  in
  let conf =
    Util.counter
      (List.map
         (fun (_, r) ->
           let c =
             match Suggestion.get r.confidence with
             | Some v when v <> "" -> v
             | _ -> "?"
           in
           List.hd (String.split_on_char ' ' c))
         items)
  in
  let prs = Util.dedup (List.map fst items) in
  let out = ref [] in
  let add l = out := List.rev_append l !out in
  add
    [
      Printf.sprintf
        "%d LLM answers in %d PRs (%s confidence), in %d groups by error."
        (List.length items) (List.length prs)
        (Md.md_text
           (String.concat ", "
              (List.map (fun (k, v) -> Printf.sprintf "%d %s" v k) conf)))
        (List.length groups);
      "";
    ];
  let multi =
    List.stable_sort
      (fun (ka, va) (kb, vb) ->
        compare (-distinct_prs va, ka) (-distinct_prs vb, kb))
      (List.filter (fun (_, v) -> distinct_prs v >= min_prs) groups)
  in
  add
    [
      Printf.sprintf "## Errors answered in %d or more PRs (%d)" min_prs
        (List.length multi);
      "";
    ];
  if multi = [] then add [ "None yet." ];
  List.iter
    (fun (key, v) ->
      let pr_list =
        List.sort_uniq compare
          (List.filter_map
             (fun (n, _, _) ->
               match n with Some n when n <> 0 -> Some n | _ -> None)
             v)
      in
      let pkgs =
        List.sort_uniq compare
          (List.map
             (fun (_, r, _) ->
               match Suggestion.get r.Suggestion.failing with
               | Some f when f <> "" -> f
               | _ -> "?")
             v)
      in
      let code = Md.md_code (Util.prefix_chars key 200) in
      add
        [
          Printf.sprintf "### %s" (if code = "" then "?" else code);
          "";
          "- PRs: "
          ^ String.concat ", "
              (List.map (fun n -> pr_ref ?page (Some n)) pr_list);
          (Printf.sprintf "- Failing packages: %s"
             (Md.md_text (String.concat ", " (Util.take 12 pkgs)))
          ^ if List.length pkgs > 12 then " \xe2\x80\xa6" else "");
        ];
      let lines =
        List.sort_uniq compare (List.map (fun (_, _, line) -> line) v)
      in
      add
        [
          ("- Errors: "
          ^ String.concat "; "
              (List.map
                 (fun l -> Md.md_code (Util.prefix_chars l 160))
                 (Util.take 3 lines))
          ^ if List.length lines > 3 then " \xe2\x80\xa6" else "");
        ];
      let diags =
        Util.counter
          (List.map
             (fun (_, r, _) ->
               ( Option.value ~default:""
                   (first_nonempty [ r.diagnosis; r.error ]),
                 (match Suggestion.get r.confidence with
                 | Some c when c <> "" -> c
                 | _ -> "?"),
                 Option.value ~default:"?"
                   (first_nonempty [ Suggestion.get r.where; r.fix_in ]) ))
             v)
      in
      add [ "- Diagnoses:" ];
      List.iter
        (fun ((dg, c, w), k) ->
          add
            [
              Printf.sprintf "  - (%s confidence, fix in %s%s) %s"
                (Md.md_text c) (Md.md_text w)
                (if k > 1 then Printf.sprintf ", %d times" k else "")
                (Md.md_text ~limit:400 dg);
            ])
        (Util.take 3 diags);
      let changes =
        List.sort_uniq compare
          (List.filter_map
             (fun (_, r, _) ->
               match Suggestion.get r.Suggestion.new_ with
               | Some s when s <> "" -> Some s
               | _ -> None)
             v)
      in
      if changes <> [] then
        add
          [
            "- Suggested changes: "
            ^ String.concat "; "
                (List.map
                   (fun c -> Md.md_code (Util.prefix_chars c 120))
                   (Util.take 3 changes));
          ];
      let logs =
        Util.take 2
          (List.filter_map (fun (_, r, _) -> Md.safe_ci_url r.Suggestion.url) v)
      in
      if logs <> [] then
        add
          [
            "- Logs: "
            ^ String.concat ", "
                (List.map (fun u -> Printf.sprintf "[log](%s)" u) logs);
          ];
      add [ "" ])
    multi;
  let single =
    List.sort
      (fun (a, _) (b, _) -> compare a b)
      (List.filter (fun (_, v) -> distinct_prs v < min_prs) groups)
  in
  if single <> [] then begin
    add
      [
        Printf.sprintf "## Answered in fewer PRs (%d)" (List.length single);
        "";
        "<details><summary>Errors</summary>";
        "";
      ];
    List.iter
      (fun (key, v) ->
        let n, r, _ = List.hd v in
        let code = Md.md_code (Util.prefix_chars key 160) in
        add
          [
            Printf.sprintf "- %s: %s %s"
              (if code = "" then "?" else code)
              (pr_ref ?page n)
              (Md.md_text
                 (match Suggestion.get r.Suggestion.failing with
                 | Some f when f <> "" -> f
                 | _ -> ""));
          ])
      (Util.take 80 single);
    add [ ""; "</details>" ]
  end;
  add
    [
      "";
      "To turn a group into a rule: check the diagnosis on the logs, add the \
       rule to `lib/` (most go in `known_errors` in `lib/rules.ml`), and a \
       regression case in `tests/test_cases.ml`. The group then no longer \
       reaches the LLM.";
    ];
  let body = String.concat "\n" (List.rev !out) in
  if Util.char_length body <= max_body then body
  else
    (* Cut at a line end: a cut line could leave a code span or a link open. *)
    let limit = Util.byte_index body max_body in
    let cut = Util.rfind (String.sub body 0 limit) "\n" in
    String.sub body 0 (if cut < 0 then String.length body - 1 else cut)
    ^ "\n\n(truncated)"

let update_issue repo title body =
  let run ?input args =
    let p = Proc.run ?input args in
    if p.code <> 0 then
      failwith
        (Printf.sprintf "%s failed (%d): %s" (String.concat " " args) p.code
           (String.trim p.stderr));
    p
  in
  let q =
    run
      [
        "gh";
        "issue";
        "list";
        "-R";
        repo;
        "--state";
        "open";
        "--search";
        Printf.sprintf "\"%s\" in:title" title;
        "--json";
        "number,title";
      ]
  in
  let found =
    List.find_map
      (fun i ->
        if Json.str "title" i = title then Some (Json.member "number" i)
        else None)
      (Json.to_list (Json.parse q.stdout))
  in
  let cmd =
    match found with
    | Some n ->
        [ "gh"; "issue"; "edit"; py_str n; "-R"; repo; "--body-file"; "-" ]
    | None ->
        [
          "gh";
          "issue";
          "create";
          "-R";
          repo;
          "--title";
          title;
          "--body-file";
          "-";
        ]
  in
  let p = run ~input:body cmd in
  prerr_endline
    (match String.trim p.stdout with
    | "" -> (
        "updated issue #"
        ^ match found with Some n -> py_str n | None -> "None")
    | s -> s)

let run ~sources ~min_prs ~issue ~title () =
  let srcs = if sources = [] then [ page_data ] else sources in
  let items = answers srcs in
  if items = [] then
    prerr_endline
      "no LLM answer in the data (is ANTHROPIC_API_KEY set for the page?)"
  else begin
    let body = report ?page:(page_url srcs) items min_prs in
    print_endline body;
    match issue with Some repo -> update_issue repo title body | None -> ()
  end

let main () =
  let open Cmdliner in
  let sources =
    Arg.(
      value & pos_all string []
      & info [] ~docv:"SOURCES"
          ~doc:
            "the page data or suggest --json outputs to read, files or URLs \
             (default: the triage page data)")
  in
  let min_prs =
    Arg.(
      value & opt int 2
      & info [ "min-prs" ] ~docv:"MIN_PRS"
          ~doc:
            "the groups seen in at least this many PRs are candidates for a \
             rule")
  in
  let issue =
    Arg.(
      value
      & opt (some string) None
      & info [ "issue" ] ~docv:"OWNER/REPO"
          ~doc:
            "also create or update the digest issue in this repository (with \
             gh)")
  in
  let title =
    Arg.(
      value & opt string title
      & info [ "title" ] ~docv:"TITLE" ~doc:"the title of the issue")
  in
  let term =
    Term.(
      const (fun sources min_prs issue title ->
          run ~sources ~min_prs ~issue ~title ())
      $ sources $ min_prs $ issue $ title)
  in
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "llm-digest"
             ~doc:
               "Group the LLM diagnoses of the triage page by error, to find \
                the recurring ones worth turning into rules.")
          term))
