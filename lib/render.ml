open Suggestion.T
open Ci

let pad w s =
  let n = Util.char_length s in
  if n >= w then s else s ^ String.make (w - n) ' '

let render_summary sha (data : data) show_all =
  let jobs = data.jobs in
  Printf.printf "PR #%s  commit %s  %s%s\n"
    (match data.pr with Some n -> string_of_int n | None -> "None")
    (Util.prefix_chars sha 12) Net.base sha;
  if data.unfinished > 0 then
    Printf.printf "CI still running: %d of %d jobs not finished\n"
      data.unfinished (List.length jobs);
  let counts =
    Util.counter (List.map (fun j -> Category.to_string j.category) jobs)
  in
  let counts = List.sort compare counts in
  Printf.printf "jobs: %d %s\n" (List.length jobs)
    (String.concat " "
       (List.map (fun (k, v) -> Printf.sprintf "%s=%d" k v) counts));
  let noise = Category.[ passed; Ignored; Not_available ] in
  List.iter
    (fun j ->
      if j.kind = Lint || j.kind = Analysis then begin
        Printf.printf "\n[%s] %s%s\n" (Kind.to_string j.kind)
          (Status.to_string j.status)
          (match j.reason with Some r when r <> "" -> ": " ^ r | _ -> "");
        List.iter (fun m -> Printf.printf "    %s\n" m) j.messages
      end)
    jobs;
  let targets =
    List.sort_uniq compare
      (List.filter_map
         (fun j ->
           match j.target with Some t when t <> "" -> Some t | _ -> None)
         jobs)
  in
  List.iter
    (fun t ->
      let tj = List.filter (fun j -> j.target = Some t) jobs in
      let own =
        List.filter
          (fun j ->
            j.kind <> Revdeps && (show_all || not (List.mem j.category noise)))
          tj
      in
      let rev =
        List.filter
          (fun j ->
            j.kind = Revdeps && (show_all || not (List.mem j.category noise)))
          tj
      in
      Printf.printf "\n== %s ==\n" t;
      if own <> [] then begin
        print_endline "  own jobs:";
        List.iter
          (fun j ->
            Printf.printf "    %s %s %s %s\n"
              (pad 13 (Kind.to_string j.kind))
              (pad 14 (Category.to_string j.category))
              (pad 45 (platform_str j))
              (Option.value ~default:"" j.reason))
          own
      end
      else print_endline "  own jobs: all passed (or ignored / not available)";
      if rev <> [] then begin
        let by_pkg = Util.group_by (fun j -> j.revdep) rev in
        Printf.printf "  reverse dependencies with problems: %d\n"
          (List.length by_pkg);
        List.iter
          (fun (pkg, js) ->
            (* Group identical reasons across platforms. *)
            let pkg = Option.value ~default:"None" pkg in
            List.iter
              (fun ((cat, reason), js) ->
                Printf.printf "    %s %s %s\n" (pad 32 pkg)
                  (pad 14 (Category.to_string cat))
                  (Option.value ~default:"" reason);
                Printf.printf "    %s %s on: %s\n" (pad 32 "") (pad 14 "")
                  (String.concat ", " (List.map platform_str js)))
              (Util.group_by (fun j -> (j.category, j.reason)) js))
          (List.sort (fun (a, _) (b, _) -> compare a b) by_pkg)
      end)
    targets

(* ------------------------------------------------------------------------ *)
(* compare *)

let jobs_json js : Json.t =
  `List
    (List.map
       (fun j ->
         `Assoc
           [
             ("platform", `String (String.concat "/" j.platform));
             ("category", `String (Category.to_string j.category));
             ("reason", Json.opt_string j.reason);
             ("url", `String j.url);
           ])
       js)

let compare jobs base_jobs : Json.t list =
  let index js =
    let idx = Hashtbl.create 64 in
    List.iter
      (fun j ->
        match ocaml_version j with
        | Some v when j.kind = Revdeps ->
            let k = (Option.value ~default:"" j.revdep, v) in
            Hashtbl.replace idx k
              (j :: Option.value ~default:[] (Hashtbl.find_opt idx k))
        | _ -> ())
      js;
    Hashtbl.filter_map_inplace (fun _ l -> Some (List.rev l)) idx;
    idx
  in
  let new_idx = index jobs and base_idx = index base_jobs in
  let keys =
    List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) new_idx [])
  in
  List.filter_map
    (fun ((revdep, v) as k) ->
      let js = Hashtbl.find new_idx k in
      let failing_js = List.filter (fun j -> List.mem j.category failing) js in
      if failing_js = [] then None
      else begin
        let passing_here =
          List.filter (fun j -> j.category = Category.passed) js
        in
        let base = Option.value ~default:[] (Hashtbl.find_opt base_idx k) in
        let base_fail =
          List.filter (fun j -> List.mem j.category failing) base
        in
        let base_pass =
          List.filter (fun j -> j.category = Category.passed) base
        in
        let verdict =
          if base_fail <> [] then
            let same =
              List.exists
                (fun j -> List.exists (fun b -> b.reason = j.reason) base_fail)
                failing_js
            in
            "pre-existing"
            ^ if same then " (same error)" else " (different error)"
          else if passing_here <> [] then "builder-specific"
            (* fails on one builder, passes on another in the same run *)
          else if base_pass <> [] then "NEW"
          else if base <> [] then
            "unknown (baseline: "
            ^ String.concat ","
                (List.sort_uniq compare
                   (List.map (fun j -> Category.to_string j.category) base))
            ^ ")"
          else "no-baseline"
        in
        Some
          (`Assoc
             [
               ("revdep", `String revdep);
               ("ocaml", `String v);
               ("verdict", `String verdict);
               ("new", jobs_json failing_js);
               ( "passes_on",
                 Json.strings
                   (List.map
                      (fun j -> String.concat "/" j.platform)
                      passing_here) );
               ("baseline", jobs_json base);
             ])
      end)
    keys

let render_compare ?(show_all = false) target (info : Json.t)
    (results : Json.t list) =
  let sha = Json.str "sha" info in
  Printf.printf "== %s vs baseline %s: PR #%s commit %s\n" target
    (Json.str "prev" info)
    (match Json.member "pr" info with
    | `Int n -> string_of_int n
    | `String s -> s
    | _ -> "None")
    (Util.prefix_chars sha 12);
  let order =
    [ "NEW"; "builder-specific"; "no-baseline"; "unknown"; "pre-existing" ]
  in
  let verdict r = Json.str "verdict" r in
  let rank r =
    let rec go i = function
      | [] -> List.length order
      | k :: rest ->
          if String.starts_with ~prefix:k (verdict r) then i
          else go (i + 1) rest
    in
    go 0 order
  in
  let counts =
    List.map
      (fun (k, l) -> (k, List.length l))
      (Util.group_by
         (fun r -> List.hd (Util.split_on " (" (verdict r)))
         results)
  in
  let pos k =
    match List.find_index (( = ) k) order with Some i -> i | None -> 99
  in
  let counts =
    List.stable_sort
      (fun (a, _) (b, _) -> Stdlib.compare (pos a) (pos b))
      counts
  in
  print_endline
    ("   "
    ^ String.concat "  "
        (List.map (fun (k, v) -> Printf.sprintf "%s=%d" k v) counts));
  let compact_header = ref false in
  let sorted =
    Util.sort_by
      (fun r -> (rank r, Json.str "revdep" r, Json.str "ocaml" r))
      results
  in
  List.iter
    (fun r ->
      let reason j = Option.value ~default:"" (Json.str_opt "reason" j) in
      if String.starts_with ~prefix:"pre-existing" (verdict r) && not show_all
      then begin
        if not !compact_header then begin
          print_endline
            "\n\
            \  pre-existing (also failed with the previous version; --all for \
             details):";
          compact_header := true
        end;
        let first =
          match Json.list "new" r with x :: _ -> reason x | [] -> ""
        in
        print_endline
          (Util.prefix_chars
             (Printf.sprintf "    %s %s"
                (pad 36
                   (Json.str "revdep" r ^ " / OCaml " ^ Json.str "ocaml" r))
                first)
             200)
      end
      else begin
        Printf.printf "\n  [%s] %s on OCaml %s\n" (verdict r)
          (Json.str "revdep" r) (Json.str "ocaml" r);
        List.iter
          (fun n ->
            Printf.printf "      new  %s %s %s\n"
              (pad 28 (Json.str "platform" n))
              (pad 13 (Json.str "category" n))
              (reason n))
          (Json.list "new" r);
        (match Json.to_str_list (Json.member "passes_on" r) with
        | [] -> ()
        | l -> Printf.printf "      but passes on: %s\n" (String.concat ", " l));
        List.iter
          (fun b ->
            Printf.printf "      base %s %s %s\n"
              (pad 28 (Json.str "platform" b))
              (pad 13 (Json.str "category" b))
              (reason b))
          (Json.list "baseline" r)
      end)
    sorted

(* ------------------------------------------------------------------------ *)
(* Suggestions in words *)

let conf_word r =
  List.hd (String.split_on_char ' ' (Suggestion.str_field r.confidence))

(** 'N failed jobs' when a suggestion stands for several jobs, else ''. *)
let jobs_note r =
  let n = List.length (Suggestion.list r.Suggestion.jobs) in
  if n > 1 then Printf.sprintf "%d failed jobs" n else ""

let sentence_end_re = Rx.v {|[.!?]\s|}

(** [s] on one line, cut to [limit] characters at the end of a sentence when
    there is one in the second half, else with "…" (by md_text). *)
let cut s limit =
  let s = Md.flat s in
  if Util.char_length s <= limit then s
  else
    (* Python: [.!?](?=\s) in s[:limit + 1] *)
    let p = Util.prefix_chars s (limit + 1) in
    let ends =
      List.map (fun m -> Rx.end_ m - 1) (Rx.finditer sentence_end_re p)
    in
    (* Ends in characters: the byte offset of the end of the punctuation. *)
    match Util.last_opt ends with
    | Some e when Util.char_length (String.sub p 0 e) > limit / 2 ->
        String.sub p 0 e
    | _ -> s

let release_date = Rx.v {|release date of (\S+) \((\d{4}-\d\d-\d\d)\)$|}

let compared_text ?year r =
  let cw = match r.compared_with with Some s when s <> "" -> s | _ -> "?" in
  match Rx.match_ release_date cw with
  | Some m ->
      Printf.sprintf "the release date of %s (%s)" (Rx.group m 1)
        (Words.short_date ?year (Some (Rx.group m 2)))
  | None -> cw

(** One candidate of a suggestion, in words: what CI used and what passes. *)
let candidate_text ?year r (c : Suggestion.Candidate.t) =
  let g = Option.value ~default:"None" in
  if c.first_after <> None then
    let after =
      match Rx.match_ release_date (Suggestion.str r.compared_with) with
      | Some m ->
          Printf.sprintf "%s (%s)" (Rx.group m 1)
            (Words.short_date ?year (Some (Rx.group m 2)))
      | None -> compared_text ?year r
    in
    Printf.sprintf "CI used %s; %s is the first release after %s" (g c.failing)
      (g c.first_after) after
  else
    Printf.sprintf "passes with %s on %s, fails with %s" (g c.passing)
      (compared_text ?year r) (g c.failing)

(** 'ca-certs-nss 3.103 no, 3.113.1 yes, 3.101-1 (used by CI) no': whether each
    version defines the identifier, sorted by version, the package named once.
*)
let evidence_text (ev : (string * bool option) list) =
  let vkey_of k =
    match String.index_opt k ' ' with
    | Some _ -> List.nth (String.split_on_char ' ' k) 1
    | None -> k
  in
  let sorted =
    List.stable_sort (fun (a, _) (b, _) -> opam_vcmp (vkey_of a) (vkey_of b)) ev
  in
  let parts = ref [] and name = ref None in
  List.iter
    (fun (k, v) ->
      match v with
      | None -> ()
      | Some v ->
          let k =
            Util.replace k " (installed in failing job)" " (used by CI)"
          in
          let pkg, _, rest = Util.partition k " " in
          parts :=
            Printf.sprintf "%s %s"
              (if Some pkg = !name && rest <> "" then rest else k)
              (if v then "yes" else "no")
            :: !parts;
          name := Some pkg)
    sorted;
  String.concat ", " (List.rev !parts)

let verdict_re = Rx.v {|(\w+)(?: \((.*)\))?$|}

(** 'possible causes (low confidence): dependencies released after …' for the
    candidates of a suggestion. *)
let confidence_text r =
  let c = Suggestion.str_field r.confidence in
  let word, why =
    match Rx.match_ verdict_re c with
    | Some m -> (Rx.group m 1, Rx.group_opt m 2)
    | None ->
        ( (match Suggestion.get r.confidence with Some v -> v | None -> "None"),
          None )
  in
  Printf.sprintf "possible causes (%s confidence)" word
  ^ match why with Some w when w <> "" -> ": " ^ w | _ -> ""

(** Whether the output names the day10 builder (then it is explained once). *)
let mentions_day10 results =
  List.exists
    (fun r ->
      List.exists
        (fun s -> Util.contains s "day10")
        (List.map Suggestion.str
           [ r.Suggestion.job; r.variant; r.error; r.note; r.title ]
        @ Suggestion.list r.jobs))
    results

let day10 =
  "The day10 builder is the newer of opam-repo-ci's two builders; the other \
   one is the main builder."

let opam_path pkg =
  let name, ver = split_pkg (Option.value ~default:"" pkg) in
  if name <> "" && ver <> "" then
    Some (Printf.sprintf "packages/%s/%s.%s/opam" name name ver)
  else None

let strip_nl s = Util.strip ~chars:"\n" s

(** The change of a suggestion as a fenced diff (Markdown). [old] and [new] are
    untrusted (they quote opam files, logs or an LLM answer): only ever shown in
    the code block, whose fence no line of them can close. *)
let diff_block r =
  let new_ = Suggestion.str_field r.Suggestion.new_ in
  let new_lines, hint =
    if String.contains (String.trim new_) '\n' then
      ( Util.splitlines (strip_nl new_),
        "" (* several lines (an LLM answer): no hint *) )
    else
      match Util.split_on "   " new_ with
      | first :: rest ->
          ( [ first ],
            String.concat " "
              (List.filter_map
                 (fun x -> match String.trim x with "" -> None | x -> Some x)
                 rest) )
      | [] -> ([ "" ], "")
  in
  let lines =
    List.map
      (fun l -> "- " ^ l)
      (Util.splitlines (strip_nl (Suggestion.str_field r.old)))
    @ List.map (fun l -> "+ " ^ l) new_lines
  in
  Md.md_fence ~lang:"diff" (String.concat "\n" lines)
  ^ if hint <> "" then "\n<sub>" ^ Md.md_text hint ^ "</sub>" else ""

(** Which versions define the identifier (or the note), as Markdown. *)
let evidence_line ?(limit = 600) r =
  match r.evidence with
  | Some (_ :: _ as ev) ->
      "Defined in each version: " ^ Md.md_text ~limit (evidence_text ev)
  | _ -> Md.md_text ~limit (cut (Suggestion.str r.note) limit)

let version_like_re = Rx.v {|[\w.+~-]{1,40}|}
let name_like_re = Rx.v {|[\w+.-]{1,80}|}

let changelog_details (c : Suggestion.Candidate.t) =
  let text, versions =
    match c.changelog with
    | Some log ->
        ( (Suggestion.str log.text ^ if log.cut = Some true then "\n…" else ""),
          Suggestion.list log.versions )
    | None -> ("", [])
  in
  (* The versions come from the changelog's headings: keep version-like ones only. *)
  let versions =
    List.filter (fun v -> Rx.fullmatch version_like_re v <> None) versions
  in
  (* The summary is in an HTML block (no Markdown, no escaping): a package name or nothing. *)
  let dep =
    match c.dep with
    | Some d when Rx.fullmatch name_like_re d <> None -> d
    | _ -> "a dependency"
  in
  "<details><summary>"
  ^ Printf.sprintf "Changelog of %s: %s" dep (String.concat ", " versions)
  ^ "</summary>\n\n"
  ^ Md.md_fence ~lang:"text" text
  ^ "\n\n</details>"

let aside_re = Rx.v {|\s{2,}\(|}
let jobs_re = Rx.v {|\d+ jobs?|}

let render_comment_counts ?(placed = 0) ?(placed_lines = 0) ~unfinished ~njobs
    ~unanalysed sha (results : Suggestion.t list) =
  let by cls =
    List.filter
      (fun r -> (not r.Suggestion.placed) && Suggestion.str r.class_ = cls)
      results
  in
  let sha_ok = Rx.fullmatch Md.sha_re sha <> None in
  let ci =
    if sha_ok then Printf.sprintf "[CI results](%s%s)" Net.base sha
    else "CI results"
  in
  let out = ref [] in
  let add l = out := List.rev_append l !out in
  add
    [
      "### Automated triage of the opam-repo-ci failures";
      "";
      (Printf.sprintf "From the %s for commit %s. Nothing was rebuilt: these "
         ci
         (if sha_ok then Md.md_code (Util.prefix_chars sha 12) else "?")
      ^ "suggestions come from the CI logs, the documentation of each package \
         version on ocaml.org, and "
      ^ "the opam files. Please check them before applying them."
      ^ if mentions_day10 results then " " ^ day10 else "");
      "";
    ];
  if unfinished <> 0 then
    add
      [
        Printf.sprintf
          "**CI is still running**: %d of %s %s not finished, so these are the \
           failures so far."
          unfinished
          (Words.counted njobs "job")
          (if unfinished = 1 then "is" else "are");
        "";
      ];
  if unanalysed <> 0 then begin
    let n = unanalysed in
    add
      [
        Printf.sprintf "**%s %s not analysed.** See the CI results."
          (Words.counted n "more failure")
          (if n = 1 then "was" else "were");
        "";
      ]
  end;
  let fixes = by "fix" in
  if placed > 0 then
    add
      [
        Printf.sprintf
          "%d suggested change%s attached to %d line%s of the diff (\"Commit \
           suggestion\" applies each one)."
          placed
          (if placed > 1 then "s are" else " is")
          placed_lines
          (if placed_lines > 1 then "s" else "");
        "";
      ];
  if fixes <> [] then begin
    add
      [
        Printf.sprintf "#### %s changes (%d)"
          (if placed > 0 then "Other suggested" else "Suggested")
          (List.length fixes);
        "";
      ];
    List.iter
      (fun r ->
        let path = opam_path (Suggestion.get r.Suggestion.failing) in
        let meta =
          [
            Md.md_text (Titles.title_of r);
            Md.md_text (conf_word r) ^ " confidence";
            jobs_note r;
          ]
        in
        let code =
          Md.md_code
            (match path with
            | Some p -> p
            | None -> Suggestion.str_field r.Suggestion.failing)
        in
        add
          [
            Printf.sprintf "**%s**: " (if code = "" then "?" else code)
            ^ String.concat " · " (List.filter (fun x -> x <> "") meta);
          ];
        if Suggestion.nonempty r.error then
          add
            [ "> " ^ Md.md_text ~limit:300 (cut (Suggestion.str r.error) 300) ];
        if Suggestion.list r.candidates <> [] then begin
          let cands = Suggestion.list r.candidates in
          add
            [
              Printf.sprintf "Possible causes, compared with %s:"
                (Md.md_text (compared_text r));
            ];
          add
            [
              Md.md_fence ~lang:"diff"
                (String.concat "\n"
                   (List.map
                      (fun c ->
                        "+ "
                        ^ Md.flat
                            (List.hd
                               (Util.split_on "   "
                                  (Suggestion.str_field
                                     c.Suggestion.Candidate.new_))))
                      cands));
            ];
          List.iter
            (fun (c : Suggestion.Candidate.t) ->
              match c.changelog with
              | Some log when not (Suggestion.changelog_is_empty log) ->
                  add [ changelog_details c ]
              | _ -> ())
            cands
        end
        else begin
          if Suggestion.nonempty_field r.Suggestion.new_ then
            add [ diff_block r ];
          if r.alternative <> None then begin
            (* also without [new]: a change described in the note. Python: \s{2,}(?=\() *)
            let a = Suggestion.str r.alternative in
            let alt =
              match Rx.search aside_re a with
              | Some m ->
                  [
                    String.sub a 0 (Rx.start m);
                    String.sub a
                      (Rx.end_ m - 1)
                      (String.length a - Rx.end_ m + 1);
                  ]
              | None -> [ a ]
            in
            add
              [
                ("Or "
                ^ Md.md_code (List.hd alt)
                ^ match alt with [ _; b ] -> " " ^ Md.md_text b | _ -> "");
              ]
          end
        end;
        let ev = evidence_line r in
        if ev <> "" then add [ "<sub>" ^ ev ^ "</sub>" ];
        add [ "" ])
      fixes
  end
  else if by "workaround" <> [] && placed = 0 then
    add
      [
        "No fix found. The workarounds below skip the failing platforms or \
         tests.";
        "";
      ]
  else if placed = 0 then add [ "No change to this PR's opam files found."; "" ];
  let details title rs fmt =
    if rs <> [] then begin
      add
        [
          "<details>";
          Printf.sprintf "<summary>%s (%d)</summary>" title (List.length rs);
          "";
        ];
      List.iter (fun r -> add (fmt r @ [ "" ])) rs;
      add [ "</details>"; "" ]
    end
  in
  let head ?(where = false) r =
    (* "- **pkg** · title · fix in X · 3 failed jobs · pre-existing · experimental platform" *)
    let title = Md.md_text (Titles.title_of r) in
    let parts =
      if Suggestion.nonempty_field r.Suggestion.failing then
        [
          Printf.sprintf "**%s**"
            (Md.md_text (Suggestion.str_field r.Suggestion.failing));
          title;
        ]
      else
        (* The job, unless it is only a count ("4 jobs"), which the number of failed jobs says. *)
        let job =
          match r.job with
          | Some j when j <> "" -> j
          | _ -> describe_job (Suggestion.str r.Suggestion.variant)
        in
        [
          Printf.sprintf "**%s**" title;
          (if Rx.fullmatch jobs_re job <> None then "" else Md.md_text job);
        ]
    in
    let w =
      if where && r.rule <> Some Rerun then Titles.where_text r else None
      (* "Jobs to rerun" says it *)
    in
    let parts =
      parts
      @
      match w with
      | Some w when w <> "" && w <> "this PR" ->
          [ Md.md_text (Titles.fix_label w) ]
      | _ -> []
    in
    let parts = parts @ [ jobs_note r ] in
    let parts =
      parts
      @
      if r.preexisting <> None && not (r.preexisting_unverified = Some true)
      then [ "pre-existing" ]
      else []
    in
    let parts =
      parts
      @
      if r.Suggestion.experimental = Some true then [ "experimental platform" ]
      else []
    in
    "- " ^ String.concat " · " (List.filter (fun p -> p <> "") parts)
  in
  let job_links ?(n = 10) r =
    (* For the entries whose content is the list of jobs (reruns, unexplained failures). *)
    if not (Titles.has_rule [ Rerun; Unrecognised ] r) then ""
    else
      let js = Suggestion.list r.Suggestion.jobs in
      let links =
        List.map
          (fun v ->
            match Md.ci_job_url Net.base sha v with
            | Some url ->
                Printf.sprintf "[%s](%s)" (Md.md_text (describe_job v)) url
            | None -> Md.md_text (describe_job v))
          (Util.take n js)
      in
      if js = [] then ""
      else
        "<br>Jobs: " ^ String.concat ", " links
        ^
        if List.length js > n then
          Printf.sprintf " and %d more" (List.length js - n)
        else ""
  in
  let note r =
    if Suggestion.nonempty r.note then
      "<br>" ^ Md.md_text ~limit:600 (cut (Suggestion.str r.note) 600)
    else ""
  in
  let error r =
    if Suggestion.nonempty r.error then
      ": " ^ Md.md_text ~limit:300 (cut (Suggestion.str r.error) 300)
    else ""
  in
  let log_link r =
    match Md.safe_ci_url r.Suggestion.url with
    | Some url -> Printf.sprintf " ([log](%s))" url
    | None -> ""
  in
  details "Workarounds: skip some platforms or tests" (by "workaround")
    (fun r ->
      [ head r ^ error r ^ note r ]
      @
      if Suggestion.nonempty_field r.Suggestion.new_ then
        [ "  " ^ Util.replace (diff_block r) "\n" "\n  " ]
      else []);
  details "Fix goes elsewhere" (by "elsewhere") (fun r ->
      [
        head ~where:true r ^ error r
        ^ (if Suggestion.nonempty_field r.Suggestion.new_ then
             "<br>Suggested change: "
             ^ Md.md_code
                 (List.hd
                    (Util.split_on "   "
                       (Suggestion.str_field r.Suggestion.new_)))
           else "")
        ^ note r ^ job_links r;
      ]);
  details "Other failures (no change suggested)" (by "explanation") (fun r ->
      [ head r ^ error r ^ note r ^ log_link r ^ job_links r ]);
  add [ "<sub>Generated by opam-ci-triage</sub>" ];
  String.concat "\n" (List.rev !out)

let render_comment ?placed ?placed_lines sha (data : data) results =
  render_comment_counts ?placed ?placed_lines ~unfinished:data.unfinished
    ~njobs:(List.length data.jobs) ~unanalysed:data.unanalysed sha results

let hunk_header_re = Rx.v {|@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@|}

(** Line numbers (new side) a review comment can be attached to. *)
let commentable_lines patch =
  let lines = ref [] and nw = ref 0 in
  List.iter
    (fun l ->
      match Rx.match_ hunk_header_re l with
      | Some m -> nw := int_of_string (Rx.group m 1)
      | None ->
          if String.starts_with ~prefix:"-" l then ()
          else if
            String.starts_with ~prefix:"+" l || String.starts_with ~prefix:" " l
          then begin
            lines := !nw :: !lines;
            incr nw
          end)
    (Util.splitlines (Option.value ~default:"" patch));
  !lines

let leading_space_re = Rx.v {|\s*|}
let field_name_re = Rx.v {|([a-z][\w-]*):|}

(** [(line number, replacement)] for a fix on the file [text], or []. *)
let line_edits r text =
  let lines = Array.of_list (Util.splitlines (Option.value ~default:"" text)) in
  let nl = Array.length lines in
  let new_ =
    String.trim
      (List.hd (Util.split_on "   " (Suggestion.str_field r.Suggestion.new_)))
  in
  let indent l =
    match Rx.match_ leading_space_re l with Some m -> Rx.group0 m | None -> ""
  in
  let find_index p =
    let rec go i =
      if i >= nl then None else if p lines.(i) then Some i else go (i + 1)
    in
    go 0
  in
  if r.rule = Some Lint && String.starts_with ~prefix:"%{_:" new_ then begin
    (* warning 41: every %{pkg:...}% *)
    let name = fst (split_pkg (Suggestion.str_field r.Suggestion.failing)) in
    let pat = "%{" ^ name ^ ":" in
    List.filter_map Fun.id
      (List.mapi
         (fun i l ->
           if Util.contains l pat then Some (i + 1, Util.replace l pat "%{_:")
           else None)
         (Array.to_list lines))
  end
  else if
    new_ = ""
    || Util.contains (List.hd (String.split_on_char '"' new_)) "<"
    || Util.contains new_ "<hash>"
    || Util.contains new_ "<URL"
    || String.starts_with ~prefix:"in " new_
    || String.starts_with ~prefix:"x-ci-" new_
    || String.starts_with ~prefix:"available:" new_
  then []
  else if Suggestion.nonempty_field r.old then begin
    let old = String.trim (Suggestion.str_field r.old) in
    match find_index (fun l -> String.trim l = old) with
    | Some i -> [ (i + 1, indent lines.(i) ^ new_) ]
    | None -> []
  end
  else if String.starts_with ~prefix:"\"" new_ then
    (* new dependency: after the last one *)
    begin match
      find_index (fun l -> String.starts_with ~prefix:"depends:" l)
    with
    | None -> []
    | Some start -> (
        let rec go i =
          if i >= nl then None
          else if String.trim lines.(i) = "]" then Some i
          else go (i + 1)
        in
        let last =
          match go (start + 1) with
          | Some e when e <> 0 -> Some (e - 1)
          | _ -> None
        in
        match last with
        | Some last when last > start ->
            [ (last + 1, lines.(last) ^ "\n" ^ indent lines.(last) ^ new_) ]
        | _ -> [])
    end
  else
    match Rx.match_ field_name_re new_ with
    | Some _ ->
        (* new field: after a related one *)
        let count c l = List.length (String.split_on_char c l) - 1 in
        List.find_map
          (fun f ->
            match
              find_index (fun l ->
                  String.starts_with ~prefix:(f ^ ":") l
                  && count '[' l = count ']' l)
            with
            | Some i -> Some [ (i + 1, lines.(i) ^ "\n" ^ new_) ]
            | None -> None)
          [ "homepage"; "bug-reports"; "maintainer"; "synopsis" ]
        |> Option.value ~default:[]
    | None -> []

let build_review ~(raw : string -> string option) sha (data : data)
    (results : Suggestion.t list) : Json.t =
  let pr = data.pr in
  let head =
    match pr with
    | Some pr ->
        Net.gh_api ~cache:false (Printf.sprintf "repos/%s/pulls/%d" Net.repo pr)
    | None -> None
  in
  let head =
    match head with
    | Some (`Assoc _ as h) -> h
    | _ -> Util.die "cannot build a review: unknown PR (pass --pr N)"
  in
  let head_sha = Json.str "sha" (Json.member "head" head) in
  if head_sha <> sha then
    Printf.eprintf
      "note: the CI results are for %s but the PR head is %s: line comments \
       are skipped (they would point at the wrong lines)\n\
       %!"
      (Util.prefix_chars sha 12)
      (Util.prefix_chars head_sha 12);
  let pr = Option.get pr in
  let files =
    match
      Net.gh_api ~cache:false
        (Printf.sprintf "repos/%s/pulls/%d/files?per_page=100" Net.repo pr)
    with
    | Some (`List l) -> List.map (fun f -> (Json.str "filename" f, f)) l
    | _ -> []
  in
  let comments = ref [] in
  List.iter
    (fun r ->
      if
        Suggestion.str r.class_ = "fix"
        && head_sha = sha
        (* An LLM answer is never a one-click suggestion (it stays in the
            summary, as a diff to check), and a change is one opam line. *)
        && r.rule <> Some Llm
        && not
             (List.exists
                (fun v ->
                  let v = Suggestion.str_field v in
                  String.contains v '\r' || String.contains v '\n')
                [ r.old; r.Suggestion.new_ ])
      then
        match opam_path (Suggestion.get r.Suggestion.failing) with
        | Some path when List.mem_assoc path files ->
            let ok =
              commentable_lines (Json.str_opt "patch" (List.assoc path files))
            in
            let text =
              raw
                (Printf.sprintf "https://raw.githubusercontent.com/%s/%s/%s"
                   Net.repo sha path)
            in
            let edits =
              List.filter (fun (n, _) -> List.mem n ok) (line_edits r text)
            in
            if edits <> [] then begin
              r.placed <- true;
              let why =
                Printf.sprintf "**%s** · %s confidence"
                  (Md.md_text (Titles.title_of r))
                  (Md.md_text (conf_word r))
              in
              let why =
                if Suggestion.nonempty r.error then
                  why ^ ": "
                  ^ Md.md_text ~limit:300 (cut (Suggestion.str r.error) 300)
                else why
              in
              let ev = evidence_line ~limit:300 r in
              List.iter
                (fun (n, replacement) ->
                  comments :=
                    `Assoc
                      [
                        ("path", `String path);
                        ("line", `Int n);
                        ("side", `String "RIGHT");
                        ( "body",
                          `String
                            (why ^ "\n"
                            ^ Md.md_fence ~lang:"suggestion" replacement
                            ^ if ev <> "" then "\n<sub>" ^ ev ^ "</sub>" else ""
                            ) );
                      ]
                    :: !comments)
                edits
            end
        | _ -> ())
    results;
  let comments = List.rev !comments in
  let body =
    render_comment
      ~placed:
        (List.length (List.filter (fun (r : Suggestion.t) -> r.placed) results))
      ~placed_lines:(List.length comments) sha data results
  in
  `Assoc
    [
      ("commit_id", `String sha);
      ("event", `String "COMMENT");
      ("body", `String body);
      ("comments", `List comments);
    ]

let render_suggestions (results : Suggestion.t list) =
  if mentions_day10 results then print_endline day10;
  List.iter
    (fun r ->
      print_endline (String.make 100 '=');
      (* A lint error's job is "opam lint", and a pattern's "N packages": the title says it. *)
      print_endline
        (Titles.title_of r
        ^
        if
          Suggestion.str r.Suggestion.variant = "(lint)" || Titles.is_systemic r
        then ""
        else
          ": "
          ^
          match r.job with
          | Some j when j <> "" -> j
          | _ -> describe_job (Suggestion.str r.Suggestion.variant));
      let jobs = Suggestion.list r.Suggestion.jobs in
      if Titles.has_rule [ Rerun; Unrecognised ] r then
        List.iter
          (fun v -> Printf.printf "  job             : %s\n" (describe_job v))
          jobs
      else if List.length jobs > 1 then begin
        let others =
          List.filter (fun v -> v <> Suggestion.str r.Suggestion.variant) jobs
        in
        (* A pattern's variant is a label, not one of its jobs: then all are listed. *)
        let label =
          if List.length others < List.length jobs then
            Printf.sprintf "same failure on : %s: "
              (Words.counted (List.length others) "other job")
          else Printf.sprintf "failed jobs     : %d: " (List.length jobs)
        in
        print_endline
          ("  " ^ label
          ^ String.concat ", " (List.map describe_job (Util.take 5 others))
          ^
          if List.length others > 5 then
            Printf.sprintf " and %d more" (List.length others - 5)
          else "")
      end;
      let w = Titles.where_text r in
      if Suggestion.nonempty_field r.Suggestion.failing then
        print_endline
          (Printf.sprintf "  failing package : %s"
             (Suggestion.str_field r.Suggestion.failing)
          ^ (match w with
            | Some w when w <> "" -> "   " ^ Titles.fix_label w
            | _ -> "")
          ^
          if r.Suggestion.experimental = Some true then
            "   (experimental platform: does not block)"
          else "");
      if Suggestion.nonempty r.error then
        Printf.printf "  error           : %s\n" (Suggestion.str r.error);
      if Suggestion.nonempty r.source then
        Printf.printf "  looked up in    : %s\n" (Suggestion.str r.source);
      (match r.evidence with
      | Some (_ :: _ as ev) ->
          Printf.printf "  defined in      : %s\n" (evidence_text ev)
      | _ -> ());
      List.iter
        (fun t -> Printf.printf "  tried           : %s\n" t)
        (Suggestion.list r.tried);
      if Suggestion.nonempty_field r.Suggestion.new_ then begin
        let label =
          match Suggestion.str r.class_ with
          | "workaround" -> "workaround"
          | "elsewhere" -> "fix goes elsewhere"
          | _ -> "suggested change"
        in
        Printf.printf "  %s (%s confidence):\n" label (conf_word r);
        if Suggestion.nonempty_field r.old then
          Printf.printf "    - %s\n" (Suggestion.str_field r.old);
        Printf.printf "    + %s\n" (Suggestion.str_field r.Suggestion.new_);
        if r.alternative <> None then
          Printf.printf "    or %s\n" (Suggestion.str r.alternative)
      end
      else if r.alternative <> None then
        (* a change described in the note, and an alternative *)
        Printf.printf "  alternative     : %s\n" (Suggestion.str r.alternative);
      if Suggestion.list r.candidates <> [] then
        Printf.printf "  %s\n" (confidence_text r);
      List.iter
        (fun (c : Suggestion.Candidate.t) ->
          Printf.printf "    %s: %s\n"
            (Option.value ~default:"None" c.dep)
            (candidate_text r c);
          (match Suggestion.get c.old with
          | Some old when old <> "" -> Printf.printf "    - %s\n" old
          | _ -> ());
          Printf.printf "    + %s\n" (Suggestion.str_field c.new_);
          match c.changelog with
          | Some log when not (Suggestion.changelog_is_empty log) ->
              let lines = Util.splitlines (Suggestion.str log.text) in
              Printf.printf
                "      changelog (%s, %s), written by its authors:\n"
                (match Suggestion.get log.file with
                | Some f when f <> "" -> f
                | _ -> "?")
                (String.concat ", " (Suggestion.list log.versions));
              List.iter
                (fun line -> Printf.printf "        | %s\n" line)
                (Util.take 12 lines);
              if List.length lines > 12 || log.cut = Some true then
                print_endline "        | …"
          | _ -> ())
        (Suggestion.list r.candidates);
      if Suggestion.list r.also_newer <> [] then
        Printf.printf "  also released after it (not ranked): %s\n"
          (String.concat ", " (Suggestion.list r.also_newer));
      if Suggestion.nonempty r.note then
        Printf.printf "  note            : %s\n" (Suggestion.str r.note);
      if r.llm_prompt <> None then
        print_endline
          ("  LLM prompt (dry run):\n    "
          ^ Util.replace (Suggestion.str r.llm_prompt) "\n" "\n    "))
    results
