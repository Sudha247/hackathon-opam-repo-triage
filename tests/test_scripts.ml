(* Tests of the scripts: the digest of the LLM answers (scripts/llm_digest.ml),
   the accuracy report (scripts/accuracy_report.ml). *)

open Triage

(* A suggestion from its JSON members. *)
let sugg kv = Suggestion.of_json (`Assoc kv)

module D = Scripts.Llm_digest

let base_url = "https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/"

let hostile =
  "@ocaml/maintainers ![x](https://evil.example/x.png) <img src=x> see #123, \
   ocaml/dune#4 GH-5 www.evil.example"

let allowed_tags = Rx.v {|</?(details|summary|sub|br)>|}
let trusted_link = Rx.v {|\]\(https://opam\.ci\.ocaml\.org/[\w.~,+%/=-]+\)|}
let fence_re = Rx.v {|\s*(`{3,})|}

(** What GitHub would parse as Markdown in [md]: without fenced code blocks,
    code spans and backslash-escaped characters (a rough CommonMark reading). *)
let outside_code md =
  let out = ref [] and fence = ref None in
  List.iter
    (fun line ->
      let m = Rx.match_ fence_re line in
      match !fence with
      | Some f -> (
          match m with
          | Some m
            when String.trim line = Rx.group m 1
                 && String.length (Rx.group m 1) >= String.length f ->
              fence := None
          | _ -> ())
      | None -> (
          match m with
          | Some m -> fence := Some (Rx.group m 1)
          | None -> out := line :: !out))
    (String.split_on_char '\n' md);
  let text = String.concat "\n" (List.rev !out) in
  let n = String.length text in
  let res = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if text.[!i] = '\\' && !i + 1 < n && not (Util.is_alnum text.[!i + 1]) then
      i := !i + 2
    else if text.[!i] = '`' then begin
      let k = ref 0 in
      while !i + !k < n && text.[!i + !k] = '`' do
        incr k
      done;
      let k = !k in
      (* (?<!`) run (?!`) after the opening run *)
      let rec find j =
        if j + k > n then None
        else if
          String.sub text j k = String.make k '`'
          && text.[j - 1] <> '`'
          && (j + k >= n || text.[j + k] <> '`')
        then Some (j + k)
        else find (j + 1)
      in
      match find (!i + k) with
      | Some e -> i := e
      | None ->
          Buffer.add_string res (String.make k '`');
          i := !i + k
    end
    else (
      Buffer.add_char res text.[!i];
      incr i)
  done;
  Buffer.contents res

let assert_inert md =
  let rest =
    Rx.sub allowed_tags "" (Rx.sub trusted_link "]" (outside_code md))
  in
  List.iter
    (fun bad ->
      if Rx.test (Rx.v bad) rest then
        Alcotest.failf "%S in:\n%s\n--- from:\n%s" bad rest md)
    [ {|@\w|}; {|#\d|}; "<"; {|\]\(|}; "://"; {|www\.|}; {|GH-\d|} ]

let contains s sub =
  Alcotest.(check bool)
    (Printf.sprintf "%S in the report" sub)
    true (Util.contains s sub)

let https_github_io_re =
  Rx.v {|\[`#\d+`\]\(https://x\.github\.io/triage/#pr-\d+\)|}

let test_llm_digest () =
  let r =
    [
      ("rule", `String "llm");
      ("log_error", `String ("Error: `x` " ^ hostile));
      ("diagnosis", `String hostile);
      ("failing", `String hostile);
      ("fix_in", `String hostile);
      ("confidence", `String hostile);
      ("new", `String ("a ``` " ^ hostile));
      ("url", `String ("https://evil.example/" ^ hostile));
    ]
  in
  let with_ kv = sugg (r @ kv) in
  let good =
    with_ [ ("url", `String (base_url ^ String.make 40 'a' ^ "/variant/v")) ]
  in
  let items =
    [
      (Some 123, sugg r);
      (Some 456, good);
      (Some 789, with_ [ ("log_error", `String ("other " ^ hostile)) ]);
    ]
  in
  let md = ref "" in
  List.iter
    (fun page ->
      md := D.report ?page items 2;
      assert_inert (Rx.sub https_github_io_re "" !md);
      Alcotest.(check bool)
        "no PR link" false
        (Util.contains !md "github.com/ocaml/opam-repository/pull"))
    [ None; Some "https://x.github.io/triage/" ];
  contains (D.report items 2) "`ocaml/opam-repository#123`";
  contains !md "[`#123`](https://x.github.io/triage/#pr-123)";
  contains !md ("[log](" ^ base_url)

let test_llm_digest_groups () =
  let eq = Alcotest.(check string) in
  (* Numbers, not the digits of names; N.N is not a module path; the word
     after "package" is not a name (a name is quoted). *)
  eq "collect2" "collect2: error: ld returned N exit status"
    (D.masked "collect2: error: ld returned 1 exit status");
  eq "glib" {|conf-glib-N.N failed at "pkg-config glib-N.N" on msys2|}
    (D.masked {|conf-glib-2.1 failed at "pkg-config glib-2.0" on msys2|});
  eq "ImageMagick" "no ImageMagick N development package is installed"
    (D.masked "no ImageMagick 6 development package is installed");
  eq "Conv" "Unbound value x" (D.masked "Unbound value Conv.output");
  (* One answer repeated on several jobs or packages of a PR counts once (#29110: 5 times). *)
  let a () =
    sugg
      [
        ("rule", `String "llm");
        ("log_error", `String "llvm-config: error: missing: /usr/lib/x.a");
        ("diagnosis", `String "d");
        ("failing", `String "conf-llvm-static.20");
        ("confidence", `String "medium");
      ]
  in
  let b = a () in
  b.Suggestion.failing <- Set "b.1";
  let md =
    D.report (List.init 5 (fun _ -> (Some 29110, a ())) @ [ (Some 1, b) ]) 2
  in
  Alcotest.(check bool)
    (Util.prefix_chars md 80) true
    (String.starts_with
       ~prefix:"2 LLM answers in 2 PRs (2 medium confidence), in 1 groups" md)

let test_masked_quotes () =
  let eq = Alcotest.(check string) in
  (* Quoted names, also between typographic quotes; not across spaces. *)
  eq "typographic" "undefined reference to '\xe2\x80\xa6'"
    (D.masked "undefined reference to \xe2\x80\x98foo_bar\xe2\x80\x99");
  eq "spaces" "a 'b c' d" (D.masked "a 'b c' d");
  eq "N.N" "version N.N.N of M.x" (D.masked "version 1.2.3 of Foo.Bar")

(* The page's data read and printed back: nothing is lost, the members of
   another name or type included (as stored by another version). *)
let test_page_data_round_trip () =
  let module P = Scripts.Page_data in
  let rec sorted (v : Json.t) : Json.t =
    match v with
    | `Assoc kv ->
        `Assoc (List.sort compare (List.map (fun (k, v) -> (k, sorted v)) kv))
    | `List l -> `List (List.map sorted l)
    | v -> v
  in
  let same msg (v : Json.t) (w : Json.t) =
    Alcotest.(check string) msg (Json.dumps (sorted v)) (Json.dumps (sorted w))
  in
  let entry =
    Json.parse
      {|{"number": 1, "title": null, "draft": false, "author": null, "labels": ["a"],
         "ci_state": null, "ci_state_now": null, "ci_unfinished": null, "seconds": 1.5,
         "result": {"sha": "s", "pr": null, "suggestions": [{"rule": "x-new", "where_kind": "elsewhere"}],
                    "errors": {"mcp-errors": 1}, "future": [1]},
         "jobs": {"total": 3, "failed": {"build": 1, "weird": 2},
                  "failing_revdeps": {"r.1": [{"variant": "v", "category": null, "target": null, "url": "u"}]}},
         "since_base": {"sha": "b", "result": {"suggestions": []}},
         "since_last_push": {"previous_sha": "b", "fixed": [], "new": ["x"], "still": 2},
         "last_error_ci_state": null, "llm_calls": null, "attempts": "two", "unknown": {"x": 1}}|}
  in
  same "entry" entry (P.Entry.to_json (P.Entry.of_json entry));
  let e = P.Entry.of_json entry in
  Alcotest.(check bool) "wrong type" true (e.attempts = None);
  let data =
    `Assoc
      [
        ("generated", `String "g");
        ("limit", `Null);
        ("llm", Json.parse {|{"enabled": true, "calls": 0, "off": null}|});
        ( "run",
          Json.parse
            {|{"github_api_calls": 1, "tool_requests": {"http": 2}, "analysed": 1, "note": "x"}|}
        );
        ("prs", `List [ entry ]);
        ("passing", `List [ Json.parse {|{"number": 2, "draft": true}|} ]);
        ("future", `Bool true);
      ]
  in
  same "data" data (P.to_json (P.of_json data));
  let state =
    `Assoc [ ("updated", `String "u"); ("prs", `Assoc [ ("1", entry) ]) ]
  in
  same "state" state (P.State.to_json (P.State.of_json state))

(* accuracy-report on an evaluation, after [prev] (the history's latest):
   its exit code, its report, and the lines of history.jsonl. *)
let accuracy_report ~prev cur =
  let dir = Filename.temp_dir "test-accuracy" "" in
  let write f v =
    Out_channel.with_open_bin (Filename.concat dir f) (fun oc ->
        output_string oc (Json.dumps v))
  in
  write "latest.json" prev;
  Out_channel.with_open_bin (Filename.concat dir "history.jsonl") (fun oc ->
      output_string oc
        "{\"date\": \"2026-11-23\", \"prs\": 1, \"hit\": 2, \"scored\": 2}\n");
  write "cur.json" cur;
  let out =
    Proc.run
      [
        Filename.concat
          (Filename.dirname Sys.executable_name)
          "../scripts/bin/accuracy_report.exe";
        Filename.concat dir "cur.json";
        "--history";
        dir;
      ]
  in
  let history =
    List.map Json.parse
      (Util.splitlines
         (In_channel.with_open_bin
            (Filename.concat dir "history.jsonl")
            In_channel.input_all))
  in
  (out.code, out.stdout, history)

let test_accuracy_scoring_break () =
  (* A run scored the new way after one scored the old way (27 November 2026:
     the review bot's changes, new archives, alternatives): not compared, no
     failure on the precision drop nor on the lost hit; the history stays one
     JSON object per line. *)
  let row hit =
    Json.parse
      (Printf.sprintf
         {|{"pr": 1, "initial": "a", "suggestions": [{"package": "p.1", "rule": "r", "hit": %b, "tokens": ["dep:x"]}]}|}
         hit)
  in
  let eval totals hit =
    `Assoc [ ("totals", Json.parse totals); ("prs", `List [ row hit ]) ]
  in
  let prev =
    eval
      {|{"hit": 2, "scored": 2, "fix_covered": 1, "fix_tokens": 2, "health_hit": 0, "health_scored": 0}|}
      true
  in
  let cur =
    eval
      {|{"scoring": 3, "hit": 0, "scored": 6, "fix_covered": 0, "fix_tokens": 2, "health_hit": 0, "health_scored": 0}|}
      false
  in
  let code, report, history = accuracy_report ~prev cur in
  Alcotest.(check int) ("exit code: " ^ report) 0 code;
  Alcotest.(check bool)
    "break said" true
    (Util.contains report "scored differently (scoring 2, this run 3");
  Alcotest.(check (list int))
    "history" [ 2; 0 ]
    (List.map (fun h -> Json.to_int (Json.member "hit" h)) history);
  Alcotest.(check bool)
    "scoring recorded" true
    (Json.member "scoring" (List.nth history 1) = `Int 3);
  (* Two runs scored the same way: the regression fails the check. *)
  let prev =
    eval
      {|{"scoring": 3, "hit": 2, "scored": 2, "fix_covered": 1, "fix_tokens": 2}|}
      true
  in
  let code, report, _ = accuracy_report ~prev cur in
  Alcotest.(check int) ("exit code: " ^ report) 1 code;
  (* 3 then 4 (advice scored): a break, which says what changed *)
  let cur4 =
    eval
      {|{"scoring": 4, "hit": 0, "scored": 6, "fix_covered": 0, "fix_tokens": 2}|}
      false
  in
  let code, report, _ = accuracy_report ~prev cur4 in
  Alcotest.(check int) ("exit code: " ^ report) 0 code;
  Alcotest.(check bool)
    "break 3 -> 4" true
    (Util.contains report
       "scoring 3, this run 4; since 28 November 2026 a suggestion that only \
        gives advice")

(* evaluate-closed-prs scores an advice-only suggestion when its advice
   names the field to change (scoring 4): #31292 elpi-json's
   test-needs-other-package advice is about the test command, which its
   author changed. *)
let test_scoring_advice () =
  let module Sc = Scripts.Scoring in
  let sugg fields = Triage.Suggestion.make fields in
  let toks s = Sc.S.elements (Sc.suggestion_tokens s) in
  let runtest =
    sugg
      [
        `Rule Triage.Rule.Test_needs_other_package;
        `Advice
          "Run only elpi-json's tests: give the test command its directories, \
           as in `[\"dune\" \"runtest\" \"-p\" name \"-j\" jobs \"DIR\"] \
           {with-test}`, or remove the test command.";
      ]
  in
  Alcotest.(check bool) "runtest scored" true (Sc.has_change runtest);
  Alcotest.(check (list string)) "runtest" [ "field:runtest" ] (toks runtest);
  Alcotest.(check (list string))
    "author's runtest line" [ "field:runtest" ]
    (Sc.S.elements
       (Sc.line_tokens {|  ["dune" "runtest" "-p" name "-j" jobs] {with-test}|}));
  let depexts =
    sugg [ `Advice "Add depexts for cygwin, msys2 to conf-llvm-static" ]
  in
  Alcotest.(check (list string)) "depexts" [ "field:depexts" ] (toks depexts);
  let pkgconf =
    sugg
      [
        `Advice
          "On Windows, conf-pkg-config provides `pkgconf`: x should run \
           `pkgconf` on Windows, or exclude cygwin (`x-ci-accept-failures` or \
           `available`).";
      ]
  in
  Alcotest.(check (list string))
    "fields named"
    [ "field:available"; "field:x-ci-accept-failures" ]
    (toks pkgconf);
  let other = sugg [ `Advice "Make the test lighter, or `pkgconf` it." ] in
  Alcotest.(check bool) "no field named" false (Sc.has_change other);
  (* with a change, the advice adds nothing *)
  let both =
    sugg
      [
        `New (Some {|x-ci-accept-failures: ["alpine-3.24"]|});
        `Advice "Add the missing depext for alpine to x, or exclude alpine";
      ]
  in
  Alcotest.(check (list string))
    "new only"
    [ "field:x-ci-accept-failures" ]
    (toks both)

let () =
  Alcotest.run "scripts"
    [
      ( "llm_digest",
        [
          Alcotest.test_case "report" `Quick test_llm_digest;
          Alcotest.test_case "groups" `Quick test_llm_digest_groups;
          Alcotest.test_case "masked quotes" `Quick test_masked_quotes;
        ] );
      ( "page_data",
        [ Alcotest.test_case "round trip" `Quick test_page_data_round_trip ] );
      ( "accuracy_report",
        [
          Alcotest.test_case "scoring break" `Quick test_accuracy_scoring_break;
          Alcotest.test_case "scoring advice" `Quick test_scoring_advice;
        ] );
    ]
