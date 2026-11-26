(* scripts/triage_open_prs: which PRs are analysed again, retries,
   timeouts, GitHub errors, shared failures and stale labels (the Python
   tests McpFailureTest.test_reuse, PageRunTest, RefreshTest, PageMainTest
   and parts of PageTest). *)

open Triage
module T = Scripts.Triage_open_prs

let json =
  Alcotest.testable (fun f v -> Format.pp_print_string f (Json.dumps v)) ( = )

let j s = Json.parse s
let d s = Dict.of_json (j s)
let triple = Alcotest.(triple bool bool bool)

let iso_at t =
  let t = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d+00:00" (t.tm_year + 1900)
    (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

(* dict(d, k=v) *)
let with_ (x : Dict.t) kv =
  let y = Dict.copy x in
  Dict.update y kv;
  y

let tmpdir () =
  let d = Filename.temp_file "test-page-run" "" in
  Sys.remove d;
  Unix.mkdir d 0o755;
  d

(* ------------------------------------------------------------------------ *)

let test_reuse () =
  let p = d {|{"sha": "a", "ci_state": "FAILURE"}|} in
  let prev =
    d
      {|{"sha": "a", "ci_state": "FAILURE", "tool_version": "v", "result": {}, "attempts": 1}|}
  in
  Alcotest.check triple "same" (true, true, false)
    (T.reuse (Some prev) p "v" false);
  Alcotest.check triple "mcp errors" (false, true, true)
    (T.reuse (Some (with_ prev [ ("mcp_errors", `Int 2) ])) p "v" false);
  Alcotest.check triple "attempts used" (true, true, false)
    (T.reuse
       (Some
          (with_ prev
             [ ("mcp_errors", `Int 2); ("attempts", `Int T.max_attempts) ]))
       p "v" false);
  Alcotest.check triple "other sha" (false, false, false)
    (T.reuse (Some prev) (with_ p [ ("sha", `String "b") ]) "v" false)

let test_attempts_expire () =
  let p = d {|{"sha": "a", "ci_state": "FAILURE"}|} in
  let now = Unix.time () in
  let recent = `String (iso_at (now -. 3600.))
  and old = `String (iso_at (now -. (2. *. 86400.))) in
  let prev =
    with_
      (d
         {|{"sha": "a", "ci_state": "FAILURE", "tool_version": "v", "result": {}, "mcp_errors": 2}|})
      [ ("attempts", `Int T.max_attempts); ("analysed_at", recent) ]
  in
  Alcotest.check triple "recent" (true, true, false)
    (T.reuse ~now_:now (Some prev) p "v" false);
  Alcotest.check triple "old" (false, true, true)
    (T.reuse ~now_:now (Some (with_ prev [ ("analysed_at", old) ])) p "v" false);
  Alcotest.check triple "net errors" (false, true, true)
    (T.reuse ~now_:now
       (Some
          (with_ prev
             [
               ("mcp_errors", `Int 0);
               ("net_errors", `Int 1);
               ("attempts", `Int 1);
             ]))
       p "v" false);
  Alcotest.(check int)
    "expired" 0
    (T.attempts_used ~now_:now (Some (with_ prev [ ("analysed_at", old) ])));
  let failed = with_ prev [ ("result", `Null); ("mcp_errors", `Int 0) ] in
  Alcotest.(check bool) "kept" true (T.keep_failed ~now_:now (Some failed));
  Alcotest.(check bool)
    "expired" false
    (T.keep_failed ~now_:now (Some (with_ failed [ ("analysed_at", old) ])));
  Alcotest.(check bool)
    "attempts left" false
    (T.keep_failed ~now_:now (Some (with_ failed [ ("attempts", `Int 1) ])))

let test_timeout_keeps_counts () =
  let dir = tmpdir () in
  let tool = Filename.concat dir "slow_tool" in
  let oc = open_out tool in
  output_string oc
    "#!/bin/sh\n\
     printf '{\"http\": 0, \"llm\": 1, \"mcp-errors\": 2}' > \
     \"$OPAM_CI_NET_FILE\"\n\
     exec sleep 60\n";
  close_out oc;
  Unix.chmod tool 0o755;
  let t0 = Unix.gettimeofday () in
  let result, err, net = T.run_tool ~tool [] 3 in
  Alcotest.(check bool) "fast" true (Unix.gettimeofday () -. t0 < 30.);
  Alcotest.(check bool) "no result" true (result = None);
  Alcotest.(check bool)
    "timeout" true
    (Util.contains (Option.get err) "timeout");
  Alcotest.(check (list int))
    "counts" [ 1; 2; 1 ]
    [ T.count net "llm"; T.count net "mcp-errors"; T.count net "timeout" ];
  Util.rmtree dir

let test_graphql_errors () =
  let data =
    j
      {|{"repository": {"pullRequests": {"pageInfo": {"hasNextPage": false}, "nodes": [null]}}}|}
  in
  let answers =
    ref
      [
        {|{"errors": [{"message": "Something went wrong"}]}|};
        {|{"data": {"repository": null}, "errors": [{"message": "timeout"}]}|};
        Json.dumps
          (`Assoc
             [
               ("data", data); ("errors", j {|[{"message": "one node failed"}]|});
             ]);
      ]
  in
  let saved_hook = !Http.hook and saved_sleep = !T.sleep_hook in
  (Http.hook :=
     fun _ ->
       match !answers with
       | a :: rest ->
           answers := rest;
           { Http.status = 200; resp_headers = []; data = a }
       | [] -> assert false);
  (T.sleep_hook := fun _ -> ());
  let err = Unix.dup Unix.stderr in
  let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 null Unix.stderr;
  let got =
    Fun.protect
      ~finally:(fun () ->
        Unix.dup2 err Unix.stderr;
        Http.hook := saved_hook;
        T.sleep_hook := saved_sleep)
      (fun () -> T.graphql "q" (`Assoc []) "tok")
  in
  Alcotest.check json "partial data" data got;
  let saved = !T.graphql_hook in
  (T.graphql_hook := fun _ _ _ -> data);
  let prs, calls = T.list_open_prs "tok" in
  T.graphql_hook := saved;
  Alcotest.(check int) "no PR" 0 (List.length prs);
  Alcotest.(check int) "one call" 1 calls

(* ------------------------------------------------------------------------ *)
(* RefreshTest *)

let test_ci_changed () =
  let prev =
    d
      {|{"sha": "a", "ci_state": "FAILURE", "ci_created": "2026-11-20T10:00:00Z", "tool_version": "v",
                  "result": {}, "attempts": 1}|}
  in
  let p =
    d
      {|{"sha": "a", "ci_state": "FAILURE", "ci_created": "2026-11-20T10:00:00Z"}|}
  in
  let b = Alcotest.(check bool) in
  b "same" false (T.ci_changed (Some prev) p);
  Alcotest.check triple "reuse" (true, true, false)
    (T.reuse (Some prev) p "v" false);
  (* "Rebuild failed", finished within the hour: same state, newer status. *)
  let rebuilt = with_ p [ ("ci_created", `String "2026-11-20T10:30:00Z") ] in
  b "rebuilt" true (T.ci_changed (Some prev) rebuilt);
  Alcotest.check triple "reuse rebuilt" (false, false, false)
    (T.reuse (Some prev) rebuilt "v" false);
  b "pending" true
    (T.ci_changed (Some prev) (with_ p [ ("ci_state", `String "PENDING") ]));
  (* A previous analysis without a date does not say which build it saw:
     changed. Without a date now either, nothing says it changed. *)
  b "no date before" true
    (T.ci_changed (Some (with_ prev [ ("ci_created", `Null) ])) rebuilt);
  b "no date" false
    (T.ci_changed
       (Some (with_ prev [ ("ci_created", `Null) ]))
       (with_ p [ ("ci_created", `Null) ]));
  (* Another commit is not a change of status, but it is analysed with
     --refresh (free), as is a PR without a previous entry. *)
  b "other commit" false
    (T.ci_changed (Some prev) (with_ rebuilt [ ("sha", `String "b") ]));
  b "refresh other commit" true
    (T.needs_refresh (Some prev) (with_ rebuilt [ ("sha", `String "b") ]));
  b "refresh new" true (T.needs_refresh None p);
  b "no refresh" false (T.needs_refresh (Some prev) p)

let test_unfinished_jobs () =
  let job i kind status =
    {
      Ci.variant = Printf.sprintf "v%d" i;
      url = "";
      label = "";
      status = Ci.Status.of_string status;
      reason = None;
      category = Ci.Category.of_string status;
      target = None;
      kind = Ci.Kind.of_string kind;
      revdep = None;
      platform = [];
      experimental = false;
      in_summary = false;
      messages = [];
    }
  in
  let n statuses =
    let kinds =
      [ "analysis"; "lint" ]
      @ List.init (List.length statuses - 2) (fun _ -> "build")
    in
    List.length
      (Ci.unfinished_jobs
         (List.mapi (fun i (k, s) -> job i k s) (List.combine kinds statuses)))
  in
  let c = Alcotest.(check int) in
  (* Lint never starts after a failed or aborted analysis (opam-ci's status stays pending). *)
  c "failed analysis" 0 (n [ "failed"; "not-started" ]);
  c "aborted analysis" 0 (n [ "aborted"; "not-started" ]);
  c "aborted: finished" 0 (n [ "passed"; "passed"; "aborted"; "failed" ]);
  (* A job waiting for another one that failed, nothing else running: finished. *)
  c "waiting" 0 (n [ "passed"; "passed"; "failed"; "not-started" ]);
  (* Still running: a job is active, or nothing failed yet (CI starting). *)
  c "active" 2 (n [ "passed"; "passed"; "active"; "not-started" ]);
  c "starting" 1 (n [ "passed"; "not-started"; "passed" ]);
  c "both" 2 (n [ "active"; "not-started" ]);
  (* The statuses as the CI page writes them. *)
  let page =
    {|<details><summary><b>Main results</b></summary><ul class="statuses"><li class="failed"><a href="/github/ocaml/opam-repository/commit/a/variant/(analysis)">(analysis) (failed: x)</a></li><li class="not-started"><a href="/github/ocaml/opam-repository/commit/a/variant/(lint)">(lint) (not started)</a></li></ul></details>|}
  in
  let data = Ci.parse_commit_page page in
  Alcotest.(check (list string))
    "statuses"
    [ "failed"; "not-started" ]
    (List.map (fun j -> Ci.Status.to_string j.Ci.status) data.jobs);
  c "none" 0 (List.length (Ci.unfinished_jobs data.jobs))

(* tests/fixtures, from the repository (dune exec) or from _build/default/tests (dune test). *)
let fixtures =
  if Sys.file_exists "fixtures" then "fixtures"
  else Filename.concat "tests" "fixtures"

(* The Python test replaced ci.fetch; here the transport is replaced, so the
   page is fetched once and then cached. *)
let test_refresh_drops_logs () =
  let sha = "eafb2a69786279b98b31f3e5cd8a62ce432cf343" in
  let page =
    Util.read_gzip
      (Net.cache_path (Net.base ^ sha)
         (Filename.concat fixtures "goblint-cil-baseline")
      ^ ".gz")
  in
  let fetched = ref [] in
  let saved = !Http.hook in
  (Http.hook :=
     fun r ->
       fetched := r.Http.url :: !fetched;
       { Http.status = 200; resp_headers = []; data = page });
  let dir = tmpdir () in
  let data = Ci.parse_commit_page page in
  let job = List.find (fun j -> j.Ci.status = Failed) data.jobs in

  let log = Net.cache_path job.url dir in
  close_out (open_out log);
  Fun.protect
    ~finally:(fun () -> Http.hook := saved)
    (fun () ->
      ignore (Ci.load_commit sha dir);
      Alcotest.(check bool) "log kept" true (Sys.file_exists log);
      (* no rebuild: the cached log is kept *)
      Odict.remove Net.net "refreshed";
      ignore (Ci.load_commit sha dir);
      Alcotest.(check bool)
        "not refreshed" false
        (Odict.mem Net.net "refreshed");
      ignore (Ci.load_commit ~refresh:true sha dir);
      Alcotest.(check bool) "log dropped" false (Sys.file_exists log);
      (* rebuilt: fetched again when needed *)
      Alcotest.(check int) "fetches" 2 (List.length !fetched);
      Alcotest.(check (option int))
        "said" (Some 1)
        (Odict.get Net.net "refreshed");
      Odict.remove Net.net "refreshed");
  Util.rmtree dir;
  (* The page could not be fetched: the logs of the previous build stay, and it is not said. *)
  let dir = tmpdir () in
  (Http.hook := fun _ -> raise (Http.Net_error "503 after the retries"));
  close_out (open_out (Net.cache_path job.url dir));
  Fun.protect
    ~finally:(fun () -> Http.hook := saved)
    (fun () ->
      Alcotest.(check bool)
        "raises" true
        (try
           ignore (Ci.load_commit ~refresh:true sha dir);
           false
         with Http.Net_error _ -> true);
      Alcotest.(check bool)
        "logs stay" true
        (Sys.file_exists (Net.cache_path job.url dir));
      Alcotest.(check bool) "not said" false (Odict.mem Net.net "refreshed"));
  Util.rmtree dir

(* ------------------------------------------------------------------------ *)
(* PageMainTest: main() on synthetic states and a fake tool. *)

let sha_a = String.make 40 'a'

let failed_analysis =
  j
    {|[{"variant": "(analysis)", "status": "failed", "category": "analysis"},
                           {"variant": "(lint)", "status": "not-started", "category": "not-started"}]|}

let pr ?(kw = []) () =
  with_
    (Dict.of_list
       [
         ("number", `Int 1);
         ("title", `String "t");
         ("url", `String "u");
         ("draft", `Bool false);
         ("updated", `String "2026-11-20T00:00:00Z");
         ("sha", `String sha_a);
         ("author", `String "x");
         ("labels", `List []);
         ("stale_labels", `List []);
         ("ci_state", `String "FAILURE");
         ("ci_url", `Null);
         ("ci_created", `String "2026-11-20T10:00:00Z");
       ])
    kw

type env = { tmp : string; state : string }

let setup () =
  let tmp = tmpdir () in
  { tmp; state = Filename.concat tmp "prs.json" }

let read_state e =
  List.map
    (fun (k, v) -> (k, Dict.of_json v))
    (Json.assoc "prs" (Option.get (Json.load e.state)))

let ok_out =
  j {|{"suggestions": [{"rule": "x", "variant": "v"}], "ci_unfinished": 0}|}

let err_file e = Filename.concat e.tmp "stderr"

(** One run; (state after it, the tool's calls, [changed] output). [tool args]:
    the suggest runs' (result, error, request counts). *)
let run_main ?jobs ?(suggest_ok = true) ?(args = []) ?(llm = false)
    ?(version = "v") ?(summary_ok = true) ?suggest_out ?tool ?summary e prs =
  let calls = ref [] in
  let run_tool a _timeout =
    calls := a :: !calls;
    if List.hd a = "summary" then
      match summary with
      | Some f -> f a
      | None ->
          if not summary_ok then
            (None, Some "timeout after 120s", [ ("timeout", 1) ])
          else
            ( Some
                (`Assoc [ ("jobs", Option.value ~default:failed_analysis jobs) ]),
              None,
              if List.mem "--refresh" a then [ ("refreshed", 1) ] else [] )
    else
      match tool with
      | Some t -> t a
      | None ->
          if not suggest_ok then
            (* Killed after it refreshed the cache, when asked to (see test_refresh_not_done). *)
            ( None,
              Some "timeout after 600s",
              [ ("timeout", 1) ]
              @ if List.mem "--refresh" a then [ ("refreshed", 1) ] else [] )
          else
            ( Some (Option.value ~default:ok_out suggest_out),
              None,
              [ ("http", 1) ] )
  in
  let gh_out = Filename.concat e.tmp "gh-out" in
  close_out (open_out gh_out);
  (T.github_token_hook := fun () -> Some "tok");
  (T.list_open_prs_hook := fun _ -> (prs, 1));
  T.run_tool_hook := run_tool;
  (T.llm_available_hook := fun () -> llm);
  (T.tool_version_hook := fun () -> version);
  Unix.putenv "GITHUB_OUTPUT" gh_out;
  Unix.putenv "GITHUB_STEP_SUMMARY" (Filename.concat e.tmp "summary.md");
  let err = Unix.dup Unix.stderr in
  (* what the run prints: see [run_err] *)
  let null =
    Unix.openfile (err_file e)
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
      0o644
  in
  Unix.dup2 null Unix.stderr;
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 err Unix.stderr;
      Unix.close err;
      Unix.close null)
    (fun () ->
      T.main
        ~argv:
          (Array.of_list
             ([
                "triage_open_prs";
                "--state";
                e.state;
                "--out";
                Filename.concat e.tmp "data.json";
              ]
             @ args))
        ());
  ( read_state e,
    List.rev !calls,
    String.trim (In_channel.with_open_bin gh_out In_channel.input_all) )

let save e prs =
  let oc = open_out e.state in
  output_string oc
    (Json.dumps
       (`Assoc
          [ ("prs", `Assoc (List.map (fun (k, v) -> (k, Dict.to_json v)) prs)) ]));
  close_out oc

let kinds calls =
  List.map
    (fun c -> List.hd c ^ if List.mem "--refresh" c then " --refresh" else "")
    calls

let entry state = List.assoc "1" state

let check_kinds msg expected calls =
  Alcotest.(check (list string)) msg expected (kinds calls)

let no_calls msg calls = Alcotest.(check int) msg 0 (List.length calls)
let get e k = Option.value ~default:`Null (Dict.raw e k)

let test_pending_with_finished_page () =
  (* opam-ci's status stays pending (lint never starts after a failed
     analysis): analysed once, then kept while the job statuses are the same. *)
  let e = setup () in
  let p = pr ~kw:[ ("ci_state", `String "PENDING") ] () in
  let _, calls, changed = run_main e [ p ] in
  check_kinds "first" [ "summary --refresh"; "suggest" ] calls;
  Alcotest.(check string) "changed" "changed=true" changed;
  let state, calls, changed = run_main e [ p ] in
  check_kinds "page only" [ "summary" ] calls;
  (* the page only (cached once finished) *)
  Alcotest.(check string) "unchanged" "changed=false" changed;
  Alcotest.(check bool) "result" true (Dict.get (entry state) "result" <> None);
  (* A job status changed: analysed again. *)
  let jobs =
    j
      {|[{"variant": "(analysis)", "status": "passed", "category": "analysis"},
                  {"variant": "(lint)", "status": "failed", "category": "not-started"}]|}
  in
  let _, calls, _ = run_main ~jobs e [ p ] in
  check_kinds "again" [ "summary"; "suggest" ] calls

let test_pending_failures_limited () =
  (* A PR whose analysis times out while CI runs: max_attempts attempts, even
     though jobs keep finishing, then not tried again for a while. *)
  let e = setup () in
  let p = pr ~kw:[ ("ci_state", `String "PENDING") ] () in
  for i = 0 to T.max_attempts - 1 do
    let jobs =
      `List
        (Json.to_list failed_analysis
        @ List.init i (fun k ->
            `Assoc
              [
                ("variant", `String (Printf.sprintf "b%d" k));
                ("status", `String "passed");
              ]))
    in
    let state, calls, _ = run_main ~jobs ~suggest_ok:false e [ p ] in
    Alcotest.(check bool) "suggest" true (List.mem "suggest" (kinds calls));
    Alcotest.(check int) "attempts" (i + 1) (Dict.int (entry state) "attempts")
  done;
  let jobs =
    `List
      (Json.to_list failed_analysis
      @ [ j {|{"variant": "z", "status": "passed"}|} ])
  in
  let _, calls, changed = run_main ~jobs ~suggest_ok:false e [ p ] in
  no_calls "no call" calls;
  Alcotest.(check string) "unchanged" "changed=false" changed

let test_refresh () =
  let e = setup () in
  let p = pr () in
  (* No previous entry (new PR, or one dropped when its CI passed and failing again). *)
  let state, calls, _ = run_main e [ p ] in
  check_kinds "new" [ "suggest --refresh"; "summary" ] calls;
  (* Nothing changed: nothing fetched. *)
  let _, calls, _ = run_main e [ p ] in
  no_calls "unchanged" calls;
  (* An entry saved before the status dates were recorded. *)
  let e1 = Dict.copy (entry state) in
  Dict.remove e1 "ci_created";
  save e [ ("1", e1) ];
  let _, calls, _ = run_main e [ p ] in
  check_kinds "no date" [ "suggest --refresh"; "summary" ] calls;
  (* Another commit. *)
  let b = pr ~kw:[ ("sha", `String (String.make 40 'b')) ] () in
  let _, calls, _ = run_main e [ b ] in
  check_kinds "other commit" [ "suggest --refresh"; "summary" ] calls;
  (* The tool changed: analysed again from the cache. *)
  let _, calls, _ = run_main ~version:"w" e [ b ] in
  check_kinds "tool changed" [ "suggest"; "summary" ] calls

let test_deferred_keeps_result_of_the_same_commit () =
  let e = setup () in
  ignore (run_main e [ pr () ]);
  (* The tool changed, and the time budget is spent: the result stays. *)
  let state, calls, _ =
    run_main ~args:[ "--time-budget"; "-1" ] ~version:"w" e [ pr () ]
  in
  no_calls "deferred" calls;
  Alcotest.(check bool) "result" true (Dict.get (entry state) "result" <> None);
  Alcotest.(check bool)
    "not pending" false
    (Dict.truthy (entry state) "pending");
  (* Another commit: the result of the old one is not shown. *)
  let state, _, _ =
    run_main ~args:[ "--time-budget"; "-1" ] ~version:"w" e
      [ pr ~kw:[ ("sha", `String (String.make 40 'b')) ] () ]
  in
  Alcotest.(check bool) "no result" true (Dict.get (entry state) "result" = None);
  Alcotest.(check bool) "pending" true (Dict.truthy (entry state) "pending")

let test_failed_redo_keeps_result () =
  let e = setup () in
  let state, _, _ = run_main e [ pr () ] in
  let good = get (entry state) "result" in
  for i = 0 to T.max_attempts - 1 do
    let state, calls, _ = run_main ~version:"w" ~suggest_ok:false e [ pr () ] in
    check_kinds "redo" [ "suggest" ] calls;
    Alcotest.check json "kept" good (get (entry state) "result");
    Alcotest.(check string)
      "error" "timeout after 600s"
      (Dict.str (entry state) "last_error");
    Alcotest.(check int) "attempts" (i + 1) (Dict.int (entry state) "attempts");
    Alcotest.(check string)
      "what the result saw" "v"
      (Dict.str (entry state) "tool_version")
  done;
  let state, calls, _ = run_main ~version:"w" ~suggest_ok:false e [ pr () ] in
  no_calls "kept until the attempts expire" calls;
  Alcotest.check json "kept" good (get (entry state) "result");
  (* A rebuild of the same commit is analysed at once, from a fresh CI page. *)
  let rebuilt = pr ~kw:[ ("ci_created", `String "2026-11-20T11:00:00Z") ] () in
  let _, calls, _ = run_main ~version:"w" ~suggest_ok:false e [ rebuilt ] in
  check_kinds "rebuild" [ "suggest --refresh" ] calls;
  (* The attempts expired: tried again, and this time it works. *)
  Dict.set_str (entry state) "last_error_at" "2026-01-01T00:00:00+00:00";
  save e state;
  let state, calls, _ = run_main ~version:"w" e [ pr () ] in
  check_kinds "works" [ "suggest"; "summary" ] calls;
  Alcotest.(check bool)
    "no last_error" false
    (Dict.mem (entry state) "last_error");
  Alcotest.(check string) "version" "w" (Dict.str (entry state) "tool_version")

let test_failed_redo_of_a_rebuild_limited () =
  (* After a rebuild, a redo that keeps failing: the attempts at that CI run
     add up (max_attempts, then the result is kept), and only the first one
     refreshes the cache. A newer rebuild is analysed at once. *)
  let e = setup () in
  let state, _, _ = run_main e [ pr () ] in
  let good = get (entry state) "result" in
  let rebuilt = pr ~kw:[ ("ci_created", `String "2026-11-20T11:00:00Z") ] () in
  for i = 0 to T.max_attempts - 1 do
    let state, calls, _ = run_main ~suggest_ok:false e [ rebuilt ] in
    check_kinds "redo"
      [ (if i = 0 then "suggest --refresh" else "suggest") ]
      calls;
    Alcotest.check json "kept" good (get (entry state) "result");
    Alcotest.(check int) "attempts" (i + 1) (Dict.int (entry state) "attempts");
    Alcotest.(check string)
      "what the result saw" "2026-11-20T10:00:00Z"
      (Dict.str (entry state) "ci_created")
  done;
  let _, calls, _ = run_main ~suggest_ok:false e [ rebuilt ] in
  no_calls "kept" calls;
  let newer = pr ~kw:[ ("ci_created", `String "2026-11-20T12:00:00Z") ] () in
  let state, calls, _ = run_main ~suggest_ok:false e [ newer ] in
  check_kinds "newer" [ "suggest --refresh" ] calls;
  Alcotest.(check int) "attempts" 1 (Dict.int (entry state) "attempts");
  (* It works: the attempt's fields are gone. *)
  let state, calls, _ = run_main e [ newer ] in
  check_kinds "works" [ "suggest"; "summary" ] calls;
  Alcotest.(check (list string))
    "no last_error" []
    (List.filter
       (fun k -> String.starts_with ~prefix:"last_error" k)
       (Dict.keys (entry state)))

let test_deferred_new_commit_drops_old_fields () =
  (* A new commit left for a later run: nothing of the old commit's analysis stays. *)
  let e = setup () in
  save e
    [
      ( "1",
        with_
          (pr ~kw:[ ("ci_state", `String "PENDING") ] ())
          (Json.to_assoc
             (j
                {|{"result": {"suggestions": []}, "ci_unfinished": 7, "llm": true, "mcp_errors": 2, "net_errors": 1,
                             "tool_version": "v", "analysed_at": "2026-11-20T10:00:00+00:00"}|}))
      );
    ];
  let state, calls, _ =
    run_main ~args:[ "--time-budget"; "-1" ] e
      [ pr ~kw:[ ("sha", `String (String.make 40 'b')) ] () ]
  in
  no_calls "deferred" calls;
  let en = entry state in
  Alcotest.(check bool) "pending" true (Dict.truthy en "pending");
  Alcotest.(check string) "ci_state" "FAILURE" (Dict.str en "ci_state");
  List.iter
    (fun k -> Alcotest.(check bool) k false (Dict.mem en k))
    [
      "result"; "ci_unfinished"; "llm"; "mcp_errors"; "net_errors"; "ci_created";
    ]

let test_pending_summary_failed_keeps_result () =
  (* A pending PR whose CI page could not be read ([summary] failed): its result stays. *)
  let e = setup () in
  let p = pr ~kw:[ ("ci_state", `String "PENDING") ] () in
  let state, _, _ = run_main e [ p ] in
  let good = get (entry state) "result" in
  let state, calls, _ = run_main ~summary_ok:false e [ p ] in
  check_kinds "summary" [ "summary" ] calls;
  Alcotest.check json "kept" good (get (entry state) "result")

let test_llm_budget_spent () =
  (* Analysed without the LLM; this run has the LLM but no budget left: not
     analysed again (it would be without the LLM again). *)
  let e = setup () in
  ignore (run_main e [ pr () ]);
  let _, calls, _ =
    run_main ~llm:true ~args:[ "--llm-budget"; "0" ] e [ pr () ]
  in
  no_calls "no budget" calls;
  let _, calls, _ =
    run_main ~llm:true ~args:[ "--llm-budget"; "5" ] e [ pr () ]
  in
  check_kinds "budget" [ "suggest"; "summary" ] calls;
  Alcotest.(check bool) "--llm" true (List.mem "--llm" (List.hd calls))

let llm_out =
  j
    {|{"suggestions": [{"rule": "x", "variant": "v"}], "ci_unfinished": 0, "llm_unanswered": 1}|}

let set_field v k x =
  match v with
  | `Assoc kv ->
      `Assoc (List.map (fun (k', y) -> if k' = k then (k', x) else (k', y)) kv)
  | v -> v

let test_llm_flag_needs_every_answer () =
  (* [llm] is stored only when every unexplained failure got an answer: a PR
     left with some (--llm-max cut to the run's budget, an API error) gets
     LLM calls in a later run. *)
  let e = setup () in
  let state, _, _ = run_main ~llm:true ~suggest_out:llm_out e [ pr () ] in
  Alcotest.(check (pair bool int))
    "left" (false, 1)
    (Dict.truthy (entry state) "llm", Dict.int (entry state) "llm_unanswered");
  let state, calls, _ =
    run_main ~llm:true
      ~suggest_out:(set_field llm_out "llm_unanswered" (`Int 0))
      e
      [ pr () ]
  in
  check_kinds "again" [ "suggest"; "summary" ] calls;
  Alcotest.(check (pair bool int))
    "done" (true, 0)
    (Dict.truthy (entry state) "llm", Dict.int (entry state) "llm_unanswered");
  let _, calls, _ = run_main ~llm:true e [ pr () ] in
  no_calls "kept" calls;
  (* While CI runs, the LLM is not asked (see suggest): no analysis again for it. *)
  let p = pr ~kw:[ ("ci_state", `String "PENDING") ] () in
  let running = set_field llm_out "ci_unfinished" (`Int 4) in
  let state, _, _ = run_main ~llm:true ~suggest_out:running e [ p ] in
  Alcotest.(check (pair bool int))
    "running" (false, 4)
    (Dict.truthy (entry state) "llm", Dict.int (entry state) "ci_unfinished");
  let _, calls, _ = run_main ~llm:true ~suggest_out:running e [ p ] in
  check_kinds "summary only" [ "summary" ] calls

let test_llm_retries_limited () =
  (* The LLM asked, no answer came (API errors, the LLM off for the run):
     analysed again as a retry, max_attempts times a day. With answers
     coming (the per-PR limit), analysed again until every failure has its
     answer. *)
  let e = setup () in
  let first calls = List.map List.hd calls in
  for i = 0 to T.max_attempts - 1 do
    let state, calls, _ =
      run_main ~llm:true
        ~tool:(fun _ -> (Some llm_out, None, [ ("http", 1) ]))
        e
        [ pr () ]
    in
    Alcotest.(check (list string))
      "retry" [ "suggest"; "summary" ] (first calls);
    Alcotest.(check int) "attempts" (i + 1) (Dict.int (entry state) "attempts")
  done;
  let _, calls, _ =
    run_main ~llm:true
      ~tool:(fun _ -> (Some llm_out, None, [ ("http", 1) ]))
      e
      [ pr () ]
  in
  no_calls "limited" calls;
  (* Two answers this run, one left: analysed again (not a retry), until none is left. *)
  save e [];
  List.iter
    (fun left ->
      let state, calls, _ =
        run_main ~llm:true
          ~tool:(fun _ ->
            ( Some (set_field llm_out "llm_unanswered" (`Int left)),
              None,
              [ ("llm", 2) ] ))
          e
          [ pr () ]
      in
      Alcotest.(check (list string))
        "again" [ "suggest"; "summary" ] (first calls);
      Alcotest.(check (pair int bool))
        "attempts, llm"
        (1, left = 0)
        (Dict.int (entry state) "attempts", Dict.truthy (entry state) "llm"))
    [ 2; 1; 0 ];
  let _, calls, _ = run_main ~llm:true e [ pr () ] in
  no_calls "done" calls

let test_llm_not_asked_while_page_unfinished () =
  (* opam-ci says failure, but the CI page had unfinished jobs: the LLM was
     not asked (see suggest), and the PR is not analysed again for it until
     something changes. *)
  let e = setup () in
  let out = set_field llm_out "ci_unfinished" (`Int 2) in
  ignore (run_main ~llm:true ~suggest_out:out e [ pr () ]);
  let _, calls, _ = run_main ~llm:true ~suggest_out:out e [ pr () ] in
  no_calls "not again" calls

let test_llm_off_for_the_run () =
  (* The tool turned the LLM off (credentials refused), or its API failed
     llm_breaker times in a row over the PRs analysed so far: the next PRs
     are analysed without it. *)
  let e = setup () in
  let prs =
    List.map
      (fun n ->
        pr
          ~kw:
            [
              ("number", `Int n);
              ("sha", `String (String.make 40 (Char.chr (48 + n))));
            ]
          ())
      [ 1; 2; 3 ]
  in
  let seq l =
    let it = ref (l @ [ []; []; [] ]) in
    fun _ ->
      match !it with
      | x :: r ->
          it := r;
          (Some llm_out, None, x)
      | [] -> assert false
  in
  List.iter
    (fun nets ->
      save e [];
      let state, calls, _ = run_main ~llm:true ~tool:(seq nets) e prs in
      let suggests = List.filter (fun c -> List.hd c = "suggest") calls in
      let asked =
        List.filter_map
          (fun (n, c) -> if List.mem "--llm" c then Some n else None)
          (List.combine [ 1; 2; 3 ] suggests)
      in
      Alcotest.(check (list int))
        "asked"
        (Util.take (List.length nets) [ 1; 2; 3 ])
        asked;
      (* analysed without the LLM *)
      Alcotest.(check bool)
        "no llm" true
        (Dict.get
           (List.assoc (string_of_int (List.length nets + 1)) state)
           "llm_unanswered"
        = None))
    [
      [ [ ("llm-off", 1) ] ];
      [ [ ("llm-streak", 1) ]; [ ("llm-streak", 1) ] ];
      [ [ ("llm-streak", 1); ("llm", 1) ]; [ ("llm-streak", 1) ] ];
    ];
  (* Said in the data the page is built from, and in the run's summary on GitHub. *)
  save e [];
  ignore (run_main ~llm:true ~tool:(seq [ [ ("llm-off", 1) ] ]) e prs);
  let data = Option.get (Json.load (Filename.concat e.tmp "data.json")) in
  Alcotest.(check (option string))
    "off" (Some "credentials")
    (Json.str_opt "off" (Json.member "llm" data));
  Alcotest.(check bool)
    "summary" true
    (Util.contains
       (In_channel.with_open_bin
          (Filename.concat e.tmp "summary.md")
          In_channel.input_all)
       "ANTHROPIC_API_KEY");
  (* A success between failures: not in a row. *)
  save e [];
  let _, calls, _ =
    run_main ~llm:true
      ~tool:
        (seq
           [
             [ ("llm-streak", 1) ];
             [ ("llm", 1); ("llm-streak", 0) ];
             [ ("llm-streak", 1) ];
           ])
      e prs
  in
  Alcotest.(check bool)
    "all asked" true
    (List.for_all
       (fun c -> List.mem "--llm" c)
       (List.filter (fun c -> List.hd c = "suggest") calls))

let test_refresh_not_done () =
  (* The CI page of a rebuild could not be fetched (503 after the retries):
     the cache still holds the previous build's page and logs, so the next
     attempt refreshes it again, with a previous result kept or without
     one; then the cache is not refreshed again. *)
  let e = setup () in
  let down _ =
    ( None,
      Some "HTTP Error 503: Service Unavailable",
      [ ("http", 1); ("net-errors", 1) ] )
  in
  let refreshed_down _ =
    (None, Some "timeout after 600s", [ ("timeout", 1); ("refreshed", 1) ])
  in
  ignore (run_main e [ pr () ]);
  let rebuilt = pr ~kw:[ ("ci_created", `String "2026-11-20T11:00:00Z") ] () in
  let state, calls, _ = run_main ~tool:down e [ rebuilt ] in
  check_kinds "down" [ "suggest --refresh" ] calls;
  Alcotest.(check bool)
    "not refreshed" false
    (Dict.truthy (entry state) "last_error_refreshed");
  let state, calls, _ = run_main ~tool:refreshed_down e [ rebuilt ] in
  check_kinds "again" [ "suggest --refresh" ] calls;
  Alcotest.(check bool)
    "refreshed" true
    (Dict.truthy (entry state) "last_error_refreshed");
  let _, calls, _ = run_main e [ rebuilt ] in
  check_kinds "from the cache" [ "suggest"; "summary" ] calls;
  (* No previous entry (dropped when its CI passed): the failed attempt
     records that the refresh is still to do, and the attempts add up. *)
  save e [];
  let state, _, _ = run_main ~tool:down e [ rebuilt ] in
  Alcotest.(check bool)
    "refresh_pending" true
    (Dict.truthy (entry state) "refresh_pending");
  let state, calls, _ = run_main ~tool:down e [ rebuilt ] in
  check_kinds "pending" [ "suggest --refresh"; "summary" ] calls;
  Alcotest.(check int) "attempts" 2 (Dict.int (entry state) "attempts");
  let state, calls, _ = run_main e [ rebuilt ] in
  check_kinds "done" [ "suggest --refresh"; "summary" ] calls;
  Alcotest.(check bool)
    "no refresh_pending" false
    (Dict.mem (entry state) "refresh_pending");
  (* A running PR: [summary] refreshed the page. *)
  save e [];
  let state, calls, _ =
    run_main ~tool:down e [ pr ~kw:[ ("ci_state", `String "PENDING") ] () ]
  in
  check_kinds "running" [ "summary --refresh"; "suggest" ] calls;
  Alcotest.(check bool)
    "no refresh_pending" false
    (Dict.mem (entry state) "refresh_pending")

(* The Python test also checks render_page's verdict on these states (in
   test_page): only the state is checked here. *)
let test_kept_result_during_a_rebuild () =
  let e = setup () in
  let out =
    j
      {|{"suggestions": [{"rule": "x", "variant": "v", "class": "elsewhere", "where_kind": "other-package",
                                  "preexisting": "on master"}], "ci_unfinished": 0}|}
  in
  ignore (run_main ~suggest_out:out e [ pr () ]);
  let rebuilding =
    pr
      ~kw:
        [
          ("ci_state", `String "PENDING");
          ("ci_created", `String "2026-11-20T11:00:00Z");
        ]
      ()
  in
  let state, calls, _ =
    run_main ~args:[ "--time-budget"; "-1" ] e [ rebuilding ]
  in
  no_calls "deferred" calls;
  Alcotest.(check (pair string string))
    "states" ("FAILURE", "PENDING")
    (Dict.str (entry state) "ci_state", Dict.str (entry state) "ci_state_now");
  Alcotest.(check bool) "running" true (T.ci_running (entry state));
  let state, _, _ = run_main ~summary_ok:false e [ rebuilding ] in
  Alcotest.(check string)
    "still pending" "PENDING"
    (Dict.str (entry state) "ci_state_now");
  (* The rebuild failed again. *)
  let rebuilt = pr ~kw:[ ("ci_created", `String "2026-11-20T11:00:00Z") ] () in
  let state, _, _ =
    run_main ~suggest_out:out ~args:[ "--time-budget"; "-1" ] e [ rebuilt ]
  in
  Alcotest.(check string)
    "failure" "FAILURE"
    (Dict.str (entry state) "ci_state_now");
  (* Analysed: the field is gone. *)
  let state, _, _ = run_main ~suggest_out:out e [ rebuilt ] in
  Alcotest.(check bool) "gone" false (Dict.mem (entry state) "ci_state_now")

let test_since_last_push_after_a_deferred_or_failed_analysis () =
  (* The first analysis of a new commit is deferred, then fails: what changed
     since the previous commit is still computed when an analysis of the new
     commit lands. *)
  let e = setup () in
  let old =
    j
      {|{"suggestions": [{"rule": "lint", "variant": "(lint)", "failing": "x.1"},
                                 {"rule": "library-api", "variant": "v", "failing": "y.1", "title": "API"}], "ci_unfinished": 0}|}
  in
  let nw =
    j
      {|{"suggestions": [{"rule": "library-api", "variant": "v", "failing": "y.1", "title": "API"}], "ci_unfinished": 0}|}
  in
  ignore (run_main ~suggest_out:old e [ pr () ]);
  let b = pr ~kw:[ ("sha", `String (String.make 40 'b')) ] () in
  let state, _, _ = run_main ~args:[ "--time-budget"; "-1" ] e [ b ] in
  Alcotest.(check (option string))
    "base" (Some sha_a)
    (Json.str_opt "sha" (get (entry state) "since_base"));
  let state, _, _ = run_main ~suggest_ok:false e [ b ] in
  Alcotest.(check bool) "no result" true (Dict.get (entry state) "result" = None);
  let state, _, _ = run_main ~suggest_out:nw e [ b ] in
  let since = get (entry state) "since_last_push" in
  Alcotest.check json "since"
    (j (Printf.sprintf {|{"previous_sha": "%s", "still": 1, "new": []}|} sha_a))
    (`Assoc
       [
         ("previous_sha", Json.member "previous_sha" since);
         ("still", Json.member "still" since);
         ("new", Json.member "new" since);
       ]);
  Alcotest.(check bool)
    "no since_base" false
    (Dict.mem (entry state) "since_base")

let test_pending_summary_failed_keeps_failed_analysis () =
  (* A running PR whose analysis failed, then whose CI page cannot be read:
     the entry stays, so its attempts keep adding up. *)
  let e = setup () in
  let p = pr ~kw:[ ("ci_state", `String "PENDING") ] () in
  let state, _, _ = run_main ~suggest_ok:false e [ p ] in
  Alcotest.(check (pair bool int))
    "failed" (true, 1)
    (Dict.get (entry state) "result" = None, Dict.int (entry state) "attempts");
  let state, calls, _ = run_main ~summary_ok:false e [ p ] in
  check_kinds "summary" [ "summary" ] calls;
  Alcotest.(check int) "attempts" 1 (Dict.int (entry state) "attempts");
  let jobs =
    `List
      (Json.to_list failed_analysis
      @ [ j {|{"variant": "b", "status": "passed"}|} ])
  in
  let state, _, _ = run_main ~jobs ~suggest_ok:false e [ p ] in
  Alcotest.(check int) "attempts" 2 (Dict.int (entry state) "attempts")

(* What a run prints and counts: the tests below pin, for each way a PR
   can go in a run, its stderr lines, the run's counters (the [run] member
   of the page's data, in their order) and the state saved. *)

let seconds_re = Rx.v {|\(\d+\.\ds;|}

(** What the last run printed on stderr, with the seconds and the temporary
    directory masked. *)
let run_err e =
  Rx.sub seconds_re "(Ns;"
    (In_channel.with_open_bin (err_file e) In_channel.input_all)
  |> fun s ->
  Util.replace s e.tmp "TMP" |> String.split_on_char '\n'
  |> List.filter (( <> ) "")

(** The counters of the last run (the [run] member of the page's data). *)
let run_stats e =
  Json.member "run" (Option.get (Json.load (Filename.concat e.tmp "data.json")))

let check_err msg expected e =
  Alcotest.(check (list string)) msg expected (run_err e)

let check_stats msg expected e =
  Alcotest.check json msg (j expected) (run_stats e)

let sha n = String.make 40 n

let failed_suggest a =
  ( None,
    Some "timeout after 600s",
    [ ("timeout", 1) ]
    @ if List.mem "--refresh" a then [ ("refreshed", 1) ] else [] )

let test_running_without_failure_yet () =
  (* CI running and no job failed yet (or only for reasons that are not the
     package's): the PR is not listed, and nothing else is run. *)
  let e = setup () in
  let jobs =
    j
      {|[{"variant": "a", "status": "passed"},
         {"variant": "b", "status": "failed", "category": "network"}]|}
  in
  let p = pr ~kw:[ ("ci_state", `String "PENDING") ] () in
  let state, calls, changed = run_main ~jobs e [ p ] in
  check_kinds "summary only" [ "summary --refresh" ] calls;
  Alcotest.(check bool) "not listed" false (List.mem_assoc "1" state);
  Alcotest.(check string) "changed" "changed=false" changed;
  check_err "stderr"
    [
      "1 open PRs, 0 with opam-ci = failure, 1 running (1 GitHub API call(s))";
      "LLM fallback: off (no ANTHROPIC_API_KEY)";
      "analysed 0, failed 0, unchanged 0, running without failure 1, deferred \
       0, dropped 0; tool requests: none; wrote TMP/data.json";
    ]
    e;
  check_stats "stats"
    {|{"github_api_calls": 1, "tool_requests": {}, "refreshed": 1,
       "running, no failure yet": 1, "dropped": 0}|}
    e;
  (* A new commit of an analysed PR, whose CI page cannot be read: nothing
     to show for it yet, and the old commit's entry is dropped. *)
  ignore (run_main e [ pr () ]);
  let b =
    pr ~kw:[ ("ci_state", `String "PENDING"); ("sha", `String (sha 'b')) ] ()
  in
  let state, calls, changed = run_main ~summary_ok:false e [ b ] in
  check_kinds "summary only" [ "summary --refresh" ] calls;
  Alcotest.(check bool) "dropped" false (List.mem_assoc "1" state);
  Alcotest.(check string) "changed" "changed=true" changed;
  check_stats "stats"
    {|{"github_api_calls": 1, "tool_requests": {}, "refreshed": 1,
       "running, no failure yet": 1, "dropped": 1}|}
    e

let test_deferred_new_pr () =
  (* A PR without a previous entry, left for a later run: listed as pending. *)
  let e = setup () in
  let state, calls, changed =
    run_main ~args:[ "--time-budget"; "-1" ] e [ pr () ]
  in
  no_calls "deferred" calls;
  Alcotest.(check string) "changed" "changed=false" changed;
  let en = entry state in
  Alcotest.(check bool) "pending" true (Dict.truthy en "pending");
  Alcotest.(check (list string))
    "fields"
    [
      "number";
      "title";
      "url";
      "draft";
      "updated";
      "sha";
      "author";
      "labels";
      "stale_labels";
      "ci_url";
      "pending";
    ]
    (Dict.keys en);
  check_err "stderr"
    [
      "1 open PRs, 1 with opam-ci = failure, 0 running (1 GitHub API call(s))";
      "LLM fallback: off (no ANTHROPIC_API_KEY)";
      "analysed 0, failed 0, unchanged 0, running without failure 0, deferred \
       1, dropped 0; tool requests: none; wrote TMP/data.json";
    ]
    e;
  check_stats "stats"
    {|{"github_api_calls": 1, "tool_requests": {}, "deferred": 1, "dropped": 0}|}
    e;
  (* A new commit of a PR whose analysis failed: nothing to compare with. *)
  let e = setup () in
  ignore (run_main ~suggest_ok:false e [ pr () ]);
  let state, calls, _ =
    run_main ~args:[ "--time-budget"; "-1" ] e
      [ pr ~kw:[ ("sha", `String (sha 'b')) ] () ]
  in
  no_calls "deferred" calls;
  let en = entry state in
  Alcotest.(check bool) "pending" true (Dict.truthy en "pending");
  List.iter
    (fun k -> Alcotest.(check bool) k false (Dict.mem en k))
    [ "error"; "attempts"; "since_base" ]

let test_failed_analysis_of_a_new_commit () =
  (* The first analysis of a new commit fails: no result (the old commit's
     would describe another commit), what changed since the last push is
     gone, and the previous result is kept to compare with. *)
  let e = setup () in
  ignore (run_main e [ pr () ]);
  let b = pr ~kw:[ ("sha", `String (sha 'b')) ] () in
  let state, _, _ = run_main e [ b ] in
  Alcotest.(check bool) "since" true (Dict.mem (entry state) "since_last_push");
  let c = pr ~kw:[ ("sha", `String (sha 'c')) ] () in
  let state, calls, changed = run_main ~tool:failed_suggest e [ c ] in
  check_kinds "analysed" [ "suggest --refresh"; "summary" ] calls;
  Alcotest.(check string) "changed" "changed=true" changed;
  let en = entry state in
  Alcotest.(check bool) "no result" true (Dict.get en "result" = None);
  Alcotest.(check string) "error" "timeout after 600s" (Dict.str en "error");
  Alcotest.(check bool)
    "no since_last_push" false
    (Dict.mem en "since_last_push");
  Alcotest.(check (option string))
    "base"
    (Some (sha 'b'))
    (Json.str_opt "sha" (get en "since_base"));
  Alcotest.(check int) "attempts" 1 (Dict.int en "attempts");
  Alcotest.(check bool) "refreshed" false (Dict.mem en "refresh_pending");
  Alcotest.check json "jobs"
    (j {|{"total": 2, "failed": {"analysis": 1}, "failing_revdeps": {}}|})
    (get en "jobs");
  check_err "stderr"
    [
      "1 open PRs, 1 with opam-ci = failure, 0 running (1 GitHub API call(s))";
      "LLM fallback: off (no ANTHROPIC_API_KEY)";
      "#1 cccccccccccc ERROR: timeout after 600s (Ns; requests: timeout=1, \
       refreshed=1)";
      "analysed 0, failed 1, unchanged 0, running without failure 0, deferred \
       0, dropped 0; tool requests: timeout=1, refreshed=1; wrote \
       TMP/data.json";
    ]
    e;
  check_stats "stats"
    {|{"github_api_calls": 1, "tool_requests": {"timeout": 1, "refreshed": 1},
       "refreshed": 1, "failed": 1, "dropped": 0}|}
    e;
  (* After a failed analysis of the previous commit: nothing to compare with. *)
  let e = setup () in
  ignore (run_main ~suggest_ok:false e [ pr () ]);
  let state, _, _ = run_main ~tool:failed_suggest e [ b ] in
  let en = entry state in
  Alcotest.(check (pair bool string))
    "failed"
    (false, "timeout after 600s")
    (Dict.get en "result" <> None, Dict.str en "error");
  Alcotest.(check bool) "no since_base" false (Dict.mem en "since_base")

let test_analysis_without_ci_page () =
  (* The CI page could not be read after the analysis: no job overview. *)
  let e = setup () in
  let state, calls, _ = run_main ~summary_ok:false e [ pr () ] in
  check_kinds "calls" [ "suggest --refresh"; "summary" ] calls;
  Alcotest.(check bool) "result" true (Dict.get (entry state) "result" <> None);
  Alcotest.check json "jobs" `Null (get (entry state) "jobs")

let test_llm_timeout_counts_a_call () =
  (* An analysis with the LLM that timed out may have been waiting for an
     LLM call: it counts as one in the run's budget. *)
  let e = setup () in
  let p n =
    pr ~kw:[ ("number", `Int n); ("sha", `String (sha (Char.chr (48 + n)))) ] ()
  in
  let tool a =
    if List.mem (sha '1') a then failed_suggest a
    else (Some ok_out, None, [ ("llm", 1) ])
  in
  let _, calls, _ =
    run_main ~llm:true ~tool ~args:[ "--llm-budget"; "2" ] e [ p 1; p 2; p 3 ]
  in
  Alcotest.(check (list (list string)))
    "llm options"
    [ [ "--llm"; "--llm-max"; "2" ]; [ "--llm"; "--llm-max"; "1" ]; [] ]
    (List.filter_map
       (fun c ->
         if List.hd c = "suggest" then
           Some
             (List.filter
                (fun x -> x = "--llm" || x = "--llm-max" || x = "1" || x = "2")
                (List.tl c))
         else None)
       calls);
  check_err "stderr"
    [
      "3 open PRs, 3 with opam-ci = failure, 0 running (1 GitHub API call(s))";
      "LLM fallback: on (at most 2 calls)";
      "#1 111111111111 ERROR: timeout after 600s (Ns; requests: timeout=1, \
       refreshed=1)";
      "#2 222222222222 ok (Ns; requests: llm=1)";
      "#3 333333333333 ok (Ns; requests: llm=1)";
      "analysed 2, failed 1, unchanged 0, running without failure 0, deferred \
       0, dropped 0; tool requests: timeout=1, refreshed=1, llm=2; wrote \
       TMP/data.json";
    ]
    e

let test_run_log () =
  (* One run where the PRs go each way but deferred (see
     test_deferred_new_pr): its stderr lines, counters and tool calls, in
     their order. *)
  let e = setup () in
  let p n ?(kw = []) () =
    pr
      ~kw:
        ([ ("number", `Int n); ("sha", `String (sha (Char.chr (48 + n)))) ] @ kw)
      ()
  in
  let running = [ ("ci_state", `String "PENDING") ] in
  ignore (run_main e [ p 1 (); p 4 (); p 5 ~kw:running (); p 7 ~kw:running () ]);
  (* The state saved when each tool call starts ("!": a failed redo kept
     the result; "*": the PR's new title): saved after each analysis, not
     after the other PRs. *)
  let saved = ref [] in
  let look () =
    let mark (k, en) =
      k
      ^ (if Dict.mem en "last_error" then "!" else "")
      ^ if Dict.str en "title" = "new" then "*" else ""
    in
    saved := String.concat "," (List.map mark (read_state e)) :: !saved
  in
  let fails a = List.mem (sha '3') a || List.mem (sha '4') a in
  let tool a =
    look ();
    if fails a then failed_suggest a else (Some ok_out, None, [ ("http", 1) ])
  in
  let summary a =
    look ();
    if List.mem (sha '5') a then
      (None, Some "timeout after 120s", [ ("timeout", 1) ])
    else if List.mem (sha '6') a then
      ( Some (`Assoc [ ("jobs", j {|[{"variant": "a", "status": "passed"}]|}) ]),
        None,
        [] )
    else (Some (`Assoc [ ("jobs", failed_analysis) ]), None, [])
  in
  let rebuilt = [ ("ci_created", `String "2026-11-20T11:00:00Z") ] in
  let renamed = [ ("title", `String "new") ] in
  let state, calls, changed =
    run_main ~tool ~summary e
      [
        p 1 ~kw:renamed ();
        p 2 ~kw:running ();
        p 3 ();
        p 4 ~kw:rebuilt ();
        p 5 ~kw:(running @ renamed) ();
        p 6 ~kw:running ();
        p 7 ~kw:(running @ renamed) ();
      ]
  in
  Alcotest.(check (list string))
    "calls"
    [
      "summary --refresh 2222222222";
      "suggest 2222222222";
      "suggest --refresh 3333333333";
      "summary 3333333333";
      "suggest --refresh 4444444444";
      "summary 5555555555";
      "summary --refresh 6666666666";
      "summary 7777777777";
    ]
    (List.map2
       (fun k c -> k ^ " " ^ String.sub (List.nth c 1) 0 10)
       (kinds calls) calls);
  Alcotest.(check string) "changed" "changed=true" changed;
  Alcotest.(check (list string))
    "state"
    [ "1"; "2"; "3"; "4"; "5"; "7" ]
    (List.map fst state);
  check_err "stderr"
    [
      "7 open PRs, 3 with opam-ci = failure, 4 running (1 GitHub API call(s))";
      "LLM fallback: off (no ANTHROPIC_API_KEY)";
      "#2 222222222222 ok (Ns; requests: http=1)";
      "#3 333333333333 ERROR: timeout after 600s (Ns; requests: timeout=1, \
       refreshed=1)";
      "#4 444444444444 ERROR: timeout after 600s; the previous result is kept \
       (Ns; requests: timeout=1, refreshed=1)";
      "#5 555555555555 summary failed: timeout after 120s; the previous result \
       is kept";
      "analysed 1, failed 2, unchanged 2, running without failure 1, deferred \
       0, dropped 0; tool requests: http=1, timeout=2, refreshed=2; wrote \
       TMP/data.json";
    ]
    e;
  check_stats "stats"
    {|{"github_api_calls": 1, "tool_requests": {"http": 1, "timeout": 2, "refreshed": 2},
       "unchanged": 2, "refreshed": 4, "analysed": 1, "failed": 2, "summary failed": 1,
       "running, no failure yet": 1, "dropped": 0}|}
    e;
  Alcotest.(check (list string))
    "saved"
    [
      "1,4,5,7";
      "1,4,5,7";
      "1*,4,5,7,2";
      "1*,4,5,7,2";
      "1*,4,5,7,2,3";
      "1*,4!,5,7,2,3";
      "1*,4!,5,7,2,3";
      "1*,4!,5,7,2,3";
    ]
    (List.rev !saved);
  Alcotest.(check bool)
    "kept" true
    (Dict.mem (List.assoc "4" state) "last_error");
  Alcotest.(check (list string))
    "titles"
    [ "new"; "t"; "t"; "t"; "new"; "new" ]
    (List.map (fun (_, en) -> Dict.str en "title") state)

let test_git_host_down_asked_again_a_day_later () =
  (* A git host found unreachable is not asked for a day (Sources.git_down);
     the analysis that skipped it is done again once that day has passed. *)
  let p = d {|{"sha": "a", "ci_state": "FAILURE"}|} in
  let now = Unix.time () in
  let at h = `String (iso_at (now -. (h *. 3600.))) in
  let prev =
    with_
      (d
         {|{"sha": "a", "ci_state": "FAILURE", "tool_version": "v", "result": {}, "attempts": 2,
                         "git_unreachable": 1}|})
      [ ("analysed_at", at 3.) ]
  in
  let first (a, _, _) = a in
  Alcotest.check triple "recent" (true, true, false)
    (T.reuse ~now_:now (Some prev) p "v" false);
  Alcotest.(check bool)
    "a day later" false
    (first
       (T.reuse ~now_:now
          (Some (with_ prev [ ("analysed_at", at 25.) ]))
          p "v" false));
  Alcotest.(check bool)
    "no host down" true
    (first
       (T.reuse ~now_:now
          (Some
             (with_ prev
                [ ("git_unreachable", `Int 0); ("analysed_at", at 25.) ]))
          p "v" false))

(* ------------------------------------------------------------------------ *)
(* PageTest: shared failures and stale labels (functions of this script). *)

let prs_of l = List.map (fun (k, v) -> (k, Dict.of_json v)) l

let also (prs : (string * Dict.t) list) k i =
  let s =
    List.nth
      (Json.list "suggestions"
         (Option.get (Dict.get (List.assoc k prs) "result")))
      i
  in
  match Json.member "also_failing_in" s with
  | `Null -> None
  | v -> Some (List.map Json.to_int (Json.to_list v))

let ilist = Alcotest.(option (list int))

let test_shared_failures () =
  let rd = Printf.sprintf "compilers,5.4,x.1,revdeps,%s" in
  let entry ?(target = "x.1") ?(category = "build") n failing suggestions :
      Json.t =
    (* The failing reverse dependencies of a PR testing [target], as jobs_overview stores them. *)
    `Assoc
      [
        ("number", `Int n);
        ( "jobs",
          `Assoc
            [
              ( "failing_revdeps",
                `Assoc
                  (List.map
                     (fun r ->
                       ( r,
                         `List
                           [
                             `Assoc
                               [
                                 ( "variant",
                                   `String
                                     (Printf.sprintf
                                        "compilers,5.4,%s,revdeps,%s" target r)
                                 );
                                 ("category", `String category);
                                 ("target", `String target);
                               ];
                           ] ))
                     failing) );
            ] );
        ( "result",
          `Assoc
            [
              ( "suggestions",
                `List
                  (List.map
                     (fun js ->
                       `Assoc
                         [ ("variant", `String "v"); ("jobs", Json.strings js) ])
                     suggestions) );
            ] );
      ]
  in
  let prs =
    prs_of
      [
        ( "1",
          entry 1
            [ "a.1"; "b.1"; "c.1"; "d.1" ]
            [
              [ rd "a.1"; rd "b.1"; rd "c.1" ];
              (* each in one other PR *)
              [ rd "d.1" ];
              (* in 3 other PRs *)
              [ rd "d.1"; "compilers,5.4,x.1" ];
              (* and a non-revdep job *)
              [ rd "a.1"; rd "d.1" ];
            ] );
        ("2", entry ~target:"y.1" 2 [ "a.1"; "d.1" ] []);
        ("3", entry ~target:"z.2" 3 [ "b.1"; "d.1" ] []);
        ("4", entry ~target:"w.1" 4 [ "c.1"; "d.1" ] []);
      ]
  in
  T.mark_shared_failures prs;
  Alcotest.(check (list (option (list int))))
    "marked"
    [ None; Some [ 2; 3; 4 ]; None; None ]
    (List.init 4 (also prs "1"));
  (* Other releases of the same package do not vouch for each other, nor do
     other PRs where the reverse dependency fails another way. *)
  let shared ?(suggestion = []) others =
    let mine =
      `Assoc
        (suggestion
        @ [ ("variant", `String "v"); ("jobs", Json.strings [ rd "d.1" ]) ])
    in
    let first =
      match entry 1 [ "d.1" ] [ [ rd "d.1" ] ] with
      | `Assoc kv ->
          `Assoc
            (List.map
               (fun (k, v) ->
                 if k = "result" then
                   (k, `Assoc [ ("suggestions", `List [ mine ]) ])
                 else (k, v))
               kv)
      | v -> v
    in
    let prs =
      prs_of
        (("1", first)
        :: List.mapi
             (fun i (target, category) ->
               ( string_of_int (i + 2),
                 entry ~target ~category (i + 2) [ "d.1" ] [] ))
             others)
    in
    T.mark_shared_failures prs;
    also prs "1" 0
  in
  Alcotest.check ilist "three"
    (Some [ 2; 3; 4 ])
    (shared [ ("y.1", "build"); ("z.1", "build"); ("w.1", "build") ]);
  Alcotest.check ilist "same package" None
    (shared [ ("x.2", "build"); ("x.3", "build"); ("w.1", "build") ]);
  Alcotest.check ilist "another way" None
    (shared [ ("y.1", "dependency"); ("z.1", "build"); ("w.1", "build") ]);
  (* Two releases of one package (y) and one of another: two packages, not three PRs' evidence. *)
  Alcotest.check ilist "two releases" None
    (shared [ ("y.1", "build"); ("y.2", "build"); ("w.1", "build") ]);
  Alcotest.check ilist "four"
    (Some [ 2; 3; 4; 5 ])
    (shared
       [
         ("y.1", "build"); ("y.2", "build"); ("w.1", "build"); ("v.1", "build");
       ]);
  (* PRs releasing several packages each (dune, dune-configurator,
     dune-site): their tested packages overlap, so three such PRs are one
     piece of evidence. *)
  let multi ?(sugg = []) n targets : Json.t =
    `Assoc
      [
        ("number", `Int n);
        ( "jobs",
          `Assoc
            [
              ( "failing_revdeps",
                `Assoc
                  [
                    ( "X.1",
                      `List
                        (List.map
                           (fun (t, c) ->
                             `Assoc
                               [
                                 ( "variant",
                                   `String
                                     (Printf.sprintf
                                        "compilers,5.3,%s,revdeps,X.1" t) );
                                 ("category", `String c);
                                 ("target", `String t);
                                 ("url", `String "u");
                               ])
                           targets) );
                  ] );
            ] );
        ("result", `Assoc [ ("suggestions", `List sugg) ]);
      ]
  in
  let dune n v =
    multi n
      (List.map
         (fun p -> (Printf.sprintf "%s.%s" p v, "build"))
         [ "dune"; "dune-configurator"; "dune-site" ])
  in
  let mine =
    j
      {|{"rule": "missing-library", "where_kind": "other-package", "where_pkg": "X.1", "class": "elsewhere",
                  "variant": "compilers,5.3,foo.1,revdeps,X.1", "jobs": ["compilers,5.3,foo.1,revdeps,X.1"]}|}
  in
  let base =
    [
      ("1", dune 1 "3.1");
      ("2", dune 2 "3.2");
      ("3", dune 3 "3.3");
      ("4", multi ~sugg:[ mine ] 4 [ ("foo.1", "build") ]);
    ]
  in
  let prs = prs_of base in
  T.mark_shared_failures prs;
  Alcotest.check ilist "dune PRs" None (also prs "4" 0);
  (* A PR releasing bar, and one releasing dune-site alone (it overlaps the
     dune PRs): two independent PRs. A third one (baz) makes it shared;
     every PR that fails the same way is listed. *)
  let more =
    [
      ("5", multi 5 [ ("bar.1", "build") ]);
      ("6", multi 6 [ ("dune-site.3.4", "build") ]);
    ]
  in
  let prs = prs_of (base @ more) in
  T.mark_shared_failures prs;
  Alcotest.check ilist "two" None (also prs "4" 0);
  let prs = prs_of (base @ more @ [ ("7", multi 7 [ ("baz.1", "build") ]) ]) in
  T.mark_shared_failures prs;
  Alcotest.check ilist "three" (Some [ 1; 2; 3; 5; 6; 7 ]) (also prs "4" 0);
  (* Only the jobs that fail the same way name the tested packages. *)
  let prs =
    prs_of
      [
        ("2", multi 2 [ ("y.1", "build"); ("a.1", "dependency") ]);
        ("3", multi 3 [ ("y.2", "build"); ("b.1", "dependency") ]);
        ("4", multi 4 [ ("w.1", "build") ]);
        ("1", multi ~sugg:[ mine ] 1 [ ("foo.1", "build") ]);
      ]
  in
  T.mark_shared_failures prs;
  Alcotest.check ilist "other ways" None (also prs "1" 0);
  let three = [ ("y.1", "build"); ("z.1", "build"); ("w.1", "build") ] in
  let kv s = Json.to_assoc (j s) in
  (* Never on a failure the evidence puts on this PR: it builds on master. *)
  Alcotest.check ilist "health-check-new" None
    (shared
       ~suggestion:(kv {|{"rule": "health-check-new", "fix_in": "this PR"}|})
       three);
  Alcotest.check ilist "pr" None
    (shared
       ~suggestion:
         (kv
            {|{"rule": "library-api", "fix_in": "this PR", "where_kind": "pr"}|})
       three);
  (* Nor when a rule ties the failure to the PR's package, or suggests a change for it. *)
  List.iter
    (fun s ->
      let s =
        kv s
        @ kv
            {|{"fix_in": "d.1 (other package)", "where_kind": "other-package", "where_pkg": "d.1"}|}
      in
      Alcotest.check ilist "tied" None (shared ~suggestion:s three))
    [
      {|{"rule": "library-api"}|};
      {|{"rule": "previous-release-bound"}|};
      {|{"rule": "lower-bound"}|};
      {|{"rule": "missing-library", "new": "\"x\" {< \"1\"}"}|};
    ];
  Alcotest.check ilist "elsewhere"
    (Some [ 2; 3; 4 ])
    (shared
       ~suggestion:
         (kv
            {|{"rule": "missing-library", "fix_in": "d.1 (other package)", "where_kind": "other-package", "where_pkg": "d.1"}|})
       three);
  (* Nor on jobs to rerun. *)
  Alcotest.check ilist "rerun" None
    (shared
       ~suggestion:
         (kv
            {|{"rule": "rerun", "fix_in": "CI (rerun)", "where_kind": "rerun"}|})
       three);
  let prs =
    prs_of
      (( "1",
         `Assoc
           [
             ("number", `Int 1);
             ("jobs", j {|{"failing_revdeps": {}}|});
             ( "result",
               `Assoc
                 [
                   ( "suggestions",
                     `List
                       [
                         `Assoc
                           [
                             ("rule", `String "rerun");
                             ("where_kind", `String "rerun");
                             ("variant", `String "1 job");
                             ("jobs", Json.strings [ rd "d.1" ]);
                           ];
                       ] );
                 ] );
           ] )
      :: List.map
           (fun (k, t) -> (string_of_int k, entry ~target:t k [ "d.1" ] []))
           [ (2, "y.1"); (3, "z.1"); (4, "w.1") ])
  in
  T.mark_shared_failures prs;
  Alcotest.check ilist "rerun job" None (also prs "1" 0)

let test_shared_failures_same_platform () =
  (* The other PRs must fail on the same platform as each job of the suggestion. *)
  let job ?(rd = "bar.1") plat target =
    `Assoc
      [
        ( "variant",
          `String (Printf.sprintf "compilers,%s,%s,revdeps,%s" plat target rd)
        );
        ("category", `String "build");
        ("target", `String target);
      ]
  in
  let mine = [ job "4.14" "foo.2"; job "5.5" "foo.2" ] in
  let sugg =
    `Assoc
      [
        ("rule", `String "missing-library");
        ("where_kind", `String "other-package");
        ("where_pkg", `String "bar.1");
        ("class", `String "elsewhere");
        ("variant", `String "2 jobs");
        ("jobs", `List (List.map (fun j -> Json.member "variant" j) mine));
      ]
  in
  let run plats =
    let prs =
      prs_of
        (( "1",
           `Assoc
             [
               ("number", `Int 1);
               ( "jobs",
                 `Assoc
                   [ ("failing_revdeps", `Assoc [ ("bar.1", `List mine) ]) ] );
               ("result", `Assoc [ ("suggestions", `List [ sugg ]) ]);
             ] )
        :: List.map
             (fun (n, t) ->
               ( string_of_int n,
                 `Assoc
                   [
                     ("number", `Int n);
                     ( "jobs",
                       `Assoc
                         [
                           ( "failing_revdeps",
                             `Assoc
                               [
                                 ( "bar.1",
                                   `List (List.map (fun p -> job p t) plats) );
                               ] );
                         ] );
                     ("result", `Assoc [ ("suggestions", `List []) ]);
                   ] ))
             [ (2, "lwt.6"); (3, "dune.3"); (4, "cmdliner.2") ])
    in
    T.mark_shared_failures prs;
    also prs "1" 0
  in
  Alcotest.check ilist "5.5 only" None (run [ "5.5" ]);
  Alcotest.check ilist "flambda" None (run [ "4.14-flambda"; "5.5" ]);
  Alcotest.check ilist "both" (Some [ 2; 3; 4 ]) (run [ "4.14"; "5.5" ]);
  Alcotest.(check (option string))
    "platform" (Some "compilers,4.14,")
    (T.job_platform (Some "compilers,4.14,foo.2,revdeps,bar.1"))

let test_stale_labels () =
  let lab =
    {|{"__typename": "LabeledEvent", "createdAt": "2026-11-10T00:00:00Z", "label": {"name": "needs reporter action"}}|}
  in
  let node ?(labels = {|[{"name": "needs reporter action"}]|}) items =
    j
      (Printf.sprintf
         {|{"author": {"login": "me"}, "labels": {"nodes": %s}, "timelineItems": {"nodes": [%s]}}|}
         labels
         (String.concat ", " (lab :: items)))
  in
  let c = Alcotest.(check (list string)) in
  let stale = [ "needs reporter action" ] in
  c "label only" [] (T.stale_labels (node []));
  c "removed" []
    (T.stale_labels
       (node ~labels:"[]"
          [
            {|{"__typename": "HeadRefForcePushedEvent", "createdAt": "2026-11-11T00:00:00Z"}|};
          ]));
  c "push" stale
    (T.stale_labels
       (node
          [
            {|{"__typename": "HeadRefForcePushedEvent", "createdAt": "2026-11-11T00:00:00Z"}|};
          ]));
  c "commit" stale
    (T.stale_labels
       (node
          [
            {|{"__typename": "PullRequestCommit", "commit": {"committedDate": "2026-11-11T00:00:00Z"}}|};
          ]));
  (* A comment counts only from the author, and only after the label. *)
  c "maintainer" []
    (T.stale_labels
       (node
          [
            {|{"__typename": "IssueComment", "createdAt": "2026-11-11T00:00:00Z", "author": {"login": "maintainer"}}|};
          ]));
  c "before" []
    (T.stale_labels
       (node
          [
            {|{"__typename": "IssueComment", "createdAt": "2026-11-09T00:00:00Z", "author": {"login": "me"}}|};
          ]));
  c "after" stale
    (T.stale_labels
       (node
          [
            {|{"__typename": "IssueComment", "createdAt": "2026-11-11T00:00:00Z", "author": {"login": "me"}}|};
          ]))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "page_run"
    [
      ( "reuse",
        [
          t "reuse" test_reuse;
          t "attempts_expire" test_attempts_expire;
          t "git_host_down_asked_again_a_day_later"
            test_git_host_down_asked_again_a_day_later;
        ] );
      ( "run",
        [
          t "timeout_keeps_counts" test_timeout_keeps_counts;
          t "graphql_errors" test_graphql_errors;
        ] );
      ( "refresh",
        [
          t "ci_changed" test_ci_changed;
          t "unfinished_jobs" test_unfinished_jobs;
          t "refresh_drops_logs" test_refresh_drops_logs;
        ] );
      ( "main",
        [
          t "pending_with_finished_page" test_pending_with_finished_page;
          t "pending_failures_limited" test_pending_failures_limited;
          t "refresh" test_refresh;
          t "deferred_keeps_result_of_the_same_commit"
            test_deferred_keeps_result_of_the_same_commit;
          t "failed_redo_keeps_result" test_failed_redo_keeps_result;
          t "failed_redo_of_a_rebuild_limited"
            test_failed_redo_of_a_rebuild_limited;
          t "deferred_new_commit_drops_old_fields"
            test_deferred_new_commit_drops_old_fields;
          t "pending_summary_failed_keeps_result"
            test_pending_summary_failed_keeps_result;
          t "llm_budget_spent" test_llm_budget_spent;
          t "llm_flag_needs_every_answer" test_llm_flag_needs_every_answer;
          t "llm_retries_limited" test_llm_retries_limited;
          t "llm_not_asked_while_page_unfinished"
            test_llm_not_asked_while_page_unfinished;
          t "llm_off_for_the_run" test_llm_off_for_the_run;
          t "refresh_not_done" test_refresh_not_done;
          t "kept_result_during_a_rebuild" test_kept_result_during_a_rebuild;
          t "since_last_push_after_a_deferred_or_failed_analysis"
            test_since_last_push_after_a_deferred_or_failed_analysis;
          t "pending_summary_failed_keeps_failed_analysis"
            test_pending_summary_failed_keeps_failed_analysis;
          t "running_without_failure_yet" test_running_without_failure_yet;
          t "deferred_new_pr" test_deferred_new_pr;
          t "failed_analysis_of_a_new_commit"
            test_failed_analysis_of_a_new_commit;
          t "run_log" test_run_log;
          t "analysis_without_ci_page" test_analysis_without_ci_page;
          t "llm_timeout_counts_a_call" test_llm_timeout_counts_a_call;
        ] );
      ( "shared",
        [
          t "shared_failures" test_shared_failures;
          t "shared_failures_same_platform" test_shared_failures_same_platform;
          t "stale_labels" test_stale_labels;
        ] );
    ]
