(* The LLM fallback (no API call: llm_suggest or the API call are faked):
   which failures it gets, which changes are shown as a diff, which package
   names are kept, and errors that leave the failure unrecognised. (Python:
   LlmFallbackTest, and the LLM tests of EscapingTest.) *)

open Triage
open Suggestion.T

(* A suggestion from its JSON members. *)
let sugg kv = Suggestion.of_json (`Assoc kv)

let job ?(variant = "v") () : Ci.job =
  {
    variant;
    url = "";
    label = "";
    status = "failed";
    reason = None;
    category = "build";
    target = Some "x.1";
    kind = "build";
    revdep = None;
    platform = [];
    experimental = false;
    in_summary = false;
    messages = [];
  }

let data jobs : Ci.data =
  {
    pr = None;
    jobs;
    unfinished = 0;
    unanalysed = 0;
    health_stale = None;
    llm_unanswered = None;
    sha = None;
  }

let tmp = ref ""

(* Each test starts with the LLM on (its breaker is process-wide), the real
   functions, and an empty cache directory. *)
let setup () =
  Llm.llm_state.streak <- 0;
  Llm.llm_state.off <- None;
  (Llm.fetch_hook := fun _ _ -> "log");
  Llm.log_text_hook := Fun.id;
  (Llm.excerpt_hook := fun ?max_output_lines:_ _ -> [ ("error_block", "e") ]);
  (Llm.key_error_line_hook := fun _ -> Some "e");
  Llm.llm_suggest_hook := Llm.llm_suggest;
  Llm.llm_prompt_hook := Llm.llm_prompt;
  Llm.create_hook := Llm.messages_create;
  tmp := Filename.temp_dir "test-llm" ""

let result ?(variant = "v") ?(error = "E") ?(failing = "x.1")
    ?(note = "Unrecognised error.") ?(unexplained = true) extra =
  let r =
    sugg
      ([
         ("variant", `String variant);
         ("url", `String "https://opam.ci.ocaml.org/x");
         ("note", `String note);
         ("error", `String error);
         ("failing", `String failing);
       ]
      @ extra)
  in
  r.unexplained <- unexplained;
  r

(* [_names]: the package names in the prompt (llm_suggest adds them). *)
let answer_fields ?(names = true) kw =
  let base =
    [
      ("diagnosis", `String "d");
      ("fix_in", `String "this-pr");
      ("package", `String "x.1");
      ("change_old", `String "");
      ("change_new", `String "");
      ("confidence", `String "low");
      ("verify", `String "v");
    ]
    @
    if names then [ ("_names", Json.strings [ "x"; "x.1"; "zarith" ]) ] else []
  in
  List.map
    (fun (k, v) ->
      match List.assoc_opt k kw with Some v' -> (k, v') | None -> (k, v))
    base
  @ List.filter (fun (k, _) -> not (List.mem_assoc k base)) kw

let answer kw = Dict.of_list (answer_fields kw)

let quiet f =
  (* stderr notes are not checked *)
  let fd = Unix.dup Unix.stderr in
  let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 null Unix.stderr;
  Fun.protect
    ~finally:(fun () ->
      flush stderr;
      Unix.dup2 fd Unix.stderr;
      Unix.close fd;
      Unix.close null)
    f

let apply ?(pr_packages = [ "x.1" ]) ?suggest ?res r =
  (match (suggest, res) with
  | Some s, _ -> Llm.llm_suggest_hook := s
  | None, Some res ->
      Llm.llm_suggest_hook := fun ?call:_ _ _ _ _ _ _ _ _ _ -> Dict.copy res
  | None, None -> ());
  quiet (fun () ->
      ignore
        (Llm.apply_llm [ r ]
           (data [ job () ])
           (Sources.create !tmp) !tmp pr_packages 5 "m" false));
  r

let contains s sub = Util.contains s sub

let check_contains msg s sub =
  Alcotest.(check bool) (msg ^ ": " ^ s) true (contains s sub)

let test_change_shapes () =
  setup ();
  let ok old new_ =
    Llm.llm_change_as_diff ~names:[ "foo"; "bar" ] (Some old) new_
  in
  List.iter
    (fun n -> Alcotest.(check bool) n true (ok "" n))
    [
      {|"foo"|};
      {|"foo" {with-test}|};
      {|"foo" {build & with-test}|};
      {|available: os != "win32"|};
      {|x-ci-accept-failures: ["alpine-3.22"]|};
      "x-maintained: false";
      "\"foo\" {with-test}\n\"bar\"";
    ];
  Alcotest.(check bool) "old dep" true (ok {|"foo"|} {|"foo" {with-test}|});
  (* A dependency the model was not given (an invented name): text, not a diff. *)
  List.iter
    (fun n -> Alcotest.(check bool) n false (ok "" n))
    [ {|"anything"|}; "\"foo\"\n\"anything\" {with-test}" ];
  Alcotest.(check bool)
    "names unknown" false
    (Llm.llm_change_as_diff (Some "") {|"foo"|});
  Alcotest.(check bool)
    "x-maintained" true
    (Llm.llm_change_as_diff (Some "") "x-maintained: false");
  List.iter
    (fun (o, n) -> Alcotest.(check bool) (o ^ " -> " ^ n) false (ok o n))
    [
      ("", {|build: [["dune" "build" "-p" name]]|});
      ("", {|install: [make "install"]|});
      ("", {|url { src: "https://e.x/a.tgz" }|});
      ("", {|src: "https://e.x/a.tgz"|});
      ("", {|"foo" {>= "1.0"}|});
      ("", {|depexts: ["libgmp-dev"]|});
      ("", {|available: ocaml:version < "5.0"|});
      ("", "depends: [\n\"foo\"\n]");
      ({|build: [["make"]]|}, {|"foo"|});
      ({|"foo" {>= "1.0"}|}, {|"foo"|});
    ];
  (* Through apply_llm: a build command is text in the note, a missing dependency is a diff. *)
  let r =
    apply
      ~res:(answer [ ("change_new", `String {|build: [["make" "-j1"]]|}) ])
      (result [])
  in
  Alcotest.(check bool) "no new" false (r.Suggestion.new_ <> Absent);
  check_contains "note" (Suggestion.str r.note)
    {|Possible change, not checked: `build: [["make" "-j1"]]`.|};
  let r =
    apply
      ~res:(answer [ ("change_new", `String {|"zarith" {with-test}|}) ])
      (result [])
  in
  Alcotest.(check string)
    "new" {|"zarith" {with-test}|}
    (Suggestion.str_field r.Suggestion.new_);
  Alcotest.(check bool) "old null" true (r.old = Null);
  let r =
    apply ~res:(answer [ ("change_new", `String {|"anything"|}) ]) (result [])
  in
  Alcotest.(check bool) "no new" false (r.Suggestion.new_ <> Absent);
  check_contains "note" (Suggestion.str r.note)
    {|Possible change, not checked: `"anything"`.|}

let test_prompt_names () =
  (* The names the model was given: the PR's packages, the failing one, the
     installed dependencies, the opam file and the error excerpt; not the
     earlier analysis. *)
  let prompt =
    {|Failing job: v (blocking platform)
Packages of this PR: x.1, y.2
Failing package: z.3 (not part of this PR)

Dependency versions installed by CI:
{"dune": "3.20.2", "fmt": "0.9.0"}

<opam_file package="z.3">
depends: [ "ocaml" "lwt" {>= "5.0"} ]
</opam_file>

<error_excerpt>
Error: Library "ppx_deriving.show" not found.
</error_excerpt>

Earlier analysis: anything goes|}
  in
  let names = Llm.prompt_names prompt in
  List.iter
    (fun n -> Alcotest.(check bool) n true (List.mem n names))
    [
      "x";
      "y";
      "z";
      "dune";
      "fmt";
      "ocaml";
      "lwt";
      "ppx_deriving";
      "ppx_deriving.show";
    ];
  Alcotest.(check bool) "anything" false (List.mem "anything" names)

let run_with ~jobs ~suggest rs limit =
  Llm.llm_suggest_hook := suggest;
  quiet (fun () ->
      Llm.apply_llm rs (data jobs) (Sources.create !tmp) !tmp [ "x.1" ] limit
        "m" false)

let test_unanswered_counted () =
  (* apply_llm says how many distinct failures are left without an answer a
     later call may give: beyond the limit, or a transient API error; not a
     refusal (asking again would cost the same). *)
  setup ();
  let mk () =
    List.init 3 (fun i ->
        result ~variant:(Printf.sprintf "v%d" i)
          ~error:(Printf.sprintf "E%c" (Char.chr (97 + i)))
          [])
  in
  let jobs = List.init 3 (fun i -> job ~variant:(Printf.sprintf "v%d" i) ()) in
  let answers = function
    | "v0" -> answer []
    | "v1" ->
        Dict.of_list
          [
            ("llm_error", `String "API error 529: overloaded");
            ("transient", `Bool true);
          ]
    | _ -> Dict.of_list [ ("llm_error", `String "stopped: refusal") ]
  in
  let ask ?(call = true) _ r _ _ _ _ _ _ _ =
    if call then answers (Suggestion.str r.Suggestion.variant)
    else Dict.of_list [ ("llm_skipped", `String "beyond the limit") ]
  in
  Alcotest.(check int) "limit 3" 1 (run_with ~jobs ~suggest:ask (mk ()) 3)
  (* the transient error *);
  Alcotest.(check int) "limit 1" 2 (run_with ~jobs ~suggest:ask (mk ()) 1)
  (* two beyond the limit *);
  Alcotest.(check int) "limit 0" 3 (run_with ~jobs ~suggest:ask (mk ()) 0)

let test_package_checked () =
  setup ();
  (* A package the input does not name: dropped, with the claim of where the fix goes. *)
  let r =
    apply
      ~res:
        (answer
           [
             ("package", `String "evil.6.6");
             ("fix_in", `String "other-package");
             ("change_new", `String {|"zarith"|});
           ])
      (result [])
  in
  Alcotest.(check (pair string string))
    "evil" ("x.1", "unknown")
    (Suggestion.str_field r.Suggestion.failing, Suggestion.str r.fix_in);
  Alcotest.(check bool) "no new" false (r.Suggestion.new_ <> Absent);
  check_contains "note" (Suggestion.str r.note)
    "Possible change to `evil.6.6`, not checked";
  (* One of the PR's packages, or the failing one (a name alone is enough). *)
  let r =
    apply
      ~res:
        (answer
           [ ("package", `String "x"); ("change_new", `String {|"zarith"|}) ])
      (result ~failing:"y.2" [])
  in
  Alcotest.(check (list string))
    "x"
    [ "x.1"; "this PR"; {|"zarith"|} ]
    [
      Suggestion.str_field r.Suggestion.failing;
      Suggestion.str r.fix_in;
      Suggestion.str_field r.Suggestion.new_;
    ];
  let r =
    apply
      ~res:
        (answer
           [ ("package", `String "y.2"); ("fix_in", `String "other-package") ])
      (result ~failing:"y.2" [])
  in
  Alcotest.(check (pair string string))
    "y.2"
    ("y.2", "y.2 (other package)")
    (Suggestion.str_field r.Suggestion.failing, Suggestion.str r.fix_in);
  let r = apply ~res:(answer [ ("package", `String "") ]) (result []) in
  Alcotest.(check (pair string string))
    "empty" ("x.1", "this PR")
    (Suggestion.str_field r.Suggestion.failing, Suggestion.str r.fix_in)

let message ?(stop = "end_turn") text : Llm.api_result =
  Message
    (`Assoc
       [
         ("stop_reason", `String stop);
         ( "content",
           `List
             (match text with
             | None -> []
             | Some t ->
                 [ `Assoc [ ("type", `String "text"); ("text", `String t) ] ])
         );
         ("model", `String "m");
         ( "usage",
           `Assoc [ ("input_tokens", `Int 1); ("output_tokens", `Int 1) ] );
       ])

let llm_files () =
  List.filter (fun f -> String.starts_with ~prefix:"llm-" f) (Util.listdir !tmp)

let suggest_once prompt =
  (Llm.llm_prompt_hook := fun _ _ _ _ _ _ -> prompt);
  quiet (fun () ->
      Llm.llm_suggest (Sources.create !tmp) (Suggestion.create ()) (job ())
        "log" (data []) [] !tmp "m" false)

let test_errors_leave_the_failure_unrecognised () =
  setup ();
  let boom ?call:_ _ _ _ _ _ _ _ _ _ =
    raise (Http.Net_error "Connection reset by peer")
  in
  let r = apply ~suggest:boom (result []) in
  Alcotest.(check bool) "no rule" false (r.rule <> None);
  check_contains "llm_error" (Suggestion.str r.llm_error) "Net_error";
  (* The API: no credentials, an answer that is not JSON, no text, a bad field. *)
  let cases =
    [
      ((fun () -> Llm.Sdk_error ("AnthropicError", "no api_key")), "no api_key");
      ((fun () -> message (Some "not json")), "no JSON answer");
      ((fun () -> message None), "no JSON answer");
      ( (fun () ->
          message
            (Some
               (Json.dumps
                  (Dict.to_json (answer [ ("fix_in", `String "bogus") ]))))),
        "unknown fix_in" );
      ((fun () -> message (Some {|{"diagnosis": "d"}|})), "no fix_in");
      ((fun () -> message (Some "[1]")), "not a JSON object");
    ]
  in
  List.iteri
    (fun i (create, why) ->
      Llm.llm_state.off <-
        None (* no credentials turn the LLM off for the run *);
      (Llm.create_hook := fun ~model:_ ~prompt:_ -> create ());
      let res = suggest_once (Printf.sprintf "prompt %d" i) in
      check_contains why (Dict.str res "llm_error") why)
    cases;
  Alcotest.(check (list string)) "nothing cached" [] (llm_files ());
  (* A client timeout, one retry: not the SDK's 10 minutes and 2 retries. *)
  Alcotest.(check (pair (float 0.) int))
    "timeout, retries" (120., 1)
    (Llm.llm_timeout, Llm.llm_max_retries);
  (* A good answer is cached; a damaged cached one is an error, not a crash. *)
  (Llm.create_hook :=
     fun ~model:_ ~prompt:_ ->
       message
         (Some
            (Json.dumps
               (Dict.to_json (Dict.of_list (answer_fields ~names:false []))))));
  Alcotest.(check string)
    "fix_in" "this-pr"
    (Dict.str (suggest_once "good") "fix_in");
  let cached = llm_files () in
  Alcotest.(check int) "one cached" 1 (List.length cached);
  let oc = open_out (Filename.concat !tmp (List.hd cached)) in
  output_string oc {|{"diagnosis": 3}|};
  close_out oc;
  check_contains "damaged"
    (Dict.str (suggest_once "good") "llm_error")
    "unusable cached answer"

(* apply_llm on [n] distinct failures, [runs] times (each run a new process:
   the LLM state reset), through llm_suggest and its cache with a fake API
   call [create calls]; [(API calls, failures left, llm-off)] per run. *)
let sdk_run ?(n = 3) ?(limit = 2) ?(runs = 1) create =
  setup ();
  let calls = ref 0 in
  (Llm.create_hook := fun ~model:_ ~prompt:_ -> create calls);
  (Llm.llm_prompt_hook :=
     fun _ r _ _ _ _ -> "prompt " ^ Suggestion.str r.Suggestion.variant);
  let jobs = List.init n (fun i -> job ~variant:(Printf.sprintf "v%d" i) ()) in
  List.init runs (fun _ ->
      Llm.llm_state.streak <- 0;
      Llm.llm_state.off <- None;
      Net.net.Odict.kv <- [];
      let before = !calls in
      let rs =
        List.init n (fun i ->
            result ~variant:(Printf.sprintf "v%d" i)
              ~error:(Printf.sprintf "E%c" (Char.chr (97 + i)))
              [])
      in
      let left =
        quiet (fun () ->
            Llm.apply_llm rs (data jobs) (Sources.create !tmp) !tmp [ "x.1" ]
              limit "m" false)
      in
      (!calls - before, left, Odict.get Net.net "llm-off"))

let runs = Alcotest.(list (triple int int (option int)))

let test_limit_counts_api_calls_only () =
  (* Three distinct failures, two calls per run: cached answers do not count
     toward the limit, so the second run asks about the third failure, and
     the third run asks nothing. *)
  let text =
    Json.dumps (Dict.to_json (Dict.of_list (answer_fields ~names:false [])))
  in
  let create calls =
    incr calls;
    message (Some text)
  in
  Alcotest.check runs "runs"
    [ (2, 1, None); (1, 0, None); (0, 0, None) ]
    (sdk_run ~runs:3 create)

let test_credentials_refused () =
  (* A bad key (401) or no permission (403): one call, then the LLM is off for
     the run, and the failures count as unanswered (asked again once the key
     is fixed). *)
  let create calls =
    incr calls;
    Llm.Status_error (401, "invalid x-api-key")
  in
  Alcotest.check runs "runs" [ (1, 3, Some 1) ] (sdk_run ~limit:5 create)

let test_breaker () =
  (* An outage: after llm_breaker transient failures in a row, no more calls in this run. *)
  let create calls =
    incr calls;
    Llm.Connection_error "Request timed out."
  in
  Alcotest.check runs "outage"
    [ (Llm.llm_breaker, 4, Some 1) ]
    (sdk_run ~n:4 ~limit:5 create);
  (* A success in between: not in a row. *)
  let flaky calls =
    incr calls;
    if !calls mod 2 = 1 then Llm.Status_error (529, "overloaded")
    else Llm.Status_error (400, "bad request")
    (* not transient: not an outage *)
  in
  Alcotest.check runs "flaky" [ (4, 2, None) ] (sdk_run ~n:4 ~limit:5 flaky)

let test_new_on_master_reaches_the_llm () =
  setup ();
  (* opam-health-check sees the reverse dependency build on master: the hint
     is put before the note; the failure is still unexplained. *)
  let hint =
    "y.2 builds on master (opam-health-check, 17 Nov), so this PR probably \
     causes this failure."
  in
  let r =
    apply ~res:(answer [])
      (result
         ~note:(hint ^ " Unrecognised error.")
         [ ("new_on_master", `String hint) ])
  in
  Alcotest.(check string) "rule" "llm" (Suggestion.str r.rule);
  Alcotest.(check bool)
    "note" true
    (String.starts_with
       ~prefix:(hint ^ " d Suggested next step")
       (Suggestion.str r.note));
  let r =
    apply ~res:(answer [])
      (result ~note:"Pre-existing: z. Unrecognised error."
         [ ("preexisting", `String "z") ])
  in
  Alcotest.(check string) "rule" "llm" (Suggestion.str r.rule);
  (* Without the rules' flag (a rule explained it), the LLM is not asked. *)
  let r = apply ~res:(answer []) (result ~unexplained:false []) in
  Alcotest.(check bool) "no rule" false (r.rule <> None)

(* EscapingTest.test_llm_change_is_code (its render_comment part is in test_page). *)
let test_llm_change_is_code () =
  setup ();
  let r =
    sugg
      [
        ("variant", `String "v");
        ("url", `String "https://opam.ci.ocaml.org/x");
        ("note", `String "Unrecognised error.");
        ("error", `String "E");
      ]
  in
  r.unexplained <- true;
  let res =
    Dict.of_list
      [
        ("fix_in", `String "this-pr");
        ("confidence", `String "low");
        ("diagnosis", `String "d");
        ("verify", `String "v");
        ( "change_new",
          `String "depexts: [\"a\"] {os-distribution = \"debian\"}\n```\n@x `y`"
        );
        ("package", `String "x.1");
      ]
  in
  let r = apply ~res r in
  check_contains "note" (Suggestion.str r.note)
    {|not checked: ```` depexts: ["a"] {os-distribution = "debian"} ``` @x `y` ````.|};
  Alcotest.(check bool) "no new" false (r.Suggestion.new_ <> Absent)

(* EscapingTest.test_key_error_line *)
let test_key_error_line () =
  (* A check that prints nothing (conf-glib-2 on msys2, #31054), or test output
     without an error line (#27694): opam's line naming the command. *)
  let head =
    "[ERROR] The compilation of conf-glib-2.1 failed at \"pkg-config \
     glib-2.0\".\n\n"
  in
  let block =
    "#=== ERROR while compiling conf-glib-2.1 ===#\n\
     # exit-code 1\n\
     # output-file ~/x.out\n"
  in
  Alcotest.(check (option string))
    "conf"
    (Some {|The compilation of conf-glib-2.1 failed at "pkg-config glib-2.0"|})
    (Llm.key_error_line (head ^ block));
  Alcotest.(check (option string))
    "tests"
    (Some {|The compilation of pds.6.55 failed at "pkg-config glib-2.0"|})
    (Llm.key_error_line
       (Util.replace head "conf-glib-2.1" "pds.6.55"
       ^ Util.replace block "conf-glib-2.1" "pds.6.55"
       ^ "### output ###\n# Passed tests\n"))

(* The request sent to the Messages API, and the SDK's retry: not in the
   Python tests (the SDK did that), checked here on a fake transport. *)
let with_env k v f =
  let old = Sys.getenv_opt k in
  Unix.putenv k v;
  Fun.protect
    ~finally:(fun () -> Unix.putenv k (Option.value ~default:"" old))
    f

let test_http_request () =
  setup ();
  let sent = ref [] in
  let replies = ref [] in
  let real = !Http.hook in
  (Http.hook :=
     fun req ->
       sent := req :: !sent;
       match !replies with
       | r :: rest ->
           replies := rest;
           r ()
       | [] -> assert false);
  Fun.protect ~finally:(fun () -> Http.hook := real) @@ fun () ->
  with_env "ANTHROPIC_BASE_URL" "" @@ fun () ->
  with_env "ANTHROPIC_API_KEY" "sk-test" @@ fun () ->
  let ok_body =
    {|{"stop_reason": "end_turn", "model": "m", "content": [{"type": "text", "text": "{}"}], "usage": {"input_tokens": 1, "output_tokens": 2}}|}
  in
  let resp ?(headers = []) status data () =
    { Http.status; resp_headers = headers; data }
  in
  (* An overloaded API: one retry (after its retry-after-ms), then the answer. *)
  replies :=
    [
      resp ~headers:[ ("retry-after-ms", "1") ] 529 {|{"type": "error"}|};
      resp 200 ok_body;
    ];
  (match Llm.messages_create ~model:"claude-opus-5-5" ~prompt:"P" with
  | Message m ->
      Alcotest.(check string) "stop" "end_turn" (Json.str "stop_reason" m)
  | _ -> Alcotest.fail "no message");
  Alcotest.(check int) "two requests" 2 (List.length !sent);
  let req = List.hd !sent in
  Alcotest.(check string) "url" "https://api.anthropic.com/v1/messages" req.url;
  Alcotest.(check string) "method" "POST" req.meth;
  Alcotest.(check (float 0.)) "timeout" 120. req.timeout;
  let h k = Http.header req.headers k in
  Alcotest.(check (option string))
    "auth" (Some "Bearer sk-test") (h "Authorization");
  Alcotest.(check (option string))
    "version" (Some "2026-10-21") (h "anthropic-version");
  Alcotest.(check (option string))
    "beta" (Some "server-side-fallback-2026-07-01") (h "anthropic-beta");
  let body = Json.parse (Option.get req.body) in
  Alcotest.(check string) "model" "claude-opus-5-5" (Json.str "model" body);
  Alcotest.(check (option int))
    "max_tokens" (Some 16000)
    (Json.int_opt "max_tokens" body);
  Alcotest.(check string) "fallbacks" "default" (Json.str "fallbacks" body);
  Alcotest.(check string) "system" Llm.llm_system (Json.str "system" body);
  Alcotest.(check string)
    "effort" "medium"
    (Json.str "effort" (Json.member "output_config" body));
  Alcotest.(check bool)
    "schema" true
    (Json.member "schema"
       (Json.member "format" (Json.member "output_config" body))
    = Llm.llm_schema);
  (* A bad key: not retried, the SDK's message. *)
  sent := [];
  replies :=
    [
      resp 401
        {|{"type": "error", "error": {"type": "authentication_error", "message": "invalid x-api-key"}}|};
    ];
  (match Llm.messages_create ~model:"m" ~prompt:"P" with
  | Status_error (401, m) ->
      Alcotest.(check string)
        "message"
        "Error code: 401 - {'type': 'error', 'error': {'type': \
         'authentication_error', 'message': 'invalid x-api-key'}}"
        m
  | _ -> Alcotest.fail "no 401");
  Alcotest.(check int) "one request" 1 (List.length !sent);
  (* x-should-retry: false is obeyed. *)
  sent := [];
  replies := [ resp ~headers:[ ("x-should-retry", "false") ] 500 "oops" ];
  (match Llm.messages_create ~model:"m" ~prompt:"P" with
  | Status_error (500, "oops") -> ()
  | _ -> Alcotest.fail "no 500");
  Alcotest.(check int) "not retried" 1 (List.length !sent);
  (* A network error: retried once, then a connection error. *)
  sent := [];
  replies :=
    [
      (fun () -> raise (Http.Net_error "timed out (x)"));
      (fun () -> raise (Http.Net_error "timed out (x)"));
    ];
  (match Llm.messages_create ~model:"m" ~prompt:"P" with
  | Connection_error "Request timed out." -> ()
  | _ -> Alcotest.fail "no connection error");
  Alcotest.(check int) "retried" 2 (List.length !sent)

let () =
  Alcotest.run "llm"
    [
      ( "llm",
        [
          Alcotest.test_case "change_shapes" `Quick test_change_shapes;
          Alcotest.test_case "prompt_names" `Quick test_prompt_names;
          Alcotest.test_case "unanswered_counted" `Quick test_unanswered_counted;
          Alcotest.test_case "package_checked" `Quick test_package_checked;
          Alcotest.test_case "errors_leave_the_failure_unrecognised" `Quick
            test_errors_leave_the_failure_unrecognised;
          Alcotest.test_case "limit_counts_api_calls_only" `Quick
            test_limit_counts_api_calls_only;
          Alcotest.test_case "credentials_refused" `Quick
            test_credentials_refused;
          Alcotest.test_case "breaker" `Quick test_breaker;
          Alcotest.test_case "new_on_master_reaches_the_llm" `Quick
            test_new_on_master_reaches_the_llm;
          Alcotest.test_case "llm_change_is_code" `Quick test_llm_change_is_code;
          Alcotest.test_case "key_error_line" `Quick test_key_error_line;
          Alcotest.test_case "http_request" `Quick test_http_request;
        ] );
    ]
