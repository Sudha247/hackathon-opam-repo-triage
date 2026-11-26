(* Tests of the PR-level checks (lib/context.ml), on synthetic jobs: no
   fixture, no network. Ported from tests/test_triage.py (LintTest,
   BoundedOutTest, and the context tests of RulesTest). *)

open Triage
open Ci

let check_str = Alcotest.(check string)
let check_bool = Alcotest.(check bool)

let check_in ?(msg = "") sub s =
  if not (Util.contains s sub) then Alcotest.failf "%s: %S not in %S" msg sub s

let check_not_in sub s =
  if Util.contains s sub then Alcotest.failf "%S in %S" sub s

let one = function
  | [ r ] -> r
  | l -> Alcotest.failf "expected one result, got %d" (List.length l)

(* A member of a suggestion, as printed. *)
let s r k = Json.str k (Suggestion.to_json r)

let mk ?(platform = []) ?target ?(kind = "build") ?revdep ?(status = "failed")
    ?(category = "build") ?reason ?(experimental = false) ?(url = "")
    ?(messages = []) variant =
  {
    variant;
    url;
    label = "";
    status;
    reason;
    category;
    target;
    kind;
    revdep;
    platform;
    experimental;
    in_summary = false;
    messages;
  }

(** A synthetic CI job, as parse_commit_page makes it. *)
let job ?(category = "build") ?(reason = "The compilation of x failed")
    ?(experimental = false) variant =
  let parts = String.split_on_char ',' variant in
  let i = if List.hd parts = "day10" then 4 else 2 in
  let platform = Util.take i parts and rest = Util.drop i parts in
  let kind =
    match rest with
    | _ :: "revdeps" :: _ -> "revdeps"
    | _ :: k :: _ -> k
    | _ -> "build"
  in
  let status = if category = "passed" then "passed" else "failed" in
  {
    (mk ~platform ~target:(List.hd rest) ~kind ~status ~category ~experimental
       ~url:(Net.base ^ String.make 40 '0' ^ "/variant/" ^ variant)
       variant)
    with
    revdep = (if kind = "revdeps" then List.nth_opt rest 2 else None);
    reason = Some (if status = "passed" then "" else reason);
  }

let aborted j =
  { j with status = "aborted"; category = "aborted"; reason = None }

let data jobs =
  {
    pr = None;
    jobs;
    unfinished = 0;
    unanalysed = 0;
    health_stale = None;
    llm_unanswered = None;
    sha = None;
  }

let dummy_src () = Sources.create "/nonexistent"

(* The hooks a test replaces, restored afterwards. *)
let with_hooks f =
  let saved =
    ( !Context.fetch_hook,
      !Context.log_text_hook,
      !Context.parse_commit_page_hook,
      !Context.gh_api_hook,
      !Context.find_baseline_commit_hook,
      !Context.opam_file_hook,
      !Context.versions_hook,
      !Context.mcp_hook,
      !Context.pr_opam_changes_hook )
  in
  Fun.protect f ~finally:(fun () ->
      let a, b, c, d, e, g, h, i, j = saved in
      Context.fetch_hook := a;
      Context.log_text_hook := b;
      Context.parse_commit_page_hook := c;
      Context.gh_api_hook := d;
      Context.find_baseline_commit_hook := e;
      Context.opam_file_hook := g;
      Context.versions_hook := h;
      Context.mcp_hook := i;
      Context.pr_opam_changes_hook := j)

(* What [f] prints on stderr. *)
let capture_stderr f =
  let tmp = Filename.temp_file "stderr" "" in
  flush stderr;
  let saved = Unix.dup Unix.stderr in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  let r =
    Fun.protect f ~finally:(fun () ->
        flush stderr;
        Unix.dup2 saved Unix.stderr;
        Unix.close saved)
  in
  let out = In_channel.with_open_bin tmp In_channel.input_all in
  Sys.remove tmp;
  (r, out)

(* ------------------------------------------------------------------------ *)
(* LintTest: the messages of a failed lint job: findings of this PR, or a CI error. *)

let lint_job messages =
  mk ~kind:"lint" ~url:"https://x/(lint)" ~messages "(lint)"

let lint_rules messages =
  Context.lint_rules ~cache_dir:"/nonexistent" (data [ lint_job messages ])

let test_warning_is_a_finding () =
  (* opam-ci-check fails the lint job on a warning alone (#30007). *)
  let r =
    one
      (lint_rules
         [
           "Warning in ocaml-r.0.7.0: Dubious use of 'dune subst'. 'dune \
            subst' should always only be called with {dev}";
         ])
  in
  check_str "rule" "lint" (s r "rule");
  check_str "failing" "ocaml-r.0.7.0" (s r "failing");
  check_str "where_kind" "pr" (s r "where_kind");
  check_in "Dubious use" (s r "error");
  check_in "fails on warnings too" (s r "note")

let test_field_already_missing_on_master () =
  (* The PR deprecates an old version that never had a license (#30832, bjack): said so,
     the change still suggested (#31205's author added it). *)
  with_hooks @@ fun () ->
  (Context.opam_file_hook :=
     fun ?ref:_ _ _ version ->
       match version with
       | "1.0" -> Some "opam-version: \"2.0\"\nsynopsis: \"Old\"\n"
       | "2.0" -> Some "opam-version: \"2.0\"\nlicense: \"MIT\"\n"
       | _ -> None);
  (Context.versions_hook := fun _ _ -> Some [ "1.0"; "2.0" ]);
  let d =
    data
      [
        lint_job
          [
            "Warning in foo.1.0: Opam lint warning 68: Missing field 'license'";
            "Warning in foo.3.0: Opam lint warning 68: Missing field 'license'";
          ];
      ]
  in
  match Context.lint_rules ~src:(dummy_src ()) ~cache_dir:"/nonexistent" d with
  | [ old; new_ ] ->
      check_str "old new" {|license: "MIT"|} (s old "new");
      check_str "new new" {|license: "MIT"|} (s new_ "new");
      check_str "old note"
        "Taken from foo.2.0 in opam-repository. Already missing on master: \
         this PR, which changes an existing version, did not remove it."
        (s old "note");
      check_str "new note" "Taken from foo.2.0 in opam-repository."
        (s new_ "note")
  | l -> Alcotest.failf "expected two results, got %d" (List.length l)

let test_ci_error () =
  let r =
    one
      (lint_rules
         [
           {|Command "git" "-C" "/tmp/git-checkout5e08f64" "reset" "--hard" "-q" "7386318" exited with status 128|};
         ])
  in
  check_str "rule" "lint-ci-error" (s r "rule");
  check_str "where_kind" "rerun" (s r "where_kind");
  check_in "git could not check out" (s r "note")

let test_failed_analysis () =
  (* The analysis job failed: no build job ran (#31172). Its reason is shown, the fix is in the PR,
     unless the CI cancelled it. *)
  let d ?(messages = []) reason =
    data
      [
        mk ~kind:"analysis" ?reason ~category:"analysis"
          ~url:"https://x/(analysis)" ~messages "(analysis)";
      ]
  in
  let r =
    one
      (Context.analysis_rules
         (d (Some "Failed to parse packages/x/x.1/opam: unexpected token"))
         "/nonexistent")
  in
  check_str "rule" "analysis" (s r "rule");
  check_str "where_kind" "pr" (s r "where_kind");
  check_in "unexpected token" (s r "error");
  check_in "no build job ran" (s r "note");
  let r = one (Context.analysis_rules (d (Some "Cancelled")) "/nonexistent") in
  check_str "where_kind" "rerun" (s r "where_kind");
  let r =
    one
      (Context.analysis_rules
         (d ~messages:[ "Error: x.1 changes files outside packages/" ] None)
         "/nonexistent")
  in
  check_in "outside packages/" (s r "error")

(* PageTest.test_lint_warning_title *)
let test_lint_warning_title () =
  let d =
    data
      [
        mk ~kind:"lint" ~url:"u"
          ~messages:
            [
              "Warning in x.1: Unnecessary field 'version'";
              "Error in x.1: Missing field 'license'";
            ]
          "";
      ]
  in
  let rs = Context.lint_rules d in
  Alcotest.(check (list string))
    "titles"
    [ "opam lint warning"; "opam lint error" ]
    (List.map Titles.title_of rs)

(* EscapingTest.test_checksum_command *)
let test_checksum_command () =
  let good = "https://github.com/o/r/releases/download/v1.0/r-1.0.tbz" in
  Alcotest.(check (option string))
    "good"
    (Some (Printf.sprintf "curl -sL '%s' | sha256sum" good))
    (Context.checksum_command (Some good));
  List.iter
    (fun bad ->
      Alcotest.(check (option string))
        (Option.value ~default:"None" bad)
        None
        (Context.checksum_command bad))
    [
      None;
      Some "<archive URL>";
      Some "http://e.x/a.tgz";
      Some "https://e.x/a.tgz; rm -rf ~";
      Some "https://e.x/$(id).tgz";
      Some "https://e.x/a'b.tgz";
      Some "https://e.x/`id`";
      Some "https://e.x/a b";
      Some "https://e.x/a|b";
      Some "https://e.x/a&b";
      Some "https://-e.x/a";
      Some "file:///etc/passwd";
    ]

(* ------------------------------------------------------------------------ *)
(* RulesTest: rules on synthetic job lists (no fixture). *)

let test_rerun_experimental () =
  (* An entry listing only experimental jobs to rerun says so (it does not hold the PR back). *)
  let exp =
    job ~category:"cancelled" ~reason:"Cancelled" ~experimental:true
      "macos-homebrew,4.14,x.1"
  in
  let r = one (Context.rerun_rules (data [ exp ]) []) in
  check_str "where_kind" "rerun" (s r "where_kind");
  check_bool "experimental" true (r.Suggestion.experimental = Some true);
  let r =
    one
      (Context.rerun_rules
         (data
            [
              exp;
              job ~category:"network" ~reason:"curl failed" "compilers,4.14,x.1";
            ])
         [])
  in
  check_bool "not experimental" false (r.Suggestion.experimental = Some true)

let test_jobs_not_run () =
  (* Jobs CI never ran to the end (aborted, or never started while CI counts
     as finished) are jobs to rerun. (The verdicts of render_page on them are
     tested with the page.) *)
  let ab = aborted (job "compilers,5.3,foo.1") in
  let r =
    one
      (Context.rerun_rules
         (data [ ab; job ~reason:"x" "compilers,5.3,foo.1,revdeps,X.1" ])
         [])
  in
  check_str "where_kind" "rerun" (s r "where_kind");
  check_str "error" "1 aborted job" (s r "error");
  Alcotest.(check (list string))
    "jobs" [ "compilers,5.3,foo.1" ]
    (Suggestion.list r.Suggestion.jobs);
  check_bool "experimental" false (r.Suggestion.experimental = Some true);
  (* Aborted on riscv64 or s390x, where CI aborts jobs routinely: listed, but does not hold the PR back. *)
  List.iter
    (fun v ->
      let r = one (Context.rerun_rules (data [ aborted (job v) ]) []) in
      check_bool v true (r.Suggestion.experimental = Some true);
      check_in "routinely" (s r "note"))
    [
      "extras,riscv64-ocaml-4.14,foo.1";
      "day10,day10-riscv64-ocaml-5.5,foo.1,revdeps,X.1";
      "extras,s390x-ocaml-5.4,foo.1";
    ];
  (* Not a package whose name mentions riscv64. *)
  let r =
    one
      (Context.rerun_rules
         (data [ aborted (job "compilers,5.3,riscv64-tools.1") ])
         [])
  in
  check_bool "riscv64-tools" false (r.Suggestion.experimental = Some true);
  (* An aborted analysis: lint never starts; both to rerun. *)
  let an status =
    mk ~kind:"analysis" ~status
      ~category:(if status = "failed" then "analysis" else status)
      "(analysis)"
  in
  let lint =
    mk ~kind:"lint" ~status:"not-started" ~category:"not-started" "(lint)"
  in
  let r = one (Context.rerun_rules (data [ an "aborted"; lint ]) []) in
  check_str "error" "1 aborted job, 1 job not run" (s r "error");
  Alcotest.(check (list string))
    "jobs" [ "(analysis)"; "(lint)" ]
    (List.sort compare (Suggestion.list r.Suggestion.jobs));
  (* A failed analysis: the analysis rule explains it, lint included. *)
  Alcotest.(check int)
    "failed analysis" 0
    (List.length (Context.rerun_rules (data [ an "failed"; lint ]) []));
  (* Still running: a job waiting for another one is not finished. *)
  let running =
    { (job "compilers,5.3,foo.1") with status = "active"; category = "active" }
  in
  Alcotest.(check int)
    "running" 0
    (List.length (Context.rerun_rules (data [ an "passed"; lint; running ]) []));
  (* Aborted experimental jobs alone do not hold the PR back. *)
  let r =
    one (Context.rerun_rules (data [ { ab with experimental = true } ]) [])
  in
  check_bool "experimental aborted" true (r.Suggestion.experimental = Some true)

let test_eval_filter () =
  let f t env = Context.eval_filter (Some t) env in
  let linux = Context.platform_vars (job "compilers,5.4,x.1") in
  let cygwin = Context.platform_vars (job "day10,cygwin,x86_64,5.4,x.1") in
  check_str "os" "linux" (List.assoc "os" linux);
  check_str "arch" "x86_64" (List.assoc "arch" linux);
  check_str "os-distribution" "cygwin" (List.assoc "os-distribution" cygwin);
  let old = {|os = "linux" & (arch = "arm64" | arch = "x86_64")|} in
  let ob = Alcotest.(option bool) in
  Alcotest.check ob "linux" (Some true) (f old linux);
  Alcotest.check ob "cygwin" (Some false) (f old cygwin);
  Alcotest.check ob "empty" (Some true) (f "" cygwin);
  Alcotest.check ob "not cygwin" (Some false)
    (f {|!(os-distribution = "cygwin")|} cygwin);
  Alcotest.check ob "opam-version" None (f {|opam-version >= "2.1"|} linux)
  (* not a platform variable *);
  Alcotest.check ob "conj" (Some false)
    (f {|opam-version >= "2.1" & os != "win32"|} cygwin)

let test_relaxed_available () =
  with_hooks @@ fun () ->
  let jobs =
    [
      job "day10,cygwin,x86_64,5.4,x.1";
      job "day10,cygwin,x86_64,5.4,y.1";
      job "day10,macos-homebrew,arm64,5.4,x.1";
      job ~category:"passed" "compilers,5.4,x.1";
    ]
  in
  let rules old new_ =
    (Context.pr_opam_changes_hook := fun _ _ _ -> [ ("x.1", (old, new_)) ]);
    Context.relaxed_available_rules (data jobs) "s" (Some "b") "d"
      [ "x.1"; "y.1" ]
  in
  let r = one (rules {|os = "linux"|} "") in
  check_str "confidence" "high" (s r "confidence");
  (* Only x.1's jobs: y.1 failing on cygwin is not caused by x.1's `available`. *)
  Alcotest.(check (list string))
    "jobs"
    [ "day10,cygwin,x86_64,5.4,x.1"; "day10,macos-homebrew,arm64,5.4,x.1" ]
    (Suggestion.list r.Suggestion.jobs);
  (* Only the platforms the old filter excluded and the new one allows. *)
  let r = List.hd (rules {|os != "macos" & os != "win32"|} {|os != "win32"|}) in
  Alcotest.(check (list string))
    "jobs"
    [ "day10,macos-homebrew,arm64,5.4,x.1" ]
    (Suggestion.list r.Suggestion.jobs);
  Alcotest.(check int)
    "opam-version" 0
    (List.length
       (rules {|os != "win32"|} {|os != "win32" & opam-version >= "2.1"|}));
  Alcotest.(check int)
    "arch" 0
    (List.length
       (rules {|arch != "arm32"|} {|arch != "arm32" & arch != "x86_32"|}))

let test_preexisting_same_failure () =
  with_hooks @@ fun () ->
  let build v n =
    Printf.sprintf {|The compilation of %s failed at "dune build -p x -j %d"|} v
      n
  in
  let new_ =
    [
      job ~reason:(build "x.2" 32) "compilers,5.4,x.2";
      job ~reason:"Error: Unbound module Foo" "compilers,5.3,x.2";
      job ~category:"dependency" ~reason:"y.1 failed to build"
        "compilers,5.2,x.2";
      job ~category:"dependency" ~reason:"z.1 failed to build"
        "compilers,5.1,x.2";
      job ~reason:(build "r.1" 8) "compilers,5.4,x.2,revdeps,r.1";
    ]
  in
  let old =
    [
      job ~reason:(build "x.1" 8) "compilers,5.4,x.1";
      job ~reason:"Error: Unbound module Bar"
        "compilers,5.3,x.1" (* another error *);
      job ~category:"dependency" ~reason:"y.1 failed to build"
        "compilers,5.2,x.1";
      job ~reason:"z.1 failed to build"
        "compilers,5.1,x.1" (* another category *);
      job ~reason:(build "r.1" 8) "compilers,5.4,x.1,revdeps,r.1";
    ]
  in
  (Context.find_baseline_commit_hook :=
     fun _ _ ->
       Some
         (Dict.of_list
            [
              ("prev", `String "x.1");
              ("pr", `Int 7);
              ("sha", `String (String.make 40 'b'));
            ]));
  (Context.fetch_hook := fun _ _ -> "");
  (Context.parse_commit_page_hook := fun _ -> data old);
  let notes = Context.new_notes () in
  let pre = Context.preexisting_failures ~notes (data new_) "s" None "d" in
  let key v = failure_key (List.find (fun j -> j.variant = v) new_) in
  let expected =
    List.sort compare
      [
        key "compilers,5.4,x.2";
        key "compilers,5.2,x.2";
        key "compilers,5.4,x.2,revdeps,r.1";
      ]
  in
  check_bool "keys" true (List.sort compare (List.map fst pre) = expected);
  check_in "x.1 failed the same job in the CI of #7"
    (List.assoc (key "compilers,5.2,x.2") pre);
  (* The release's own build failing at the same step may fail with another error. *)
  check_bool "unverified" true (notes.unverified = [ key "compilers,5.4,x.2" ])

let test_old_health_check_run_ignored () =
  (* An opam-health-check run older than a week describes an older master:
     neither "pre-existing" nor "Broken by this PR" from it. *)
  with_hooks @@ fun () ->
  let saved = !Net.offline_ref in
  Fun.protect ~finally:(fun () -> Net.offline_ref := saved) @@ fun () ->
  Net.offline_ref := false;
  let today = Option.get (Date.days_of_date (Date.today ())) in
  let jobs =
    [
      job "compilers,4.14,x.2,revdeps,r.1"; job "compilers,4.14,x.2,revdeps,g.1";
    ]
  in
  (Context.find_baseline_commit_hook := fun _ _ -> None);
  (Context.versions_hook := fun _ _ -> Some []);
  List.iter
    (fun (days, used) ->
      let date = Date.date_of_days (today - days) in
      (Context.mcp_hook :=
         fun ?max_age:_ _ _ args ->
           let package = Json.to_str (List.assoc "package" args) in
           let status = if package = "r" then "bad" else "good" in
           `Assoc
             [
               ("run", `Assoc [ ("date", `String date) ]);
               ( "results",
                 `List
                   [
                     `Assoc
                       [
                         ("package", `String package);
                         ("version", `String "1");
                         ( "builds",
                           `List
                             [
                               `Assoc
                                 [
                                   ("compiler", `String "4.14");
                                   ("status", `String status);
                                 ];
                             ] );
                       ];
                   ] );
             ]);
      let notes = Context.new_notes () in
      let pre, err =
        capture_stderr (fun () ->
            Context.preexisting_failures ~src:(dummy_src ()) ~notes (data jobs)
              "s" None "d")
      in
      check_bool (Printf.sprintf "pre %d" days) used (pre <> []);
      check_bool
        (Printf.sprintf "new %d" days)
        used
        (Hashtbl.length notes.new_ > 0);
      check_bool err (not used) (Util.contains err "not used"))
    [ (2, true); (30, false) ];
  check_bool "undated" false (Context.health_run_stale None);
  check_bool "old" true
    (Context.health_run_stale ~today:"2026-11-20" (Some "2026-11-01"));
  check_bool "recent" false
    (Context.health_run_stale ~today:"2026-11-20" (Some "2026-11-14"))

(* ------------------------------------------------------------------------ *)
(* BoundedOutTest: a PR package that installs nowhere because of an upper
   bound the PR adds is explained as intended, not as a problem to fix. *)

let patch =
  {|@@ -10,7 +10,7 @@ depends: [
-  "ocaml" {>= "4.02.0"}
+  "ocaml" {>= "4.02.0" & < "4.06"}
   "dune"
-  "cmdliner" {with-test}
+  "cmdliner" {with-test & < "2.0.0"}
+  "fmt" {>= "0.9"}
|}

let log =
  {|kinetic-client.0.0.9 is not installed. Install it? [Y/n] y
  * Missing dependency:
    - kinetic-client < 0.0.11 -> ocaml < 4.06 -> ocaml-base-compiler < 4.11.0 | ocaml-system (<= 3.07+1 | = 3.07+2 | >= 3.08.0) | ocaml-variants < 4.05.1~
    no matching version
|}

let compare_answer p : Json.t =
  `Assoc
    [
      ( "files",
        `List
          [
            `Assoc
              [
                ( "filename",
                  `String "packages/kinetic-client/kinetic-client.0.0.9/opam" );
                ("status", `String "modified");
                ("patch", `String p);
              ];
          ] );
    ]

let added p =
  with_hooks @@ fun () ->
  (Context.gh_api_hook := fun _ _ -> Some (compare_answer p));
  List.sort compare
    (List.map fst
       (Option.value ~default:[]
          (List.assoc_opt "kinetic-client.0.0.9"
             (Context.pr_added_bounds (Some "base") "sha" "/nonexistent"))))

let test_added_bounds () =
  Alcotest.(check (list string)) "bounds" [ "cmdliner"; "ocaml" ] (added patch)
(* not fmt: no upper bound *)

let test_only_tightened_depends () =
  (* Only a `depends` line whose upper bound is added or lowered: not a changed lower bound, a
     loosened bound, nor `conflicts` or `depopts`. *)
  Alcotest.(check (list string))
    "fmt" [ "fmt" ]
    (added
       {|@@ -10,7 +10,7 @@ depends: [
-  "ocaml" {>= "4.02.0" & < "5.0"}
+  "ocaml" {>= "4.08.0" & < "5.0"}
-  "lwt" {< "5.0"}
+  "lwt" {< "6.0"}
-  "fmt" {< "0.10"}
+  "fmt" {< "0.9.1"}
 ]
 conflicts: [
-  "base-domains"
+  "base-domains" {< "0.2"}
 ]
 depopts: [
+  "zarith" {< "1.0"}
 ]
|});
  (* The field comes from the hunk header when the hunk starts inside it. *)
  Alcotest.(check (list string))
    "conflicts" []
    (added
       {|@@ -20,3 +20,3 @@ conflicts: [
-  "base-domains"
+  "base-domains" {< "0.2"}
|})

let ocaml_versions =
  Some [ ("ocaml", [ "4.02.3"; "4.05.0"; "4.14.2"; "5.4.0" ]) ]

let run_rule ?(versions = ocaml_versions) bounds =
  with_hooks @@ fun () ->
  let jobs =
    List.map
      (fun v ->
        mk ~target:"kinetic-client.0.0.9" ~category:"not-available"
          ~url:("https://x/" ^ v)
          (Printf.sprintf "compilers,%s,kinetic-client.0.0.9" v))
      [ "4.14"; "5.4"; "5.5" ]
  in
  (Context.fetch_hook := fun _ _ -> "");
  (Context.log_text_hook := fun _ -> log);
  (Context.versions_hook :=
     fun _ name ->
       match versions with Some v -> List.assoc_opt name v | None -> None);
  Context.skipped_rules ~src:(dummy_src ()) ~bounds (data jobs) "/nonexistent"
    [ "kinetic-client.0.0.9" ]

let bound line = [ ("kinetic-client.0.0.9", [ ("ocaml", line) ]) ]

let test_unsatisfiable_bound () =
  (* A typo such as `{>= "3.0" & < "2.0"}` excludes every version: a mistake in the PR, not intended. *)
  let r = one (run_rule (bound {|"ocaml" {>= "4.07" & < "4.06"}|})) in
  check_str "rule" "uninstallable" (s r "rule");
  check_str "where_kind" "pr" (s r "where_kind");
  check_in "no version of ocaml satisfies it" (s r "note")

let test_no_published_version () =
  (* A typo such as `{< "0.3"}` that no published version satisfies: a mistake, not intended. *)
  let r = one (run_rule (bound {|"ocaml" {< "4.0"}|})) in
  check_str "rule" "uninstallable" (s r "rule");
  check_str "where_kind" "pr" (s r "where_kind");
  check_in "no published version of ocaml satisfies it" (s r "note");
  (* The versions are unknown (ocaml.org did not answer): not assumed intended either. *)
  let r =
    one (run_rule ~versions:None (bound {|"ocaml" {>= "4.02.0" & < "4.06"}|}))
  in
  check_str "rule" "uninstallable" (s r "rule");
  check_str "where_kind" "pr" (s r "where_kind");
  check_in "could not be read" (s r "note")

let test_intended () =
  let r = one (run_rule (bound {|"ocaml" {>= "4.02.0" & < "4.06"}|})) in
  check_str "rule" "bounded-out" (s r "rule");
  check_bool "fix_in" true (s r "fix_in" <> "this PR");
  check_in {|< "4.06"|} (s r "note")

let test_bound_with_alternatives_or_variables () =
  (* A bound with alternatives is not checked: not counted as intended. A comparison of a
     variable (`os = "linux"`) is not a version bound: no "lower bound above an upper bound". *)
  let r = one (run_rule (bound {|"ocaml" {< "4.06" | = "4.14.2"}|})) in
  check_str "rule" "uninstallable" (s r "rule");
  check_str "where_kind" "pr" (s r "where_kind");
  check_str "confidence" "medium" (s r "confidence");
  check_in "has alternatives" (s r "note");
  let r = one (run_rule (bound {|"ocaml" {os = "linux" & < "4.06"}|})) in
  check_str "rule" "bounded-out" (s r "rule");
  let terms = Alcotest.(list (pair string string)) in
  Alcotest.check terms "variables"
    [ (">=", "1.0") ]
    (Constraints.version_terms
       {|"x" {os = "linux" & >= "1.0" & ocaml:version < "5.0" & os-family != "bsd"}|});
  Alcotest.check terms "conj"
    [ (">=", "1.0"); ("<", "2.0") ]
    (Constraints.version_terms {|>= "1.0" & < "2.0"|})

let test_not_intended () =
  (* Without a bound added by the PR, it is a problem, and the alternatives
     of a disjunction are not read as one package's constraint. *)
  let r = one (run_rule []) in
  check_str "rule" "uninstallable" (s r "rule");
  check_not_in "No version of `ocaml-base-compiler`" (s r "note");
  check_in "`ocaml < 4.06`" (s r "note")

let () =
  Alcotest.run "context"
    [
      ( "lint",
        [
          Alcotest.test_case "warning_is_a_finding" `Quick
            test_warning_is_a_finding;
          Alcotest.test_case "field_already_missing_on_master" `Quick
            test_field_already_missing_on_master;
          Alcotest.test_case "ci_error" `Quick test_ci_error;
          Alcotest.test_case "failed_analysis" `Quick test_failed_analysis;
          Alcotest.test_case "lint_warning_title" `Quick test_lint_warning_title;
          Alcotest.test_case "checksum_command" `Quick test_checksum_command;
        ] );
      ( "rules",
        [
          Alcotest.test_case "rerun_experimental" `Quick test_rerun_experimental;
          Alcotest.test_case "jobs_not_run" `Quick test_jobs_not_run;
          Alcotest.test_case "eval_filter" `Quick test_eval_filter;
          Alcotest.test_case "relaxed_available" `Quick test_relaxed_available;
          Alcotest.test_case "preexisting_same_failure" `Quick
            test_preexisting_same_failure;
          Alcotest.test_case "old_health_check_run_ignored" `Quick
            test_old_health_check_run_ignored;
        ] );
      ( "bounded_out",
        [
          Alcotest.test_case "added_bounds" `Quick test_added_bounds;
          Alcotest.test_case "only_tightened_depends" `Quick
            test_only_tightened_depends;
          Alcotest.test_case "unsatisfiable_bound" `Quick
            test_unsatisfiable_bound;
          Alcotest.test_case "no_published_version" `Quick
            test_no_published_version;
          Alcotest.test_case "intended" `Quick test_intended;
          Alcotest.test_case "bound_with_alternatives_or_variables" `Quick
            test_bound_with_alternatives_or_variables;
          Alcotest.test_case "not_intended" `Quick test_not_intended;
        ] );
    ]
