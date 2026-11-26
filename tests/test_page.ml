(* The triage page (scripts/render_page) and the Markdown the tool posts on
   GitHub (lib/render): which PRs seem ready, labels, verdicts, headlines,
   the Markdown comment, and the escaping of untrusted text.
   Ported from tests/test_triage.py, classes PageTest and EscapingTest. *)

open Triage
open Suggestion.T

(* A suggestion from its JSON members. *)
let sugg kv = Suggestion.of_json (`Assoc kv)

(* The page's functions on PRs and page data written as JSON: the tests'
   literals go through Page_data's readers. *)
module R = struct
  include Scripts.Render_page
  module P = Scripts.Page_data

  let entry = P.Entry.of_json
  let render j = render (P.of_json j)
  let render_feed j = render_feed (P.of_json j)
  let verdict j = verdict (entry j)
  let seems_ready j = seems_ready (entry j)
  let headline ?v ?as_html j = headline ?v ?as_html (entry j)
  let render_pr ?year ?max_failures j = render_pr ?year ?max_failures (entry j)
  let ready_section ?shown j = ready_section ?shown (P.of_json j)
  let chronic ?min_prs l = chronic ?min_prs (List.map entry l)
  let help_section j = help_section (P.of_json j)
  let comment_markdown j = comment_markdown (entry j)
end

let base_url = "https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/"
let contains s sub = Util.contains s sub

let check_in sub s =
  if not (contains s sub) then Alcotest.failf "%S not in:\n%s" sub s

let check_not_in sub s =
  if contains s sub then Alcotest.failf "%S unexpectedly in:\n%s" sub s

let check_true msg b = Alcotest.(check bool) msg true b
let check_false msg b = Alcotest.(check bool) msg false b
let check_str msg a b = Alcotest.(check string) msg a b

(* Python's dict(base, **kw): the keys of [kw] replace those of [base] in place, others are added. *)
let upd base kw =
  List.map
    (fun (k, v) ->
      match List.assoc_opt k kw with Some v' -> (k, v') | None -> (k, v))
    base
  @ List.filter (fun (k, _) -> not (List.mem_assoc k base)) kw

let s x : Json.t = `String x
let ints l : Json.t = `List (List.map (fun n -> `Int n) l)
let strs l : Json.t = Json.strings l

let pr ?(kw = []) suggestions : Json.t =
  `Assoc
    (upd
       [
         ("number", `Int 1);
         ("title", s "t");
         ("sha", s (String.make 40 'a'));
         ("ci_state", s "FAILURE");
         ("labels", `List []);
         ( "jobs",
           `Assoc
             [ ("total", `Int 10); ("failed", `Assoc [ ("build", `Int 1) ]) ] );
         ("ci_unfinished", `Int 0);
         ( "result",
           `Assoc
             [
               ( "suggestions",
                 `List (List.map (fun kv -> `Assoc kv) suggestions) );
               ("ci_unfinished", `Int 0);
             ] );
       ]
       kw)

(* pr with result.unanalysed set *)
let with_unanalysed n (p : Json.t) : Json.t =
  match p with
  | `Assoc kv ->
      `Assoc
        (List.map
           (fun (k, v) ->
             if k = "result" then
               (k, `Assoc (Json.to_assoc v @ [ ("unanalysed", `Int n) ]))
             else (k, v))
           kv)
  | v -> v

let day10 =
  [
    ("class", s "elsewhere");
    ("rule", s "builder-specific");
    ("fix_in", s "the CI environment (day10 builder)");
    ("failing", s "x.1");
    ("variant", s "v");
    ("title", s "Fails only on the day10 builder");
  ]

let revdep =
  [
    ("class", s "elsewhere");
    ("rule", s "library-api");
    ("fix_in", s "y.1 (other package)");
    ("failing", s "y.1");
    ("variant", s "v");
    ("title", s "API change");
  ]

let lint =
  [
    ("class", s "fix");
    ("rule", s "lint");
    ("fix_in", s "this PR");
    ("failing", s "x.1");
    ("variant", s "v");
    ("title", s "Lint");
  ]

let d kv = sugg kv

(* ------------------------------------------------------------------------ *)
(* PageTest *)

let test_help_texts () =
  (* Jobs aborted on riscv64 or s390x do not hold a PR back: not "every job ran". *)
  check_not_in "every job ran" (R.verdict_help "ready");
  check_in "riscv64" (R.verdict_help "ready");
  let page = R.render_pr (pr ~kw:[ ("git_unreachable", `Int 1) ] [ day10 ]) in
  check_in "a git host that did not answer is asked again a day later" page;
  check_not_in "a few times a day" page

let test_seems_ready () =
  let sr = R.seems_ready in
  check_true "day10" (sr (pr [ day10 ]));
  (* A revdep failure is the PR's, unless it happens without the PR. *)
  check_false "revdep" (sr (pr [ day10; revdep ]));
  check_true "pre"
    (sr (pr [ day10; upd revdep [ ("preexisting", s "on master") ] ]));
  check_true "shared"
    (sr
       (pr
          [
            upd revdep
              [
                ("rule", s "missing-library"); ("also_failing_in", ints [ 2; 3 ]);
              ];
          ]));
  (* ... but not when a rule ties it to the PR's package (its API changed), nor for jobs to rerun. *)
  check_false "api"
    (sr (pr [ upd revdep [ ("also_failing_in", ints [ 2; 3 ]) ] ]));
  let rerun =
    [
      ("class", s "elsewhere");
      ("rule", s "rerun");
      ("fix_in", s "CI (rerun)");
      ("where_kind", s "rerun");
      ("variant", s "1 job");
    ]
  in
  check_false "rerun"
    (sr (pr [ upd rerun [ ("also_failing_in", ints [ 2; 3; 4 ]) ] ]));
  (* ... but not against evidence that it is this PR's (stored before mark_shared_failures skipped them). *)
  let nw =
    [
      ("class", s "fix");
      ("rule", s "health-check-new");
      ("fix_in", s "this PR");
      ("failing", s "y.1");
      ("variant", s "v");
    ]
  in
  check_false "new"
    (sr (pr [ upd nw [ ("also_failing_in", ints [ 2; 3; 4 ]) ] ]));
  check_false "lint shared"
    (sr (pr [ upd lint [ ("also_failing_in", ints [ 2; 3; 4 ]) ] ]));
  check_true "lint exp" (sr (pr [ upd lint [ ("experimental", `Bool true) ] ]));
  (* A lint job that failed before linting did not check anything: not ready, to rerun. *)
  check_false "lint-ci-error"
    (sr
       (pr
          [
            [
              ("class", s "elsewhere");
              ("rule", s "lint-ci-error");
              ("fix_in", s "CI (rerun)");
              ("variant", s "(lint)");
            ];
          ]));
  check_false "lint" (sr (pr [ lint ]));
  (* Unexplained failures (only skipped jobs), CI not finished, drafts, labels. *)
  check_false "empty" (sr (pr []));
  check_false "unfinished" (sr (pr ~kw:[ ("ci_unfinished", `Int 2) ] [ day10 ]));
  (* Distinct failures beyond --max were not analysed: nothing says they are not the PR's. *)
  let unanalysed = with_unanalysed 3 (pr [ day10 ]) in
  check_false "unanalysed" (sr unanalysed);
  check_in "3 more failures were not analysed" (R.render_pr unanalysed);
  check_in "3 more failures were not analysed." (R.comment_markdown unanalysed);
  check_false "draft" (sr (pr ~kw:[ ("draft", `Bool true) ] [ day10 ]));
  check_false "label"
    (sr (pr ~kw:[ ("labels", strs [ "needs reporter action" ]) ] [ day10 ]));
  check_true "merge label"
    (sr (pr ~kw:[ ("labels", strs [ "consider for merge" ]) ] [ day10 ]));
  (* A "needs reporter action" label the author acted on since may be stale. *)
  check_true "stale"
    (sr
       (pr
          ~kw:
            [
              ("labels", strs [ "needs reporter action" ]);
              ("stale_labels", strs [ "needs reporter action" ]);
            ]
          [ day10 ]));
  check_in "(author replied since)"
    (R.label_badges [ "needs reporter action" ] [ "needs reporter action" ])

let test_pending_status_with_finished_page () =
  (* opam-ci's status stays pending when a job never starts: the CI page has
     finished (no unfinished job), so the PR is not "CI running". *)
  let done_ =
    pr ~kw:[ ("ci_state", s "PENDING"); ("ci_unfinished", `Int 0) ] [ day10 ]
  in
  check_true "ready" (R.seems_ready done_);
  check_not_in "CI running" (R.render_pr done_);
  let running =
    pr ~kw:[ ("ci_state", s "PENDING"); ("ci_unfinished", `Int 3) ] [ day10 ]
  in
  check_false "running" (R.seems_ready running);
  check_in "CI running, 3 jobs left" (R.render_pr running)

let test_kept_result_after_a_failed_analysis () =
  let p =
    pr
      ~kw:
        [
          ("last_error", s "timeout after 600s <b>");
          ("last_error_at", s "2026-11-20T10:00:00+00:00");
        ]
      [ day10 ]
  in
  let html = R.render_pr p in
  check_in
    "a newer analysis failed 2026-11-20 10:00 UTC; this result may be out of \
     date"
    html;
  check_in "title=\"timeout after 600s &lt;b&gt;\"" html;
  check_not_in "newer analysis failed" (R.render_pr (pr [ day10 ]))

let test_incomplete_analysis_shown () =
  (* Failed lookups (ocaml.org, HTTP, git, a git host down earlier today) are shown. *)
  let html =
    R.render_pr
      (pr
         ~kw:
           [
             ("mcp_errors", `Int 2);
             ("net_errors", `Int 1);
             ("git_unreachable", `Int 1);
           ]
         [ day10 ])
  in
  check_in "analysis incomplete (4 failed lookups)" html;
  check_in "analysis incomplete (1 failed lookup)"
    (R.render_pr (pr ~kw:[ ("net_errors", `Int 1) ] [ day10 ]));
  check_not_in "analysis incomplete"
    (R.render_pr (pr ~kw:[ ("mcp_errors", `Int 0) ] [ day10 ]));
  check_not_in "analysis incomplete"
    (R.render_pr
       (pr ~kw:[ ("net_errors", `Int 1); ("pending", `Bool true) ] [ day10 ]))

let index_of s sub = Util.find s sub

let test_ready_section_and_labels () =
  let data : Json.t =
    `Assoc
      [
        ( "prs",
          `List
            [
              pr
                ~kw:[ ("number", `Int 5); ("labels", strs [ "question" ]) ]
                [ day10 ];
              pr ~kw:[ ("number", `Int 6) ] [ lint ];
            ] );
        ( "passing",
          `List
            [
              `Assoc
                [
                  ("number", `Int 7);
                  ("title", s "p");
                  ("labels", strs [ "consider for merge" ]);
                  ("updated", s "2026-01-01");
                ];
              `Assoc
                [
                  ("number", `Int 8);
                  ("title", s "d");
                  ("labels", `List []);
                  ("draft", `Bool true);
                ];
              `Assoc
                [
                  ("number", `Int 9);
                  ("title", s "n");
                  ("labels", strs [ "do not merge" ]);
                ];
            ] );
      ]
  in
  let html = R.ready_section data in
  check_in "(2)" html;
  check_true "labelled first" (index_of html "/pull/7" < index_of html "/pull/5");
  List.iter
    (fun n -> check_not_in (Printf.sprintf "/pull/%d" n) html)
    [ 6; 8; 9 ];
  check_not_in "<table" html (* a list, which wraps on a phone *);
  check_in "href=\"#pr-5\"" html;
  (* More than [shown]: the rest is folded. *)
  let many : Json.t =
    `Assoc
      [
        ( "passing",
          `List
            (List.map
               (fun n ->
                 `Assoc
                   [
                     ("number", `Int n); ("title", s "p"); ("labels", `List []);
                   ])
               (List.init 7 (fun i -> 10 + i))) );
      ]
  in
  check_in "2 more</summary>" (R.ready_section ~shown:5 many);
  let page =
    R.render_pr (pr ~kw:[ ("labels", strs [ "consider for merge" ]) ] [ day10 ])
  in
  check_in "data-v=\"ready\"" page;
  check_in "data-merge" page;
  check_in "&lt;b&gt;" (R.label_badges [ "<b>" ] [])

let test_verdict () =
  let v = R.verdict in
  let workaround =
    upd lint
      [
        ("class", s "workaround");
        ("rule", s "platform-matrix");
        ("new", s "available: os != \"win32\"");
      ]
  in
  check_str "ready" "ready" (v (pr [ day10 ]));
  check_str "change" "change" (v (pr [ lint; revdep ]))
  (* change first, then blocked *);
  check_str "workaround" "change" (v (pr [ workaround ]));
  check_str "blocked" "blocked" (v (pr [ day10; revdep ]));
  check_str "ci" "ci" (v (pr ~kw:[ ("ci_unfinished", `Int 3) ] [ day10 ]))
  (* not ready while CI runs *);
  check_str "other" "other"
    (v
       (pr
          [
            upd day10
              [
                ("class", s "explanation");
                ("fix_in", s "this PR");
                ("rule", s "uninstallable");
              ];
          ]));
  check_str "pending" "error" (v (pr ~kw:[ ("pending", `Bool true) ] []));
  check_str "no result" "error" (v (pr ~kw:[ ("result", `Null) ] []));
  (* A draft is never ready: then "on hold". *)
  check_str "draft" "ci" (v (pr ~kw:[ ("draft", `Bool true) ] [ day10 ]))

let test_ci_only_is_an_allow_list () =
  (* "On hold" (key "ci") needs every failure to be not the PR's; anything
     else not this PR's change nor another package's needs a look. *)
  let v = R.verdict in
  let draft =
    [ ("draft", `Bool true) ]
    (* never ready: the verdicts below are not "ready" *)
  in
  check_str "draft" "ci" (v (pr ~kw:draft [ day10 ]));
  (* No suggestion at all, though jobs failed (a failed analysis job, stored before its rule). *)
  let empty =
    pr
      ~kw:
        [
          ( "jobs",
            `Assoc
              [ ("total", `Int 2); ("failed", `Assoc [ ("analysis", `Int 1) ]) ]
          );
        ]
      []
  in
  check_str "empty" "other" (v empty);
  check_in "no explanation for the failed jobs (1 analysis failed)"
    (R.headline empty);
  check_in "No explanation for the failed jobs" (R.render_pr empty);
  let unknown =
    [
      ("class", s "explanation");
      ("rule", s "unrecognised");
      ("fix_in", s "unknown");
      ("failing", s "x.1");
      ("variant", s "1 job");
    ]
  in
  check_str "unknown" "other" (v (pr ~kw:draft [ day10; unknown ]));
  check_str "rule-error" "other"
    (v (pr ~kw:draft [ upd unknown [ ("rule", s "rule-error") ] ]));
  let systemic =
    [
      ("class", s "explanation");
      ("rule", s "platform-matrix-systemic");
      ("variant", s "6 packages [p]");
      ("fix_in", s "the PR, a shared dependency, or the CI");
    ]
  in
  check_str "systemic" "other" (v (pr ~kw:draft [ systemic ]));
  (* ... unless every package fails through one dependency: the fix goes there. *)
  check_str "systemic dep" "blocked"
    (v (pr [ upd systemic [ ("failing_dep", s "dep.1") ] ]));
  let own_sources =
    [
      ("class", s "elsewhere");
      ("rule", s "sources");
      ("failing", s "x.1");
      ("fix_in", s "x.1 (upstream archive)");
      ("variant", s "v");
    ]
  in
  check_str "sources" "other" (v (pr ~kw:draft [ own_sources ]));
  let llm_upstream =
    [
      ("class", s "elsewhere");
      ("rule", s "llm");
      ("failing", s "x.1");
      ("variant", s "v");
      ("fix_in", s "upstream (the package's source)");
    ]
  in
  check_str "llm" "other" (v (pr ~kw:draft [ day10; llm_upstream ]));
  (* Failures beyond --max were not analysed: they may be anything. *)
  check_str "unanalysed" "other"
    (v (with_unanalysed 2 (pr ~kw:draft [ day10 ])))

let test_on_hold () =
  (* No failure is the PR's, but it is a draft, labelled, or its CI runs:
     "not this PR's, on hold", whatever makes the failures not the PR's. *)
  let v = R.verdict in
  let pre = upd revdep [ ("preexisting", s "on master") ] in
  let hc =
    [
      ("class", s "elsewhere");
      ("rule", s "health-check-preexisting");
      ("fix_in", s "y.1 (other package)");
      ("where_kind", s "other-package");
      ("failing", s "y.1");
      ("variant", s "v");
    ]
  in
  List.iter
    (fun kw ->
      check_str "pre" "ci" (v (pr ~kw [ pre ]));
      let exp =
        [
          ("class", s "explanation");
          ("rule", s "unrecognised");
          ("fix_in", s "unknown");
          ("variant", s "v");
          ("experimental", `Bool true);
        ]
      in
      check_str "hc" "ci" (v (pr ~kw [ hc; exp ]));
      check_str "day10 pre" "ci" (v (pr ~kw [ day10; pre ])))
    [
      [ ("draft", `Bool true) ];
      [ ("labels", strs [ "do not merge" ]) ];
      [ ("ci_unfinished", `Int 3) ];
    ];
  check_in "No failure is this PR's fault" (R.verdict_help "ci");
  check_str "label" "On hold" (R.verdict_label "ci")

let test_llm_never_ci_side () =
  (* An old stored LLM answer saying "the CI environment" (its where_kind is
     ci): not a reason for "not this PR's fault", nor for "on hold" or "needs
     a rerun". *)
  let llm_ci =
    [
      ("class", s "elsewhere");
      ("rule", s "llm");
      ("failing", s "x.1");
      ("variant", s "v");
      ("fix_in", s "the CI environment");
    ]
  in
  check_str "kind" "ci" (Where_kind.to_string (Where.where_kind (d llm_ci)));

  check_false "not_this_pr" (R.not_this_pr (d llm_ci));
  check_false "ready" (R.seems_ready (pr [ day10; llm_ci ]));
  check_str "other" "other" (R.verdict (pr [ day10; llm_ci ]));
  check_str "draft" "other"
    (R.verdict (pr ~kw:[ ("draft", `Bool true) ] [ day10; llm_ci ]));
  let rerun =
    [
      ("class", s "elsewhere");
      ("rule", s "rerun");
      ("fix_in", s "CI (rerun)");
      ("variant", s "7 jobs");
    ]
  in
  check_str "rerun" "other" (R.verdict (pr [ rerun; llm_ci ]));
  (* Still shared with other PRs, pre-existing or experimental: those do not depend on the model. *)
  check_true "experimental"
    (R.not_this_pr (d (upd llm_ci [ ("experimental", `Bool true) ])))

let test_needs_a_rerun () =
  (* Jobs that failed because of the CI were not tested: such a PR is not
     ready, even when nothing else points at it. *)
  let v = R.verdict in
  let rerun =
    [
      ("class", s "elsewhere");
      ("rule", s "rerun");
      ("fix_in", s "CI (rerun)");
      ("variant", s "7 jobs");
      ("error", s "4 cancelled, 3 network errors (curl failed)");
    ]
  in
  List.iter
    (fun sugg ->
      let p = pr sugg in
      check_false "ready" (R.seems_ready p);
      check_str "rerun" "rerun" (v p))
    [
      [ rerun ];
      [ day10; rerun ];
      [ upd revdep [ ("preexisting", s "on master") ]; rerun ];
    ];
  check_str "headline"
    "Jobs to rerun (4 cancelled, 3 network errors (curl failed))"
    (R.headline ~as_html:false (pr [ day10; rerun ]));
  let lint_ci =
    [
      ("class", s "elsewhere");
      ("rule", s "lint-ci-error");
      ("fix_in", s "CI (rerun)");
      ("variant", s "(lint)");
    ]
  in
  check_str "lint ci" "rerun" (v (pr [ lint_ci ]));
  (* Reruns of experimental jobs only do not hold a PR back. *)
  check_str "exp" "ready"
    (v (pr [ upd rerun [ ("experimental", `Bool true) ] ]));
  (* Anything else of the PR comes first. *)
  check_str "change" "change" (v (pr [ lint; rerun ]));
  check_str "blocked" "blocked" (v (pr [ revdep; rerun ]));
  check_str "other" "other"
    (v (pr [ upd lint [ ("new", `Null); ("class", s "explanation") ]; rerun ]));
  let page =
    R.render
      (`Assoc
         [
           ("prs", `List [ pr [ rerun ] ]);
           ("generated", s "2026-11-20T10:00:00+00:00");
         ])
  in
  check_in "data-f=\"rerun\"" page;
  check_in ".v-rerun{" page;
  check_not_in "<section id=\"ready\">" page;
  check_in "Needs a rerun" (R.help_section (`Assoc []))

let test_bounded_out () =
  (* A checked bound (where_kind set by the rule) is the point of the change;
     one stored before the check (no where_kind) keeps the PR out of the
     ready list. *)
  let b =
    [
      ("class", s "elsewhere");
      ("rule", s "bounded-out");
      ("fix_in", s "nothing (this PR's bound)");
      ("failing", s "x.1");
      ("variant", s "x.1 [all build jobs skipped]");
    ]
  in
  check_true "checked"
    (R.seems_ready (pr [ upd b [ ("where_kind", s "none") ] ]));
  check_false "unchecked" (R.seems_ready (pr [ b ]));
  check_str "other" "other" (R.verdict (pr [ b ]))

let test_unknown_is_an_explanation () =
  let c kv = Titles.classify (d kv) in
  check_str "unrecognised" "explanation"
    (c [ ("rule", s "unrecognised"); ("fix_in", s "unknown") ]);
  check_str "systemic" "explanation"
    (c
       [
         ("rule", s "platform-matrix-systemic");
         ("fix_in", s "the PR, a shared dependency, or the CI");
       ]);
  check_str "systemic dep" "elsewhere"
    (c
       [
         ("rule", s "platform-matrix-systemic");
         ("failing_dep", s "d.1");
         ("fix_in", s "the PR, a shared dependency, or the CI");
       ]);
  check_str "builder" "elsewhere"
    (c
       [
         ("rule", s "builder-specific-systemic");
         ("fix_in", s "the CI environment (day10 builder)");
       ])

let data_re = Rx.v {|data-f="(\w+)"[^>]*>.*?<b>(\d+)</b>|}

let test_verdict_counts_add_up () =
  let prs =
    [
      pr ~kw:[ ("number", `Int 1) ] [ day10 ];
      pr ~kw:[ ("number", `Int 2) ] [ lint; revdep ];
      pr ~kw:[ ("number", `Int 3) ] [ day10; revdep ];
      pr ~kw:[ ("number", `Int 4); ("pending", `Bool true) ] [];
      pr ~kw:[ ("number", `Int 5); ("draft", `Bool true) ] [ day10 ];
      pr
        ~kw:
          [
            ("number", `Int 6);
            ("ci_state", s "PENDING");
            ("labels", strs [ "consider for merge" ]);
          ]
        [ lint ];
    ]
  in
  let page =
    R.render
      (`Assoc
         [
           ("prs", `List prs);
           ("generated", s "2026-11-20T10:00:00+00:00");
           ("open_prs", `Int 9);
           ("failing_prs", `Int 6);
         ])
  in
  (* Python: data-f="(?!all)\w+"[^>]*>.*?<b>(\d+)</b> *)
  let counts =
    List.filter_map
      (fun m ->
        if Rx.group m 1 = "all" then None
        else Some (int_of_string (Rx.group m 2)))
      (Rx.finditer data_re page)
  in
  Alcotest.(check int)
    "counts" (List.length prs)
    (List.fold_left ( + ) 0 counts);
  check_in (Printf.sprintf "All<b>%d</b>" (List.length prs)) page;
  Alcotest.(check int)
    "prs" (List.length prs)
    (List.length (Util.split_on "<details class=\"pr " page) - 1);
  check_in "consider for merge (1)" page;
  check_in "CI running" page;
  check_not_in "class=\"stat" page;
  check_not_in "id=\"expand\"" page;
  check_in "rel=\"alternate\" type=\"application/atom+xml\"" page;
  let feed = R.render_feed (`Assoc [ ("prs", `List prs) ]) in
  check_in "Change this PR: " feed

let test_merge_similar () =
  let a =
    d
      (upd day10
         [
           ("failing", s "a.1");
           ("jobs", strs [ "day10,a,1"; "day10,a,2" ]);
           ("note", s "a.1 note");
           ("error", s "E");
         ])
  in
  let b =
    d
      (upd day10
         [
           ("failing", s "b.1");
           ("jobs", strs [ "day10,b,1" ]);
           ("note", s "b.1 note");
           ("error", s "E");
           ("also_failing_in", ints [ 7 ]);
         ])
  in
  let c = d revdep in
  let llm =
    [
      ("class", s "elsewhere");
      ("rule", s "llm");
      ("title", s "LLM");
      ("fix_in", s "x");
      ("failing", s "c.1");
    ]
  in
  let merged =
    R.merge_similar [ a; c; b; d llm; d (upd llm [ ("failing", s "d.1") ]) ]
  in
  Alcotest.(check int) "merged" 4 (List.length merged)
  (* a and b merged; LLM answers never *);
  let m = List.hd merged in
  Alcotest.(check int) "merged count" 2 (Option.value ~default:0 m.merged);
  Alcotest.(check (list string))
    "packages" [ "a.1"; "b.1" ]
    (Suggestion.list m.packages);
  Alcotest.(check (list string))
    "jobs"
    [ "day10,a,1"; "day10,a,2"; "day10,b,1" ]
    (Suggestion.list m.Suggestion.jobs);
  Alcotest.(check (list string)) "errors" [ "E" ] (Suggestion.list m.errors);
  Alcotest.(check (list string))
    "notes" [ "a.1 note"; "b.1 note" ] (Suggestion.list m.notes);
  Alcotest.(check (list int)) "also" [ 7 ] (Suggestion.list m.also_failing_in);
  check_true "same" (List.nth merged 1 == c);
  (* A different new constraint is a different suggestion. *)
  Alcotest.(check int)
    "different new" 2
    (List.length
       (R.merge_similar
          [ d (upd lint [ ("new", s "x") ]); d (upd lint [ ("new", s "y") ]) ]));
  let item = R.render_item ~sha:(String.make 40 'a') m in
  check_in "2 failures, 3 jobs" item;
  check_in "a.1, b.1" item;
  check_not_in "Fix in: this PR" item;
  let page =
    R.render_pr
      (pr
         ~kw:[ ("number", `Int 3) ]
         [
           upd day10
             [
               ("failing", s "a.1");
               ("jobs", strs [ "day10,a,1"; "day10,a,2" ]);
               ("note", s "a.1 note");
               ("error", s "E");
             ];
           upd day10
             [
               ("failing", s "b.1");
               ("jobs", strs [ "day10,b,1" ]);
               ("note", s "b.1 note");
               ("error", s "E");
               ("also_failing_in", ints [ 7 ]);
             ];
         ])
  in
  check_in "Fix goes elsewhere (1)" page;
  Alcotest.(check int)
    "items" 1
    (List.length (Util.split_on "<details class=\"it\">" page) - 1)

let test_compact_job_links () =
  let link = R.job_link (Some (String.make 40 'a')) "compilers,5.4,x.1" in
  check_in "data-job=\"compilers,5.4,x.1\"" link;
  check_not_in "href" link;
  check_not_in "<a" (R.job_link (Some (String.make 40 'a')) "x\"><script>")
  (* not a variant: plain text *);
  let page = R.render_pr (pr [ upd day10 [ ("jobs", strs [ "v1"; "v2" ]) ] ]) in
  check_in
    (Printf.sprintf "data-ci=\"%s%s/variant/\"" R.base (String.make 40 'a'))
    page

let test_headline_blocked_without_package () =
  let depext =
    [
      ("class", s "elsewhere");
      ("rule", s "missing-system-package");
      ("failing", s "p.1");
      ("fix_in", s "the dependency declaring these depexts");
      ("variant", s "v");
      ("title", s "System package not found");
    ]
  in
  let p = pr [ depext ] in
  check_str "verdict" "blocked" (R.verdict p);
  let h = R.headline ~as_html:false p in
  check_not_in "the dependency declaring" h;
  check_in "System package not found (p.1)" h

let test_where_kind () =
  (* The verdict, the headline and the shared-failures table follow
     where_kind and where_pkg, whatever the words of fix_in. *)
  let other =
    [
      ("class", s "elsewhere");
      ("rule", s "library-api");
      ("failing", s "y.1");
      ("variant", s "v");
      ("fix_in", s "the y maintainers");
      ("where_kind", s "other-package");
      ("where_pkg", s "y.1");
    ]
  in
  check_str "blocked" "blocked" (R.verdict (pr [ day10; other ]));
  check_str "headline" "needs a fix in y.1"
    (R.headline ~as_html:false (pr [ day10; other ]));
  let not_other = upd revdep [ ("where_kind", s "pr") ] in
  check_true "not blocked" (R.verdict (pr [ day10; not_other ]) <> "blocked");
  let table =
    R.chronic
      (List.map (fun n -> pr ~kw:[ ("number", `Int n) ] [ other ]) [ 1; 2 ])
  in
  check_in "<b>y.1</b>" table

let test_skipped_jobs () =
  let p =
    pr
      ~kw:
        [
          ( "jobs",
            `Assoc
              [
                ("total", `Int 10);
                ("failed", `Assoc [ ("not-available", `Int 9) ]);
              ] );
        ]
      [
        upd lint
          [
            ("rule", s "uninstallable");
            ("new", `Null);
            ("class", s "explanation");
          ];
      ]
  in
  let page = R.render_pr p in
  check_in "9 skipped jobs" page;
  check_not_in "0 failed jobs" page

let test_llm_off_note () =
  let page =
    R.render
      (`Assoc
         [
           ("prs", `List []);
           ( "llm",
             `Assoc
               [
                 ("enabled", `Bool true);
                 ("calls", `Int 0);
                 ("off", s "credentials");
               ] );
         ])
  in
  check_in "Anthropic refused the API key" page;
  check_in "They were turned off during this update." page;
  check_not_in "Anthropic refused"
    (R.render
       (`Assoc
          [
            ("prs", `List []);
            ("llm", `Assoc [ ("enabled", `Bool true); ("calls", `Int 3) ]);
          ]))

let test_stale_health_check_note () =
  let p =
    match pr [ lint ] with
    | `Assoc kv ->
        `Assoc
          (List.map
             (fun (k, v) ->
               if k = "result" then
                 ( k,
                   `Assoc
                     (Json.to_assoc v @ [ ("health_stale", s "2026-10-01") ]) )
               else (k, v))
             kv)
    | v -> v
  in
  check_in "opam-health-check's latest results are from" (R.render_pr p);
  check_not_in "opam-health-check's latest" (R.render_pr (pr [ lint ]))

let test_headline () =
  let h ?as_html p = R.headline ?as_html p in
  let fix =
    upd lint
      [
        ("new", s "\"ocaml\" {>= \"5.1\"}   (a comment)");
        ("failing", s "colibri2.0.7");
      ]
  in
  let workaround =
    upd lint
      [ ("class", s "workaround"); ("new", s "available: os != \"win32\"") ]
  in
  check_str "fix"
    "<code>&quot;ocaml&quot; {&gt;= &quot;5.1&quot;}</code> for colibri2.0.7"
    (h (pr [ fix ]));
  check_in "for x.1 (workaround)" (h (pr [ workaround ]));
  check_in "also blocked by y.1" (h (pr [ fix; revdep ]));
  check_str "blocked" "needs a fix in y.1" (h (pr [ day10; revdep ]));
  let two =
    List.map
      (fun i ->
        upd revdep [ ("fix_in", s (Printf.sprintf "z.%d (other package)" i)) ])
      [ 1; 2 ]
    @ [ revdep ]
  in
  check_str "two" "needs fixes in z (2 versions), y.1" (h (pr two));
  check_str "day10" "Fails only on the day10 builder (x.1)" (h (pr [ day10 ]));
  check_str "text" "`\"ocaml\" {>= \"5.1\"}` for colibri2.0.7"
    (h ~as_html:false (pr [ fix ]));
  check_in "&lt;" (h (pr [ upd day10 [ ("rule", s "<b>") ] ]))
  (* an unknown rule is its own title *);
  (* Fix in "this PR" goes without saying; confidence only when not high. *)
  let card = R.render_suggestion (d (upd fix [ ("confidence", s "high") ])) in
  check_not_in "Fix in" card;
  check_not_in "confidence" card;
  check_in "medium confidence"
    (R.render_suggestion (d (upd fix [ ("confidence", s "medium") ])))

let test_changelog_in_comment () =
  let c =
    [
      ("dep", s "x");
      ( "changelog",
        `Assoc
          [
            ("file", s "CHANGES");
            ("versions", strs [ "2.0.0"; "<img src=a>" ]);
            ("text", s "- uses ``` fences\n- @someone [link](https://e.x)");
            ("cut", `Bool true);
          ] );
    ]
  in
  let md =
    match (sugg [ ("candidates", `List [ `Assoc c ]) ]).candidates with
    | Some [ c ] -> Render.changelog_details c
    | _ -> Alcotest.fail "candidate"
  in
  check_in "````text\n" md (* a fence longer than any backtick run inside *);
  check_true "ends"
    (String.ends_with
       ~suffix:(Util.rstrip "````\n\n</details>")
       (Util.rstrip md));
  check_not_in "<img" md (* not a version: left out of the summary *);
  check_in "\n\xe2\x80\xa6\n" md;
  let page =
    R.render_suggestion
      (d
         [
           ("rule", s "test-output");
           ( "candidates",
             `List [ `Assoc (upd c [ ("new", s "\"x\" {< \"2.0.0\"}") ]) ] );
         ])
  in
  check_in "&lt;img src=a&gt;" page;
  check_not_in "<img" page

let test_comment_markdown () =
  let md = R.comment_markdown (pr [ day10 ]) in
  check_in "None of the failures is this PR's fault" md;
  let llm_low =
    [
      ("class", s "fix");
      ("rule", s "llm");
      ("confidence", s "low");
      ("fix_in", s "this PR");
      ("failing", s "z.1");
      ("variant", s "v");
      ("title", s "LLM diagnosis (unchecked)");
    ]
  in
  let md = R.comment_markdown (pr [ lint; llm_low ]) in
  check_not_in "None of the failures" md;
  check_not_in "z.1" md;
  let page = R.render_pr (pr [ lint ]) in
  check_in "data-for=\"md-1\"" page;
  check_in "</textarea>" page;
  Alcotest.(check int)
    "textareas" 1
    (List.length (Util.split_on "<textarea" page) - 1);
  (* Only workarounds: the comment says so, as the verdict does ("Change this PR"). *)
  let workaround =
    upd lint [ ("class", s "workaround"); ("rule", s "platform-matrix") ]
  in
  let md = R.comment_markdown (pr [ workaround ]) in
  check_in "The workarounds below skip" md;
  check_not_in "No change to this PR" md

(* ------------------------------------------------------------------------ *)
(* EscapingTest: untrusted text (logs, opam files, LLM answers) in the
   Markdown posted on GitHub: no link, image, HTML, mention or reference,
   and no way out of a code block. *)

let hostile =
  "@ocaml/maintainers ![x](https://evil.example/x.png) <img src=x> see #123, \
   ocaml/dune#4 GH-5 www.evil.example"

let allowed_tags = Rx.v {|</?(details|summary|sub|br)>|}
let trusted_link = Rx.v {|\]\(https://opam\.ci\.ocaml\.org/[\w.~,+%/=-]+\)|}

(* The first run of exactly [run] backticks at or after [from] (Python:
   (?<!`)RUN(?!`)): its end, or None. *)
let find_closing text run from =
  let n = String.length text and k = String.length run in
  let rec go i =
    if i + k > n then None
    else if
      String.sub text i k = run
      && (i = 0 || text.[i - 1] <> '`')
      && (i + k >= n || text.[i + k] <> '`')
    then Some (i + k)
    else go (i + 1)
  in
  go from

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
      let j = ref !i in
      while !j < n && text.[!j] = '`' do
        incr j
      done;
      let run = String.sub text !i (!j - !i) in
      match find_closing text run !j with
      | Some e -> i := e
      | None ->
          Buffer.add_string res run;
          i := !j
    end
    else (
      Buffer.add_char res text.[!i];
      incr i)
  done;
  Buffer.contents res

let assert_inert md =
  let rest = Rx.sub trusted_link "]" (outside_code md) in
  let rest = Rx.sub allowed_tags "" rest in
  List.iter
    (fun bad ->
      if Rx.test (Rx.v bad) rest then
        Alcotest.failf "%S in:\n%s\n--- from:\n%s" bad rest md)
    [ {|@\w|}; {|#\d|}; "<"; {|\]\(|}; "://"; {|www\.|}; {|GH-\d|} ]

let test_md_text () =
  List.iter
    (fun x ->
      let md = Md.md_text x in
      check_not_in "\n" md;
      assert_inert md)
    [
      hostile;
      "a `b` c";
      "x@y`z` ``a`b``";
      "tick ` alone";
      "line1\n```\n@x";
      "\\`@x`";
      "[a](javascript:x) | *b* _c_ &#64;d";
      "`` ` `` @e";
    ];
  check_str "ref" "see `#123`." (Md.md_text "see #123.");
  check_str "code" "Add a `license:` field"
    (Md.md_text "Add a `license:` field");
  check_str "escape" "ppx\\_deriving \\< 6" (Md.md_text "ppx_deriving < 6");
  check_true "cut"
    (String.ends_with ~suffix:"\xe2\x80\xa6"
       (Md.md_text ~limit:10 (String.make 50 'x')));
  check_str "md_code" "``a`b``" (Md.md_code "a`b");
  check_str "md_code pad" "`` `x ``" (Md.md_code "`x");
  check_str "md_fence" "`````\na\n````\nb\n`````" (Md.md_fence "a\n````\nb")

let hostile_sugg kw =
  d
    (upd
       [
         ("rule", s "lint");
         ("class", s "fix");
         ("confidence", s "high");
         ("fix_in", s "this PR");
         ("failing", s "x.1");
         ("variant", s "v");
         ("title", s hostile);
         ("error", s hostile);
         ("note", s hostile);
         ("old", s "depends: \"a\" @old");
         ("new", s ("depends: \"b\"\n```\n" ^ hostile ^ "\n```"));
         ("alternative", s ("`@x` [y](https://e)  (" ^ hostile ^ ")"));
       ]
       kw)

let no_data =
  {
    Ci.pr = None;
    jobs = [];
    unfinished = 0;
    unanalysed = 0;
    health_stale = None;
    llm_unanswered = None;
    sha = None;
  }

let test_render_comment () =
  let sha = String.make 40 'a' in
  let rs =
    [
      hostile_sugg [];
      hostile_sugg
        [
          ("failing", s hostile);
          ("new", s ("x-ci-accept-failures: [" ^ hostile ^ "]   " ^ hostile));
        ];
      hostile_sugg [ ("class", s "workaround") ];
      hostile_sugg
        [
          ("class", s "elsewhere");
          ("fix_in", s hostile);
          ("rule", s "rerun");
          ( "jobs",
            strs
              [
                "day10,debian-13,x86_64,5.5,x.1";
                "x)[evil](https://e.x";
                hostile;
              ] );
        ];
      hostile_sugg
        [
          ("class", s "explanation");
          ("failing", `Null);
          ("variant", s hostile);
          ("url", s "https://evil.example/log");
          ("rule", s "unrecognised");
          ("jobs", strs [ hostile ]);
        ];
      hostile_sugg
        [
          ("class", s "explanation");
          ("url", s (base_url ^ sha ^ "/variant/v,1"));
        ];
      hostile_sugg
        [
          ("evidence", `Assoc [ ("x 1.0", `Bool true); (hostile, `Bool false) ]);
        ];
      hostile_sugg
        [
          ( "candidates",
            `List
              [
                `Assoc
                  [
                    ("dep", s hostile);
                    ("new", s hostile);
                    ( "changelog",
                      `Assoc
                        [
                          ("text", s hostile);
                          ("versions", strs [ hostile; "1.0" ]);
                        ] );
                  ];
              ] );
          ("compared_with", s hostile);
        ];
    ]
  in
  let md = Render.render_comment sha no_data rs in
  assert_inert md;
  (* Our own links stay. *)
  check_in
    (Printf.sprintf "](%s%s/variant/day10,debian-13,x86_64,5.5,x.1)" base_url
       sha)
    md;
  check_in (Printf.sprintf "([log](%s%s/variant/v,1))" base_url sha) md;
  check_not_in "](https://e" (outside_code md);
  (* A hostile commit is not linked. *)
  assert_inert (Render.render_comment "x)[a](https://e" no_data [ List.hd rs ])

let test_build_review () =
  let sha = String.make 40 'b' in
  let text = "opam-version: \"2.0\"\ndepends: \"a\" @old\nlicense: \"x\"\n" in
  let patch =
    "@@ -0,0 +1,3 @@\n+" ^ String.concat "\n+" (Util.splitlines text)
  in
  (* The GitHub API answers, as Net.gh_api's memory cache holds them. *)
  Hashtbl.replace Net.gh_cache "repos/ocaml/opam-repository/pulls/1"
    (`Assoc [ ("head", `Assoc [ ("sha", s sha) ]) ]);
  Hashtbl.replace Net.gh_cache
    "repos/ocaml/opam-repository/pulls/1/files?per_page=100"
    (`List
       [ `Assoc [ ("filename", s "packages/x/x.1/opam"); ("patch", s patch) ] ]);
  let ok =
    hostile_sugg
      [ ("new", s "depends: \"b\" `@x` ```"); ("alternative", `Null) ]
  in
  let llm = hostile_sugg [ ("rule", s "llm"); ("new", s "depends: \"c\"") ] in
  let multi = hostile_sugg [ ("new", s "depends: \"d\"\n```\n@x") ] in
  let review =
    Render.build_review
      ~raw:(fun _ -> Some text)
      sha
      { no_data with pr = Some 1 }
      [ ok; llm; multi ]
  in
  let comments = Json.list "comments" review in
  Alcotest.(check int) "comments" 1 (List.length comments)
  (* not the LLM answer, not a multi-line change *);
  let body = Json.str "body" (List.hd comments) in
  check_in "\n````suggestion\ndepends: \"b\" `@x` ```\n````" body;
  assert_inert body;
  assert_inert (Json.str "body" review);
  check_false "llm not placed" llm.placed;
  check_in "+ depends: \"c\"" (Json.str "body" review)
(* the LLM change stays in the summary, as a diff *)

let test_git_urls () =
  List.iter
    (fun (url, exp) ->
      Alcotest.(check (option string)) url exp (Net.repo_url url))
    [
      ("https://github.com/o/r.git", Some "https://github.com/o/r");
      ( "git+https://gitlab.com/g/s/r.git#main",
        Some "https://gitlab.com/g/s/r.git" );
      ("git@github.com:o/r.git", Some "https://github.com/o/r");
      ("git://git.example.org/r", Some "https://git.example.org/r");
      ("--upload-pack=touch /tmp/x", None);
      ("ssh://git@e.x/r", None);
      ("file:///tmp/r", None);
      ("ext::sh -c id", None);
      ("https://e.x/a b", None);
      ("https://-e.x/r", None);
      ("http://e.x/r", None);
    ]

let test_git_command () =
  let calls = ref [] in
  let saved = !Proc.run_hook in
  (Proc.run_hook :=
     fun ?cwd:_ ?env:_ ?input:_ ?timeout args ->
       calls := (args, timeout) :: !calls;
       raise (Proc.Timeout 1.));
  Fun.protect
    ~finally:(fun () -> Proc.run_hook := saved)
    (fun () ->
      let r = Net.git ~check:false [ "rev-parse"; "HEAD" ] in
      check_true "failed" (r.code <> 0);
      match Net.git [ "rev-parse"; "HEAD" ] with
      | _ -> Alcotest.fail "no exception"
      | exception Failure _ -> ());
  let cmd, timeout = List.nth !calls (List.length !calls - 1) in
  Alcotest.(check (list string))
    "protocols"
    [ "git"; "-c"; "protocol.allow=never"; "-c"; "protocol.https.allow=always" ]
    (Util.take 5 cmd);
  check_true "timeout" (timeout <> None)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "page"
    [
      ( "PageTest",
        [
          t "help_texts" test_help_texts;
          t "seems_ready" test_seems_ready;
          t "pending_status_with_finished_page"
            test_pending_status_with_finished_page;
          t "kept_result_after_a_failed_analysis"
            test_kept_result_after_a_failed_analysis;
          t "incomplete_analysis_shown" test_incomplete_analysis_shown;
          t "ready_section_and_labels" test_ready_section_and_labels;
          t "verdict" test_verdict;
          t "ci_only_is_an_allow_list" test_ci_only_is_an_allow_list;
          t "on_hold" test_on_hold;
          t "llm_never_ci_side" test_llm_never_ci_side;
          t "needs_a_rerun" test_needs_a_rerun;
          t "bounded_out" test_bounded_out;
          t "unknown_is_an_explanation" test_unknown_is_an_explanation;
          t "verdict_counts_add_up" test_verdict_counts_add_up;
          t "merge_similar" test_merge_similar;
          t "compact_job_links" test_compact_job_links;
          t "headline_blocked_without_package"
            test_headline_blocked_without_package;
          t "where_kind" test_where_kind;
          t "skipped_jobs" test_skipped_jobs;
          t "llm_off_note" test_llm_off_note;
          t "stale_health_check_note" test_stale_health_check_note;
          t "headline" test_headline;
          t "changelog_in_comment" test_changelog_in_comment;
          t "comment_markdown" test_comment_markdown;
        ] );
      ( "EscapingTest",
        [
          t "md_text" test_md_text;
          t "render_comment" test_render_comment;
          t "build_review" test_build_review;
          t "git_urls" test_git_urls;
          t "git_command" test_git_command;
        ] );
    ]
