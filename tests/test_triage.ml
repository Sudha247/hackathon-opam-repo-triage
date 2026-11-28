(* Unit tests of the library: checks of suggested changes, where fixes go,
   the rules on synthetic logs and fake sources, ocaml.org's MCP answers
   (served by a fake transport), network and git failures, caches, and runs
   of [suggest] on a fixture with a rule or a fetch replaced. Ported from
   tests/test_triage.py (the fixture cases themselves are in test_cases.ml). *)

open Triage
open Suggestion.T

(* A suggestion from its JSON members. *)
let sugg kv = Suggestion.of_json (`Assoc kv)

open Ci

let s x : Json.t = `String x
let strs = Json.strings
let check_str = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_true msg b = check_bool msg true b
let check_false msg b = check_bool msg false b
let check_strs = Alcotest.(check (list string))

let check_json msg (a : Json.t) (b : Json.t) =
  check_str msg (Json.dumps a) (Json.dumps b)

let check_in ?(msg = "") sub s =
  if not (Util.contains s sub) then Alcotest.failf "%s: %S not in %S" msg sub s

let check_not_in ?(msg = "") sub s =
  if Util.contains s sub then Alcotest.failf "%s: %S in %S" msg sub s

let check_none msg = function
  | None -> ()
  | Some _ -> Alcotest.failf "%s: expected None" msg

(* A member of a suggestion, as printed ([`Null] when absent). *)
let get r k = Json.member k (Suggestion.to_json r)
let d kv = sugg kv

let one = function
  | [ r ] -> r
  | l -> Alcotest.failf "expected one result, got %d" (List.length l)

let fixtures =
  if Sys.file_exists "fixtures" then "fixtures"
  else Filename.concat "tests" "fixtures"

let tmpdir () =
  let d = Filename.temp_file "test-triage" "" in
  Sys.remove d;
  Unix.mkdir d 0o755;
  d

(** Make [path] [days] old. *)
let age path days =
  let t = Unix.gettimeofday () -. (days *. 86400.) in
  Unix.utimes path t t

let age_all dir days =
  List.iter (fun f -> age (Filename.concat dir f) days) (Util.listdir dir)

let remove_all dir =
  List.iter (fun f -> Util.rmtree (Filename.concat dir f)) (Util.listdir dir)

(* ------------------------------------------------------------------------ *)
(* The process-wide state and the seams, reset around each test. *)

let originals =
  let h = !Http.hook
  and p = !Proc.run_hook
  and rn = !Net.rename_hook
  and sl = !Net.sleep_hook in
  let g = !Sources.git_hook
  and raw = !Sources.raw_hook
  and tags = !Sources.tags_hook
  and tf = !Sources.tag_for_hook in
  let tr = !Sources.tree_hook
  and fa = !Sources.file_at_hook
  and vs = !Sources.versions_hook in
  let dm = !Sources.doc_modules_hook
  and dp = !Sources.doc_present_hook
  and ah = !Sources.api_history_hook in
  let mp = !Sources.module_providers_hook
  and pb = !Sources.published_hook
  and of_ = !Sources.opam_file_hook in
  let up = !Sources.upstream_hook
  and sf = !Suggest.suggest_for_job_hook
  and fe = !Suggest.fetch_hook in
  let ls = !Llm.llm_suggest_hook in
  fun () ->
    Http.hook := h;
    Proc.run_hook := p;
    Net.rename_hook := rn;
    Net.sleep_hook := sl;
    Sources.git_hook := g;
    Sources.raw_hook := raw;
    Sources.tags_hook := tags;
    Sources.tag_for_hook := tf;
    Sources.tree_hook := tr;
    Sources.file_at_hook := fa;
    Sources.versions_hook := vs;
    Sources.doc_modules_hook := dm;
    Sources.doc_present_hook := dp;
    Sources.api_history_hook := ah;
    Sources.module_providers_hook := mp;
    Sources.published_hook := pb;
    Sources.opam_file_hook := of_;
    Sources.upstream_hook := up;
    Suggest.suggest_for_job_hook := sf;
    Suggest.fetch_hook := fe;
    Llm.llm_suggest_hook := ls

let reset_state ~offline =
  originals ();
  Net.net.kv <- [ ("http", 0); ("mcp", 0); ("git", 0); ("github-api", 0) ];
  Net.offline_misses := [];
  Net.offline_ref := offline;
  Hashtbl.reset Net.gh_cache;
  Sources.mcp_failures := 0;
  Hashtbl.reset Sources.mcp_unavailable;
  Llm.llm_state.streak <- 0;
  Llm.llm_state.off <- None

(** A test without network: everything must come from the hooks (unhooked
    lookups are offline cache misses). *)
let pure f () =
  reset_state ~offline:true;
  Fun.protect f ~finally:(fun () -> reset_state ~offline:true)

(** A test of the network code (NetTestBase): online, with a fake transport. *)
let net f () =
  reset_state ~offline:false;
  Fun.protect f ~finally:(fun () -> reset_state ~offline:true)

let errors ?(k = "mcp-errors") () = Net.net_get k

let nothing_listens () =
  Http.hook := fun _ -> raise (Http.Net_error "[Errno 111] Connection refused")

let http_json v =
  Some
    {
      Http.status = 200;
      resp_headers = [ ("Content-Type", "application/json") ];
      data = Json.dumps v;
    }

(** A tool's error, as ocaml.org's MCP server sends it (plain text). *)
let tool_error message =
  http_json
    (`Assoc
       [
         ("jsonrpc", s "2.0");
         ("id", `Int 1);
         ( "result",
           `Assoc
             [
               ("isError", `Bool true);
               ( "content",
                 `List [ `Assoc [ ("type", s "text"); ("text", s message) ] ] );
             ] );
       ])

let mcp_answer ?(error = false) v =
  http_json
    (`Assoc
       [
         ("jsonrpc", s "2.0");
         ("id", `Int 1);
         ( "result",
           `Assoc
             [
               ("isError", `Bool error);
               ( "content",
                 `List
                   [ `Assoc [ ("type", s "text"); ("text", s (Json.dumps v)) ] ]
               );
             ] );
       ])

(** The MCP server answers [answer body] (None: the connection is closed); the
    requests' bodies are recorded, in order. *)
let serve answer =
  let requests = ref [] in
  (Http.hook :=
     fun (r : Http.request) ->
       if r.url <> Sources.mcp_url then raise (Http.Net_error "unexpected URL");
       let body = Option.value ~default:"" r.body in
       requests := !requests @ [ body ];
       match answer body with
       | Some resp -> resp
       | None ->
           raise
             (Http.Net_error "Remote end closed connection without response"));
  requests

(** [serve] with the tool call's params (name, arguments). *)
let serve_params answer =
  serve (fun body -> answer (Json.member "params" (Json.parse body)))

let params body = Json.member "params" (Json.parse body)
let arg k body = Json.member k (Json.member "arguments" (params body))

(* ------------------------------------------------------------------------ *)
(* Synthetic jobs *)

let mk ?(platform = []) ?target ?(kind = "build") ?revdep ?(status = "failed")
    ?(category = "build") ?reason ?(experimental = false) ?(url = "") variant =
  {
    variant;
    url;
    label = "";
    status = Ci.Status.of_string status;
    reason;
    category = Ci.Category.of_string category;
    target;
    kind = Ci.Kind.of_string kind;
    revdep;
    platform;
    experimental;
    in_summary = false;
    messages = [];
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

let src0 () = Sources.create "/nonexistent"

let info ?(installed = []) ?action ?ocaml () : Loginfo.info =
  {
    installed;
    action;
    failing = None;
    ocaml;
    error_lines = [];
    test_diff = false;
    base = None;
  }

(* ------------------------------------------------------------------------ *)
(* ChecksTest: the checks of suggested changes (no network). *)

let test_unsatisfiable () =
  let terms = Constraints.version_terms in
  check_true "eq"
    (Constraints.unsatisfiable (terms {|>= "11.0.0" & < "11.0.0"|}));
  check_true "v"
    (Constraints.unsatisfiable (terms {|>= "v0.9.15" & < "v0.9.15"|}));
  check_false "ok"
    (Constraints.unsatisfiable (terms {|>= "4.0.0" & < "5.0.0"|}))

let test_allows () =
  let terms = Constraints.version_terms in
  check_true "allows" (Constraints.allows "6.0.0" (terms {|>= "4.0.0"|}));
  check_false "excludes"
    (Constraints.allows "6.0.0" (terms {|>= "4.0.0" & < "5.0.0"|}))

(* One version of a package failing on two platforms with notes that differ
   by the platform's name is one entry naming both (conf-rust-2018 on Cygwin
   and MSYS2, PR 30751); a note that says more on one platform stays an entry
   of its own (conf-gcc's cygwinports depexts, PR 31316). *)
let test_merge_versions_platforms () =
  let entry pkg plat extra =
    Suggestion.make
      [
        `Rule Rule.Missing_tool;
        `Failing (Some pkg);
        `Error {|"cargo": command not found|};
        `Note
          (Printf.sprintf
             "%s does not provide `cargo` on %s: its depexts have no entry for \
              that platform. Add one.%s"
             (fst (Ci.split_pkg pkg))
             plat extra);
        `Jobs [ "day10," ^ plat ^ ",x86_64,4.14,randoml.0.1.5" ];
      ]
  in
  let plats v = [ List.nth (String.split_on_char ',' v) 1 ] in
  (match
     Checks.merge_versions ~plats
       [
         entry "conf-rust-2018.1" "cygwin" "";
         entry "conf-rust-2018.1" "msys2" "";
       ]
   with
  | [ r ] ->
      check_str "note"
        "conf-rust-2018 does not provide `cargo` on cygwin, msys2: its depexts \
         have no entry for these platforms. Add one."
        (Suggestion.str r.note);
      check_int "jobs" 2 (List.length (Checks.jobs_or_variant r))
  | l -> Alcotest.failf "%d entries, expected 1" (List.length l));
  match
    Checks.merge_versions ~plats
      [
        entry "conf-gcc.1.0" "cygwin"
          " Its depexts for os-distribution \"cygwinports\" do not apply.";
        entry "conf-gcc.1.0" "msys2" "";
      ]
  with
  | [ a; b ] ->
      check_true "cygwinports"
        (Util.contains (Suggestion.str a.note) "cygwinports");
      check_true "msys2" (Util.contains (Suggestion.str b.note) "on msys2")
  | l -> Alcotest.failf "%d entries, expected 2" (List.length l)

let test_applied () =
  let text =
    "opam-version: \"2.0\"\ndepends: [\n  \"ocaml\" {>= \"4.08\"}\n]\n"
  in
  let app kv = Option.value ~default:"" (Checks.applied text (d kv)) in
  check_in {|"ocaml" {>= "4.08" & < "5.4"}|}
    (app
       [
         ("old", s {|"ocaml" {>= "4.08"}|});
         ("new", s {|"ocaml" {>= "4.08" & < "5.4"}|});
       ]);
  check_in "  \"conf-python-3\" {build}\n  \"ocaml\""
    (app [ ("new", s {|"conf-python-3" {build}   (new line in depends)|}) ])

(* ------------------------------------------------------------------------ *)
(* SuggestionTest: reading and printing stored suggestions. *)

let test_round_trip () =
  (* Members as the Python and older versions of the tool wrote them: no
     where_kind, nulls, an unknown member, values of another type, a
     candidate with a changelog, evidence in its order. *)
  let stored =
    Json.parse
      {|{"variant": "v", "failing": null, "fix_in": "lwt.5.9.0 (other package)",
         "rule": "test-output", "old": null, "new": "x", "confidence": null,
         "where": null, "note": 3, "jobs": ["a", 1], "future_member": {"k": [1]},
         "evidence": {"b 2.0": true, "a 1.0": null, "c 0.1": false},
         "candidates": [{"dep": "d", "failing": "1.1", "first_after": "1.0",
                         "old": null, "new": "n", "extra": true,
                         "changelog": {"file": null, "versions": ["1.0"],
                                       "text": "t", "cut": false}}],
         "also_failing_in": [12, 34], "experimental": false}|}
  in
  let r = Suggestion.of_json stored in
  check_str "same members"
    (Json.dumps ~sort_keys:true stored)
    (Json.dumps ~sort_keys:true (Suggestion.to_json r));
  check_none "absent where_kind" r.where_kind;
  check_str "derived" "other-package"
    (Where_kind.to_string (Where.where_kind r));
  check_true "null failing" (r.Suggestion.failing = Null);
  check_none "a number as note" r.note;
  check_none "a mixed list as jobs" r.Suggestion.jobs;
  Alcotest.(check (list string))
    "kept apart"
    [ "note"; "jobs"; "future_member" ]
    (List.map fst r.extra);
  Alcotest.(check (list (pair string (option bool))))
    "evidence order"
    [ ("b 2.0", Some true); ("a 1.0", None); ("c 0.1", Some false) ]
    (Suggestion.list r.evidence);
  check_true "evidence printed in order"
    (Json.member "evidence" (Suggestion.to_json r)
    = Json.member "evidence" stored);
  (* A copy is independent of the original. *)
  let c = Suggestion.copy r in
  c.note <- Some "n";
  check_none "original note" r.note;
  check_true "copy" (c != r);
  (* Setting a member replaces a value of another type kept apart. *)
  check_str "set" "n" (Json.str "note" (Suggestion.to_json c))

(* ------------------------------------------------------------------------ *)
(* WhereTest: where a fix goes, as data ([where_kind]), and derived from the
   words of [fix_in] for results stored before it existed. *)

let test_legacy () =
  let kind f =
    Where_kind.to_string (Where.where_kind (d [ ("fix_in", s f) ]))
  in
  let pkg f = Where.where_pkg (d [ ("fix_in", s f) ]) in
  check_str "pr" "pr" (kind "this PR");
  check_str "other" "other-package" (kind "lwt.5.9.0 (other package)");
  Alcotest.(check (option string))
    "other pkg" (Some "lwt.5.9.0")
    (pkg "lwt.5.9.0 (other package)");
  check_str "depexts" "other-package"
    (kind "the dependency declaring these depexts");
  check_none "depexts pkg" (pkg "the dependency declaring these depexts");
  check_str "rerun" "rerun" (kind "CI (rerun)");
  check_str "ci" "ci" (kind "the CI environment (day10 builder)");
  check_str "upstream" "upstream" (kind "x.1 (upstream archive)");
  check_str "none" "none" (kind "nothing (this PR's bound)");
  check_str "unknown" "unknown" (kind "the PR, a shared dependency, or the CI");
  let shared =
    d
      [
        ("fix_in", s "the PR, a shared dependency, or the CI");
        ("failing_dep", s "dep.1");
      ]
  in
  check_str "shared" "other-package"
    (Where_kind.to_string (Where.where_kind shared));
  Alcotest.(check (option string))
    "shared pkg" (Some "dep.1") (Where.where_pkg shared);
  check_str "new words" "unknown" (kind "somewhere new")

(* An LLM answer has no [where_kind] (suggest removes the rule's), only
   [fix_in]: [Titles.readable] derives both the kind and the package from it,
   the package before setting the kind. *)
(* The names of statuses, categories, kinds, rules and where_kind: printed as
   read, unknown ones included, and only [of_string] makes an [Other]. *)
let test_variant_names () =
  let names to_s of_s l =
    List.iter (fun x -> check_str x x (to_s (of_s x))) l
  in
  names Status.to_string Status.of_string
    [ "passed"; "failed"; "active"; "aborted"; "not-started"; "queued"; "" ];
  names Category.to_string Category.of_string
    [
      "build";
      "build-or-test";
      "dependency";
      "solver";
      "network";
      "cancelled";
      "ignored";
      "not-available";
      "sources";
      "lint";
      "analysis";
      "passed";
      "failed";
      "active";
      "aborted";
      "not-started";
      "skipped";
    ];
  names Kind.to_string Kind.of_string
    [
      "build";
      "tests";
      "lower-bounds";
      "revdeps";
      "revdeps-list";
      "lint";
      "analysis";
      "doc";
    ];
  names Rule.to_string Rule.of_string
    [ "library-api"; "stdlib"; "platform-matrix-systemic"; "transient"; "x" ];
  check_true "failed" (Status.of_string "failed" = Failed);
  check_true "passed" (Category.of_string "passed" = Category.passed);
  check_true "lint" (Kind.of_string "lint" = Lint);
  check_true "stdlib" (Rule.of_string "stdlib" = Stdlib_change);
  check_true "systemic"
    (Rule.systemic Platform_matrix = Rule.of_string "platform-matrix-systemic");
  check_true "is_systemic" (Rule.is_systemic (Rule.of_string "x-systemic"));
  List.iter
    (fun k ->
      check_true (Where_kind.to_string k)
        (Where_kind.of_string (Where_kind.to_string k) = Some k))
    Where_kind.all;
  check_true "where_kind" (Where_kind.of_string "elsewhere" = None);
  (* A page with a status the parser does not know. *)
  let page =
    {|<details><summary><b>Main results</b></summary><ul class="statuses"><li class="queued"><a href="/github/ocaml/opam-repository/commit/a/variant/compilers,5.3,x.1">x.1 (queued)</a></li><li class="weird"><a href="/github/ocaml/opam-repository/commit/a/variant/(doc)">(doc) (weird: y)</a></li></ul></details>|}
  in
  let d = Ci.parse_commit_page page in
  check_strs "statuses" [ "queued"; "weird" ]
    (List.map (fun j -> Status.to_string j.status) d.jobs);
  check_strs "categories" [ "queued"; "weird" ]
    (List.map (fun j -> Category.to_string j.category) d.jobs);
  check_strs "kinds" [ "build"; "doc" ]
    (List.map (fun j -> Kind.to_string j.kind) d.jobs)

(* Stored suggestions with a rule, kind or where_kind of another version:
   read and printed back as they were. *)
let test_unknown_names_round_trip () =
  let v : Json.t =
    `Assoc
      [
        ("variant", s "v");
        ("kind", s "doc");
        ("where_kind", s "elsewhere");
        ("rule", s "a-future-rule");
        ("fix_in", s "this PR");
      ]
  in
  let r = Suggestion.of_json v in
  check_true "rule" (r.rule = Some (Rule.of_string "a-future-rule"));
  check_true "where_kind" (r.where_kind = None);
  check_str "derived" "pr" (Where_kind.to_string (Where.where_kind r));
  let sorted (j : Json.t) =
    match j with `Assoc kv -> `Assoc (List.sort compare kv) | j -> j
  in
  check_json "round trip" (sorted v) (sorted (Suggestion.to_json r))

let test_readable_from_fix_in () =
  let r =
    d [ ("rule", s "llm"); ("fix_in", s "ojs_filetree.0.8.0 (other package)") ]
  in
  ignore (Titles.readable [ r ]);
  Alcotest.(check (option string))
    "where_kind" (Some "other-package")
    (Option.map Where_kind.to_string r.Suggestion.where_kind);
  Alcotest.(check (option string))
    "where_pkg" (Some "ojs_filetree.0.8.0")
    (Suggestion.get r.Suggestion.where_pkg);
  Alcotest.(check (option string))
    "where" (Some "ojs_filetree.0.8.0 (another package)")
    (Suggestion.get r.Suggestion.where)

let test_structured_wins () =
  (* The words of fix_in are only read when where_kind is missing. *)
  let r = d [ ("fix_in", s "this PR"); ("where_kind", s "ci") ] in
  check_str "kind" "ci" (Where_kind.to_string (Where.where_kind r));
  Alcotest.(check (option string)) "text" (Some "the CI") (Titles.where_text r);
  let r' = Suggestion.copy r in
  r'.Suggestion.new_ <- Set "x";
  check_str "classify" "elsewhere" (Titles.classify r');
  let r =
    d
      [
        ("fix_in", s "the maintainers of y (new wording)");
        ("where_kind", s "other-package");
        ("where_pkg", s "y.1");
      ]
  in
  Alcotest.(check (option string))
    "pkg" (Some "y.1 (another package)") (Titles.where_text r);
  Alcotest.(check (option string))
    "unnamed"
    (Some "the dependency that declares these depexts (another package)")
    (Titles.where_text
       (d
          [
            ("where_kind", s "other-package");
            ("rule", s "missing-system-package");
          ]))

(* ------------------------------------------------------------------------ *)
(* HealthExplainedElsewhereTest: opam-health-check saw a reverse dependency
   build on master, but its log points at another dependency (#31170: matrix
   0.2.0, not the PR's k7). *)

let test_drop () =
  let a = "day10,5.5,k7.0.10.0,revdeps,console-eio.0.9.0"
  and b = "day10,5.5,k7.0.10.0,revdeps,other.1.0" in
  let notes = Context.new_notes () in
  Hashtbl.replace notes.good "console-eio.0.9.0"
    (Context.PS.singleton ("5.5", a));
  Hashtbl.replace notes.good "other.1.0" (Context.PS.singleton ("5.5", b));
  Hashtbl.replace notes.via "console-eio.0.9.0" (Context.SS.singleton "x.1");
  let matrix =
    d
      [
        ("rule", s "library-api");
        ("new", s {|"matrix" {< "0.2.0"}|});
        ("dep", s "matrix");
        ("jobs", strs [ a ]);
      ]
  in
  let on_pr =
    d
      [
        ("rule", s "library-api");
        ("new", s {|"k7" {< "0.10.0"}|});
        ("dep", s "k7");
        ("variant", s b);
      ]
  in
  let ids =
    Suggest.drop_health_explained_elsewhere notes
      [ matrix; on_pr; d [ ("rule", s "lint"); ("new", s "x") ] ]
      [ "k7.0.10.0" ]
  in
  (* The job explained by matrix loses its comparison; a bound on the PR's own package agrees with it. *)
  check_strs "good" [ "other.1.0" ]
    (Hashtbl.fold (fun k _ acc -> k :: acc) notes.good []);
  check_true "other kept"
    (Context.PS.equal
       (Hashtbl.find notes.good "other.1.0")
       (Context.PS.singleton ("5.5", b)));
  check_int "via" 0 (Hashtbl.length notes.via);
  check_true "ids" (match ids with [ x ] -> x == matrix | _ -> false)

(* ------------------------------------------------------------------------ *)
(* ConfWithoutDepextTest: a conf package whose check fails on a platform it
   has no depext for (#31054: conf-glib-2 on msys2, its depexts cover Cygwin only). *)

let conf_opam =
  "opam-version: \"2.0\"\nbuild: [[\"pkg-config\" \"glib-2.0\"]]\ndepexts: [\n"
  ^ "  [\"libglib2.0-dev\"] {os-family = \"debian\"}\n"
  ^ "  [\"glib2.0\"] {os = \"win32\" & os-distribution = \"cygwinports\"}\n"
  ^ "  [\"glib2-devel\"] {os-distribution = \"fedora\" & os-version >= \"40\"}\n\
     ]\n\
     flags: conf\n"

let conf_log =
  "[ERROR] The compilation of conf-glib-2.1 failed at \"pkg-config glib-2.0\".\n"

let conf_rule ?(opam = conf_opam) ?(fname = "conf-glib-2") ?(log = conf_log)
    variant =
  let ctx =
    { Rules.pr_packages = []; full_text = log; job_variant = variant }
  in
  Option.map Suggestion.make
    (Rules.conf_without_depext fname (Some opam)
       (info ~action:"compiling" ())
       ctx (job variant) "here")

let test_conf_rule () =
  let r = Option.get (conf_rule "day10,msys2,x86_64,5.4,ocgio.1") in
  check_str "rule" "conf-no-depext"
    (Option.fold ~none:"" ~some:Rule.to_string r.rule);
  check_str "error" "conf-glib-2 failed at `pkg-config glib-2.0` on here"
    (Suggestion.str r.error);
  (* A change described in words (on the page, a fix rather than an
     explanation), kept next to the workarounds from the pattern of passes
     and failures. *)
  check_str "advice" "Add depexts to conf-glib-2" (Suggestion.str r.advice);
  check_true "keep" r.keep;
  check_str "note"
    "conf-glib-2 has no depext for here, so no system package was installed \
     for its check to find. Add the here package that provides it to the \
     depexts of conf-glib-2, or list \"msys2\" in its `x-ci-accept-failures` \
     if here has no such package."
    (Suggestion.str r.note);
  (* A depext for the platform: another cause. A filter the tool cannot
     evaluate there (the Fedora version): no conclusion. *)
  check_none "debian" (conf_rule "day10,debian-13,x86_64,5.4,ocgio.1");
  check_none "fedora" (conf_rule "day10,fedora-43,x86_64,5.4,ocgio.1");
  (* Not a conf package, another package's failure, no depexts field: nothing. *)
  check_none "not conf"
    (conf_rule ~fname:"glib" "day10,msys2,x86_64,5.4,ocgio.1");
  check_none "other"
    (conf_rule ~log:"[ERROR] The compilation of x.1 failed"
       "day10,msys2,x86_64,5.4,ocgio.1");
  check_none "no field"
    (conf_rule ~opam:"build: [[\"true\"]]\n" "day10,msys2,x86_64,5.4,ocgio.1");
  (* Empty depexts (conf-magick-core6, #27798): every platform. *)
  let r =
    Option.get
      (conf_rule ~opam:"depexts: []\nflags: conf\n"
         "day10,debian-13,x86_64,5.4,ocgio.1")
  in
  check_true (Suggestion.str r.note)
    (String.starts_with ~prefix:"conf-glib-2 has no depexts at all"
       (Suggestion.str r.note))

(* opam's os-family is the first word of ID_LIKE: "suse" on openSUSE Leap
   (the CI's opensuse-16.0), "opensuse" on Tumbleweed; os-distribution is ID,
   "opensuse-leap" or "opensuse-tumbleweed". A depext for os-family "suse"
   only (plcairo's "plplot-devel", PR 30297) misses Tumbleweed. *)
let test_opensuse_family () =
  let leap = "distributions,opensuse-16.0-ocaml-5.5,a.1"
  and tw = "day10,opensuse-tumbleweed,x86_64,5.5,a.1" in
  let ev f v = Filter.eval_filter (Some f) (Filter.platform_vars (job v)) in
  let pb = Alcotest.(option bool) in
  Alcotest.check pb "suse on Leap" (Some true) (ev {|os-family = "suse"|} leap);
  Alcotest.check pb "suse on Tumbleweed" (Some false)
    (ev {|os-family = "suse"|} tw);
  Alcotest.check pb "opensuse on Tumbleweed" (Some true)
    (ev {|os-family = "opensuse"|} tw);
  Alcotest.check pb "leap" (Some true)
    (ev {|os-distribution = "opensuse-leap"|} leap);
  Alcotest.check pb "either" (Some true)
    (ev {|os-family = "suse" | os-family = "opensuse"|} tw);
  let opam =
    Util.replace conf_opam {|["libglib2.0-dev"] {os-family = "debian"}|}
      {|["glib2-devel"] {os-family = "suse"}|}
  in
  check_true "no depext on Tumbleweed" (conf_rule ~opam tw <> None);
  check_none "a depext on Leap" (conf_rule ~opam leap)

let test_platforms_of_the_group () =
  (* The jobs with the same error on other platforms (#27642): those without
     a depext are named, an entry whose filter misses its platform (there,
     os-family "ubuntu"; here os "alpine") is pointed out, a platform with a
     depext is said apart. *)
  let opam =
    Util.replace conf_opam {|["glib2.0"]|}
      "[\"glib-dev\"] {os = \"alpine\"}\n  [\"glib2.0\"]"
  in
  let vs =
    [
      "day10,msys2,x86_64,5.4,a.1";
      "day10,alpine-3.24,x86_64,5.4,a.1";
      "day10,debian-13,x86_64,5.4,a.1";
    ]
  in
  let r = Option.get (conf_rule ~opam (List.hd vs)) in
  r.jobs <- Some vs;
  let tbl = Hashtbl.create 4 in
  List.iter (fun v -> Hashtbl.replace tbl v (job v)) vs;
  Rules.conf_depext_platforms r tbl;
  check_false "depexts" (r.depexts <> None);
  check_str "error"
    "conf-glib-2 failed at `pkg-config glib-2.0` on alpine-3.24, msys2"
    (Suggestion.str r.error);
  check_str "advice" "Add depexts for alpine-3.24, msys2 to conf-glib-2"
    (Suggestion.str r.advice);
  check_str "note"
    "conf-glib-2 has no depext for alpine-3.24, msys2, so no system package \
     was installed there for its check to find. Add the packages that provide \
     it to the depexts of conf-glib-2, or add \"alpine-3.24\" \"msys2\" to its \
     `x-ci-accept-failures` if these platforms have no such package. An entry \
     names alpine, but its filter does not hold there: on alpine, opam's \
     os-distribution is \"alpine\" and its os-family \"alpine\". On debian-13 \
     a depext applies (or the tool cannot tell): the check fails there for \
     another reason."
    (Suggestion.str r.note)

let test_depext_entries () =
  let pp = Alcotest.(option (list (pair (list string) string))) in
  Alcotest.check pp "two"
    (Some [ ([ "libgmp-dev" ], {|os-family = "debian"|}); ([ "gmp" ], "") ])
    (Rules.depext_entries
       (Some "depexts: [ \"libgmp-dev\" {os-family = \"debian\"} \"gmp\" ]\n"));
  Alcotest.check pp "empty" (Some [])
    (Rules.depext_entries (Some "depexts: []\n"));
  Alcotest.check pp "none" None (Rules.depext_entries (Some "build: []\n"))

(* ------------------------------------------------------------------------ *)
(* RulesTest: rules on synthetic job lists (no fixture). *)

(* The page's functions on PRs and page data written as JSON: the tests'
   literals go through Page_data's readers. *)
module R = struct
  include Scripts.Render_page
  module P = Scripts.Page_data

  let entry = P.Entry.of_json
  let verdict j = verdict (entry j)
  let seems_ready j = seems_ready (entry j)
  let ci_running j = ci_running (entry j)
  let render_pr ?year ?max_failures j = render_pr ?year ?max_failures (entry j)
end

let test_jobs_not_run_page () =
  (* The render_page side of test_jobs_not_run (its rule side is in test_context). *)
  let aborted =
    {
      (job "compilers,5.3,foo.1") with
      status = Aborted;
      category = Status Aborted;
      reason = None;
    }
  in
  let r =
    one
      (Context.rerun_rules
         (data [ aborted; job ~reason:"x" "compilers,5.3,foo.1,revdeps,X.1" ])
         [])
  in
  let pre =
    `Assoc
      [
        ("rule", s "health-check-preexisting");
        ("where_kind", s "other-package");
        ("class", s "elsewhere");
      ]
  in
  let pr sugg =
    `Assoc
      [
        ("number", `Int 5);
        ("ci_state", s "FAILURE");
        ("ci_unfinished", `Int 0);
        ("result", `Assoc [ ("suggestions", `List sugg) ]);
      ]
  in
  check_str "ready" "ready" (R.verdict (pr [ pre ]));
  check_false "seems ready" (R.seems_ready (pr [ pre; Suggestion.to_json r ]));
  check_str "rerun" "rerun" (R.verdict (pr [ pre; Suggestion.to_json r ]));
  (* Aborted on riscv64 or s390x, where CI aborts jobs routinely: listed, but does not hold the PR back. *)
  List.iter
    (fun v ->
      let routine =
        {
          (job v) with
          status = Aborted;
          category = Status Aborted;
          reason = None;
        }
      in
      let r = one (Context.rerun_rules (data [ routine ]) []) in
      check_true v (r.Suggestion.experimental = Some true);
      check_in "routinely" (Suggestion.str r.note);
      check_str v "ready" (R.verdict (pr [ pre; Suggestion.to_json r ])))
    [
      "extras,riscv64-ocaml-4.14,foo.1";
      "day10,day10-riscv64-ocaml-5.5,foo.1,revdeps,X.1";
      "extras,s390x-ocaml-5.4,foo.1";
    ]

let test_preexisting_page () =
  check_false "unverified"
    (R.not_this_pr
       (d [ ("preexisting", s "x"); ("preexisting_unverified", `Bool true) ]));
  check_true "verified" (R.not_this_pr (d [ ("preexisting", s "x") ]))

let test_unanalysed_order () =
  (* The PR's own failures are analysed before those of its reverse
     dependencies, experimental platforms last; classic builder first. *)
  let jobs =
    [
      job ~reason:"Error in r" "compilers,5.4,a.1,revdeps,r.1";
      job ~reason:"Error on freebsd" ~experimental:true
        "freebsd,freebsd-15.1-ocaml-5.5-amd64,a.1";
      job ~reason:"Error in a" "day10,debian-13,x86_64,5.4,a.1";
      job ~reason:"Error in a" "compilers,5.4,a.1";
      job ~category:"passed" "compilers,5.4,a.1,revdeps,s.1";
    ]
  in
  Alcotest.(check (list (list string)))
    "groups"
    [
      [ "compilers,5.4,a.1"; "day10,debian-13,x86_64,5.4,a.1" ];
      [ "compilers,5.4,a.1,revdeps,r.1" ];
      [ "freebsd,freebsd-15.1-ocaml-5.5-amd64,a.1" ];
    ]
    (List.map
       (List.map (fun j -> j.variant))
       (Suggest.distinct_failures jobs None))

let matrix_rules ?(earlier = []) jobs =
  List.map
    (fun r -> Option.fold ~none:"" ~some:Rule.to_string r.rule)
    (Matrix.matrix_rules jobs earlier None [ "x.1" ])

let test_builder_specific_same_arch () =
  let rules js = matrix_rules js in
  let classic = job ~category:"passed" "compilers,5.3,x.1" in
  (* debian-13, x86_64 *)
  check_true "same"
    (List.mem "builder-specific"
       (rules [ job "day10,debian-13,x86_64,5.3,x.1"; classic ]));
  (* day10 fails on arm64 only: the classic builder passing on x86_64 says nothing about the builder. *)
  check_false "arm64"
    (List.mem "builder-specific"
       (rules
          [
            job "day10,debian-13,arm64,5.3,x.1";
            classic;
            job ~category:"passed" "day10,debian-13,x86_64,5.3,x.1";
          ]));
  check_false "arm64 alone"
    (List.mem "builder-specific"
       (rules [ job "day10,debian-13,arm64,5.3,x.1"; classic ]));
  (* FreeBSD's classic jobs say amd64. *)
  check_true "freebsd"
    (List.mem "builder-specific"
       (rules
          [
            job "day10,freebsd-15.1,x86_64,5.5,x.1";
            job ~category:"passed" "freebsd,freebsd-15.1-ocaml-5.5-amd64,x.1";
          ]));
  (* A classic pass on the same arch, but day10 passes on another arch: the arch matters. *)
  check_false "macos"
    (List.mem "builder-specific"
       (rules
          [
            job "day10,macos-homebrew,arm64,5.5,x.1";
            job ~category:"passed" "macos,macos-homebrew-ocaml-5.5-arm64,x.1";
            job ~category:"passed" "day10,macos-homebrew,x86_64,5.5,x.1";
          ]))

let test_builder_specific_twin_age () =
  (* The main builder's pass vouches for a day10 failure only from the same
     master commit or push (high), or within a few days (medium); not from a run
     months older (PR 26907: day10 on 2026-10-13, main builder on
     2026-02-10), nor from a run the logs do not date. *)
  let fail = job "day10,debian-13,x86_64,5.3,x.1"
  and classic = job ~category:"passed" "compilers,5.3,x.1" in
  let conf day10 main =
    let run j : Matrix.run = if j == fail then day10 else main in
    Matrix.twin_confidence (Some run) fail classic
  in
  let at ?base day : Matrix.run = { day = Some day; base } in
  Alcotest.(check (list (option string)))
    "tiers"
    [
      Some "high";
      Some "high";
      Some "high";
      Some "medium";
      None;
      None;
      Some "high";
    ]
    [
      conf (at ~base:"b" 20739) (at ~base:"b" 20494);
      conf (at ~base:"b" 20739) (at ~base:"b" 20739);
      conf (at ~base:"b" 20739) (at ~base:"c" 20738);
      conf (at ~base:"b" 20739) (at ~base:"c" 20736);
      conf (at ~base:"b" 20739) (at ~base:"c" 20494);
      conf { day = None; base = None } (at 20739);
      Matrix.twin_confidence None fail classic;
    ];
  (* In the pattern: a stale pass leaves the failure to the log. *)
  let rules run =
    List.map
      (fun r -> Option.fold ~none:"" ~some:Rule.to_string r.rule)
      (Matrix.matrix_rules ~run [ fail; classic ] [] None [ "x.1" ])
  in
  check_true "fresh"
    (List.mem "builder-specific"
       (rules (fun j -> if j == fail then at 20739 else at 20737)));
  check_false "stale"
    (List.mem "builder-specific"
       (rules (fun j -> if j == fail then at 20739 else at 20494)));
  (* Suggest analyses these beyond its limit: nothing else explains them. *)
  let stale run among =
    List.map
      (fun j -> j.variant)
      (Matrix.stale_twins ~run ~among [ fail; classic ] None)
  in
  let old_run j = if j == fail then at 20739 else at 20494 in
  Alcotest.(check (list string))
    "stale twins"
    [ "day10,debian-13,x86_64,5.3,x.1" ]
    (stale old_run [ fail ]);
  Alcotest.(check (list string))
    "fresh twin" []
    (stale (fun j -> if j == fail then at 20739 else at 20738) [ fail ]);
  Alcotest.(check (list string)) "not among" [] (stale old_run [])

let test_windows_pattern () =
  (* "Fails only on Windows" ([available: os != "win32"]) only when no Windows
     job passes: ocaml-compiler.5.3 (#29582) fails on msys2 (no diff) and
     passes on cygwin; the pattern is then msys2's. *)
  let errors js =
    List.map
      (fun r -> Suggestion.str r.error)
      (Matrix.matrix_rules js [] None [])
  in
  let linux = job ~category:"passed" "day10,debian-13,x86_64,5.3,x.1" in
  let msys2 = job "day10,msys2,x86_64,5.3,x.1" in
  Alcotest.(check (list string))
    "windows"
    [ "Fails only on Windows (msys2): the compilation of x failed" ]
    (errors [ msys2; linux ]);
  Alcotest.(check (list string))
    "cygwin passes"
    [ "Fails only on distributions msys2: the compilation of x failed" ]
    (errors
       [ msys2; linux; job ~category:"passed" "day10,cygwin,x86_64,5.3,x.1" ]);
  (* The cygwin depext of a missing tool, in the failing dependency: only for
     a failure on Windows only; none for pkg-config, which conf-pkg-config
     provides as `pkgconf` there (#31054: conf-glib-2.1). *)
  let news tool distros =
    let dep d = job ~category:"dependency" ~reason:"conf-y.1 failed:" d in
    let earlier =
      d
        [
          ("failing", s "conf-y.1");
          ("rule", s "missing-tool");
          ("error", s (Printf.sprintf {|"%s": command not found|} tool));
        ]
    in
    List.map
      (fun r ->
        (Suggestion.str_field r.new_, Option.value ~default:"" r.advice))
      (Matrix.matrix_rules
         (linux
         :: List.map
              (fun d -> dep (Printf.sprintf "day10,%s,x86_64,5.3,x.1" d))
              distros)
         [ earlier ] None [])
  in
  Alcotest.(check (list string))
    "gawk on cygwin"
    [ {|depexts: ["gawk"] {os-distribution = "cygwin"}   (in conf-y.1)|} ]
    (List.map fst (news "awk" [ "cygwin" ]));
  Alcotest.(check (list string))
    "not with fedora-45" [ "" ]
    (List.map fst (news "awk" [ "cygwin"; "fedora-45" ]));
  check_true "pkg-config"
    (match news "pkg-config" [ "cygwin"; "msys2" ] with
    | [ ("", a) ] -> Util.contains a "run `pkgconf` on Windows"
    | _ -> false)

let test_experimental_only () =
  (* A failure only on experimental platforms (macOS, FreeBSD), the job
     passing elsewhere: it does not block the merge (#30751: randoml on
     macOS arm64). Not when the same failure (key) fails on a
     non-experimental platform too; an alternative only after a log's
     diagnosis that is a property of the platform. *)
  let linux = job ~category:"passed" "day10,debian-13,x86_64,5.3,x.1" in
  let macos =
    job ~experimental:true "macos,macos-homebrew-ocaml-5.3-arm64,x.1"
  in
  let run ?(earlier = []) js =
    List.map
      (fun r ->
        ( Option.fold ~none:"" ~some:Rule.to_string r.rule,
          Suggestion.str r.error,
          Suggestion.str r.alternative,
          r.experimental ))
      (Matrix.matrix_rules js earlier None [])
  in
  Alcotest.(check (list (pair string string)))
    "macOS only"
    [
      ( "experimental-only",
        "Fails only on macOS (macos-homebrew), an experimental platform" );
    ]
    (List.map (fun (r, e, _, _) -> (r, e)) (run [ macos; linux ]));
  check_true "experimental"
    (List.for_all (fun (_, _, _, x) -> x = Some true) (run [ macos; linux ]));
  (* the same failure on Linux too: the other patterns' (none here) *)
  Alcotest.(check int)
    "also on alpine" 0
    (List.length
       (List.filter
          (fun (r, _, _, _) -> r = "experimental-only")
          (run [ macos; linux; job "day10,alpine-3.24,x86_64,5.3,x.1" ])));
  (* no non-experimental pass: nothing says the job works elsewhere *)
  Alcotest.(check int) "no pass" 0 (List.length (run [ macos ]));
  (* a missing tool on macOS: x-ci-accept-failures as the alternative *)
  let earlier =
    d
      [
        ("failing", s "x.1");
        ("rule", s "missing-tool");
        ("error", s {|"gawk": command not found|});
      ]
  in
  earlier.key <- Some (Ci.failure_key macos);
  Alcotest.(check (list string))
    "alternative"
    [
      {|x-ci-accept-failures: ["macos-homebrew"]   (in x, if it cannot support macOS)|};
    ]
    (List.map
       (fun (_, _, a, _) -> a)
       (run ~earlier:[ earlier ] [ macos; linux ]))

let test_experimental_only_cause () =
  (* Where the fix of a failure only on experimental platforms goes follows
     its cause: an error in the code (#30751 randoml: rustc's
     "error[E0308]: mismatched types" in vendored ocaml-interop) is
     upstream's, medium confidence; a conf package's check failing (#31316:
     conf-postgresql.2 on macOS) is a property of the platform, nothing to
     fix, with x-ci-accept-failures in the conf package; no log read: nothing
     to fix, medium. A log read without an error line or a rule's
     diagnosis gives no such entry: the job's own entry stays (for the
     LLM). Five packages or more: one entry, whose place is the most
     conservative of theirs. *)
  let linux v =
    job ~category:"passed" (Printf.sprintf "day10,debian-13,x86_64,5.3,%s" v)
  in
  let macos ?(category = "build") ?(reason = "The compilation of x failed") v =
    job ~experimental:true ~category ~reason
      (Printf.sprintf "macos,macos-homebrew-ocaml-5.3-arm64,%s" v)
  in
  let unexplained j line =
    let r = d [ ("failing", s "x.1"); ("note", s "Unrecognised error.") ] in
    r.unexplained <- true;
    r.key <- Some (Ci.failure_key j);
    r.key_line <- line;
    r
  in
  let place ?(earlier = []) js =
    List.map
      (fun r ->
        ( Where_kind.to_string (Where.where_kind r),
          Suggestion.str_field r.confidence,
          Suggestion.str r.alternative,
          Suggestion.str r.note ))
      (List.filter
         (fun r -> r.rule = Some Rule.Experimental_only)
         (Matrix.collapse_systemic (Matrix.matrix_rules js earlier None [])))
  in
  let m = macos "x.1" in
  (match
     place
       ~earlier:[ unexplained m (Some "error[E0308]: mismatched types") ]
       [ m; linux "x.1" ]
   with
  | [ (w, c, a, n) ] ->
      check_str "code: where" "upstream" w;
      check_str "code: confidence" "medium" c;
      check_str "code: no alternative" "" a;
      check_true "code: the line"
        (Util.contains n "`error[E0308]: mismatched types`")
  | l -> Alcotest.failf "code: %d entries" (List.length l));
  (match place [ m; linux "x.1" ] with
  | [ (w, c, _, n) ] ->
      check_str "unread: where" "none" w;
      check_str "unread: confidence" "medium" c;
      check_true "unread: note"
        (Util.contains n "No log of these jobs was read")
  | l -> Alcotest.failf "unread: %d entries" (List.length l));
  Alcotest.(check int)
    "read, no error line" 0
    (List.length
       (place
          ~earlier:[ unexplained m (Some "make: *** [all] Stopped") ]
          [ m; linux "x.1" ]));
  let dep =
    macos ~category:"dependency"
      ~reason:"conf-postgresql.2 conf-mariadb.2 failed:" "x.1,tests"
  in
  (match place [ dep; linux "x.1,tests" ] with
  | [ (w, c, a, _) ] ->
      check_str "conf: where" "none" w;
      check_str "conf: confidence" "high" c;
      check_str "conf: alternative"
        {|x-ci-accept-failures: ["macos-homebrew"]   (in conf-postgresql, if it cannot support macOS)|}
        a
  | l -> Alcotest.failf "conf: %d entries" (List.length l));
  (* five packages, one of them with an error in its code *)
  let pkgs = List.init 5 (fun i -> Printf.sprintf "p%d.1" i) in
  let js = List.concat_map (fun p -> [ macos p; linux p ]) pkgs in
  let code = List.hd js in
  match
    place
      ~earlier:[ unexplained code (Some "fatal error: x.h: No such file") ]
      js
  with
  | [ (w, c, _, n) ] ->
      check_str "collapsed: where" "upstream" w;
      check_str "collapsed: confidence" "medium" c;
      check_true "collapsed: note" (Util.contains n "1 of the 5 packages")
  | l -> Alcotest.failf "collapsed: %d entries" (List.length l)

let test_unknown_outcome () =
  (* A job the CI failed is neither a pass nor a failure: "fails only on
     Windows" needs a real pass on its platform (#28291: fedora-45's image
     could not start a container). *)
  let rules ?unknown js =
    List.map
      (fun r -> Option.fold ~none:"" ~some:Rule.to_string r.rule)
      (Matrix.matrix_rules ?unknown js [] None [])
  in
  let jobs =
    [
      job "day10,cygwin,x86_64,5.5,x.1";
      job ~category:"passed" "day10,debian-13,x86_64,5.5,x.1";
    ]
  in
  let fedora = job "day10,fedora-45,x86_64,5.5,x.1" in
  check_strs "no unknown" [ "platform-matrix" ] (rules jobs);
  check_strs "unknown platform" [] (rules ~unknown:[ fedora ] jobs);
  check_strs "a real pass there" [ "platform-matrix" ]
    (rules ~unknown:[ fedora ]
       (job ~category:"passed" "day10,fedora-45,x86_64,4.14,x.1" :: jobs));
  check_strs "on Windows" [ "platform-matrix" ]
    (rules ~unknown:[ job "day10,msys2,x86_64,5.5,x.1" ] jobs)

let test_unread_on_a_broken_image () =
  (* #29582: day10's fedora-45 image has no /usr/bin/env, and the log of one
     job there says so; ocaml-option-llvm.1's own failure on fedora-45 is
     beyond --max. Its cygwin failure is then no pattern of "cygwin,
     fedora-45": fedora-45's outcome is not known. *)
  let y = job ~category:"dependency" ~reason:"compiler-cloning.enabled failed:"
  and fedora_x =
    job ~category:"dependency" ~reason:"base-threads.base failed:"
      "day10,fedora-45,x86_64,5.5,x.1"
  in
  let jobs =
    [
      y "day10,fedora-45,x86_64,5.5,y.1";
      y "day10,fedora-45,arm64,5.5,y.1";
      job ~category:"passed" "day10,debian-13,x86_64,5.5,y.1";
      job "day10,cygwin,x86_64,5.5,x.1";
      job ~category:"passed" "day10,debian-13,x86_64,5.5,x.1";
      fedora_x;
    ]
  in
  let image =
    {|runc run failed: unable to start container process: exec: "/usr/bin/env": stat /usr/bin/env: no such file or directory|}
  in
  let rules ?(max = 1) jobs =
    let fetch = !Suggest.fetch_hook in
    (Suggest.fetch_hook :=
       fun url _ -> if Util.contains url "y.1" then image else "failed");
    Net.offline_ref := true;
    Fun.protect
      ~finally:(fun () -> Suggest.fetch_hook := fetch)
      (fun () ->
        List.map
          (fun r ->
            ( Option.fold ~none:"" ~some:Rule.to_string r.rule,
              Suggestion.str r.error ))
          (Suggest.suggest (String.make 40 '0') (data jobs)
             (Sources.create (tmpdir ()))
             (tmpdir ()) max None None))
  in
  let patterns ?max jobs =
    List.filter_map
      (fun (rule, error) ->
        if rule = "platform-matrix" then Some error else None)
      (rules ?max jobs)
  in
  check_true "the image"
    (List.mem
       ( "ci-failure",
         "The build container did not start: its image has no /usr/bin/env" )
       (rules jobs));
  check_strs "fedora-45 unknown" [] (patterns jobs);
  (* Not on the main builder: its fedora-45 image is another one (its job
     sorts first: two logs read, y's and this one's). *)
  check_strs "main builder"
    [
      "Fails only on distributions cygwin, fedora-45: the compilation of x \
       failed";
    ]
    (patterns ~max:2
       (List.map
          (fun j ->
            if j == fedora_x then
              job ~category:"dependency" ~reason:"base-threads.base failed:"
                "distributions,fedora-45-ocaml-5.5,x.1"
            else j)
          jobs))

let test_compiler_option () =
  (* A job that fails only on a compiler-option variant and passes on the
     plain compiler of that version (#31312: nx on no-flat-float-array;
     #31318: hegel on flambda). *)
  let t v = job ~category:"build-or-test" (v ^ ",x.1,tests") in
  let plain = job ~category:"passed" "compilers,5.5,x.1,tests" in
  let rules ?(earlier = []) js =
    List.filter_map
      (fun r ->
        if r.rule = Some Rule.Compiler_option then
          Some
            ( Suggestion.str_field r.new_,
              Option.value ~default:"" r.alternative,
              Where_kind.to_string (Where.where_kind r),
              List.length r.keys )
        else None)
      (Matrix.matrix_rules js earlier None [ "x.1" ])
  in
  let no_flat = t "extras,ocaml-5.5-no-flat-float-array" in
  Alcotest.(check (list (pair string string)))
    "no-flat-float-array"
    [ ({|conflicts: ["ocaml-option-no-flat-float-array"]|}, "pr") ]
    (List.map (fun (n, _, w, _) -> (n, w)) (rules [ plain; no_flat ]));
  check_int "its failure" 1
    (List.length
       (List.filter (fun (_, _, _, k) -> k = 1) (rules [ plain; no_flat ])));
  (* The same CI reason on riscv64: the failure is not the option's alone. *)
  check_int "shared failure" 0
    (match rules [ plain; no_flat; t "extras,riscv64-ocaml-5.5" ] with
    | [ (_, _, _, k) ] -> k
    | _ -> -1);
  check_int "no plain pass" 0
    (List.length
       (rules [ no_flat; job ~category:"passed" "extras,arm64-ocaml-5.5,x.1" ]));
  Alcotest.(check (list (pair string string)))
    "flambda: upstream"
    [
      ( {|conflicts: ["ocaml-option-flambda"]   (only hides it: prefer the upstream fix)|},
        "upstream" );
    ]
    (List.map
       (fun (_, a, w, _) -> (a, w))
       (rules [ plain; t "extras,ocaml-5.5-flambda" ]));
  (* A rule explained the log (alcobar: flambda's stack overflow). *)
  let flambda = t "extras,ocaml-5.5-flambda" in
  let earlier = d [ ("failing", s "x.1"); ("rule", s "test-platform") ] in
  earlier.key <- Some (failure_key flambda);
  check_int "explained by the log" 0
    (List.length (rules ~earlier:[ earlier ] [ plain; flambda ]));
  (* Five packages on one option stay five suggestions (each its place). *)
  let five =
    List.concat_map
      (fun i ->
        let t p v = job ~category:p (Printf.sprintf "%s,p%d.1,tests" v i) in
        [ t "passed" "compilers,5.5"; t "build-or-test" "extras,ocaml-5.5-afl" ])
      [ 1; 2; 3; 4; 5 ]
  in
  check_strs "not systemic"
    (List.init 5 (fun _ -> "compiler-option"))
    (List.map
       (fun r -> Option.fold ~none:"" ~some:Rule.to_string r.rule)
       (Matrix.collapse_systemic (Matrix.matrix_rules five [] None [])))

let test_pattern_after_a_change_from_the_log () =
  (* liquidsoap 2.4.6 (#30906): its tests fail on OCaml 5 with [Library
     "bytes" not found]; the log gives base-bytes, so no workaround from the
     pattern of passes and failures. *)
  let fails =
    List.map
      (fun v ->
        {
          (job ~reason:"failed: Library \"bytes\" not found."
             ("compilers," ^ v ^ ",x.1,tests"))
          with
          kind = Tests;
        })
      [ "5.2"; "5.5" ]
  in
  let jobs =
    fails
    @ [
        {
          (job ~category:"passed" "compilers,4.14,x.1,tests") with
          kind = Tests;
        };
      ]
  in
  check_strs "pattern" [ "compiler-matrix" ] (matrix_rules jobs);
  let from_log () =
    let r =
      d
        [
          ("failing", s "x.1");
          ("rule", s "missing-library");
          ("new", s {|"base-bytes" {with-test}|});
        ]
    in
    r.key <- Some (failure_key (List.hd fails));
    r
  in
  check_strs "from log" [] (matrix_rules ~earlier:[ from_log () ] jobs);
  (* A change in words (conf-no-depext): the same. *)
  let words = from_log () in
  Suggestion.update words
    [
      `Rule Rule.Conf_no_depext; `New None; `Advice "Add depexts for msys2 to x";
    ];
  check_strs "words" [] (matrix_rules ~earlier:[ words ] jobs);
  (* A note without a change: the pattern still gives its suggestion. *)
  let note = from_log () in
  note.new_ <- Null;
  check_strs "note" [ "compiler-matrix" ] (matrix_rules ~earlier:[ note ] jobs)

(* ------------------------------------------------------------------------ *)
(* Runs of [suggest] on a fixture, in this process (the Python ran the tool
   with some function replaced). *)

let capture f =
  let out = Filename.temp_file "stdout" ""
  and err = Filename.temp_file "stderr" "" in
  flush stdout;
  flush stderr;
  let so = Unix.dup Unix.stdout and se = Unix.dup Unix.stderr in
  let fo = Unix.openfile out [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  and fe = Unix.openfile err [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Unix.dup2 fo Unix.stdout;
  Unix.dup2 fe Unix.stderr;
  Unix.close fo;
  Unix.close fe;
  Fun.protect f ~finally:(fun () ->
      flush stdout;
      flush stderr;
      Unix.dup2 so Unix.stdout;
      Unix.dup2 se Unix.stderr;
      Unix.close so;
      Unix.close se);
  let o = In_channel.with_open_bin out In_channel.input_all
  and e = In_channel.with_open_bin err In_channel.input_all in
  Sys.remove out;
  Sys.remove err;
  (o, e)

let cases =
  [
    ( "lwt-revdeps",
      ( "1e2d45eb8b297fea701407d41e2d2af656bd5d9a",
        6,
        Some
          {|^compilers,(4\.14,lwt.5.10.2,revdeps,kinetic-client|5\.5,lwt.5.10.2,revdeps,passage\.0\.1\.7)|}
      ) );
    ("colibri2-stdlib", ("e162f8cea2ee791d6b3c418bb56611b92a93e286", 1, None));
    ("websocket-bound", ("f290556e5d03adbda8dc847188358e06abe16436", 6, None));
  ]

(** [suggest --json] on a fixture, offline: (its JSON output, stderr). *)
let run_case ?max ?(llm = false) ?unfinished ?(before = fun () -> ()) case =
  let sha, max0, match_ = List.assoc case cases in
  let dir = Filename.concat fixtures case in
  Net.net.kv <- [ ("http", 0); ("mcp", 0); ("git", 0); ("github-api", 0) ];
  Net.offline_misses := [];
  Net.offline_ref := true;
  before ();
  let out, err =
    capture (fun () ->
        let _, data = Ci.load_commit sha dir in
        (match unfinished with Some n -> data.unfinished <- n | None -> ());
        Cli.suggest_command
          {
            Cli.command = Suggest;
            target = Some sha;
            pr = None;
            json = true;
            all = false;
            match_;
            category = [];
            max = Option.value ~default:max0 max;
            llm;
            llm_max = 5;
            llm_model = Llm_model.default;
            llm_dry_run = false;
            post = false;
            baseline = None;
            cache_dir = dir;
            refresh = false;
          }
          sha data)
  in
  ( (match Json.parse_opt out with
    | Some v -> v
    | None -> Alcotest.failf "no JSON output; stderr:\n%s" err),
    err )

let suggestions v = List.map Suggestion.of_json (Json.list "suggestions" v)

let test_unanalysed_count () =
  (* Failures beyond --max are counted in the JSON output. *)
  let v, err = run_case ~max:2 "websocket-bound" in
  check_in "offline cache misses: 0" err;
  check_true "unanalysed" (Json.to_int (Json.member "unanalysed" v) > 0)

(* Replaces the kinetic-client result by an unexplained failure. *)
let unexplained_kinetic () =
  let orig = !Suggest.suggest_for_job_hook in
  Suggest.suggest_for_job_hook :=
    fun src j text pr_packages sha ->
      let r = orig src j text pr_packages sha in
      if Util.contains j.variant "kinetic-client" then begin
        let r' =
          Suggestion.make
            [
              `Error "Unbound value frobnicate";
              `Note "Unrecognised error.";
              `Unexplained;
            ]
        in
        r'.variant <- r.variant;
        r'.url <- r.url;
        r'.kind <- r.kind;
        r'.experimental <- r.experimental;
        r'.failing <- r.failing;
        r'
      end
      else r

let test_unreadable_log () =
  (* The log of one job cannot be fetched (connection reset): the analysis
     says so ([incomplete]), and the triage page analyses the PR again. *)
  let tmp = tmpdir () in
  let v, _ =
    run_case "lwt-revdeps" ~before:(fun () ->
        Suggest.fetch_hook :=
          fun url cache_dir ->
            if not (Util.contains url "kinetic-client") then
              Net.fetch url cache_dir
            else begin
              let h = !Http.hook in
              (Http.hook :=
                 fun _ ->
                   raise (Http.Net_error "[Errno 104] Connection reset by peer"));
              Net.offline_ref := false;
              Fun.protect
                (fun () -> Net.fetch url tmp)
                ~finally:(fun () ->
                  Http.hook := h;
                  Net.offline_ref := true)
            end)
  in
  check_true "unrecognised"
    (List.exists
       (fun r ->
         Option.fold ~none:"" ~some:Rule.to_string r.rule = "unrecognised"
         && Util.contains (Json.dumps (get r "jobs")) "kinetic-client")
       (suggestions v));
  check_true "incomplete" (Json.bool "incomplete" v);
  check_json "errors"
    (`Assoc [ ("net-errors", `Int 1) ])
    (Json.member "errors" v)

let test_one_job_fails () =
  (* An exception in the rules for one job does not lose the other jobs. *)
  let v, _ =
    run_case "lwt-revdeps" ~before:(fun () ->
        let orig = !Suggest.suggest_for_job_hook in
        (Suggest.suggest_for_job_hook :=
           fun src j text pr sha ->
             if Util.contains j.variant "kinetic-client" then
               failwith ("boom " ^ String.make 1000 'x')
             else orig src j text pr sha);
        Net.net_set "mcp-errors" 2)
  in
  let sugg = suggestions v in
  let err =
    List.filter
      (fun r -> Option.fold ~none:"" ~some:Rule.to_string r.rule = "rule-error")
      sugg
  in
  check_int "one" 1 (List.length err);
  let e = List.hd err in
  check_true (Suggestion.str e.error)
    (String.starts_with ~prefix:"Failure: boom" (Suggestion.str e.error));
  check_true "short" (String.length (Suggestion.str e.error) < 400);
  check_strs "fields"
    [ "explanation"; "unknown"; "Analysis failed (tool error)" ]
    [ Suggestion.str e.class_; Suggestion.str e.fix_in; Suggestion.str e.title ];
  check_true "passage"
    (List.exists
       (fun r -> Suggestion.str_field r.Suggestion.failing = "passage.0.1.7")
       sugg);
  check_true "incomplete" (Json.bool "incomplete" v);
  check_json "errors"
    (`Assoc [ ("mcp-errors", `Int 2) ])
    (Json.member "errors" v)

let test_unbound_stdlib_value_without_tags () =
  let v, _ =
    run_case "colibri2-stdlib" ~before:(fun () ->
        Sources.tags_hook := fun _ _ -> [])
  in
  let sugg = suggestions v in
  (* Was an IndexError (no OCaml release): now the identifier is looked up in the libraries. *)
  check_false "rule-error"
    (List.exists
       (fun r ->
         Option.fold ~none:"" ~some:Rule.to_string r.rule = "rule-error")
       sugg);
  check_true "colibri2"
    (List.exists
       (fun r -> Suggestion.str_field r.Suggestion.failing = "colibri2.0.7")
       sugg)

let test_not_asked_while_ci_runs () =
  (* While CI runs, the prompt (the job's results on other platforms) changes
     as jobs finish: the LLM is asked once CI has finished, and the output
     says how many are left. *)
  let called = ref false in
  let before () =
    unexplained_kinetic ();
    Llm.llm_suggest_hook :=
      fun ?call:_ _ _ _ _ _ _ _ _ _ ->
        called := true;
        Llm.Failed { message = "fake"; called = true; later = true }
  in
  let v, _ = run_case ~llm:true ~unfinished:3 ~before "lwt-revdeps" in
  check_false "not called" !called;
  check_int "unanswered" 1 (Json.to_int (Json.member "llm_unanswered" v));
  let v, _ = run_case ~llm:true ~unfinished:0 ~before "lwt-revdeps" in
  check_true "called" !called;
  check_int "transient" 1 (Json.to_int (Json.member "llm_unanswered" v))

let test_flag_set_by_the_rules_and_not_output () =
  let j = job "compilers,5.3,x.1" in
  let text =
    "#=== ERROR while compiling x.1 ===#\n\
     # exit-code 2\n\
     ### output ###\n\
     # something odd happened\n"
  in
  (Sources.opam_file_hook := fun ?ref:_ _ _ _ -> None);
  let r =
    Rules.suggest_for_job (src0 ()) j text [ "x.1" ] (String.make 40 'a')
  in
  check_str "note" "Unrecognised error." (Suggestion.str r.note);
  check_true "flag" r.unexplained;
  originals ();
  (* The flag is not in the output. *)
  let v, _ = run_case ~before:unexplained_kinetic "lwt-revdeps" in
  let out = Json.dumps v in
  check_in "frobnicate" out;
  check_not_in "_unexplained" out

(* ------------------------------------------------------------------------ *)
(* McpFailureTest / McpRobustnessTest: ocaml.org's MCP server failing in
   various ways: counted as failures, not cached, not raised; after a few
   failures it is not called any more. *)

let test_failed_call_counted () =
  let tmp = tmpdir () in
  nothing_listens ();
  let src = Sources.create tmp in
  let before = errors () in
  check_json "none" `Null
    (Sources.mcp src "ocaml_package_versions" [ ("package", s "x") ]);
  check_int "counted" (before + 1) (errors ());
  check_strs "not cached" [] (Util.listdir tmp);
  check_true "unknown" (Sources.versions src "x" = None)
(* unknown, not "not a package" *)

let test_malformed_answers () =
  let tmp = tmpdir () in
  let answers =
    [
      http_json
        (`Assoc [ ("jsonrpc", s "2.0"); ("id", `Int 1); ("result", `Null) ]);
      (* "result": null *)
      http_json
        (`Assoc
           [
             ("jsonrpc", s "2.0");
             ("id", `Int 1);
             ("error", `Assoc [ ("code", `Int (-1)) ]);
           ]);
      Some { Http.status = 0; resp_headers = []; data = "HELLO" } (* not HTTP *);
      None (* connection closed *);
      http_json
        (`Assoc
           [
             ("jsonrpc", s "2.0");
             ("id", `Int 1);
             ( "result",
               `Assoc
                 [
                   ("isError", s "false");
                   ( "content",
                     `List [ `Assoc [ ("type", s "text"); ("text", s "[]") ] ]
                   );
                 ] );
           ]);
      (* isError not a boolean *)
    ]
  in
  List.iteri
    (fun i a ->
      Sources.mcp_failures := 0;
      ignore (serve (fun _ -> a));
      let src = Sources.create tmp in
      let before = errors () in
      check_json (string_of_int i) `Null
        (Sources.mcp src "ocaml_package_versions" [ ("package", s "x") ]);
      check_int (string_of_int i) (before + 1) (errors ());
      check_strs "not cached" [] (Util.listdir tmp))
    answers

let test_circuit_breaker () =
  let tmp = tmpdir () in
  nothing_listens ();
  let src = Sources.create tmp in
  let sent = Net.net_get "mcp" and before = errors () in
  for i = 0 to 5 do
    check_json "none" `Null
      (Sources.mcp src "ocaml_package_versions"
         [ ("package", s (Printf.sprintf "p%d" i)) ])
  done;
  check_int "then no more calls" Sources.mcp_max_failures
    (Net.net_get "mcp" - sent);
  check_int "each one counted" (before + 6) (errors ());
  check_true "timeout" (Sources.mcp_timeout <= 30.)

let test_versions_unknown_vs_no_answer () =
  let tmp = tmpdir () in
  ignore (serve (fun _ -> mcp_answer ~error:true (s "Package not found")));
  Alcotest.(check (option (list string)))
    "not a package" (Some [])
    (Sources.versions (Sources.create tmp) "not-a-package");
  nothing_listens ();
  let src = Sources.create tmp in
  Alcotest.(check (option (list string)))
    "no answer" None (Sources.versions src "x");
  ignore
    (serve (fun _ ->
         mcp_answer
           (`Assoc
              [
                ( "versions",
                  `List
                    [
                      `Assoc [ ("version", s "1.0") ];
                      `Assoc [ ("version", s "0.9") ];
                    ] );
              ])));
  Alcotest.(check (option (list string)))
    "versions"
    (Some [ "0.9"; "1.0" ])
    (Sources.versions src "y")

let test_cached_error_expires () =
  let tmp = tmpdir () in
  let reqs = serve (fun _ -> mcp_answer ~error:true (s "no docs yet")) in
  let ask () =
    Sources.mcp (Sources.create tmp) "ocaml_package_documentation"
      [ ("package", s "x"); ("version", s "1") ]
  in
  check_json "none" `Null (ask ());
  check_json "none" `Null (ask ());
  check_int "the error is cached..." 1 (List.length !reqs);
  age_all tmp 2.;
  ignore (ask ());
  check_int "...for a day" 2 (List.length !reqs)

let test_docs_not_built_expire () =
  (* Documentation not built (yet) is an answer, not an error, but it changes
     when ocaml.org builds the docs: kept a day; built docs are kept. *)
  let tmp = tmpdir () in
  let answers =
    [
      ("1", `Assoc [ ("documentation_status", s "unknown") ]);
      ( "2",
        `Assoc
          [ ("documentation_status", s "success"); ("libraries", `List []) ] );
    ]
  in
  let reqs =
    serve (fun body ->
        mcp_answer (List.assoc (Json.to_str (arg "version" body)) answers))
  in
  let ask v =
    Sources.mcp (Sources.create tmp) "ocaml_package_documentation"
      [ ("package", s "x"); ("version", s v) ]
  in
  List.iter
    (fun v ->
      check_json v (List.assoc v answers) (ask v);
      ignore (ask v))
    [ "1"; "2" ];
  check_int "cached" 2 (List.length !reqs);
  age_all tmp 2.;
  ignore (ask "1");
  ignore (ask "2");
  check_int "only the one without docs asked again" 3 (List.length !reqs)

let test_dune_select_module () =
  (* dune >= 3.22 reads the target of a select as a path (mopsa.1.2, #26907):
     in the PR, the bound; dune 3.21 does not, and a name without dots is a
     module name. *)
  let j = job "compilers,5.5,x.1" in
  let err =
    [
      {|File "analyzer/dune", lines 14-16, characters 2-204:|};
      {|14 |   (select a.b.elinainst.ml from|};
      {|15 |    (elina -> a.b.elinainst.real.ml)|};
      {|16 |    (-> a.b.elinainst.dummy.ml))|};
      {|Error: "a.b.elinainst" is an invalid|};
      {|module name.|};
    ]
  in
  let rule ?(err = err) dune =
    Option.map Suggestion.make
      (Rules.log_rules (src0 ()) err "mopsa"
         (Some "depends: [\n  \"dune\" {>= \"3.7\"}\n]\n")
         {
           (info ~installed:[ ("dune", dune) ] ()) with
           failing = Some ("mopsa", "1.2");
         }
         {
           Rules.pr_packages = [ "mopsa.1.2" ];
           full_text = "";
           job_variant = j.variant;
         }
         j)
  in
  (match rule "3.24.2" with
  | Some r ->
      Alcotest.(check (option string))
        "bound" (Some {|"dune" {>= "3.7" & < "3.22"}|}) (Suggestion.get r.new_);
      check_in "dune#13175" (Suggestion.str r.note)
  | None -> Alcotest.fail "no rule");
  check_true "dune 3.21"
    (match rule "3.21.0" with
    | Some r -> r.rule <> Some Rule.Library_api
    | None -> true);
  check_true "a module name"
    (match
       rule
         ~err:(List.map (fun l -> Util.replace l "a.b.elinainst" "a-b") err)
         "3.24.2"
     with
    | Some r -> r.rule <> Some Rule.Library_api
    | None -> true)

let test_rules_do_not_assert_on_no_answer () =
  let j = job "compilers,5.3,foo.1" in
  let ctx =
    { Rules.pr_packages = []; full_text = ""; job_variant = j.variant }
  in
  List.iter
    (fun (err, pkg) ->
      let rule vs =
        (Sources.versions_hook := fun ?ref:_ _ _ -> vs);
        Suggestion.make
          (Option.get
             (Rules.log_rules (src0 ()) err "foo"
                (Some "depends: [\n  \"ocaml\"\n]\n") (info ~ocaml:"5.3" ()) ctx
                j))
      in
      let r = rule None in
      check_in ~msg:(List.hd err) "could not check" (Suggestion.str r.note);
      check_not_in (Printf.sprintf "no %s package" pkg) (Suggestion.str r.note);
      check_not_in "is not an opam package" (Suggestion.str r.note);
      check_false "new" (Suggestion.nonempty_field r.Suggestion.new_);
      let r = rule (Some [ "1" ]) in
      check_in ("\"" ^ pkg ^ "\"") (Suggestion.str_field r.Suggestion.new_))
    [
      ([ "bash: frobnicate: command not found" ], "conf-frobnicate");
      ( [ "Package libfoo was not found in the pkg-config search path." ],
        "conf-libfoo" );
      ([ "Error: Library \"barlib\" not found." ], "barlib");
    ]

(* ------------------------------------------------------------------------ *)
(* ApiHistoryTest: Sources.api_history: ocaml_module_changes's answer mapped
   to each version's presence; its errors are answers, its failures are counted. *)

(** An ocaml_module_changes answer (identifier mode) for version -> status. *)
let history_answer ?(module_ = s "M") ?(partial = false) ?(extra = []) statuses
    : Json.t =
  `Assoc
    ([
       ("package", s "foo");
       ("module", s "M");
       ("resolved_module", module_);
       ("identifier", s "x");
       ( "resolved_identifier",
         if List.exists (fun (_, st) -> st = "present") statuses then s "x"
         else `Null );
       ("kind", s "value");
       ( "versions",
         `List
           (List.map
              (fun (v, st) ->
                `Assoc
                  ([ ("version", s v); ("status", s st) ]
                  @
                  if st = "present" then [ ("signature", s "val x : int") ]
                  else []))
              statuses) );
       ("versions_truncated", `Bool false);
       ("partial", `Bool partial);
       ("deadline_reached", `Bool false);
     ]
    @ extra)

let pres h v =
  match Sources.history_presence h v with
  | Sources.Yes -> "true"
  | No -> "false"
  | No_docs -> "no-docs"
  | Unknown -> "none"

let has_mod h v =
  match Sources.history_has_module h v with
  | Some true -> "true"
  | Some false -> "false"
  | None -> "none"

let hist_versions h =
  match h with
  | Some (h : Sources.history) -> h.versions
  | None -> Alcotest.fail "no history"

let pairs = Alcotest.(list (pair string string))

let test_mapping_and_cache () =
  let tmp = tmpdir () in
  let statuses =
    [
      ("1.0", "absent");
      ("1.1", "present");
      ("1.2", "present");
      ("1.3", "absent");
      ("1.4", "page-truncated");
      ("1.5", "no-docs");
      ("1.6", "docs-failed");
      ("1.7", "module-not-found");
    ]
  in
  let reqs =
    serve (fun _ ->
        mcp_answer
          (history_answer
             ~extra:
               [
                 ("introduced_in", s "1.1");
                 ("removed_in", s "1.3");
                 ("changed_in", strs [ "1.2" ]);
               ]
             statuses))
  in
  let before = errors () in
  let h =
    Sources.api_history (Sources.create tmp) "foo" [ "M" ] "value" "x"
      [ "1.3"; "1.0"; "1.7"; "1.2" ]
  in
  check_json "args"
    (`Assoc
       [
         ("name", s "ocaml_module_changes");
         ( "arguments",
           `Assoc
             [
               ("package", s "foo");
               ("module", s "M");
               ("identifier", s "x");
               ("kind", s "value");
               ("from_version", s "1.0");
               ("to_version", s "1.7");
               ("max_versions", `Int 60);
             ] );
       ])
    (params (List.hd !reqs));
  check_strs "presence"
    [ "false"; "true"; "true"; "false"; "none"; "no-docs"; "no-docs"; "false" ]
    (List.map (fun (v, _) -> pres h v) statuses);
  check_str "not checked" "none" (pres h "0.9");
  check_str "no history" "none" (pres None "1.0");
  check_strs "has module"
    [ "true"; "true"; "true"; "none"; "false"; "none" ]
    (List.map (has_mod h) [ "1.0"; "1.1"; "1.4"; "1.5"; "1.7"; "0.9" ]);
  check_false "partial" (Option.get h).partial;
  check_int "errors" before (errors ());
  (* Cached on disk, for a day (new releases, docs built since). *)
  check_true "cached"
    (Sources.api_history (Sources.create tmp) "foo" [ "M" ] "value" "x"
       [ "1.0"; "1.7" ]
    = h);
  check_int "one request" 1 (List.length !reqs);
  age_all tmp 2.;
  ignore
    (Sources.api_history (Sources.create tmp) "foo" [ "M" ] "value" "x"
       [ "1.0"; "1.7" ]);
  check_int "asked again" 2 (List.length !reqs)

let test_module_in_no_version () =
  (* No documented version has the module: the docs cannot tell, the sources may. *)
  let tmp = tmpdir () in
  let reqs =
    serve (fun _ ->
        mcp_answer
          (history_answer ~module_:`Null
             [ ("1.0", "module-not-found"); ("1.1", "no-docs") ]))
  in
  let h =
    Sources.api_history (Sources.create tmp) "foo" [ "Foo"; "Bar" ] "type" "t"
      [ "1.0"; "1.1" ]
  in
  check_json "module" (s "Foo.Bar") (arg "module" (List.hd !reqs));
  check_strs "presence"
    [ "no-docs"; "no-docs"; "false" ]
    [ pres h "1.0"; pres h "1.1"; has_mod h "1.0" ]

let test_ambiguous_module_is_no_answer () =
  (* "List" in ocaml-base-compiler's docs is compiler-libs' Parameter.List
     and Stdlib.List: the answer may be about the wrong one, so it is not
     used (the callers look version by version). *)
  let tmp = tmpdir () in
  ignore
    (serve (fun _ ->
         mcp_answer
           (history_answer ~module_:(s "Parameter.List")
              ~extra:[ ("ambiguous_with", strs [ "Stdlib.List" ]) ]
              [ ("1.0", "absent"); ("1.1", "present") ])));
  let before = errors () in
  check_true "none"
    (Sources.api_history (Sources.create tmp) "foo" [ "List" ] "value" "x"
       [ "1.0"; "1.1" ]
    = None);
  check_int "an answer, not a failure" before (errors ());
  (* Written in full, the module is the code's even when the name is ambiguous elsewhere. *)
  ignore
    (serve (fun _ ->
         mcp_answer
           (history_answer ~module_:(s "Stdlib.List")
              ~extra:[ ("ambiguous_with", strs [ "Parameter.List" ]) ]
              [ ("1.0", "absent"); ("1.1", "present") ])));
  let h =
    Sources.api_history (Sources.create tmp) "foo" [ "Stdlib"; "List" ] "value"
      "x" [ "1.0"; "1.1" ]
  in
  Alcotest.check pairs "versions"
    [ ("1.0", "absent"); ("1.1", "present") ]
    (hist_versions h)

let test_ambiguous_module_chosen_by_identifier () =
  (* Newer servers check the modules of that name for the identifier: the
     only one that has it, when all of them could be checked, is the code's. *)
  let tmp = tmpdir () in
  let answer extra =
    ignore
      (serve (fun _ ->
           mcp_answer
             (history_answer ~module_:(s "Stdlib.List")
                ~extra:
                  ([ ("ambiguous_with", strs [ "Parameter.List" ]) ] @ extra)
                [ ("1.0", "absent"); ("1.1", "present") ])))
  in
  answer [ ("resolved_by", s "identifier") ];
  let h =
    Sources.api_history (Sources.create tmp) "foo" [ "List" ] "value" "x"
      [ "1.0"; "1.1" ]
  in
  Alcotest.check pairs "versions"
    [ ("1.0", "absent"); ("1.1", "present") ]
    (hist_versions h);
  Alcotest.(check (option string))
    "module" (Some "Stdlib.List") (Option.get h).hmodule;
  (* Another module of that name could not be checked (its page unread, or
     past the first 5), or the tool chose by name or order: maybe not the code's. *)
  List.iter
    (fun extra ->
      remove_all tmp;
      answer extra;
      check_true
        (Json.dumps (`Assoc extra))
        (Sources.api_history (Sources.create tmp) "foo" [ "List" ] "value" "x"
           [ "1.0"; "1.1" ]
        = None))
    [
      [
        ("resolved_by", s "identifier"); ("unchecked", strs [ "Parameter.List" ]);
      ];
      [ ("resolved_by", s "stdlib") ];
      [ ("resolved_by", s "order") ];
    ]

let test_found_false () =
  (* Newer servers answer an item in no version checked with each version's
     status ("found": false) rather than an error: it is absent where documented. *)
  let tmp = tmpdir () in
  let none = [ ("1.0", "absent"); ("1.1", "no-docs"); ("2.0", "absent") ] in
  let answers =
    ref
      [
        ( "constructor",
          mcp_answer (history_answer ~extra:[ ("found", `Bool false) ] none) );
        ( "exception",
          mcp_answer
            (history_answer
               ~extra:[ ("found", `Bool true) ]
               [ ("1.0", "absent"); ("1.1", "no-docs"); ("2.0", "present") ]) );
      ]
  in
  let reqs =
    serve (fun body -> List.assoc (Json.to_str (arg "kind" body)) !answers)
  in
  let before = errors () in
  (* A constructor may be an exception's: asked as one too. *)
  let h =
    Sources.api_history (Sources.create tmp) "foo" [ "M" ] "constructor" "Foo"
      [ "1.0"; "2.0" ]
  in
  check_strs "kinds"
    [ "constructor"; "exception" ]
    (List.map (fun r -> Json.to_str (arg "kind" r)) !reqs);
  Alcotest.(check (option string))
    "2.0" (Some "present")
    (List.assoc_opt "2.0" (hist_versions h));
  (* Of no kind: the first answer's statuses; the versions without docs are left to the callers. *)
  answers :=
    [
      List.hd !answers;
      ( "exception",
        mcp_answer (history_answer ~extra:[ ("found", `Bool false) ] none) );
    ];
  remove_all tmp;
  let h =
    Sources.api_history (Sources.create tmp) "foo" [ "M" ] "constructor" "Foo"
      [ "1.0"; "2.0" ]
  in
  check_strs "presence"
    [ "false"; "no-docs"; "false" ]
    (List.map (fun (v, _) -> pres h v) none);
  check_int "requests" 4 (List.length !reqs);
  check_int "errors" before (errors ());
  (* Not found in an ambiguous module chosen by order: maybe in the code's. *)
  ignore
    (serve (fun _ ->
         mcp_answer
           (history_answer ~module_:(s "Parameter.List")
              ~extra:
                [
                  ("found", `Bool false);
                  ("ambiguous_with", strs [ "Stdlib.List" ]);
                  ("resolved_by", s "order");
                ]
              none)));
  check_true "ambiguous"
    (Sources.api_history (Sources.create tmp) "foo" [ "List" ] "value" "x"
       [ "1.0"; "2.0" ]
    = None)

let test_history_partial_is_a_failure () =
  (* docs-ci could not be read for a version: what was read is used, the rest
     is unknown, the call counts as failed and is not cached. *)
  let tmp = tmpdir () in
  let reqs =
    serve (fun _ ->
        mcp_answer
          (history_answer ~partial:true
             [
               ("1.0", "absent");
               ("1.1", "docs-unavailable");
               ("1.2", "skipped");
               ("1.3", "present");
             ]))
  in
  let before = errors () in
  let src = Sources.create tmp in
  let h = Sources.api_history src "foo" [ "M" ] "value" "x" [ "1.0"; "1.3" ] in
  check_strs "presence"
    [ "false"; "none"; "none"; "true" ]
    (List.map (pres h) [ "1.0"; "1.1"; "1.2"; "1.3" ]);
  check_true "partial" (Option.get h).partial;
  check_int "counted" (before + 1) (errors ());
  check_strs "not cached" [] (Util.listdir tmp);
  check_true "same"
    (Sources.api_history src "foo" [ "M" ] "value" "x" [ "1.0"; "1.3" ] = h);
  check_int "once per process" 1 (List.length !reqs);
  check_int "counted once" (before + 1) (errors ());
  ignore
    (Sources.api_history (Sources.create tmp) "foo" [ "M" ] "value" "x"
       [ "1.0"; "1.3" ]);
  check_int "asked again later" 2 (List.length !reqs);
  (* Marked partial only in the result's _meta: the same. *)
  ignore
    (serve (fun _ ->
         http_json
           (`Assoc
              [
                ("jsonrpc", s "2.0");
                ("id", `Int 1);
                ( "result",
                  `Assoc
                    [
                      ("isError", `Bool false);
                      ("_meta", `Assoc [ ("org.ocaml/partial", `Bool true) ]);
                      ( "content",
                        `List
                          [
                            `Assoc
                              [
                                ("type", s "text");
                                ( "text",
                                  s
                                    (Json.dumps
                                       (history_answer [ ("1.0", "error") ])) );
                              ];
                          ] );
                    ] );
              ])));
  check_true "some"
    (Sources.api_history (Sources.create tmp) "foo" [ "M" ] "value" "x"
       [ "1.0" ]
    <> None);
  check_int "counted" (before + 3) (errors ())

let test_errors_are_answers () =
  let tmp = tmpdir () in
  let not_found = {|constructor "Foo" not found in M in versions 1.0 to 2.0|} in
  let answers =
    ref
      [
        ("constructor", tool_error not_found);
        ( "exception",
          mcp_answer (history_answer [ ("1.0", "absent"); ("2.0", "present") ])
        );
      ]
  in
  let reqs =
    serve (fun body -> List.assoc (Json.to_str (arg "kind" body)) !answers)
  in
  let before = errors () in
  (* A constructor may be an exception's: asked as one when no constructor has that name. *)
  let h =
    Sources.api_history (Sources.create tmp) "foo" [ "M" ] "constructor" "Foo"
      [ "1.0"; "2.0" ]
  in
  check_strs "kinds"
    [ "constructor"; "exception" ]
    (List.map (fun r -> Json.to_str (arg "kind" r)) !reqs);
  Alcotest.check pairs "versions"
    [ ("1.0", "absent"); ("2.0", "present") ]
    (hist_versions h);
  (* In no documented version: no history (the callers look version by
     version, versions without docs included); the error is cached, not counted. *)
  answers :=
    [
      List.hd !answers;
      ( "exception",
        tool_error (Util.replace not_found "constructor" "exception") );
    ];
  remove_all tmp;
  check_true "none"
    (Sources.api_history (Sources.create tmp) "foo" [ "M" ] "constructor" "Foo"
       [ "1.0"; "2.0" ]
    = None);
  check_true "none"
    (Sources.api_history (Sources.create tmp) "foo" [ "M" ] "constructor" "Foo"
       [ "1.0"; "2.0" ]
    = None);
  check_int "requests" 4 (List.length !reqs);
  check_int "not counted" before (errors ());
  (* Another error (an unknown package): no history, not asked as another kind. *)
  let reqs = serve (fun _ -> tool_error {|unknown package: "foo"|}) in
  check_true "unknown"
    (Sources.api_history (Sources.create tmp) "foo" [ "M" ] "constructor" "Foo"
       [ "1.0" ]
    = None);
  check_int "one" 1 (List.length !reqs);
  check_int "not counted" before (errors ())

let test_history_failures () =
  let tmp = tmpdir () in
  let before = errors () in
  nothing_listens ();
  let src = Sources.create tmp in
  check_true "none"
    (Sources.api_history src "foo" [ "M" ] "value" "x" [ "1.0" ] = None);
  check_int "counted" (before + 1) (errors ());
  check_strs "not cached" [] (Util.listdir tmp);
  (* No module path (an unbound top-level module), no version: no call. *)
  check_true "no path"
    (Sources.api_history src "foo" [] "module" "M" [ "1.0" ] = None);
  check_true "no version"
    (Sources.api_history src "foo" [ "M" ] "value" "x" [] = None);
  check_int "no call" (before + 1) (errors ())

let unknown_tool name =
  serve (fun _ ->
      http_json
        (`Assoc
           [
             ("jsonrpc", s "2.0");
             ("id", `Int 1);
             ( "error",
               `Assoc
                 [
                   ("code", `Int (-32602));
                   ("message", s ("unknown tool: " ^ name));
                 ] );
           ]))

let test_history_tool_not_deployed () =
  (* A server without the tool: no history, not a failure, not asked again. *)
  let tmp = tmpdir () in
  let reqs = unknown_tool "ocaml_module_changes" in
  let before = errors () in
  let src = Sources.create tmp in
  check_true "x"
    (Sources.api_history src "foo" [ "M" ] "value" "x" [ "1.0" ] = None);
  check_true "y"
    (Sources.api_history src "foo" [ "M" ] "value" "y" [ "1.0" ] = None);
  check_int "one request" 1 (List.length !reqs);
  check_int "no error" before (errors ());
  check_int "no failure" 0 !Sources.mcp_failures;
  (* Other tools: still a failure. *)
  check_json "other" `Null
    (Sources.mcp src "ocaml_package_versions" [ ("package", s "foo") ]);
  check_int "counted" (before + 1) (errors ())

(* ------------------------------------------------------------------------ *)
(* Fake sources for the rules (the Python tests passed fake Src objects). *)

let calls : string list ref = ref []
let call x = calls := !calls @ [ x ]

let hist_record statuses module_ =
  Some { Sources.versions = statuses; hmodule = module_; partial = false }

(* ------------------------------------------------------------------------ *)
(* StdlibHistoryTest: the standard-library rule with ocaml.org's API history:
   ocaml-base-compiler up to 5.2, ocaml-compiler from 5.3, the .mli of a
   release it cannot tell. *)

let stdlib_log =
  "-> installed ocaml-base-compiler.5.0.0\n\
   #=== ERROR while compiling x.1 ===#\n\
   # exit-code 1\n\
   ### output ###\n"
  ^ "# File \"x.ml\", line 1, characters 0-7:\n\
     # Error: Unbound value List.is_empty\n"

let stdlib_suggest statuses =
  calls := [];
  (Sources.opam_file_hook :=
     fun ?ref:_ _ _ _ ->
       Some "opam-version: \"2.0\"\ndepends: [\n  \"ocaml\"\n]\n");
  (Sources.raw_hook :=
     fun ?max_age:_ _ url ->
       let rel =
         List.hd
           (String.split_on_char '/'
              (match Util.partition url "/ocaml/ocaml/" with _, _, r -> r))
       in
       call ("mli " ^ rel);
       if not (Util.contains url "stdlib/list.mli") then None
       else
         Some
           (if rel >= "5.1.0" then "val is_empty : 'a list -> bool\n"
            else "val length : 'a list -> int\n"));
  (Sources.tags_hook :=
     fun _ _ -> [ "4.14.0"; "5.0.0"; "5.1.0"; "5.2.0"; "5.3.0"; "5.4.0" ]);
  (Sources.versions_hook :=
     fun ?ref:_ _ name ->
       Some
         (match name with
         | "ocaml-base-compiler" -> [ "4.14.0"; "5.0.0"; "5.1.0"; "5.2.0" ]
         | "ocaml-compiler" -> [ "5.3.0"; "5.4.0" ]
         | _ -> []));
  (Sources.api_history_hook :=
     fun _ pkg mods kind ident versions ->
       call
         (String.concat " "
            [
              "history";
              pkg;
              String.concat "." mods;
              kind;
              ident;
              String.concat "," (List.sort compare versions);
            ]);
       match List.assoc_opt pkg statuses with
       | Some (_ :: _ as st) -> hist_record st (Some (String.concat "." mods))
       | _ -> None);
  let j = { (job "compilers,5.0,x.1") with url = "u" } in
  Rules.suggest_for_job (src0 ()) j stdlib_log [ "x.1" ] (String.make 40 'a')

let test_stdlib_history_answers () =
  let r =
    stdlib_suggest
      [
        ( "ocaml-base-compiler",
          [
            ("4.14.0", "absent");
            ("5.0.0", "absent");
            ("5.1.0", "present");
            ("5.2.0", "present");
          ] );
        ("ocaml-compiler", [ ("5.3.0", "present"); ("5.4.0", "present") ]);
      ]
  in
  check_strs "rule"
    [ "stdlib"; {|"ocaml" {>= "5.1"}|} ]
    [
      Option.fold ~none:"" ~some:Rule.to_string r.rule;
      Suggestion.str_field r.Suggestion.new_;
    ];
  check_str "note"
    "`List.is_empty` is new in OCaml 5.1: to keep supporting older releases, \
     do without it instead."
    (Suggestion.str r.note);
  check_str "source" "ocaml.org API history" (Suggestion.str r.source);
  check_strs "history"
    [
      "history ocaml-base-compiler Stdlib.List value is_empty \
       4.14.0,5.0.0,5.1.0,5.2.0";
      "history ocaml-compiler Stdlib.List value is_empty 5.3.0,5.4.0";
    ]
    (List.filter (fun c -> String.starts_with ~prefix:"history" c) !calls)

let test_releases_without_docs_read_from_the_mli () =
  (* 5.1.0 has no docs: its .mli tells; the other releases come from the history. *)
  let r =
    stdlib_suggest
      [
        ( "ocaml-base-compiler",
          [
            ("4.14.0", "absent");
            ("5.0.0", "absent");
            ("5.1.0", "no-docs");
            ("5.2.0", "present");
          ] );
      ]
  in
  check_str "new" {|"ocaml" {>= "5.1"}|}
    (Suggestion.str_field r.Suggestion.new_);
  check_json "evidence"
    (`Assoc [ ("ocaml 5.1.0", `Bool true) ])
    (get r "evidence")
  (* the only release the search checked *);
  check_str "source" "OCaml's stdlib .mli files" (Suggestion.str r.source);
  check_true "mli read" (List.mem "mli 5.1.0" !calls)

let test_no_history () =
  (* The tool not deployed, or failing: the .mli of each release, as before. *)
  let r = stdlib_suggest [] in
  check_strs "new"
    [ {|"ocaml" {>= "5.1"}|}; "OCaml's stdlib .mli files" ]
    [ Suggestion.str_field r.Suggestion.new_; Suggestion.str r.source ]

(* ------------------------------------------------------------------------ *)
(* ApiHistoryRuleTest: the library rule with ocaml.org's API history: its
   answer where it has one, the docs and git for the versions it could not check. *)

let lib_log =
  "-> installed foo.1.0\n\
   #=== ERROR while compiling x.1 ===#\n\
   # exit-code 1\n\
   ### output ###\n"
  ^ "# File \"x.ml\", line 1, characters 0-7:\n# Error: Unbound value Foo.bar\n"

let lib_suggest ?(docs = []) ?git ?(log = lib_log) ?(dep = "foo") statuses =
  calls := [];
  (Sources.opam_file_hook :=
     fun ?ref:_ _ _ _ ->
       Some
         (Printf.sprintf
            "opam-version: \"2.0\"\ndepends: [\n  \"ocaml\"\n  \"%s\"\n]\n" dep));
  (Sources.raw_hook := fun ?max_age:_ _ _ -> None) (* not the stdlib *);
  (Sources.tags_hook := fun _ _ -> []);
  (Sources.versions_hook :=
     fun ?ref:_ _ _ -> Some [ "0.9"; "1.0"; "1.1"; "1.2"; "1.3" ]);
  (Sources.api_history_hook :=
     fun _ pkg mods kind ident versions ->
       call
         (String.concat " "
            [
              "history";
              pkg;
              String.concat "." mods;
              kind;
              ident;
              String.concat "," (List.sort compare versions);
            ]);
       match statuses with
       | Some (_ :: _ as st) -> hist_record st (Some "Foo")
       | _ -> None);
  (Sources.module_providers_hook := fun _ _ _ -> None)
  (* not deployed, or no answer *);
  (Sources.doc_present_hook :=
     fun _ _ v _ _ _ ->
       call ("docs " ^ v);
       List.assoc_opt v docs);
  (Sources.upstream_hook := fun ?ref:_ _ _ _ -> Some "https://github.com/x/foo");
  (Sources.tag_for_hook :=
     fun _ _ _ v ->
       call ("git " ^ v);
       match git with
       | Some g when List.mem_assoc v g -> Some ("v" ^ v)
       | _ -> None);
  (Sources.tree_hook := fun ?is_tag:_ _ _ _ -> [ "src/foo.mli" ]);
  (Sources.file_at_hook :=
     fun ?is_tag:_ _ _ ref _ ->
       let v = String.sub ref 1 (String.length ref - 1) in
       Some
         (if List.assoc v (Option.get git) then "val bar : int\n"
          else "val baz : int\n"));
  let j = { (job "compilers,5.3,x.1") with url = "u" } in
  Rules.suggest_for_job (src0 ()) j log [ "x.1" ] (String.make 40 'a')

let not_history () =
  List.filter (fun c -> not (String.starts_with ~prefix:"history" c)) !calls

let test_lib_history_answers () =
  let r =
    lib_suggest
      (Some
         [
           ("0.9", "absent");
           ("1.0", "absent");
           ("1.1", "present");
           ("1.2", "present");
           ("1.3", "present");
         ])
  in
  check_strs "rule"
    [ "library-api"; {|"foo" {>= "1.1"}|} ]
    [
      Option.fold ~none:"" ~some:Rule.to_string r.rule;
      Suggestion.str_field r.Suggestion.new_;
    ];
  check_json "evidence"
    (`Assoc
       [
         ("foo 1.1", `Bool true);
         ("foo 1.0 (installed in failing job)", `Bool false);
       ])
    (get r "evidence");
  check_str "source" "ocaml.org API history" (Suggestion.str r.source);
  (* One call for every version the search may check; no docs read, no git. *)
  check_strs "calls" [ "history foo Foo value bar 0.9,1.0,1.1,1.2,1.3" ] !calls

let test_combined_with_docs_and_git () =
  (* 1.1 has no docs: git only; 1.2's page is too large: its docs read another way. *)
  let r =
    lib_suggest
      ~docs:[ ("1.2", true) ]
      ~git:[ ("1.1", false) ]
      (Some
         [
           ("1.0", "absent");
           ("1.1", "no-docs");
           ("1.2", "page-truncated");
           ("1.3", "present");
         ])
  in
  check_str "new" {|"foo" {>= "1.2"}|} (Suggestion.str_field r.Suggestion.new_);
  check_json "evidence"
    (`Assoc
       [
         ("foo 1.1", `Bool false);
         ("foo 1.2", `Bool true);
         ("foo 1.0 (installed in failing job)", `Bool false);
       ])
    (get r "evidence");
  check_str "source"
    "git https://github.com/x/foo, ocaml.org API history, ocaml.org docs"
    (Suggestion.str r.source);
  check_strs "calls" [ "git 1.1"; "docs 1.2" ] (not_history ())

let test_removed () =
  (* Removed in the version used: the bound is below it. *)
  let r =
    lib_suggest
      (Some
         [
           ("0.9", "present");
           ("1.0", "absent");
           ("1.1", "absent");
           ("1.2", "absent");
           ("1.3", "absent");
         ])
  in
  check_str "new" {|"foo" {< "1.0"}|} (Suggestion.str_field r.Suggestion.new_);
  check_str "source" "ocaml.org API history" (Suggestion.str r.source)

let test_without_history () =
  (* No history (not deployed, a failure, the item in no documented version):
     the docs version by version, then git, as before. *)
  let r =
    lib_suggest
      ~docs:[ ("1.0", false); ("1.1", false) ]
      ~git:[ ("1.2", true) ]
      None
  in
  check_str "new" {|"foo" {>= "1.2"}|} (Suggestion.str_field r.Suggestion.new_);
  check_str "source" "git https://github.com/x/foo, ocaml.org docs"
    (Suggestion.str r.source);
  check_strs "calls"
    [ "docs 1.0"; "docs 1.1"; "docs 1.2"; "git 1.2" ]
    (not_history ())

let test_module_through_an_open () =
  (* The module (why's Why.Term, opened) does not name the package: a
     dependency is a candidate only if the version used has the module. *)
  let log =
    Util.replace (Util.replace lib_log "foo.1.0" "why.1.0") "Foo.bar" "Term.bar"
  in
  let r =
    lib_suggest ~log ~dep:"why"
      (Some [ ("1.0", "module-not-found"); ("1.1", "present") ])
  in
  check_false "new" (r.Suggestion.new_ <> Absent);
  check_strs "calls" [ "history" ]
    (List.map (fun c -> List.hd (String.split_on_char ' ' c)) !calls);
  let r =
    lib_suggest ~log ~dep:"why" (Some [ ("1.0", "absent"); ("1.1", "present") ])
  in
  check_strs "new"
    [ {|"why" {>= "1.1"}|}; "ocaml.org API history" ]
    [ Suggestion.str_field r.Suggestion.new_; Suggestion.str r.source ];
  check_strs "calls" [ "history why Term value bar 0.9,1.0,1.1,1.2,1.3" ] !calls

let test_kinds () =
  let unb l =
    match Rules.find_unbound l with
    | Some (k, m, i) -> k ^ " " ^ String.concat "." m ^ " " ^ i
    | None -> "none"
  in
  check_str "module type" "module-type Foo S"
    (unb [ "Error: Unbound module type Foo.S" ]);
  check_str "class" "class Foo c" (unb [ "Error: Unbound class Foo.c" ]);
  check_str "class type" "class-type Foo c"
    (unb [ "Error: Unbound class type Foo.c" ]);
  check_str "module" "module  Foo" (unb [ "Error: Unbound module Foo" ]);
  check_true "module type"
    (Sources.defines "module type S = sig end" "module-type" "S");
  check_false "module" (Sources.defines "module S : sig end" "module-type" "S");
  check_true "class"
    (Sources.defines "class virtual ['a] c : object end" "class" "c");
  check_true "exception" (Sources.defines "exception E of int" "exception" "E")

(* ------------------------------------------------------------------------ *)
(* ModuleFindTest: Sources.module_providers: ocaml_module_find's answer as the
   modules each candidate documents; its errors are answers, its failures are counted. *)

(** An ocaml_module_find answer: [matches] (package, version, module path,
    library, kind), [checked] package -> status. *)
let find_answer ?(partial = false) ?(extra = []) matches checked : Json.t =
  `Assoc
    ([
       ("name", s "Term");
       ("kind", s "module");
       ("searched_as", `Assoc [ ("library", `Null); ("module", s "Term") ]);
       ( "matches",
         `List
           (List.map
              (fun (p, v, m, lib, k) ->
                `Assoc
                  [
                    ("package", s p);
                    ("version", s v);
                    ("kind", s k);
                    ("library", Json.opt_string lib);
                    ("module", Json.opt_string m);
                    ( "path",
                      s
                        (Printf.sprintf "%s/%s/index.html"
                           (match lib with Some l -> l | None -> "None")
                           (Util.replace (Option.value ~default:"" m) "." "/"))
                    );
                  ])
              matches) );
       ("matches_truncated", `Bool false);
       ( "checked",
         `List
           (List.map
              (fun (p, st) ->
                `Assoc
                  ([ ("package", s p); ("version", s "1.0"); ("status", s st) ]
                  @
                  if st = "no-docs" then [ ("versions_probed", `Int 5) ] else []
                  ))
              checked) );
       ("unknown_packages", `List []);
       ("candidates_truncated", `Bool false);
       ("partial", `Bool partial);
       ("deadline_reached", `Bool false);
     ]
    @ extra)

let find_desc (f : Sources.module_find option) =
  match f with
  | None -> "None"
  | Some f ->
      Printf.sprintf
        "matches=%s checked=%s no_docs=%s distribution=%b partial=%b"
        (String.concat ";"
           (List.map
              (fun (m : Sources.find_match) ->
                Printf.sprintf "%s %s %s %s" m.package
                  (String.concat "." m.module_path)
                  (Option.value ~default:"None" m.mversion)
                  (Option.value ~default:"None" m.library))
              f.matches))
        (String.concat "," (List.map (fun (p, st) -> p ^ ":" ^ st) f.checked))
        (String.concat "," f.no_docs)
        f.distribution f.fpartial

let test_match_through_a_wrapper_and_cache () =
  let tmp = tmpdir () in
  let reqs =
    serve (fun _ ->
        mcp_answer
          (find_answer
             [ ("why3", "1.9.0", Some "Why3.Term", Some "why3", "module") ]
             [
               ("why3", "match"); ("cmdliner", "no-match"); ("nodoc", "no-docs");
             ]))
  in
  let before = errors () in
  let f =
    Sources.module_providers (Sources.create tmp) "Term"
      [ "why3"; "cmdliner"; "nodoc"; "why3" ]
  in
  check_json "args"
    (`Assoc
       [
         ("name", s "ocaml_module_find");
         ( "arguments",
           `Assoc
             [
               ("name", s "Term");
               ("kind", s "module");
               ("packages", strs [ "why3"; "cmdliner"; "nodoc" ]);
             ] );
       ])
    (params (List.hd !reqs));
  check_str "answer"
    "matches=why3 Why3.Term 1.9.0 why3 \
     checked=why3:match,cmdliner:no-match,nodoc:no-docs no_docs=nodoc \
     distribution=false partial=false"
    (find_desc f);
  check_int "errors" before (errors ());
  (* Cached on disk, for a day (new releases, docs built since). *)
  check_str "cached" (find_desc f)
    (find_desc
       (Sources.module_providers (Sources.create tmp) "Term"
          [ "why3"; "cmdliner"; "nodoc" ]));
  check_int "one" 1 (List.length !reqs);
  age_all tmp 2.;
  ignore
    (Sources.module_providers (Sources.create tmp) "Term"
       [ "why3"; "cmdliner"; "nodoc" ]);
  check_int "two" 2 (List.length !reqs);
  (* No candidate, no call. *)
  check_str "none" "None"
    (find_desc (Sources.module_providers (Sources.create tmp) "Term" []));
  check_int "still two" 2 (List.length !reqs)

let test_find_several_matches () =
  (* Modules only, in the tool's order; not a library, a module type, or an odd path. *)
  let tmp = tmpdir () in
  ignore
    (serve (fun _ ->
         mcp_answer
           (find_answer
              [
                ("lwt", "5.9.0", Some "Lwt.Infix", Some "lwt", "module");
                ("lwt", "5.9.0", Some "Lwt_result.Infix", Some "lwt", "module");
                ("sigs", "1.0", Some "Sigs.Infix", Some "sigs", "module-type");
                ("lib", "1.0", None, Some "infix", "library");
                ("odd", "1.0", Some "Odd.<b>Infix</b>", Some "odd", "module");
                ("base", "v0.17", Some "Base.Infix", None, "module");
                ("@evil", "1.0", Some "Evil.Infix", Some "evil", "module");
                ("ok", "1.0 [x](y)", Some "Ok.Infix", Some "<i>", "module");
              ]
              [
                ("lwt", "match");
                ("sigs", "match");
                ("lib", "match");
                ("odd", "match");
                ("base", "match");
              ])));
  let f =
    Option.get
      (Sources.module_providers (Sources.create tmp) "Infix"
         [ "lwt"; "sigs"; "lib"; "odd"; "base" ])
  in
  check_str "matches"
    "lwt Lwt.Infix 5.9.0 lwt;lwt Lwt_result.Infix 5.9.0 lwt;base Base.Infix \
     v0.17 None;ok Ok.Infix None None"
    (String.concat ";"
       (List.map
          (fun (m : Sources.find_match) ->
            Printf.sprintf "%s %s %s %s" m.package
              (String.concat "." m.module_path)
              (Option.value ~default:"None" m.mversion)
              (Option.value ~default:"None" m.library))
          f.matches))

let test_no_match_and_distribution () =
  let tmp = tmpdir () in
  ignore
    (serve_params (fun p ->
         mcp_answer
           (find_answer []
              [ ("foo", "no-match"); ("nodoc", "no-docs") ]
              ~extra:
                (if Json.str "name" (Json.member "arguments" p) = "Unix" then
                   [
                     ( "note",
                       s
                         "Unix comes with the OCaml compiler, not with an opam \
                          package: see https://ocaml.org/api/" );
                   ]
                 else []))));
  let f =
    Option.get
      (Sources.module_providers (Sources.create tmp) "Bar" [ "foo"; "nodoc" ])
  in
  check_true "no match"
    (f.matches = [] && f.no_docs = [ "nodoc" ] && (not f.distribution)
   && not f.fpartial);
  check_true "distribution"
    (Option.get
       (Sources.module_providers (Sources.create tmp) "Unix" [ "foo" ]))
      .distribution

let test_find_partial_is_a_failure () =
  (* A candidate's docs could not be read, or the call ran out of time: what
     was found is used, the call counts as failed and is not cached. *)
  let tmp = tmpdir () in
  let reqs =
    serve (fun _ ->
        mcp_answer
          (find_answer ~partial:true
             ~extra:[ ("deadline_reached", `Bool true) ]
             [ ("why3", "1.9.0", Some "Why3.Term", Some "why3", "module") ]
             [ ("why3", "match"); ("flaky", "error"); ("slow", "skipped") ]))
  in
  let before = errors () in
  let src = Sources.create tmp in
  let f = Sources.module_providers src "Term" [ "why3"; "flaky"; "slow" ] in
  check_true "partial" (Option.get f).fpartial;
  check_strs "matches" [ "why3" ]
    (List.map
       (fun (m : Sources.find_match) -> m.package)
       (Option.get f).matches);
  check_int "counted" (before + 1) (errors ());
  check_strs "not cached" [] (Util.listdir tmp);
  check_str "same" (find_desc f)
    (find_desc
       (Sources.module_providers src "Term" [ "why3"; "flaky"; "slow" ]));
  check_int "once per process" 1 (List.length !reqs);
  check_int "counted once" (before + 1) (errors ())

let test_tool_error_vs_failure () =
  (* The tool's error (no known package among the candidates): no answer,
     cached a day, not a failure. *)
  let tmp = tmpdir () in
  let reqs =
    serve (fun _ ->
        tool_error
          {|no package to search for "Xyz": none is named after it, and no known package was given in "packages" (unknown packages: "nope")|})
  in
  let before = errors () in
  check_str "none" "None"
    (find_desc (Sources.module_providers (Sources.create tmp) "Xyz" [ "nope" ]));
  check_str "none" "None"
    (find_desc (Sources.module_providers (Sources.create tmp) "Xyz" [ "nope" ]));
  check_int "one" 1 (List.length !reqs);
  check_int "not counted" before (errors ());
  (* A failed call: no answer, counted, not cached. *)
  nothing_listens ();
  check_str "none" "None"
    (find_desc
       (Sources.module_providers (Sources.create tmp) "Term" [ "why3" ]));
  check_int "counted" (before + 1) (errors ());
  check_int "the tool's error only" 1 (List.length (Util.listdir tmp))

let test_find_tool_not_deployed () =
  (* A server without the tool: no answer, not a failure, not asked again. *)
  let tmp = tmpdir () in
  let reqs = unknown_tool "ocaml_module_find" in
  let before = errors () in
  let src = Sources.create tmp in
  check_str "none" "None"
    (find_desc (Sources.module_providers src "Term" [ "why3" ]));
  check_str "none" "None"
    (find_desc (Sources.module_providers src "Ez" [ "ez_api" ]));
  check_int "one" 1 (List.length !reqs);
  check_int "not counted" before (errors ());
  check_int "no failure" 0 !Sources.mcp_failures

(* ------------------------------------------------------------------------ *)
(* ModuleFindRuleTest: the library rule with ocaml.org's module search: the
   packages that document the module come first, with its full path;
   without an answer, the guess from the package names and the
   dependencies, as before. *)

(** A Sources.module_providers answer: [matches] (package, module path). *)
let found ?(checked = []) ?(partial = false) ?(distribution = false) matches =
  Some
    {
      Sources.matches =
        List.map
          (fun (p, m) ->
            {
              Sources.package = p;
              module_path = String.split_on_char '.' m;
              mversion = Some "2.0";
              library = Some p;
            })
          matches;
      checked;
      no_docs =
        List.filter_map
          (fun (p, st) -> if st = "no-docs" then Some p else None)
          checked;
      distribution;
      fpartial = partial;
    }

(** The rule's result and the lookups it made. *)
let find_suggest ?(history = []) ?(docs = []) ?(depopts = []) ?(published = [])
    ?(tops = []) error installed depends providers =
  calls := [];
  let log =
    String.concat ""
      (List.map
         (fun (p, v) -> Printf.sprintf "-> installed %s.%s\n" p v)
         installed)
    ^ "#=== ERROR while compiling x.1 ===#\n# exit-code 1\n### output ###\n"
    ^ Printf.sprintf "# File \"x.ml\", line 1, characters 0-7:\n# Error: %s\n"
        error
  in
  let opam =
    "opam-version: \"2.0\"\ndepends: [\n"
    ^ String.concat ""
        (List.map (fun x -> Printf.sprintf "  \"%s\"\n" x) depends)
    ^ "]\n"
    ^
    if depopts <> [] then
      Printf.sprintf "depopts: [ %s ]\n"
        (String.concat " " (List.map (fun x -> "\"" ^ x ^ "\"") depopts))
    else ""
  in
  (Sources.opam_file_hook := fun ?ref:_ _ _ _ -> Some opam);
  (Sources.raw_hook := fun ?max_age:_ _ _ -> None);
  (Sources.tags_hook := fun _ _ -> []);
  (Sources.versions_hook :=
     fun ?ref:_ _ _ -> Some [ "0.9"; "1.0"; "1.1"; "1.2" ]);
  (Sources.module_providers_hook :=
     fun _ name packages ->
       call (Printf.sprintf "find %s %s" name (String.concat "," packages));
       providers);
  (Sources.api_history_hook :=
     fun _ pkg mods kind ident _ ->
       call
         (String.concat " "
            [ "history"; pkg; String.concat "." mods; kind; ident ]);
       match List.assoc_opt (pkg, String.concat "." mods) history with
       | Some (_ :: _ as st) -> hist_record st (Some (String.concat "." mods))
       | _ -> None);
  (Sources.doc_present_hook :=
     fun _ pkg v mods _ ident ->
       call
         (String.concat " " [ "docs"; pkg; v; String.concat "." mods; ident ]);
       List.assoc_opt (pkg, v) docs);
  (Sources.published_hook :=
     fun _ pkg v ->
       call (Printf.sprintf "published %s %s" pkg v);
       List.assoc_opt (pkg, v) published);
  (Sources.doc_modules_hook :=
     fun _ pkg v ->
       call (Printf.sprintf "modules %s %s" pkg v);
       List.assoc_opt (pkg, v) tops);
  (Sources.upstream_hook := fun ?ref:_ _ _ _ -> None) (* no git *);
  let j = { (job "compilers,5.3,x.1") with url = "u" } in
  Rules.suggest_for_job (src0 ()) j log [ "x.1" ] (String.make 40 'a')

let test_candidates () =
  (* The packages named after the module, then the direct and optional
     dependencies installed, but not the compiler, dune, conf-* packages. *)
  ignore
    (find_suggest ~depopts:[ "zmq"; "nope" ] "Unbound value Foo_bar.x"
       [
         ("foo", "1.0");
         ("why", "1.0");
         ("conf-gmp", "4");
         ("dune", "3.0");
         ("zmq", "5.0");
         ("tr", "1.0");
       ]
       [ "ocaml"; "dune"; "conf-gmp"; "why"; "absent"; "foo" ]
       None);
  check_str "find" "find Foo_bar foo,why,zmq" (List.hd !calls);
  check_strs "depends" [ "a"; "b-c+d"; "e"; "f" ]
    (Loginfo.dep_names
       (Some
          "depends: [\n\
          \  \"a\" {>= \"1.0\" & < \"2.0\"}\n\
          \  \"b-c+d\"\n\
          \  (\"e\" | \"f\")\n\
           ]\n\
           depopts: [\"g\" {>= \"1\"} \"h\"]\n")
       "depends");
  check_strs "depopts" [ "g"; "h" ]
    (Loginfo.dep_names (Some "depopts: [\"g\" {>= \"1\"} \"h\"]\n") "depopts");
  check_strs "none" []
    (Loginfo.dep_names (Some "build: [[\"dune\" \"build\"]]\n") "depends")

let test_module_through_a_wrapper () =
  (* Term is why's Why.Term (opened): the history is asked for the full path,
     and the package need not have the module in the version used. *)
  let r =
    find_suggest "Unbound value Term.bar"
      [ ("why", "1.0"); ("cmdliner", "1.0") ]
      [ "why"; "cmdliner" ]
      (found [ ("why", "Why.Term") ])
      ~history:
        [
          ( ("why", "Why.Term"),
            [ ("1.0", "module-not-found"); ("1.1", "present") ] );
        ]
  in
  check_strs "new"
    [ "library-api"; {|"why" {>= "1.1"}|} ]
    [
      Option.fold ~none:"" ~some:Rule.to_string r.rule;
      Suggestion.str_field r.Suggestion.new_;
    ];
  check_json "evidence"
    (`Assoc
       [
         ("why 1.1", `Bool true);
         ("why 1.0 (installed in failing job)", `Bool false);
       ])
    (get r "evidence");
  check_str "source" "ocaml.org API history, ocaml.org module search"
    (Suggestion.str r.source);
  check_strs "calls" [ "history why Why.Term value bar" ] (List.tl !calls);
  (* Without the search (not deployed, a failure), the guess: why.1.0 has no
     module Term, so it is not a candidate. *)
  let r =
    find_suggest "Unbound value Term.bar"
      [ ("why", "1.0"); ("cmdliner", "1.0") ]
      [ "why"; "cmdliner" ] None
      ~history:
        [
          (("why", "Term"), [ ("1.0", "module-not-found"); ("1.1", "present") ]);
        ]
  in
  check_false "new" (r.Suggestion.new_ <> Absent)

let test_top_level_module_appeared_later () =
  (* EzLwtSys is in ez_api (not named after it) from 1.1: a lower bound. *)
  let docs = [ (("ez_api", "1.0"), false); (("ez_api", "1.1"), true) ] in
  let go p =
    find_suggest ~docs "Unbound module EzLwtSys"
      [ ("ez_api", "1.0") ]
      [ "ocaml"; "ez_api" ] p
  in
  let r = go (found [ ("ez_api", "EzLwtSys") ]) in
  check_strs "new"
    [ "library-api"; {|"ez_api" {>= "1.1"}|}; "Unbound module EzLwtSys" ]
    [
      Option.fold ~none:"" ~some:Rule.to_string r.rule;
      Suggestion.str_field r.Suggestion.new_;
      Suggestion.str r.error;
    ];
  check_str "source" "ocaml.org docs, ocaml.org module search"
    (Suggestion.str r.source);
  check_strs "docs"
    [ "docs ez_api 1.0  EzLwtSys"; "docs ez_api 1.1  EzLwtSys" ]
    (List.filter (fun c -> String.starts_with ~prefix:"docs" c) !calls);
  (* Before: ez_api.1.0 has no module EzLwtSys, so it was not a candidate. *)
  let r = go None in
  check_false "new" (r.Suggestion.new_ <> Absent);
  check_str "note"
    "The identifier was not found in the history of any dependency the tool \
     checked."
    (Suggestion.str r.note)

let test_top_level_module_in_a_wrapper () =
  (* [Unbound module Term] (Why opened): the history of item Term of Why. *)
  let r =
    find_suggest "Unbound module Term"
      [ ("why", "1.0") ]
      [ "why" ]
      (found [ ("why", "Why.Term") ])
      ~history:[ (("why", "Why"), [ ("1.0", "absent"); ("1.1", "present") ]) ]
  in
  check_str "new" {|"why" {>= "1.1"}|} (Suggestion.str_field r.Suggestion.new_);
  check_true "call" (List.mem "history why Why module Term" !calls)

let test_defined_in_the_version_used () =
  (* The package found has the module and the item in the version used: not
     a version problem; the note says where the module is. *)
  let r =
    find_suggest "Unbound module Bar"
      [ ("foo", "1.0") ]
      [ "foo" ]
      (found [ ("foo", "Foo.Bar") ])
      ~history:[ (("foo", "Foo"), [ ("1.0", "present") ]) ]
  in
  check_false "new" (r.Suggestion.new_ <> Absent);
  check_strs "tried"
    [
      "foo.1.0 defines it (ocaml.org API history, ocaml.org module search): \
       not a version problem of foo";
    ]
    (Suggestion.list r.tried);
  check_true (Suggestion.str r.note)
    (String.ends_with
       ~suffix:
         " ocaml.org's docs have module Bar in foo 2.0 (as Foo.Bar, library \
          foo)."
       (Suggestion.str r.note));
  check_true "flag" r.unexplained

let test_rule_several_matches () =
  (* In the tool's order: the first package giving a bound; a package with
     two modules of that name is looked up through the path as written. *)
  let r =
    find_suggest "Unbound value Infix.bar"
      [ ("a", "1.0"); ("b", "1.0") ]
      [ "a"; "b" ]
      (found [ ("a", "A.Infix"); ("a", "A_result.Infix"); ("b", "B.Infix") ])
      ~history:
        [
          ( ("a", "Infix"),
            [
              ("1.0", "absent");
              ("1.1", "absent");
              ("1.2", "absent");
              ("0.9", "absent");
            ] );
          ( ("b", "B.Infix"),
            [ ("1.0", "absent"); ("1.2", "present"); ("1.1", "absent") ] );
        ]
  in
  check_str "new" {|"b" {>= "1.2"}|} (Suggestion.str_field r.Suggestion.new_);
  check_strs "history" [ "a Infix"; "b B.Infix" ]
    (List.filter_map
       (fun c ->
         match String.split_on_char ' ' c with
         | "history" :: p :: m :: _ -> Some (p ^ " " ^ m)
         | _ -> None)
       !calls)

let test_rule_no_match () =
  (* Nothing among the dependencies' docs: the guess, and the note says so. *)
  let r =
    find_suggest "Unbound module Bar"
      [ ("foo", "1.0"); ("nodoc", "1.0") ]
      [ "foo"; "nodoc" ]
      (found
         ~checked:
           [ ("foo", "no-match"); ("nodoc", "no-docs"); ("bar", "no-docs") ]
         [])
      ~docs:[ (("foo", "1.0"), false) ]
  in
  check_false "new" (r.Suggestion.new_ <> Absent);
  check_true "the guess, as before" (List.mem "docs foo 1.0  Bar" !calls);
  check_str "note"
    "The identifier was not found in the history of any dependency the tool \
     checked. ocaml.org's docs have no module Bar in the latest documented \
     version of the dependencies (without docs: nodoc)."
    (Suggestion.str r.note);
  (* A module of the OCaml distribution (Unix, Str...). *)
  let r =
    find_suggest "Unbound module Unix"
      [ ("foo", "1.0") ]
      [ "foo" ]
      (found ~distribution:true [])
  in
  check_true (Suggestion.str r.note)
    (String.ends_with
       ~suffix:
         " ocaml.org: Unix comes with the OCaml compiler, not with an opam \
          package."
       (Suggestion.str r.note))

let test_module_removed_by_a_recent_release () =
  (* console-eio's tests used Grid, a top-level module of matrix 0.1.0
     (Matrix_grid in 0.2.0, released 3 days before the job), installed
     through another dependency (#31170): the module search finds no
     dependency with it in its newest docs. *)
  let recent =
    Date.date_of_days (Option.get (Date.days_of_date (Date.today ())) - 3)
  in
  let installed = [ ("foo", "1.0"); ("matrix", "1.2") ] in
  let published =
    [ (("matrix", "1.2"), recent); (("foo", "1.0"), "2020-01-01") ]
  in
  let tops =
    [
      ( ("matrix", "1.1"),
        [ ("Matrix", "matrix/Matrix"); ("Grid", "matrix.grid/Grid") ] );
      ( ("matrix", "1.2"),
        [
          ("Matrix", "matrix/Matrix"); ("Matrix_grid", "matrix.grid/Matrix_grid");
        ] );
    ]
  in
  let none = found ~checked:[ ("foo", "no-match") ] [] in
  let go ?(published = published) deps providers =
    find_suggest ~published ~tops "Unbound module Grid" installed deps providers
  in
  let r = go [ "foo" ] none in
  check_strs "new"
    [ "library-api"; {|"matrix" {< "1.2"}   (new line in depends)|} ]
    [
      Option.fold ~none:"" ~some:Rule.to_string r.rule;
      Suggestion.str_field r.Suggestion.new_;
    ];
  check_json "evidence"
    (`Assoc
       [
         ("matrix 1.1", `Bool true);
         ("matrix 1.2 (installed in failing job)", `Bool false);
       ])
    (get r "evidence");
  check_str "note"
    (Printf.sprintf
       "`Grid` is a top-level module of matrix.1.1, not of matrix.1.2 \
        (released %s). x uses it without declaring matrix: declare it, with \
        this bound. ocaml.org's docs have no module Grid in the latest \
        documented version of the dependencies."
       recent)
    (Suggestion.str r.note);
  (* The declared dependencies first; only the recent releases are looked up. *)
  check_strs "lookups"
    [
      "published foo 1.0";
      "published matrix 1.2";
      "modules matrix 1.1";
      "modules matrix 1.2";
    ]
    (List.filter
       (fun c ->
         String.starts_with ~prefix:"published" c
         || String.starts_with ~prefix:"modules" c)
       !calls);
  (* Declared: its line gets the bound. *)
  let r = go [ "foo"; "matrix" ] none in
  check_str "declared" {|"matrix" {< "1.2"}|}
    (Suggestion.str_field r.Suggestion.new_);
  (* An older release, or a module search that did not answer (or only in part): not looked up. *)
  let r = go ~published:[ (("matrix", "1.2"), "2020-01-01") ] [ "foo" ] none in
  check_false "old" (r.Suggestion.new_ <> Absent);
  check_false "not looked up" (List.mem "modules matrix 1.1" !calls);
  List.iter
    (fun providers ->
      let r = go [ "foo" ] providers in
      check_false "new" (r.Suggestion.new_ <> Absent);
      check_false "published"
        (List.exists (fun c -> String.starts_with ~prefix:"published" c) !calls))
    [ None; found ~checked:[ ("foo", "error") ] ~partial:true [] ]

let test_findlib_library_of_another_package () =
  (* [bytes] is installed by base-bytes on OCaml 5 (liquidsoap 2.4.6, #30906). *)
  let r =
    find_suggest {|Library "bytes" not found.|}
      [ ("foo", "1.0") ]
      [ "foo" ] None
  in
  check_strs "rule"
    [
      "missing-library";
      {|"base-bytes"   (new line in depends)|};
      "The library `bytes` is installed by the base-bytes package.";
    ]
    [
      Option.fold ~none:"" ~some:Rule.to_string r.rule;
      Suggestion.str_field r.Suggestion.new_;
      Suggestion.str r.note;
    ]

let test_match_outside_the_installed_packages () =
  (* In a package named after the module (searched by the tool), or an
     optional dependency not installed: a note, no bound. *)
  let r =
    find_suggest ~depopts:[ "zmq" ] "Unbound module Bar"
      [ ("foo", "1.0") ]
      [ "foo" ]
      (found [ ("bar", "Bar"); ("zmq", "Zmq.Bar") ])
  in
  check_false "new" (r.Suggestion.new_ <> Absent);
  check_true (Suggestion.str r.note)
    (String.ends_with
       ~suffix:
         " ocaml.org's docs have module Bar in bar 2.0 (library bar, not a \
          dependency of x); zmq 2.0 (as Zmq.Bar, library zmq, not installed in \
          this job)."
       (Suggestion.str r.note));
  (* The failing package's own module (its latest docs have it): said so. *)
  let r =
    find_suggest "Unbound module X_lexer"
      [ ("foo", "1.0") ]
      [ "foo" ]
      (found [ ("x", "X_lexer") ])
  in
  check_true (Suggestion.str r.note)
    (String.ends_with
       ~suffix:
         " ocaml.org's docs have module X_lexer in x 2.0 (library x, the \
          failing package)."
       (Suggestion.str r.note))

let test_rule_partial () =
  (* Partial without a match: no answer (the guess, no note); with a match, the match is used. *)
  let r =
    find_suggest "Unbound module Bar"
      [ ("foo", "1.0") ]
      [ "foo" ]
      (found ~checked:[ ("foo", "error") ] ~partial:true [])
  in
  check_str "note"
    "The identifier was not found in the history of any dependency the tool \
     checked."
    (Suggestion.str r.note);
  let r =
    find_suggest "Unbound value Term.bar"
      [ ("why", "1.0") ]
      [ "why" ]
      (found
         ~checked:[ ("why", "match"); ("foo", "skipped") ]
         ~partial:true
         [ ("why", "Why.Term") ])
      ~history:
        [ (("why", "Why.Term"), [ ("1.0", "absent"); ("1.1", "present") ]) ]
  in
  check_str "new" {|"why" {>= "1.1"}|} (Suggestion.str_field r.Suggestion.new_)

(* ------------------------------------------------------------------------ *)
(* NetErrorTest: network errors of the plain HTTP and GitHub API fetches:
   None, counted, not cached. *)

let with_token f =
  let old = Sys.getenv_opt "GITHUB_TOKEN" in
  Unix.putenv "GITHUB_TOKEN" "x" (* no [gh auth token] *);
  Fun.protect f ~finally:(fun () ->
      Unix.putenv "GITHUB_TOKEN" (Option.value ~default:"" old))

let net_errors =
  [
    "[Errno 104] Connection reset by peer";
    "timed out";
    "<urlopen error no route>";
    "BadStatusLine: x";
    "IncompleteRead(0 bytes read)";
  ]

let test_raw_and_gh_api () =
  let tmp = tmpdir () in
  List.iter
    (fun e ->
      (Http.hook := fun _ -> raise (Http.Net_error e));
      with_token (fun () ->
          let before = errors ~k:"net-errors" () in
          check_true e
            (Sources.raw (Sources.create tmp) "https://example.invalid/a" = None);
          Hashtbl.reset Net.gh_cache;
          check_true e (Net.gh_api ~cache_dir:tmp "repos/x/y/pulls/1" = None);
          check_int e (before + 2) (errors ~k:"net-errors" ()));
      check_strs e [] (Util.listdir tmp))
    net_errors

let ok_body body = { Http.status = 200; resp_headers = []; data = body }

let test_upstream_path_quoted () =
  (* A path with a space or non-ASCII characters is quoted. *)
  let tmp = tmpdir () in
  let urls = ref [] in
  (Http.hook :=
     fun r ->
       urls := !urls @ [ r.url ];
       String.iter
         (fun c ->
           if Char.code c > 127 || c = ' ' then Alcotest.fail "not quoted")
         r.url;
       ok_body "body");
  let body =
    Sources.file_at (Sources.create tmp) "https://github.com/o/r" "v1.0"
      "doc/a b/\xc3\xa9+x.ml"
  in
  Alcotest.(check (option string)) "body" (Some "body") body;
  check_strs "urls"
    [ "https://raw.githubusercontent.com/o/r/v1.0/doc/a%20b/%C3%A9+x.ml" ]
    !urls

let test_not_found_and_master_files_expire () =
  let tmp = tmpdir () in
  let n = ref 0 in
  (Http.hook :=
     fun _ ->
       incr n;
       ok_body "now here");
  let url = "https://example.invalid/missing" in
  close_out (open_out (Net.cache_path url tmp ^ ".404"));
  (match Net.fetch url tmp with
  | _ -> Alcotest.fail "no error"
  | exception Http.Http_error { code = 404; _ } -> ());
  age (Net.cache_path url tmp ^ ".404") 2.;
  check_str "fetched" "now here" (Net.fetch url tmp);
  (* An opam file on master is fetched again after a day; one at a commit is not. *)
  List.iter
    (fun ref ->
      let oc =
        open_out
          (Net.cache_path
             (Printf.sprintf
                "https://raw.githubusercontent.com/%s/%s/packages/x/x.1/opam"
                Net.repo ref)
             tmp)
      in
      output_string oc "old";
      close_out oc)
    [ "master"; String.make 40 'a' ];
  age_all tmp 2.;
  Alcotest.(check (option string))
    "master" (Some "now here")
    (Sources.opam_file (Sources.create tmp) "x" "1");
  Alcotest.(check (option string))
    "commit" (Some "old")
    (Sources.opam_file ~ref:(String.make 40 'a') (Sources.create tmp) "x" "1");
  check_int "calls" 2 !n

(* ------------------------------------------------------------------------ *)
(* FailedFetchTest: a page, a log or a git listing that could not be fetched
   is counted in "net-errors" (the analysis is incomplete and is retried); a
   clean negative answer (404, a missing tag) is not. *)

let test_fetch_counts_failures () =
  let tmp = tmpdir () in
  Net.sleep_hook := ignore;
  let url = "https://example.invalid/log" in
  let status code =
   fun _ -> { Http.status = code; resp_headers = []; data = "" }
  in
  List.iter
    (fun (name, hook, is_http, counted) ->
      Http.hook := hook;
      let before = errors ~k:"net-errors" () in
      (match Net.fetch ~refresh:true url tmp with
      | _ -> Alcotest.fail "no error"
      | exception Http.Http_error _ -> check_true name is_http
      | exception Http.Net_error _ -> check_false name is_http);
      check_int name (before + counted) (errors ~k:"net-errors" ());
      (* Sources.raw catches it: counted once, not twice. *)
      let before = errors ~k:"net-errors" () in
      check_true name (Sources.raw (Sources.create tmp) (url ^ "2") = None);
      check_int name (before + counted) (errors ~k:"net-errors" ()))
    [
      ( "reset",
        (fun _ -> raise (Http.Net_error "[Errno 104] Connection reset by peer")),
        false,
        1 );
      ("timeout", (fun _ -> raise (Http.Net_error "timed out")), false, 1);
      ("503", status 503, true, 1);
      ("404", status 404, true, 0);
    ]

let git_result code stderr = { Proc.code; stdout = ""; stderr }

let test_git_failures () =
  List.iter
    (fun (stderr, counted) ->
      (Proc.run_hook :=
         fun ?cwd:_ ?env:_ ?input:_ ?timeout _ ->
           match stderr with
           | None -> raise (Proc.Timeout (Option.value ~default:0. timeout))
           | Some e -> git_result 128 e);
      List.iter
        (fun args ->
          let before = errors ~k:"net-errors" () in
          check_true "failed" ((Net.git ~check:false args).code <> 0);
          check_int
            (Option.value ~default:"timeout" stderr ^ " " ^ List.hd args)
            (before + counted)
            (errors ~k:"net-errors" ()))
        [
          [ "ls-remote"; "--tags"; "--refs"; "--"; "https://e.x/r" ];
          [
            "fetch";
            "-q";
            "--depth";
            "1";
            "origin";
            "+refs/tags/v1.0:refs/tags/v1.0";
          ];
        ])
    [
      ( Some
          "fatal: unable to access 'https://e.x/r/': Could not resolve host: \
           e.x",
        1 );
      ( Some
          "error: RPC failed; HTTP 503 curl 22 The requested URL returned \
           error: 503",
        1 );
      ( Some
          "fatal: unable to access 'https://e.x/r/': Failed to connect to e.x \
           port 443",
        1 );
      (None, 1 (* git killed after its timeout *));
      (Some "fatal: couldn't find remote ref refs/tags/v1.0", 0);
      ( Some
          "remote: Repository not found.\n\
           fatal: repository 'https://e.x/r/' not found",
        0 );
      ( Some
          "fatal: could not read Username for 'https://github.com': terminal \
           prompts disabled",
        0 );
    ];
  (* Other git commands failing are not network errors. *)
  (Proc.run_hook :=
     fun ?cwd:_ ?env:_ ?input:_ ?timeout:_ _ -> git_result 1 "Connection reset");
  let before = errors ~k:"net-errors" () in
  ignore (Net.git ~check:false [ "rev-parse"; "--verify"; "-q"; "v1^{tree}" ]);
  check_int "rev-parse" before (errors ~k:"net-errors" ())

(* ------------------------------------------------------------------------ *)
(* CacheWriteTest: cache files are written whole or not at all, and a damaged one is a miss. *)

let test_truncated_cache_is_a_miss () =
  let tmp = tmpdir () in
  let p = Filename.concat tmp "a.json" in
  let oc = open_out p in
  output_string oc {|{"versions": [|};
  close_out oc;
  check_true "truncated" (Net.cached_json p = None);
  Util.write_gzip (p ^ "x") {|["a", "b"]|};
  let gz = In_channel.with_open_bin (p ^ "x") In_channel.input_all in
  let oc = open_out_bin (Filename.concat tmp "b.json.gz") in
  output_string oc (String.sub gz 0 (String.length gz - 6));
  close_out oc;
  Sys.remove (p ^ "x");
  Sys.remove p;
  check_true "truncated gzip"
    (Net.cached_json (Filename.concat tmp "b.json") = None)

exception Killed

let test_killed_write_leaves_no_file () =
  let tmp = tmpdir () in
  (Sources.git_hook :=
     fun ?cwd:_ ?check:_ ?git_dir:_ _ ->
       { Proc.code = 0; stdout = "abc\trefs/tags/v1.0\n"; stderr = "" });
  (Net.rename_hook := fun _ _ -> raise Killed);
  (match Sources.tags (Sources.create tmp) "https://example.invalid/repo" with
  | _ -> Alcotest.fail "not killed"
  | exception Killed -> ());
  check_strs "no file" [] (Util.listdir tmp);
  originals ();
  let p = Filename.concat tmp "c.json" in
  Net.write_atomic p {|"old"|};
  (Net.rename_hook := fun _ _ -> raise Killed);
  (match Net.write_atomic p {|"new"|} with
  | _ -> Alcotest.fail "not killed"
  | exception Killed -> ());
  check_true "old" (Net.cached_json p = Some (s "old"));
  check_strs "files" [ "c.json" ] (Util.listdir tmp)

(* ------------------------------------------------------------------------ *)
(* TagsTest: a failed [git ls-remote] is not cached: the next run asks again. *)

let test_failed_listing_not_cached () =
  let tmp = tmpdir () in
  let url = "https://example.invalid/repo" in
  let n = ref 0 in
  let set r =
    Sources.git_hook :=
      fun ?cwd:_ ?check:_ ?git_dir:_ _ ->
        incr n;
        r
  in
  let bad =
    { Proc.code = 128; stdout = ""; stderr = "fatal: unable to access" }
  in
  let good = { Proc.code = 0; stdout = "abc\trefs/tags/v1.0\n"; stderr = "" } in
  set bad;
  check_strs "bad" [] (Sources.tags (Sources.create tmp) url);
  check_strs "bad" [] (Sources.tags (Sources.create tmp) url);
  check_int "asked twice" 2 !n;
  check_strs "not cached" [] (Util.listdir tmp);
  set good;
  check_strs "good" [ "v1.0" ] (Sources.tags (Sources.create tmp) url);
  set bad;
  n := 0;
  check_strs "cached" [ "v1.0" ] (Sources.tags (Sources.create tmp) url);
  check_int "not asked" 0 !n

let test_unreachable_host_remembered () =
  (* A host that cannot be reached (DNS, TLS, nothing listening, no answer) is
     not asked again for a day: one net error, then cached failures that are
     not new net errors. A transient error is not remembered. *)
  let tmp = tmpdir () in
  let url = "https://git.dead.example/repo" in
  let calls = ref [] in
  let run stderr =
    Proc.run_hook :=
      fun ?cwd:_ ?env:_ ?input:_ ?timeout:_ cmd ->
        let sub = List.nth cmd (List.length Net.git_protocols + 1) in
        calls := !calls @ [ sub ];
        if sub = "ls-remote" || sub = "fetch" then git_result 128 stderr
        else
          git_result (if sub = "rev-parse" then 1 else 0) "" (* a new mirror *)
  in
  run
    "fatal: unable to access 'https://git.dead.example/repo/': Could not \
     resolve host: git.dead.example";
  let n = errors ~k:"net-errors" () and u = errors ~k:"git-unreachable" () in
  check_strs "tags" [] (Sources.tags (Sources.create tmp) url);
  check_strs "calls" [ "ls-remote" ] !calls;
  check_int "net error" (n + 1) (errors ~k:"net-errors" ());
  calls := [];
  let src = Sources.create tmp in
  check_strs "tags" [] (Sources.tags src url);
  check_true "ensure" (Sources.ensure src url "v1.0" true = None)
  (* the fetch of a tag too *);
  check_false "ls-remote" (List.mem "ls-remote" !calls);
  check_false "fetch" (List.mem "fetch" !calls);
  check_int "net errors" (n + 1) (errors ~k:"net-errors" ());
  check_int "unreachable" (u + 1) (errors ~k:"git-unreachable" ());
  (* A day later, asked again. *)
  List.iter
    (fun f ->
      if String.starts_with ~prefix:"gitdown-" f then
        age (Filename.concat tmp f) 2.)
    (Util.listdir tmp);
  calls := [];
  check_strs "tags" [] (Sources.tags (Sources.create tmp) url);
  check_strs "again" [ "ls-remote" ] !calls;
  (* A transient error (a 503) is asked again in the next analysis. *)
  run
    "error: RPC failed; HTTP 503 curl 22 The requested URL returned error: 503";
  calls := [];
  ignore (Sources.tags (Sources.create tmp) (url ^ "2"));
  ignore (Sources.tags (Sources.create tmp) (url ^ "2"));
  check_strs "transient" [ "ls-remote"; "ls-remote" ] !calls

(* ------------------------------------------------------------------------ *)
(* GitMirrorTest: a git mirror whose files were deleted (cache pruning) is
   made again, and git never runs in a repository found in a parent directory. *)

let test_broken_mirror_reinitialised () =
  let tmp = tmpdir () in
  let url = "https://example.invalid/repo" in
  let cache =
    Filename.concat (Filename.concat tmp "checkout") ".opam-ci-cache"
  in
  Util.makedirs cache;
  ignore (Proc.run [ "git"; "init"; "-q"; Filename.concat tmp "checkout" ]);
  let d = Sources.gitdir (Sources.create cache) url in
  check_true "stamp" (Sys.file_exists (Filename.concat d "opam-ci-last-used"));
  List.iter (fun f -> Sys.remove (Filename.concat d f)) [ "HEAD"; "config" ];
  let d2 = Sources.gitdir (Sources.create cache) url in
  check_str "same" d d2;
  let cfg =
    Proc.run
      ~env:[ ("GIT_DIR", d) ]
      [ "git"; "config"; "--get"; "remote.origin.url" ]
  in
  check_str "url" url (String.trim cfg.stdout);
  check_true "HEAD" (Sys.file_exists (Filename.concat d "HEAD"))

let rec find_up dir rel n =
  let p = Filename.concat dir rel in
  if Sys.file_exists p then Some p
  else if n = 0 then None
  else find_up (Filename.dirname dir) rel (n - 1)

let run_re = Rx.v {|(\s+)run: \|$|}

let test_prune_step () =
  match find_up (Sys.getcwd ()) ".github/workflows/triage-page.yml" 6 with
  | None -> Alcotest.skip ()
  | Some wf ->
      (* The "Prune cache" step's script (Python: a regex with a backreference). *)
      let lines =
        Array.of_list
          (String.split_on_char '\n'
             (In_channel.with_open_bin wf In_channel.input_all))
      in
      let n = Array.length lines in
      let start = ref (-1) in
      Array.iteri
        (fun i l ->
          if !start < 0 && Util.contains l "- name: Prune cache" then start := i)
        lines;
      if !start < 0 then Alcotest.fail "no Prune cache step";
      let run_line = lines.(!start + 1) in
      let indent =
        match Rx.match_ run_re run_line with
        | Some m -> Rx.group m 1
        | None -> Alcotest.fail "no run: |"
      in
      let body = ref [] and i = ref (!start + 2) in
      while
        !i < n
        && (String.starts_with ~prefix:(indent ^ "  ") lines.(!i)
           || String.trim lines.(!i) = "")
      do
        body :=
          !body @ [ Util.slice ~start:(String.length indent + 2) lines.(!i) ];
        incr i
      done;
      let script = String.concat "\n" !body in
      let tmp = tmpdir () in
      let c = Filename.concat tmp ".opam-ci-cache" in
      let files =
        [
          ("old.json", 40.);
          ("new.json", 1.);
          ("git/used/HEAD", 40.);
          ("git/used/opam-ci-last-used", 2.);
          ("git/unused/HEAD", 40.);
          ("git/unused/opam-ci-last-used", 40.);
          ("git/nostamp/HEAD", 40.);
        ]
      in
      List.iter
        (fun (f, days) ->
          let p = Filename.concat c f in
          Util.makedirs (Filename.dirname p);
          close_out (open_out p);
          age p days)
        files;
      let r = Proc.run ~cwd:tmp [ "bash"; "-e"; "-c"; script ] in
      if r.code <> 0 then Alcotest.failf "prune failed: %s" r.stderr;
      let rec walk rel =
        let p = if rel = "" then c else Filename.concat c rel in
        if Util.is_dir p then
          List.concat_map
            (fun f -> walk (if rel = "" then f else Filename.concat rel f))
            (Util.listdir p)
        else [ rel ]
      in
      check_strs "left"
        [ "git/used/HEAD"; "git/used/opam-ci-last-used"; "new.json" ]
        (List.sort compare (walk ""))

(* ------------------------------------------------------------------------ *)
(* EscapingTest leftovers (the others are in test_page and test_llm). *)

let hostile =
  "@ocaml/maintainers ![x](https://evil.example/x.png) <img src=x> see #123, \
   ocaml/dune#4 GH-5 www.evil.example"

let allowed_tags = Rx.v {|</?(details|summary|sub|br)>|}
let trusted_link = Rx.v {|\]\(https://opam\.ci\.ocaml\.org/[\w.~,+%/=-]+\)|}

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
  let rest =
    Rx.sub allowed_tags "" (Rx.sub trusted_link "]" (outside_code md))
  in
  List.iter
    (fun bad ->
      if Rx.test (Rx.v bad) rest then
        Alcotest.failf "%S in:\n%s\n--- from:\n%s" bad rest md)
    [ {|@\w|}; {|#\d|}; "<"; {|\]\(|}; "://"; {|www\.|}; {|GH-\d|} ]

let hostile_sugg kw =
  let r =
    d
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
  in
  Suggestion.update r kw;
  r

let test_module_find_note () =
  (* Names from ocaml.org's module search (checked by Sources.module_providers) in a note. *)
  let note =
    Rules.module_find_note
      (found
         ~checked:[ ("z_z", "no-docs") ]
         [ ("x_y", "Foo__bar.T"); ("a+b", "A_b") ])
      "T"
      (info ~installed:[ ("x_y", "1") ] ())
      (Some "depends: [\"x_y\"]\n") "pkg_x"
  in
  check_in "x_y 2.0 (as Foo__bar.T, library x_y)" note;
  assert_inert
    (Render.render_comment (String.make 40 'c') (data [])
       [ hostile_sugg [ `Note note ] ])

let test_llm_change_is_code () =
  let r =
    d
      [
        ("variant", s "v");
        ("url", s "https://opam.ci.ocaml.org/x");
        ("note", s "Unrecognised error.");
        ("error", s "E");
      ]
  in
  r.unexplained <- true;
  let res =
    Llm.Answer
      {
        names = None;
        cached = false;
        answer =
          Llm.Answer.of_json
            (`Assoc
               [
                 ("fix_in", s "this-pr");
                 ("confidence", s "low");
                 ("diagnosis", s "d");
                 ("verify", s "v");
                 ( "change_new",
                   s
                     "depexts: [\"a\"] {os-distribution = \"debian\"}\n\
                      ```\n\
                      @x `y`" );
                 ("package", s "x.1");
               ]);
      }
  in
  let saved =
    ( !Llm.fetch_hook,
      !Llm.log_text_hook,
      !Llm.excerpt_hook,
      !Llm.key_error_line_hook )
  in
  (Llm.fetch_hook := fun _ _ -> "log");
  Llm.log_text_hook := Fun.id;
  (Llm.excerpt_hook := fun ?max_output_lines:_ _ -> [ ("error_block", "e") ]);
  (Llm.key_error_line_hook := fun _ -> Some "e");
  (Llm.llm_suggest_hook := fun ?call:_ _ _ _ _ _ _ _ _ _ -> res);
  let tmp = tmpdir () in
  Fun.protect
    (fun () ->
      ignore
        (Llm.apply_llm [ r ]
           (data [ { (job "compilers,5.4,x.1") with variant = "v" } ])
           (Sources.create tmp) tmp [ "x.1" ] 5 "m" false))
    ~finally:(fun () ->
      let a, b, c, e = saved in
      Llm.fetch_hook := a;
      Llm.log_text_hook := b;
      Llm.excerpt_hook := c;
      Llm.key_error_line_hook := e);
  check_in
    {|not checked: ```` depexts: ["a"] {os-distribution = "debian"} ``` @x `y` ````.|}
    (Suggestion.str r.note);
  check_false "new" (r.Suggestion.new_ <> Absent);
  let r' = Suggestion.copy r in
  r'.class_ <- Some "explanation";
  assert_inert (Render.render_comment (String.make 40 'c') (data []) [ r' ])

let test_git_command_sources () =
  (* An unsafe URL never reaches git. *)
  let tmp = tmpdir () in
  (Sources.git_hook :=
     fun ?cwd:_ ?check:_ ?git_dir:_ _ -> Alcotest.fail "git ran");
  let src = Sources.create tmp in
  check_strs "tags" [] (Sources.tags src "--upload-pack=x");
  check_true "file_at" (Sources.file_at src "ssh://e.x/r" "v1" "a" = None);
  check_strs "tree" [] (Sources.tree src "file:///r" "v1");
  let last = ref [] in
  (Sources.git_hook :=
     fun ?cwd:_ ?check:_ ?git_dir:_ args ->
       last := args;
       { Proc.code = 0; stdout = ""; stderr = "" });
  ignore (Sources.tags (Sources.create (tmpdir ())) "https://e.x/r");
  check_strs "args"
    [ "ls-remote"; "--tags"; "--refs"; "--"; "https://e.x/r" ]
    !last

(* ------------------------------------------------------------------------ *)
(* PageMainTest.test_kept_result_during_a_rebuild: its render_page side (the
   state side is in test_page_run). *)

module T = Scripts.Triage_open_prs

let pr_entry ?(kw = []) () =
  let p =
    Dict.of_list
      [
        ("number", `Int 1);
        ("title", s "t");
        ("url", s "u");
        ("draft", `Bool false);
        ("updated", s "2026-11-20T00:00:00Z");
        ("sha", s (String.make 40 'a'));
        ("author", s "x");
        ("labels", `List []);
        ("stale_labels", `List []);
        ("ci_state", s "FAILURE");
        ("ci_url", `Null);
        ("ci_created", s "2026-11-20T10:00:00Z");
      ]
  in
  Dict.update p kw;
  p

let failed_analysis =
  Json.parse
    {|[{"variant": "(analysis)", "status": "failed", "category": "analysis"},
                {"variant": "(lint)", "status": "not-started", "category": "not-started"}]|}

(** One run of triage_open_prs with a fake tool: the state entry of PR 1 (as
    JSON) and the tool's calls. *)
let run_main ?(args = []) ?(summary_ok = true) ?suggest_out tmp prs =
  let state = Filename.concat tmp "prs.json" in
  let calls = ref [] in
  let run_tool a _timeout =
    calls := a :: !calls;
    if List.hd a = "summary" then
      if not summary_ok then
        (None, Some "timeout after 120s", [ ("timeout", 1) ])
      else
        ( Some (`Assoc [ ("jobs", failed_analysis) ]),
          None,
          if List.mem "--refresh" a then [ ("refreshed", 1) ] else [] )
    else
      ( Some
          (Option.value
             ~default:
               (Json.parse
                  {|{"suggestions": [{"rule": "x", "variant": "v"}], "ci_unfinished": 0}|})
             suggest_out),
        None,
        [ ("http", 1) ] )
  in
  let gh_out = Filename.concat tmp "gh-out" in
  close_out (open_out gh_out);
  (T.github_token_hook := fun () -> Some "tok");
  (T.list_open_prs_hook :=
     fun _ ->
       ( List.map (fun p -> Scripts.Page_data.Entry.of_json (Dict.to_json p)) prs,
         1 ));
  T.run_tool_hook := run_tool;
  (T.llm_available_hook := fun () -> false);

  (T.tool_version_hook := fun () -> "v");
  Unix.putenv "GITHUB_OUTPUT" gh_out;
  Unix.putenv "GITHUB_STEP_SUMMARY" (Filename.concat tmp "summary.md");
  ignore
    (capture (fun () ->
         T.main
           ~argv:
             (Array.of_list
                ([
                   "triage_open_prs";
                   "--state";
                   state;
                   "--out";
                   Filename.concat tmp "data.json";
                 ]
                @ args))
           ()));
  ( Json.member "1" (Json.member "prs" (Option.get (Json.load state))),
    List.rev !calls )

let test_kept_result_during_a_rebuild_page () =
  (* A result that says "not this PR's fault", then a rebuild runs and its
     analysis is deferred, or its CI page cannot be read: on hold while CI
     runs, not ready. *)
  let tmp = tmpdir () in
  let out =
    Json.parse
      {|{"suggestions": [{"rule": "x", "variant": "v", "class": "elsewhere", "where_kind": "other-package",
                                            "preexisting": "on master"}], "ci_unfinished": 0}|}
  in
  let e, _ = run_main ~suggest_out:out tmp [ pr_entry () ] in
  check_str "ready" "ready" (R.verdict e);
  let rebuilding =
    pr_entry
      ~kw:
        [ ("ci_state", s "PENDING"); ("ci_created", s "2026-11-20T11:00:00Z") ]
      ()
  in
  let e, calls = run_main ~args:[ "--time-budget"; "-1" ] tmp [ rebuilding ] in
  check_int "deferred" 0 (List.length calls);
  check_str "on hold" "ci" (R.verdict e);
  check_true "running" (R.ci_running e);
  check_in "CI running</span>" (R.render_pr e)
  (* no count of jobs left: unknown *);
  let e, _ = run_main ~summary_ok:false tmp [ rebuilding ] in
  check_str "still on hold" "ci" (R.verdict e);
  (* The rebuild failed again: the result kept is shown for what it is (a rerun to analyse). *)
  let rebuilt = pr_entry ~kw:[ ("ci_created", s "2026-11-20T11:00:00Z") ] () in
  let e, _ =
    run_main ~suggest_out:out ~args:[ "--time-budget"; "-1" ] tmp [ rebuilt ]
  in
  check_str "failure" "FAILURE" (Json.str "ci_state_now" e);
  check_str "ready again" "ready" (R.verdict e)

(* ------------------------------------------------------------------------ *)
(* ProcTest: [Proc.real_run] on real processes (the other tests replace it). *)

let real ?cwd ?env ?input ?timeout args =
  Proc.real_run ?cwd ?env ?input ?timeout args

let test_proc_outputs () =
  let r = real [ "sh"; "-c"; "echo out; echo err >&2; exit 3" ] in
  check_int "code" 3 r.Proc.code;
  check_str "stdout" "out\n" r.stdout;
  check_str "stderr" "err\n" r.stderr

let test_proc_input () =
  (* More than a pipe holds, both ways: stdin must be written while stdout is read. *)
  let big = String.init (1 lsl 20) (fun i -> Char.chr (97 + (i mod 26))) in
  let r = real ~input:big [ "cat" ] in
  check_int "code" 0 r.Proc.code;
  check_true "echoed" (r.stdout = big);
  (* A command that exits without reading its input: no SIGPIPE, no error. *)
  let r = real ~input:big [ "true" ] in
  check_int "code of true" 0 r.code

let test_proc_cwd_env () =
  let tmp = Unix.realpath (Filename.temp_dir "proc" "") in
  let r =
    real ~cwd:tmp
      ~env:[ ("PROC_TEST", "v") ]
      [ "sh"; "-c"; "pwd; echo $PROC_TEST" ]
  in
  check_str "cwd and env" (tmp ^ "\nv\n") r.Proc.stdout;
  check_true "missing program"
    (match real [ "no-such-program-opam-ci" ] with
    | _ -> false
    | exception Unix.Unix_error _ -> true)

let test_proc_timeout () =
  let t0 = Unix.gettimeofday () in
  check_true "timeout raised"
    (match real ~timeout:0.3 [ "sleep"; "5" ] with
    | _ -> false
    | exception Proc.Timeout t -> t = 0.3);
  check_true "killed, not waited for" (Unix.gettimeofday () -. t0 < 2.);
  (* Output before the deadline does not stop it; input too. *)
  check_true "timeout with output"
    (match
       real ~timeout:0.3 ~input:"x" [ "sh"; "-c"; "echo started; sleep 5" ]
     with
    | _ -> false
    | exception Proc.Timeout _ -> true);
  (* A command that finishes in time is not affected by the timeout. *)
  let r = real ~timeout:5. [ "sh"; "-c"; "echo fast" ] in
  check_str "in time" "fast\n" r.Proc.stdout

(* ------------------------------------------------------------------------ *)

let () =
  let p name f = Alcotest.test_case name `Quick (pure f) in
  let n name f = Alcotest.test_case name `Quick (net f) in
  Alcotest.run "triage"
    [
      ( "ChecksTest",
        [
          p "unsatisfiable" test_unsatisfiable;
          p "allows" test_allows;
          p "applied" test_applied;
          p "merge_versions_platforms" test_merge_versions_platforms;
        ] );
      ("SuggestionTest", [ p "round_trip" test_round_trip ]);
      ( "ProcTest",
        [
          p "outputs" test_proc_outputs;
          p "input" test_proc_input;
          p "cwd_env" test_proc_cwd_env;
          p "timeout" test_proc_timeout;
        ] );
      ( "WhereTest",
        [
          p "legacy" test_legacy;
          p "structured_wins" test_structured_wins;
          p "readable_from_fix_in" test_readable_from_fix_in;
          p "variant_names" test_variant_names;
          p "unknown_names_round_trip" test_unknown_names_round_trip;
        ] );
      ("HealthExplainedElsewhereTest", [ p "drop" test_drop ]);
      ( "ConfWithoutDepextTest",
        [
          p "rule" test_conf_rule;
          p "platforms_of_the_group" test_platforms_of_the_group;
          p "depext_entries" test_depext_entries;
          p "opensuse_family" test_opensuse_family;
        ] );
      ( "RulesTest",
        [
          p "jobs_not_run (render_page)" test_jobs_not_run_page;
          p "preexisting_same_failure (render_page)" test_preexisting_page;
          p "unanalysed_order" test_unanalysed_order;
          p "builder_specific_same_arch" test_builder_specific_same_arch;
          p "builder_specific_twin_age" test_builder_specific_twin_age;
          p "windows_pattern" test_windows_pattern;
          p "unknown_outcome" test_unknown_outcome;
          p "experimental_only" test_experimental_only;
          p "experimental_only_cause" test_experimental_only_cause;
          p "unread_on_a_broken_image" test_unread_on_a_broken_image;
          p "compiler_option" test_compiler_option;
          p "pattern_after_a_change_from_the_log"
            test_pattern_after_a_change_from_the_log;
          p "unanalysed_count" test_unanalysed_count;
        ] );
      ("McpFailureTest", [ n "failed_call_counted" test_failed_call_counted ]);
      ( "McpRobustnessTest",
        [
          n "malformed_answers" test_malformed_answers;
          n "circuit_breaker" test_circuit_breaker;
          n "versions_unknown_vs_no_answer" test_versions_unknown_vs_no_answer;
          n "cached_error_expires" test_cached_error_expires;
          n "docs_not_built_expire" test_docs_not_built_expire;
          p "rules_do_not_assert_on_no_answer"
            test_rules_do_not_assert_on_no_answer;
          p "dune_select_module" test_dune_select_module;
        ] );
      ( "ApiHistoryTest",
        [
          n "mapping_and_cache" test_mapping_and_cache;
          n "module_in_no_version" test_module_in_no_version;
          n "ambiguous_module_is_no_answer" test_ambiguous_module_is_no_answer;
          n "ambiguous_module_chosen_by_identifier"
            test_ambiguous_module_chosen_by_identifier;
          n "found_false" test_found_false;
          n "partial_is_a_failure" test_history_partial_is_a_failure;
          n "errors_are_answers" test_errors_are_answers;
          n "failures" test_history_failures;
          n "tool_not_deployed" test_history_tool_not_deployed;
        ] );
      ( "StdlibHistoryTest",
        [
          p "history_answers" test_stdlib_history_answers;
          p "releases_without_docs_read_from_the_mli"
            test_releases_without_docs_read_from_the_mli;
          p "no_history" test_no_history;
        ] );
      ( "ApiHistoryRuleTest",
        [
          p "history_answers" test_lib_history_answers;
          p "combined_with_docs_and_git" test_combined_with_docs_and_git;
          p "removed" test_removed;
          p "without_history" test_without_history;
          p "module_through_an_open" test_module_through_an_open;
          p "kinds" test_kinds;
        ] );
      ( "ModuleFindTest",
        [
          n "match_through_a_wrapper_and_cache"
            test_match_through_a_wrapper_and_cache;
          n "several_matches" test_find_several_matches;
          n "no_match_and_distribution" test_no_match_and_distribution;
          n "partial_is_a_failure" test_find_partial_is_a_failure;
          n "tool_error_vs_failure" test_tool_error_vs_failure;
          n "tool_not_deployed" test_find_tool_not_deployed;
        ] );
      ( "ModuleFindRuleTest",
        [
          p "candidates" test_candidates;
          p "module_through_a_wrapper" test_module_through_a_wrapper;
          p "top_level_module_appeared_later"
            test_top_level_module_appeared_later;
          p "top_level_module_in_a_wrapper" test_top_level_module_in_a_wrapper;
          p "defined_in_the_version_used" test_defined_in_the_version_used;
          p "several_matches" test_rule_several_matches;
          p "no_match" test_rule_no_match;
          p "module_removed_by_a_recent_release"
            test_module_removed_by_a_recent_release;
          p "findlib_library_of_another_package"
            test_findlib_library_of_another_package;
          p "match_outside_the_installed_packages"
            test_match_outside_the_installed_packages;
          p "partial" test_rule_partial;
        ] );
      ( "NetErrorTest",
        [
          n "raw_and_gh_api" test_raw_and_gh_api;
          n "upstream_path_quoted" test_upstream_path_quoted;
          n "not_found_and_master_files_expire"
            test_not_found_and_master_files_expire;
        ] );
      ( "FailedFetchTest",
        [
          n "fetch_counts_failures" test_fetch_counts_failures;
          n "git_failures" test_git_failures;
        ] );
      ( "UnreadableLogTest",
        [ p "unreadable_log_makes_the_analysis_incomplete" test_unreadable_log ]
      );
      ( "CacheWriteTest",
        [
          n "truncated_cache_is_a_miss" test_truncated_cache_is_a_miss;
          n "killed_write_leaves_no_file" test_killed_write_leaves_no_file;
        ] );
      ( "TagsTest",
        [
          n "failed_listing_not_cached" test_failed_listing_not_cached;
          n "unreachable_host_remembered" test_unreachable_host_remembered;
          p "unbound_stdlib_value_without_tags"
            test_unbound_stdlib_value_without_tags;
        ] );
      ("RuleErrorTest", [ p "one_job_fails" test_one_job_fails ]);
      ( "GitMirrorTest",
        [
          n "broken_mirror_reinitialised" test_broken_mirror_reinitialised;
          n "prune_step" test_prune_step;
        ] );
      ( "EscapingTest",
        [
          p "module_find_note" test_module_find_note;
          p "llm_change_is_code (render_comment)" test_llm_change_is_code;
          p "git_command (Sources)" test_git_command_sources;
        ] );
      ( "LlmFallbackTest",
        [
          p "not_asked_while_ci_runs" test_not_asked_while_ci_runs;
          p "flag_set_by_the_rules_and_not_output"
            test_flag_set_by_the_rules_and_not_output;
        ] );
      ( "PageMainTest",
        [
          p "kept_result_during_a_rebuild (render_page)"
            test_kept_result_during_a_rebuild_page;
        ] );
    ]
