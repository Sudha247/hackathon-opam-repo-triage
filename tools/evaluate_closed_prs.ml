(* Evaluate `opam-ci-triage suggest` on merged opam-repository PRs.

   For each recently merged PR whose *initial* version (the commit before
   the first force-push, or else the first commit) failed opam-ci and was
   then changed before the merge:
     1. run `suggest` on the initial commit,
     2. diff the opam files between the initial and the merged version (the
        fix the author actually made),
     3. count a suggestion as a hit when the author changed the same
        dependency or field of the same package. A new upstream archive
        (checksum, x-commit-hash) is counted apart: it is not a change the
        tool suggests. So are the suggestions that compare with
        opam-health-check, whose latest run describes today's master, not
        the one of the PR.

   Usage:
     evaluate-closed-prs [--since 2026-08-01] [--limit 150] [--json out.json]

   Needs a logged-in `gh` (one GraphQL query per 50 PRs; for each
   candidate, its list of files and one GraphQL query per 50 opam files).
   opam-repo-ci deletes old runs, so only recent PRs can be used. *)

open Triage
module Scoring = Scripts.Scoring

let repo = "ocaml/opam-repository"

(* The triage tool: $OPAM_CI_TRIAGE_TOOL, else opam-ci-triage next to this
   executable, else the dune build's, else opam-ci-triage on the PATH. *)
let tool () =
  match Util.getenv_nonempty "OPAM_CI_TRIAGE_TOOL" with
  | Some t -> t
  | None ->
      let dir = Filename.dirname (Util.abspath Sys.executable_name) in
      let near = Filename.concat dir "opam-ci-triage" in
      let dune = Filename.concat dir "../bin/opam_repo_ci_triage.exe" in
      if Sys.file_exists near then near
      else if Sys.file_exists dune then dune
      else "opam-ci-triage"

let query =
  {|
query($q: String!, $after: String) {
  search(query: $q, type: ISSUE, first: 50, after: $after) {
    pageInfo { hasNextPage endCursor }
    nodes { ... on PullRequest {
      number title mergedAt headRefOid
      commits(first: 1) { totalCount nodes { commit { oid status { contexts { context state } } } } }
      timelineItems(itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT], first: 1) {
        nodes { ... on HeadRefForcePushedEvent { beforeCommit { oid status { contexts { context state } } } } }
      }
    } }
  }
}|}

let gh ?input args =
  let out = Proc.run ?input ("gh" :: args) in
  if out.code <> 0 then
    failwith (Util.prefix_chars (String.trim out.stderr) 300);
  Json.parse out.stdout

let ( |. ) = Json.( |. )

let opam_ci_state (commit : Json.t) =
  List.find_map
    (fun c ->
      if Json.str "context" c = "opam-ci" then
        Some (String.lowercase_ascii (Json.str "state" c))
      else None)
    (* GraphQL: FAILURE, REST: failure *)
    (Json.to_list (commit |. "status" |. "contexts"))

let merged_prs since limit =
  let q = Printf.sprintf "repo:%s is:pr is:merged merged:>=%s" repo since in
  let rec go after prs =
    if List.length prs >= limit then prs
    else begin
      let args =
        [ "api"; "graphql"; "-f"; "query=" ^ query; "-f"; "q=" ^ q ]
        @ match after with Some a -> [ "-f"; "after=" ^ a ] | None -> []
      in
      let d = gh args |. "data" |. "search" in
      let prs =
        prs @ List.filter (fun n -> n <> `Null) (Json.to_list (d |. "nodes"))
      in
      if not (Json.to_bool (d |. "pageInfo" |. "hasNextPage")) then prs
      else go (Json.to_str_opt (d |. "pageInfo" |. "endCursor")) prs
    end
  in
  Util.take limit (go None [])

let initial_commit pr =
  match Json.to_list (pr |. "timelineItems" |. "nodes") with
  | fp :: _ when fp |. "beforeCommit" <> `Null -> Some (fp |. "beforeCommit")
  | _ -> (
      match Json.to_list (pr |. "commits" |. "nodes") with
      | n :: _ -> Some (n |. "commit")
      | [] -> None)

module S = Scoring.S

let line_tokens = Scoring.line_tokens

(** The files of a PR's merged version (relative to its own base, so not what
    master gained meanwhile). *)
let pr_files number =
  let out =
    Proc.run
      [
        "gh";
        "api";
        "--paginate";
        Printf.sprintf "repos/%s/pulls/%d/files?per_page=100" repo number;
        "--jq";
        ".[].filename";
      ]
  in
  if out.code <> 0 then
    failwith (Util.prefix_chars (String.trim out.stderr) 300);
  Util.split out.stdout

(** [path -> text] of the files that exist at [commit] (GraphQL, 50 paths per
    query). *)
let blobs commit paths =
  let rec chunks l =
    if l = [] then [] else Util.take 50 l :: chunks (Util.drop 50 l)
  in
  List.concat_map
    (fun chunk ->
      let fields =
        String.concat " "
          (List.mapi
             (fun k p ->
               Printf.sprintf
                 "b%d: object(expression: %s) { ... on Blob { text } }" k
                 (Json.dumps (`String (commit ^ ":" ^ p))))
             chunk)
      in
      let owner, _, name = Util.partition repo "/" in
      let q =
        Printf.sprintf
          "query { repository(owner: \"%s\", name: \"%s\") { %s } }" owner name
          fields
      in
      let r =
        gh [ "api"; "graphql"; "-f"; "query=" ^ q ] |. "data" |. "repository"
      in
      List.concat
        (List.mapi
           (fun k p ->
             match r |. Printf.sprintf "b%d" k with
             | `Null -> []
             | b -> (
                 match b |. "text" with `String t -> [ (p, t) ] | _ -> []))
           chunk))
    (chunks paths)

(* difflib.SequenceMatcher(None, a, b).get_matching_blocks() (autojunk on). *)
let matching_blocks (a : string array) (b : string array) =
  let la = Array.length a and lb = Array.length b in
  let b2j = Hashtbl.create 64 in
  Array.iteri
    (fun j x ->
      Hashtbl.replace b2j x
        (j :: Option.value ~default:[] (Hashtbl.find_opt b2j x)))
    b;
  let b2j =
    Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) b2j []
    |> List.to_seq |> Hashtbl.of_seq
  in
  if lb >= 200 then begin
    let ntest = (lb / 100) + 1 in
    let popular =
      Hashtbl.fold
        (fun k v acc -> if List.length v > ntest then k :: acc else acc)
        b2j []
    in
    List.iter (Hashtbl.remove b2j) popular
  end;
  let find_longest alo ahi blo bhi =
    let besti = ref alo and bestj = ref blo and bestsize = ref 0 in
    let j2len = ref (Hashtbl.create 8) in
    for i = alo to ahi - 1 do
      let newj2len = Hashtbl.create 8 in
      (* the positions of a.(i) in b, in order: those before blo skipped,
         up to bhi *)
      let rec scan = function
        | [] -> ()
        | j :: rest when j < blo -> scan rest
        | j :: _ when j >= bhi -> ()
        | j :: rest ->
            let k =
              Option.value ~default:0 (Hashtbl.find_opt !j2len (j - 1)) + 1
            in
            Hashtbl.replace newj2len j k;
            if k > !bestsize then (
              besti := i - k + 1;
              bestj := j - k + 1;
              bestsize := k);
            scan rest
      in
      scan (Option.value ~default:[] (Hashtbl.find_opt b2j a.(i)));
      j2len := newj2len
    done;
    while !besti > alo && !bestj > blo && a.(!besti - 1) = b.(!bestj - 1) do
      decr besti;
      decr bestj;
      incr bestsize
    done;
    while
      !besti + !bestsize < ahi
      && !bestj + !bestsize < bhi
      && a.(!besti + !bestsize) = b.(!bestj + !bestsize)
    do
      incr bestsize
    done;
    (!besti, !bestj, !bestsize)
  in
  let blocks = ref [] in
  let queue = ref [ (0, la, 0, lb) ] in
  while !queue <> [] do
    let alo, ahi, blo, bhi = List.hd !queue in
    queue := List.tl !queue;
    let i, j, k = find_longest alo ahi blo bhi in
    if k > 0 then begin
      blocks := (i, j, k) :: !blocks;
      if alo < i && blo < j then queue := (alo, i, blo, j) :: !queue;
      if i + k < ahi && j + k < bhi then
        queue := (i + k, ahi, j + k, bhi) :: !queue
    end
  done;
  List.sort compare !blocks

(** The lines [difflib.unified_diff(a, b, lineterm="", n=0)] prints with a "-"
    or "+" prefix (headers included, as "---" / "+++" lines). *)
let unified_diff_lines a b =
  let a = Array.of_list a and b = Array.of_list b in
  let blocks = matching_blocks a b in
  let in_a = Array.make (Array.length a) false
  and in_b = Array.make (Array.length b) false in
  List.iter
    (fun (i, j, k) ->
      for d = 0 to k - 1 do
        in_a.(i + d) <- true;
        in_b.(j + d) <- true
      done)
    blocks;
  let changed = ref [] in
  Array.iteri
    (fun i l -> if not in_a.(i) then changed := ("-" ^ l) :: !changed)
    a;
  Array.iteri
    (fun j l -> if not in_b.(j) then changed := ("+" ^ l) :: !changed)
    b;
  if !changed = [] then [] else "---" :: "+++" :: List.rev !changed

let packages_opam_re = Rx.v "packages/[^/]+/[^/]+/opam$"

(** [package.version -> tokens] changed in the PR's opam files between its
    initial and merged versions. Each file is compared as a whole: a compare of
    the two commits would go through their merge base, which for a PR rebased
    before its force-push shows the PR's files as added and master's changes as
    the author's. *)
let actual_fix number initial final =
  let paths =
    List.filter (fun f -> Rx.test_match packages_opam_re f) (pr_files number)
  in
  let packages =
    List.map (fun path -> List.nth (String.split_on_char '/' path) 2) paths
  in
  let old = blobs initial paths and new_ = blobs final paths in
  let changed = Odict.create () in
  List.iter
    (fun path ->
      match (List.assoc_opt path old, List.assoc_opt path new_) with
      | Some o, Some n ->
          let toks =
            List.fold_left
              (fun acc line ->
                if
                  String.length line >= 1
                  && (line.[0] = '+' || line.[0] = '-')
                  && not
                       (String.starts_with ~prefix:"+++" line
                       || String.starts_with ~prefix:"---" line)
                then S.union acc (line_tokens (Util.drop_chars line 1))
                else acc)
              S.empty
              (unified_diff_lines (Util.splitlines o) (Util.splitlines n))
          in
          Odict.set changed (List.nth (String.split_on_char '/' path) 2) toks
      | _ ->
          () (* packages added/removed between the two versions are not fixes *))
    paths;
  (Odict.to_list changed, packages)

(* A new upstream archive (a fix made upstream, then released again): not a
   change of the opam file that the tool could suggest. Such a package is not
   scored at all: its other changes come with the new release. *)
let new_archive = S.of_list [ "field:checksum"; "field:x-commit-hash" ]

(* Changes that fix no CI failure: what opam-repository's review bot asks for
   (x-maintenance-intent), and the description. *)
let not_fixes = S.of_list [ "field:x-maintenance-intent"; "field:description" ]

(* The rules that compare with opam-health-check's latest run (see main). *)
let health_rules : Rule.t list = [ Health_check_new ]
let error_re = Rx.v {|Error in (\S+?):|}

(** Packages of the PR that failed in that CI run: non-experimental build, test
    or solver failures, or lint errors. *)
let failing_packages sha =
  let out =
    try Some (Proc.run [ tool (); "summary"; sha; "--json" ]) with _ -> None
  in
  match Option.bind out (fun o -> Json.parse_opt o.Proc.stdout) with
  | Some d when Json.mem "jobs" d ->
      let jobs = Json.list "jobs" d in
      let failed =
        List.filter_map
          (fun j ->
            if
              List.mem
                (Ci.Kind.of_string (Json.str "kind" j))
                [ Build; Tests; Lower_bounds ]
              && (not (Json.bool "experimental" j))
              && List.mem
                   (Ci.Category.of_string (Json.str "category" j))
                   Ci.failing
            then Some (Json.str "target" j)
            else None)
          jobs
      in
      let msgs =
        List.concat_map
          (fun j ->
            List.filter_map
              (fun m ->
                Option.map (fun mm -> Rx.group mm 1) (Rx.match_ error_re m))
              (Json.to_str_list (j |. "messages")))
          jobs
      in
      S.of_list (failed @ msgs)
  | _ -> S.empty

let strs s = Json.strings (S.elements s)

let run ~since ~limit ~max_ ~json () =
  let prs = merged_prs since limit in
  let cands =
    List.filter_map
      (fun pr ->
        match initial_commit pr with
        | Some init
          when Json.str "oid" init <> Json.str "headRefOid" pr
               && opam_ci_state init = Some "failure" ->
            Some (pr, Json.str "oid" init)
        | _ -> None)
      prs
  in
  Printf.eprintf
    "%d merged PRs since %s; %d whose initial version failed opam-ci and was \
     changed before the merge\n\
     %!"
    (List.length prs) since (List.length cands);
  let totals =
    Odict.of_list
      [
        ("scoring", Scoring.version);
        ("scored", 0);
        ("hit", 0);
        ("fix_tokens", 0);
        ("fix_covered", 0);
        ("new_archive", 0);
        ("health_scored", 0);
        ("health_hit", 0);
      ]
  in
  let tadd k n = Odict.set totals k (Option.get (Odict.get totals k) + n) in
  let rows = ref [] in
  List.iter
    (fun (pr, init) ->
      let number = Json.to_int (pr |. "number") in
      let fix, pr_packages =
        actual_fix number init (Json.str "headRefOid" pr)
      in
      let fix =
        List.filter_map
          (fun (p, v) ->
            let v = S.diff v not_fixes in
            if S.is_empty v then None else Some (p, v))
          fix
      in
      (* Packages that got a new upstream archive: not scored. *)
      let archived =
        List.filter_map
          (fun (p, v) ->
            if S.is_empty (S.inter v new_archive) then None else Some p)
          fix
      in
      if fix <> [] then begin
        (* (else the author changed nothing in opam files, e.g. CI rerun only) *)
        let out =
          Proc.run
            [ tool (); "suggest"; init; "--max"; string_of_int max_; "--json" ]
        in
        match
          Option.bind (Json.parse_opt out.stdout) (fun d ->
              if Json.mem "suggestions" d then Some (Json.list "suggestions" d)
              else None)
        with
        | None ->
            rows :=
              !rows
              @ [
                  `Assoc
                    [
                      ("pr", `Int number);
                      ( "error",
                        `String (Util.suffix_chars (String.trim out.stderr) 200)
                      );
                    ];
                ]
        | Some sugg ->
            let sugg = List.map Suggestion.of_json sugg in
            let has_change = Scoring.has_change in
            (* The packages a suggestion changes: the failing ones (several
               versions when merged: "a.1, a.2"), or the other package of
               the PR where it places the fix (#31170: a bound on
               console-eio, modified by k7's PR). *)
            let packages (s : Suggestion.t) =
              match (Where.where_kind s, Where.where_pkg s) with
              | Pr, _ ->
                  Util.split_on ", "
                    (Option.value ~default:"" (Suggestion.get s.failing))
              | Other_package, Some p when List.mem p pr_packages -> [ p ]
              | _ -> []
            in
            let scored =
              List.filter_map
                (fun (s : Suggestion.t) ->
                  let pkgs =
                    List.filter
                      (fun p -> p <> "" && not (List.mem p archived))
                      (packages s)
                  in
                  if pkgs = [] || not (has_change s) then None
                  else begin
                    let pkg = Json.opt_string (Suggestion.get s.failing) in
                    let toks = Scoring.suggestion_tokens s in
                    let fixed =
                      List.fold_left
                        (fun acc p ->
                          S.union acc
                            (Option.value ~default:S.empty
                               (List.assoc_opt p fix)))
                        S.empty pkgs
                    in
                    let hit = not (S.is_empty (S.inter toks fixed)) in
                    Some
                      ( pkgs,
                        s.rule,
                        `Assoc
                          [
                            ("package", pkg);
                            ( "rule",
                              Json.opt_string (Option.map Rule.to_string s.rule)
                            );
                            ( "new",
                              `String
                                (Util.prefix_chars
                                   (if Suggestion.is_set s.new_ then
                                      Suggestion.str_field s.new_
                                    else
                                      match s.advice with
                                      | Some a when Scoring.is_advice_only s ->
                                          "(advice) " ^ a
                                      | _ -> Suggestion.str_field s.new_)
                                   90) );
                            ("hit", `Bool hit);
                            ("tokens", strs toks);
                          ],
                        hit,
                        toks )
                  end)
                sugg
            in
            (* The rules comparing with opam-health-check read today's run, not
             the one at the time of the PR: master has changed since
             (console-eio, #31170, builds on today's master thanks to a bound
             added after the PR). Their suggestions are counted apart. *)
            let health =
              List.filter
                (fun (_, rule, _, _, _) ->
                  Option.fold ~none:false
                    ~some:(fun r -> List.mem r health_rules)
                    rule)
                scored
            in
            let scored =
              List.filter
                (fun (_, rule, _, _, _) ->
                  not
                    (Option.fold ~none:false
                       ~some:(fun r -> List.mem r health_rules)
                       rule))
                scored
            in
            (* Recall: only the author's changes in packages that failed in the
             initial run (rebases, version bumps and metadata edits elsewhere
             are not fixes of a CI failure). *)
            let failed = failing_packages init in
            let fix_failed =
              List.filter
                (fun (p, _) -> S.mem p failed && not (List.mem p archived))
                fix
            in
            tadd "new_archive"
              (List.length
                 (List.filter
                    (fun p -> S.mem p failed)
                    (List.sort_uniq compare archived)));
            let covered =
              List.length
                (List.concat_map
                   (fun (p, ts) ->
                     List.filter
                       (fun t ->
                         List.exists
                           (fun (pkgs, _, _, _, toks) ->
                             List.mem p pkgs && S.mem t toks)
                           scored)
                       (S.elements ts))
                   fix_failed)
            in
            let n_fix =
              List.fold_left (fun a (_, v) -> a + S.cardinal v) 0 fix_failed
            in
            let count_hits l =
              List.length (List.filter (fun (_, _, _, h, _) -> h) l)
            in
            tadd "health_scored" (List.length health);
            tadd "health_hit" (count_hits health);
            tadd "scored" (List.length scored);
            tadd "hit" (count_hits scored);
            tadd "fix_tokens" n_fix;
            tadd "fix_covered" covered;
            rows :=
              !rows
              @ [
                  `Assoc
                    [
                      ("pr", `Int number);
                      ("title", pr |. "title");
                      ("initial", `String init);
                      ("final", pr |. "headRefOid");
                      ( "actual_fix",
                        `Assoc (List.map (fun (k, v) -> (k, strs v)) fix) );
                      ( "fix_in_failing_packages",
                        `Assoc (List.map (fun (k, v) -> (k, strs v)) fix_failed)
                      );
                      ( "suggestions",
                        `List (List.map (fun (_, _, j, _, _) -> j) scored) );
                      ( "health_suggestions",
                        `List (List.map (fun (_, _, j, _, _) -> j) health) );
                      ( "explanations",
                        `Int
                          (List.length
                             (List.filter (fun s -> not (has_change s)) sugg))
                      );
                    ];
                ]
      end)
    cands;
  (* A value in the report, "?" when absent. *)
  let text (v : Json.t) =
    match v with
    | `String s -> s
    | `Int n -> string_of_int n
    | `Null -> "?"
    | v -> Json.dumps v
  in

  List.iter
    (fun r ->
      if Json.mem "error" r then
        Printf.printf "\n#%s: suggest failed: %s\n"
          (text (r |. "pr"))
          (Json.str "error" r)
      else begin
        Printf.printf "\n#%s %s\n"
          (text (r |. "pr"))
          (Util.prefix_chars (Json.str "title" r) 70);
        List.iter
          (fun (p, toks) ->
            Printf.printf "  author changed %s: %s\n" p
              (String.concat ", " (Json.to_str_list toks)))
          (Json.assoc "actual_fix" r);
        List.iter
          (fun s ->
            Printf.printf "  %s [%s] %s: %s\n"
              (if Json.bool "hit" s then "HIT " else "miss")
              (text (s |. "rule"))
              (text (s |. "package"))
              (Json.str "new" s))
          (Json.list "suggestions" r);
        List.iter
          (fun s ->
            Printf.printf "  (%s, not counted) [%s] %s: %s\n"
              (if Json.bool "hit" s then "hit" else "miss")
              (text (s |. "rule"))
              (text (s |. "package"))
              (Json.str "new" s))
          (Json.list "health_suggestions" r);
        if
          Json.list "suggestions" r = []
          && Json.list "health_suggestions" r = []
        then
          Printf.printf "  (no suggested change; %d explanations)\n"
            (Json.to_int (r |. "explanations"))
      end)
    !rows;
  let t k = Option.get (Odict.get totals k) in
  Printf.printf "\nPRs evaluated: %d\n"
    (List.length (List.filter (fun r -> not (Json.mem "error" r)) !rows));
  Printf.printf
    "suggested changes to the PR's own files: %d, matching the author's fix: \
     %d%s\n"
    (t "scored") (t "hit")
    (if t "scored" > 0 then Printf.sprintf " (%d%%)" (100 * t "hit" / t "scored")
     else "");
  Printf.printf
    "author's changes in packages that failed (dependency/field per package): \
     %d, anticipated: %d%s\n"
    (t "fix_tokens") (t "fix_covered")
    (if t "fix_tokens" > 0 then
       Printf.sprintf " (%d%%)" (100 * t "fix_covered" / t "fix_tokens")
     else "");
  Printf.printf
    "packages that failed and got a new upstream archive (not counted above): %d\n"
    (t "new_archive");
  Printf.printf
    "suggestions from today's opam-health-check (not counted above): %d, \
     matching the author's fix: %d\n"
    (t "health_scored") (t "health_hit");
  match json with
  | Some path ->
      let oc = open_out_bin path in
      output_string oc
        (Json.dumps ~indent:2
           (`Assoc
              [
                ( "totals",
                  `Assoc
                    (List.map
                       (fun (k, v) -> (k, `Int v))
                       (Odict.to_list totals)) );
                ("prs", `List !rows);
              ]));
      close_out oc
  | None -> ()

let () =
  let open Cmdliner in
  let since =
    Arg.(
      value & opt string "2026-08-01"
      & info [ "since" ] ~docv:"SINCE"
          ~doc:"the PRs merged since this date (YYYY-MM-DD)")
  in
  let limit =
    Arg.(
      value & opt int 150
      & info [ "limit" ] ~docv:"LIMIT" ~doc:"at most this many PRs")
  in
  let max_ =
    Arg.(value & opt int 6 & info [ "max" ] ~docv:"MAX" ~doc:"suggest --max")
  in
  let json =
    Arg.(
      value
      & opt (some string) None
      & info [ "json" ] ~docv:"JSON" ~doc:"write the results to this file")
  in
  let term =
    Term.(
      const (fun since limit max_ json -> run ~since ~limit ~max_ ~json ())
      $ since $ limit $ max_ $ json)
  in
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "evaluate-closed-prs"
             ~doc:
               "Evaluate suggest on merged PRs: compare its suggestions on the \
                initial commit with the fix the author made before the merge.")
          term))
