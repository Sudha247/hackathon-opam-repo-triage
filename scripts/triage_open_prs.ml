open Triage

let owner, name = ("ocaml", "opam-repository")
let context = "opam-ci"
let max_attempts = 3

(* Attempts expire a day after the last one: a timeout or a server that failed
   (ocaml.org, opam.ci.ocaml.org) may be gone by then. *)
let attempts_expire = 24. *. 3600.

(* Failed-job categories (from [summary]) that are not about the package itself. *)
let noise = [ "passed"; "network"; "cancelled"; "not-available"; "ignored" ]

(* Transient LLM failures in a row after which the LLM is not asked again in
   a run (LLM_BREAKER of the tool's LLM fallback). *)
let llm_breaker = Triage.Llm.llm_breaker

let query =
  {|
query($owner: String!, $name: String!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequests(states: OPEN, first: 100, after: $cursor,
                 orderBy: {field: UPDATED_AT, direction: DESC}) {
      pageInfo { hasNextPage endCursor }
      nodes {
        number title url isDraft updatedAt headRefOid
        author { login }
        labels(first: 20) { nodes { name } }
        timelineItems(last: 30, itemTypes: [LABELED_EVENT, PULL_REQUEST_COMMIT, HEAD_REF_FORCE_PUSHED_EVENT,
                                            ISSUE_COMMENT]) {
          nodes {
            __typename
            ... on LabeledEvent { createdAt label { name } }
            ... on PullRequestCommit { commit { committedDate } }
            ... on HeadRefForcePushedEvent { createdAt }
            ... on IssueComment { createdAt author { login } }
          }
        }
        commits(last: 1) { nodes { commit { oid status { contexts { context state targetUrl createdAt } } } } }
      }
    }
  }
}
|}

(** The current time, as "2026-11-20T10:00:00+00:00" (Python's
    [datetime.now(timezone.utc).replace(microsecond=0).isoformat()]). *)
let now () =
  let t = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d+00:00" (t.tm_year + 1900)
    (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

let sleep_hook = ref Unix.sleepf

exception Graphql_error of string

let graphql query variables token : Json.t =
  let body =
    Json.dumps (`Assoc [ ("query", `String query); ("variables", variables) ])
  in
  let headers =
    [
      ("User-Agent", Net.user_agent);
      ("Authorization", "Bearer " ^ token);
      ("Content-Type", "application/json");
    ]
  in
  let rec attempt k =
    match
      let d =
        Json.parse
          (Http.urlopen ~meth:"POST" ~headers ~body ~timeout:60.
             "https://api.github.com/graphql")
      in
      let data = Json.member "data" d in
      if Json.truthy (Json.member "errors" d) then begin
        (* Partial errors (a node GitHub could not resolve): the data is used
           when every top-level field is there. *)
        if
          (not (Json.truthy data))
          || List.exists (fun (_, v) -> v = `Null) (Json.to_assoc data)
        then
          raise
            (Graphql_error
               ("GraphQL errors: "
               ^ Util.prefix_chars (Json.py_repr (Json.member "errors" d)) 500));
        Printf.eprintf "GitHub GraphQL errors, using the data returned: %s\n%!"
          (Util.prefix_chars (Json.py_repr (Json.member "errors" d)) 300)
      end;
      (match data with
      | `Assoc _ -> ()
      | _ -> raise (Graphql_error "GraphQL answer without data"));
      data
    with
    | data -> data
    | exception
        (( Http.Http_error _ | Http.Net_error _ | Graphql_error _
         | Yojson.Json_error _ | Sys_error _ ) as e) ->
        if k = 2 then raise e;
        let msg =
          match e with Graphql_error m -> m | e -> Http.error_text e
        in
        Printf.eprintf "GitHub API error (%s), retrying\n%!" msg;
        !sleep_hook (float_of_int (10 * (k + 1)));
        attempt (k + 1)
  in
  attempt 0

let graphql_hook = ref graphql

(* Labels that ask the PR's author to act. Such a label is often left in
   place after the author has acted: then it may be stale. *)
let author_labels = [ "needs reporter action" ]

let stale_labels (node : Json.t) =
  let open Json in
  let author = str_opt "login" (member "author" node) in
  let items = to_list (member "nodes" (member "timelineItems" node)) in
  let acts =
    List.filter_map
      (fun i ->
        match str_opt "__typename" i with
        | Some "PullRequestCommit" ->
            str_opt "committedDate" (member "commit" i)
        | Some "HeadRefForcePushedEvent" -> str_opt "createdAt" i
        | Some "IssueComment"
          when author <> None && str_opt "login" (member "author" i) = author ->
            str_opt "createdAt" i
        | _ -> None)
      items
  in
  let acts = List.filter (fun a -> a <> "") acts in
  let last_act =
    List.fold_left
      (fun m a -> match m with Some b when b >= a -> m | _ -> Some a)
      None acts
  in
  let current =
    List.filter_map
      (fun lb -> str_opt "name" lb)
      (to_list (member "nodes" (member "labels" node)))
  in
  List.filter
    (fun name ->
      List.mem name current (* else: added, then removed *)
      &&
      let added =
        List.filter_map
          (fun i ->
            if
              str_opt "__typename" i = Some "LabeledEvent"
              && str_opt "name" (member "label" i) = Some name
            then str_opt "createdAt" i
            else None)
          items
      in
      match (added, last_act) with
      | _ :: _, Some last -> last > List.fold_left max "" added
      | _ -> false)
    author_labels

let list_open_prs token =
  let open Json in
  let prs = ref [] in
  let rec page cursor calls =
    let calls = calls + 1 in
    let pg =
      !graphql_hook query
        (`Assoc
           [
             ("owner", `String owner);
             ("name", `String name);
             ("cursor", opt_string cursor);
           ])
        token
    in
    let conn = member "pullRequests" (member "repository" pg) in
    List.iter
      (fun n ->
        if n <> `Null then begin
          (* left out of a partial answer otherwise *)
          let commits = to_list (member "nodes" (member "commits" n)) in
          let head =
            match commits with c :: _ -> member "commit" c | [] -> `Assoc []
          in
          let ci = ref `Null in
          if member "oid" head = member "headRefOid" n then
            List.iter
              (fun c -> if member "context" c = `String context then ci := c)
              (to_list (member "contexts" (member "status" head)));
          let ci = !ci in
          let p =
            Dict.of_list
              [
                ("number", member "number" n);
                ("title", member "title" n);
                ("url", member "url" n);
                ("draft", member "isDraft" n);
                ("updated", member "updatedAt" n);
                ("sha", member "headRefOid" n);
                ("author", member "login" (member "author" n));
                ( "labels",
                  `List
                    (List.map
                       (fun lb -> member "name" lb)
                       (to_list (member "nodes" (member "labels" n)))) );
                ("stale_labels", strings (stale_labels n));
                ("ci_state", member "state" ci);
                ("ci_url", member "targetUrl" ci);
                ("ci_created", member "createdAt" ci);
              ]
          in
          prs := p :: !prs
        end)
      (to_list (member "nodes" conn));
    let info = member "pageInfo" conn in
    if not (to_bool (member "hasNextPage" info)) then (List.rev !prs, calls)
    else page (str_opt "endCursor" info) calls
  in
  page None 0

let list_open_prs_hook = ref list_open_prs

(* The fields of a failed attempt to redo an analysis whose previous result is kept. *)
let last_error =
  [
    "last_error";
    "last_error_at";
    "last_error_ci_state";
    "last_error_ci_created";
    "last_error_refreshed";
  ]

(** Seconds since the date [iso] (from [now ()]), or None. *)
let age ?now_ iso =
  match iso with
  | None -> None
  | Some iso -> (
      match Date.parse_iso iso with
      | Some t -> Some (Option.value ~default:(Unix.gettimeofday ()) now_ -. t)
      | None -> None)

let get_result d = Dict.get d "result"
let has_result d = get_result d <> None

let attempts_used ?now_ (prev : Dict.t option) =
  match prev with
  | None -> 0
  | Some prev -> (
      let at =
        match Dict.str_opt prev "last_error_at" with
        | Some s when s <> "" -> Some s
        | _ -> Dict.str_opt prev "analysed_at"
      in
      match age ?now_ at with
      | Some last when last >= attempts_expire -> 0
      | _ -> Dict.int prev "attempts")

let ci_changed (prev : Dict.t option) (p : Dict.t) =
  match prev with
  | None -> false
  | Some prev ->
      if Dict.str_opt prev "sha" <> Dict.str_opt p "sha" then false
      else
        let pc = Dict.str p "ci_created" and qc = Dict.str prev "ci_created" in
        if pc <> "" && qc = "" then true
        else
          let newer = pc <> "" && qc <> "" && pc > qc in
          newer || Dict.get prev "ci_state" <> Dict.get p "ci_state"

(** Whether the last failed attempt to redo [prev]'s analysis (its result kept,
    see main) was for the CI run of [p]: the same commit, status and status date
    ([last_error_ci_state], [last_error_ci_created]). *)
let tried_run (prev : Dict.t option) (p : Dict.t) =
  match prev with
  | None -> false
  | Some prev ->
      Dict.truthy prev "last_error"
      && Dict.str_opt prev "sha" = Dict.str_opt p "sha"
      && Dict.mem prev "last_error_ci_state"
      && Dict.raw prev "last_error_ci_state"
         = Some (Option.value ~default:`Null (Dict.raw p "ci_state"))
      && Dict.get prev "last_error_ci_created" = Dict.get p "ci_created"

let needs_refresh (prev : Dict.t option) (p : Dict.t) =
  match prev with
  | None -> true
  | Some pv ->
      if Dict.str_opt pv "sha" <> Dict.str_opt p "sha" then true
      else if Dict.truthy pv "refresh_pending" then true
      else if tried_run prev p && Dict.truthy pv "last_error_refreshed" then
        false
      else ci_changed prev p

(** [ci_state_now]: opam-ci's status as listed now, when the result kept in
    [entry] was made for another CI run (a rebuild whose analysis was deferred
    or failed, or whose CI page could not be read): the page shows such a PR on
    hold while that status is pending, not ready from the previous run's result
    (see Render_page.ci_running). *)
let note_listed_ci (entry : Dict.t) (p : Dict.t) =
  if has_result entry && ci_changed (Some entry) p then
    Dict.set entry "ci_state_now"
      (Option.value ~default:`Null (Dict.raw p "ci_state"))
  else Dict.remove entry "ci_state_now"

let ci_running (e : Dict.t) =
  if Dict.mem e "ci_state_now" then
    Dict.raw e "ci_state_now" = Some (`String "PENDING")
  else
    Dict.str_opt e "ci_state" = Some "PENDING"
    && Dict.raw e "ci_unfinished" <> Some (`Int 0)

(** The analysis that [since_last_push] compares commit [sha]'s with: the
    previous head's result, or the one kept when the first analysis of a newer
    commit was deferred or failed ([since_base]), or None. Only what
    [since_last_push] reads is kept. *)
let since_base (prev : Dict.t option) sha : Json.t option =
  match prev with
  | Some pv when Dict.str_opt pv "sha" <> Some sha && has_result pv ->
      let sugg =
        List.map
          (fun s ->
            let s = Suggestion.of_json s in
            `Assoc
              (List.filter_map
                 (fun (k, v) -> Option.map (fun v -> (k, `String v)) v)
                 [
                   ("rule", Option.map Rule.to_string s.rule);
                   ("title", s.title);
                   ("failing", Suggestion.get s.Suggestion.failing);
                   ("job", s.Suggestion.job);
                 ]))
          (Json.list "suggestions" (Option.get (get_result pv)))
      in
      Some
        (`Assoc
           [
             ("sha", Option.value ~default:`Null (Dict.raw pv "sha"));
             ("result", `Assoc [ ("suggestions", `List sugg) ]);
           ])
  | _ -> (
      match Option.bind prev (fun pv -> Dict.get pv "since_base") with
      | Some base when Json.truthy base && Json.member "sha" base <> `String sha
        ->
          Some base
      | _ -> None)

(** A digest of the job statuses of a CI page (from [summary --json]). While
    opam-ci's status is pending, the analysis of a PR is kept as long as the
    digest is unchanged: the status stays pending for good when a job never
    starts (lint after a failed analysis). *)
let ci_digest (summary : Json.t option) =
  let jobs = match summary with Some s -> Json.list "jobs" s | None -> [] in
  let jobs =
    List.sort compare
      (List.map (fun j -> [ Json.str "variant" j; Json.str "status" j ]) jobs)
  in
  let s = Json.dumps (`List (List.map Json.strings jobs)) in
  String.sub Digestif.SHA256.(to_hex (digest_string s)) 0 16

(* Why the LLM was turned off in a run, in words (for the run's summary and the page). *)
let llm_off_text = function
  | "credentials" ->
      "Anthropic refused the API key (expired or invalid): check the \
       ANTHROPIC_API_KEY secret."
  | _ ->
      "Anthropic's API failed several times in a row; the next update tries \
       again."

let reuse ?now_ ?digest (prev : Dict.t option) (p : Dict.t) version llm =
  let commit =
    match prev with
    | Some pv ->
        Dict.str_opt pv "sha" = Dict.str_opt p "sha"
        && Dict.str_opt pv "tool_version" = Some version
        && not (ci_changed prev p)
    | None -> false
  in
  let pv = match prev with Some pv -> pv | None -> Dict.create () in
  let same =
    if Dict.str_opt p "ci_state" = Some "PENDING" then
      (* Analysed again as jobs finish; a failed analysis is retried
         max_attempts times at most, even as jobs finish. *)
      commit
      && (digest <> None
          && Option.map (fun d -> `String d) digest = Dict.get pv "ci_digest"
         || ((not (has_result pv)) && Dict.truthy pv "error"))
    else commit
  in
  let same_commit = ref (same && has_result pv) in
  let same = ref same in
  (* With the LLM on, a PR analysed without it (key added later, the run's
     budget spent, --llm-max cut to that budget, an API error) is analysed
     again; the logs come from the cache. Not while the CI page it saw had
     unfinished jobs (whatever opam-ci's status): the LLM is only asked once
     CI has finished. An analysis that asked the LLM and got no new answer
     ([llm_calls]: API errors, the LLM off for the run) is a retry: the
     attempts add up, max_attempts a day at most. *)
  let llm_retry = ref false in
  if
    !same && llm
    && (not (Dict.truthy pv "llm"))
    && not (Dict.truthy pv "ci_unfinished")
  then
    begin if
      Dict.get pv "llm_unanswered" <> None && not (Dict.truthy pv "llm_calls")
    then begin
      llm_retry := attempts_used ?now_ prev < max_attempts;
      same := not !llm_retry
    end
    else same := false
    end;
  (* An analysis made while ocaml.org's MCP server failed lacks versions,
     docs or health-check results: done again, a few times a day at most. *)
  let retry =
    ref
      (!llm_retry
      || !same
         && (Dict.truthy pv "mcp_errors" || Dict.truthy pv "net_errors")
         && attempts_used ?now_ prev < max_attempts)
  in
  if !retry then same := false;
  (* A git host that failed is not asked again for GIT_DOWN_MAX_AGE
     (Sources.git_down): an analysis that skipped one is done again once
     that has passed (then once a day while the host stays down). *)
  if
    !same
    && Dict.truthy pv "git_unreachable"
    && Option.value ~default:0. (age ?now_ (Dict.str_opt pv "analysed_at"))
       >= Net.git_down_max_age
  then same := false;
  (* A failed attempt to redo an analysis kept the previous result: tried
     again max_attempts times at most, then the result is kept as it is. A
     new CI run (a rebuild) is always analysed: this only applies to the run
     the result saw, or to the one the failed attempts were for (the result
     kept still says it saw the previous run: ci_changed holds). *)
  if
    (not !same) && prev <> None
    && Dict.str_opt pv "sha" = Dict.str_opt p "sha"
    && Dict.truthy pv "last_error"
    && has_result pv
    && ((not (ci_changed prev p)) || tried_run prev p)
  then
    begin if attempts_used ?now_ prev >= max_attempts then (
      same := true;
      same_commit := true;
      retry := false)
    else retry := true
    end;
  (!same, !same_commit, !retry)

let keep_failed ?now_ prev = attempts_used ?now_ prev >= max_attempts

type counts = (string * int) list

let count (net : counts) k = Option.value ~default:0 (List.assoc_opt k net)

(** The request counts the tool wrote to [path] (OPAM_CI_NET_FILE), or []. *)
let read_counts path : counts =
  match Json.load path with
  | Some (`Assoc kv) ->
      List.filter_map
        (fun (k, v) -> match v with `Int n -> Some (k, n) | _ -> None)
        kv
  | _ -> []

(** The triage tool: OPAM_CI_TRIAGE_TOOL, else opam-ci-triage next to this
    program, else the dune build's ../../bin/opam_repo_ci_triage.exe, else
    opam-ci-triage on the PATH. *)
let tool_path () =
  match Util.getenv_nonempty "OPAM_CI_TRIAGE_TOOL" with
  | Some t -> t
  | None -> (
      let dir = Filename.dirname (Util.abspath Sys.executable_name) in
      let candidates =
        [
          Filename.concat dir "opam-ci-triage";
          Filename.concat
            (Filename.concat (Filename.dirname (Filename.dirname dir)) "bin")
            "opam_repo_ci_triage.exe";
          Filename.concat
            (Filename.concat (Filename.dirname dir) "bin")
            "opam_repo_ci_triage.exe";
        ]
      in
      match List.find_opt Sys.file_exists candidates with
      | Some t -> t
      | None -> "opam-ci-triage")

let set_count (net : counts) k v =
  if List.mem_assoc k net then
    List.map (fun (k', x) -> if k' = k then (k', v) else (k', x)) net
  else net @ [ (k, v) ]

let network_requests_re = Rx.v "network requests: (.*)"
let count_re = Rx.v {|([\w-]+)=(\d+)|}

let run_tool ?tool args timeout : Json.t option * string option * counts =
  let counts = Filename.temp_file "opam-ci-net-" ".json" in
  let tool = match tool with Some t -> t | None -> tool_path () in
  let res =
    Fun.protect ~finally:(fun () -> Util.remove counts) @@ fun () ->
    match
      Proc.run
        ~env:[ ("OPAM_CI_NET_FILE", counts) ]
        ~timeout:(float_of_int timeout) (tool :: args)
    with
    | p -> Ok (p, read_counts counts)
    | exception Proc.Timeout _ ->
        Error (set_count (read_counts counts) "timeout" 1)
    | exception Unix.Unix_error (e, _, _) ->
        Ok
          ( {
              Proc.code = 127;
              stdout = "";
              stderr = "cannot run " ^ tool ^ ": " ^ Unix.error_message e;
            },
            [] )
  in
  match res with
  | Error net -> (None, Some (Printf.sprintf "timeout after %ds" timeout), net)
  | Ok (p, net) -> (
      let net =
        match Rx.search network_requests_re p.stderr with
        | Some m when net = [] ->
            List.map
              (function [ k; v ] -> (k, int_of_string v) | _ -> assert false)
              (Rx.findall_groups count_re (Rx.group m 1))
        | _ -> net
      in
      if p.code <> 0 then begin
        let lines =
          List.filter
            (fun x -> String.trim x <> "")
            (Util.splitlines (String.trim p.stderr))
        in
        let last =
          match Util.last_opt lines with
          | Some l -> l
          | None -> Printf.sprintf "exit code %d" p.code
        in
        (None, Some (Util.prefix_chars last 500), net)
      end
      else
        match Json.parse_opt p.stdout with
        | Some v -> (Some v, None, net)
        | None -> (None, Some "unparsable JSON output", net))

let run_tool_hook = ref (fun args timeout -> run_tool args timeout)

(** Failed-job counts by category, and the reverse dependencies that failed to
    build or test, from [summary --json] (which reads the cached CI page). *)
let jobs_overview (summary : Json.t) : Json.t =
  let failed = Odict.create () in
  let revdeps = Odict.create () in
  let jobs = Json.list "jobs" summary in
  List.iter
    (fun j ->
      if Json.str_opt "status" j = Some "failed" then begin
        let cat =
          match Json.str_opt "category" j with
          | Some c when c <> "" -> c
          | _ -> "?"
        in
        Odict.set failed cat (1 + Option.value ~default:0 (Odict.get failed cat));
        match Json.str_opt "revdep" j with
        | Some rd
          when rd <> ""
               && (not
                     (List.mem (Json.str "category" j) noise
                     && Json.str_opt "category" j <> None))
               && not (Json.truthy (Json.member "experimental" j)) ->
            let x =
              `Assoc
                [
                  ("variant", Json.member "variant" j);
                  ("url", Json.member "url" j);
                  ("category", Json.member "category" j);
                  ("target", Json.member "target" j);
                ]
            in
            Odict.set revdeps rd
              (Option.value ~default:[] (Odict.get revdeps rd) @ [ x ])
        | _ -> ()
      end)
    jobs;
  `Assoc
    [
      ("total", `Int (List.length jobs));
      ( "failed",
        `Assoc (List.map (fun (k, n) -> (k, `Int n)) (Odict.to_list failed)) );
      ( "failing_revdeps",
        `Assoc
          (List.map
             (fun (k, v) -> (k, `List v))
             (List.sort
                (fun (a, _) (b, _) -> compare a b)
                (Odict.to_list revdeps))) );
    ]

let save_state path (prs : (string * Dict.t) list) =
  Util.makedirs (Filename.dirname (Util.abspath path));
  let oc = open_out_bin (path ^ ".tmp") in
  output_string oc
    (Json.dumps
       (`Assoc
          [
            ("updated", `String (now ()));
            ("prs", `Assoc (List.map (fun (k, e) -> (k, Dict.to_json e)) prs));
          ]));
  close_out oc;
  Sys.rename (path ^ ".tmp") path

let shared_min =
  3 (* other open PRs a failure must also appear in to be marked *)

let revdep_job = Rx.v "(?:^|,)([^,]+),revdeps,([^,]+)"

let job_platform variant =
  match Rx.search revdep_job (Option.value ~default:"" variant) with
  | Some m -> Some (String.sub (Option.get variant) 0 (Rx.start ~i:1 m))
  | None -> None

module SS = Set.Make (String)
module IS = Set.Make (Int)

(** PRs of [prs] whose tested packages ([tested pr], a set of names) do not
    overlap: picked greedily, those testing fewer packages first. *)
let independent prs (tested : int -> SS.t) =
  let sorted =
    List.sort
      (fun a b ->
        compare (SS.cardinal (tested a), a) (SS.cardinal (tested b), b))
      prs
  in
  let picked, _ =
    List.fold_left
      (fun (picked, seen) p ->
        if SS.is_empty (SS.inter (tested p) seen) then
          (picked @ [ p ], SS.union seen (tested p))
        else (picked, seen))
      ([], SS.empty) sorted
  in
  picked

let mark_shared_failures (prs : (string * Dict.t) list) =
  (* revdep -> PR -> [(platform, tested package name, category)] *)
  let where :
      ( string,
        (int * (string option * string * string option) list) list )
      Hashtbl.t =
    Hashtbl.create 64
  in
  let failing_revdeps e =
    match Dict.get e "jobs" with
    | Some j -> Json.assoc "failing_revdeps" j
    | None -> []
  in
  List.iter
    (fun (_, e) ->
      let n = Dict.int e "number" in
      List.iter
        (fun (rd, js) ->
          let l =
            List.filter_map
              (fun j ->
                match j with
                | `Assoc _ ->
                    Some
                      ( job_platform (Json.str_opt "variant" j),
                        fst (Ci.split_pkg (Json.str "target" j)),
                        Json.str_opt "category" j )
                | _ -> None)
              (Json.to_list js)
          in
          let cur = Option.value ~default:[] (Hashtbl.find_opt where rd) in
          Hashtbl.replace where rd
            (if List.mem_assoc n cur then
               List.map (fun (p, x) -> if p = n then (p, l) else (p, x)) cur
             else cur @ [ (n, l) ]))
        (failing_revdeps e))
    prs;
  List.iter
    (fun (_, e) ->
      let n = Dict.int e "number" in
      let mine = failing_revdeps e in
      match Dict.dict e "result" with
      | None -> ()
      | Some result ->
          let sugg =
            List.map
              (fun s ->
                let r = Suggestion.of_json s in
                (* computed again below (a value of another type included) *)
                r.also_failing_in <- None;
                r.extra <- List.remove_assoc "also_failing_in" r.extra;
                if not (Where.tied_to_pr r || Where.where_kind r = Rerun) then begin
                  let tested = Hashtbl.create 8 in
                  let variants =
                    match r.Suggestion.jobs with
                    | Some (_ :: _ as l) -> l
                    | _ -> [ Suggestion.str r.Suggestion.variant ]
                  in
                  (* The other PRs where each of the variants fails the same way
                   (none if a variant is not a reverse dependency's job). *)
                  let rec others_of acc = function
                    | [] -> acc
                    | v :: variants -> (
                        match Rx.search revdep_job v with
                        | None -> Some IS.empty
                        | Some m ->
                            let plat = String.sub v 0 (Rx.start ~i:1 m) in
                            let name = fst (Ci.split_pkg (Rx.group m 1))
                            and rd = Rx.group m 2 in
                            let cats =
                              List.sort_uniq compare
                                (List.filter_map
                                   (fun j ->
                                     match j with
                                     | `Assoc _
                                       when Json.str_opt "variant" j = Some v ->
                                         Json.str_opt "category" j
                                     | _ -> None)
                                   (Json.to_list
                                      (Option.value ~default:`Null
                                         (List.assoc_opt rd mine))))
                            in
                            let same =
                              List.filter_map
                                (fun (p, js) ->
                                  let names =
                                    List.fold_left
                                      (fun acc (pl, t, c) ->
                                        if
                                          p <> n && pl = Some plat && t <> ""
                                          && t <> name
                                          && (cats = []
                                             ||
                                             match c with
                                             | Some c -> List.mem c cats
                                             | None -> false)
                                        then SS.add t acc
                                        else acc)
                                      SS.empty js
                                  in
                                  if SS.is_empty names then None
                                  else Some (p, names))
                                (Option.value ~default:[]
                                   (Hashtbl.find_opt where rd))
                            in
                            let ps = IS.of_list (List.map fst same) in
                            List.iter
                              (fun (p, names) ->
                                Hashtbl.replace tested p
                                  (SS.union names
                                     (Option.value ~default:SS.empty
                                        (Hashtbl.find_opt tested p))))
                              same;
                            others_of
                              (Some
                                 (match acc with
                                 | None -> ps
                                 | Some o -> IS.inter o ps))
                              variants)
                  in
                  match others_of None variants with
                  | Some o when not (IS.is_empty o) ->
                      let tested p =
                        Option.value ~default:SS.empty
                          (Hashtbl.find_opt tested p)
                      in
                      if
                        List.length (independent (IS.elements o) tested)
                        >= shared_min
                      then r.also_failing_in <- Some (IS.elements o)
                  | _ -> ()
                end;
                Suggestion.to_json r)
              (Dict.list result "suggestions")
          in
          Dict.set result "suggestions" (`List sugg);
          Dict.set e "result" (Dict.to_json result))
    prs

(** What changed between the analysis of a PR's previous head and this one: the
    problems gone, the new ones, and how many remain. Problems are (rule,
    failing package) pairs, shown with their title (a new wording of a title is
    not a new problem); reruns and unrecognised failures are left out, since
    they come and go with CI. *)
let since_last_push (prev : Json.t) (result : Json.t) : Json.t =
  let keys res =
    let tbl = Odict.create () in
    let ( |? ) a b = match a with Some s when s <> "" -> s | _ -> b in
    List.iter
      (fun s ->
        let s = Suggestion.of_json s in
        let rule = Option.map Rule.to_string s.rule in
        if not (Titles.has_rule [ Rerun; Unrecognised; Lint ] s) then begin
          let k =
            ( rule |? (s.title |? ""),
              Suggestion.get s.Suggestion.failing |? (s.Suggestion.job |? "") )
          in
          let key = Json.dumps (`List [ `String (fst k); `String (snd k) ]) in
          Odict.set tbl key (k, s.title |? (rule |? ""))
        end)
      (Json.list "suggestions" res);
    Odict.values tbl
  in
  let before = keys (Json.member "result" prev) and after = keys result in
  let mem k l = List.exists (fun (k', _) -> k' = k) l in
  let fmt ks =
    List.sort compare
      (List.map
         (fun ((_, f), name) -> if f <> "" then name ^ ": " ^ f else name)
         ks)
  in
  `Assoc
    [
      ("previous_sha", Json.member "sha" prev);
      ( "fixed",
        Json.strings
          (fmt (List.filter (fun (k, _) -> not (mem k after)) before)) );
      ( "new",
        Json.strings
          (fmt (List.filter (fun (k, _) -> not (mem k before)) after)) );
      ( "still",
        `Int (List.length (List.filter (fun (k, _) -> mem k after) before)) );
    ]

let llm_available () = Util.getenv_nonempty "ANTHROPIC_API_KEY" <> None
let llm_available_hook = ref llm_available
let tool_version () = Tool_version.digest
let tool_version_hook = ref tool_version
let github_token_hook = ref Net.github_token

type args = {
  limit : int option;
  state : string;
  out : string;
  cache_dir : string;
  max : int;
  timeout : int;
  time_budget : int;
  llm_max : int;
  llm_budget : int;
}

let args_term =
  let open Cmdliner in
  let int_opt name default docv doc =
    Arg.(value & opt int default & info [ name ] ~docv ~doc)
  in
  let string_opt name default docv doc =
    Arg.(value & opt string default & info [ name ] ~docv ~doc)
  in
  let limit =
    Arg.(
      value
      & opt (some int) None
      & info [ "limit" ] ~docv:"N"
          ~doc:"only the N most recently updated failing PRs (for testing)")
  in
  let state =
    string_opt "state" (Filename.concat "state" "prs.json") "FILE" "the state"
  in
  let out =
    string_opt "out"
      (Filename.concat "site" "data.json")
      "FILE" "the page's data"
  in
  let cache_dir =
    string_opt "cache-dir" ".opam-ci-cache" "DIR" "the tool's cache"
  in
  let max =
    int_opt "max" 6 "N" "suggest --max (distinct failures analysed per PR)"
  in
  let timeout =
    int_opt "timeout" 600 "S" "per-PR timeout of the tool, in seconds"
  in
  let time_budget =
    int_opt "time-budget" 0 "S"
      "stop starting new analyses after this many seconds (0: no limit)"
  in
  let llm_max =
    int_opt "llm-max" 2 "N"
      "LLM calls per PR at most, for failures the tool cannot explain (needs \
       ANTHROPIC_API_KEY)"
  in
  let llm_budget =
    int_opt "llm-budget" 40 "N"
      "LLM calls per run at most; PRs left without them get them in a later run"
  in
  Term.(
    const
      (fun
        limit state out cache_dir max timeout time_budget llm_max llm_budget ->
        {
          limit;
          state;
          out;
          cache_dir;
          max;
          timeout;
          time_budget;
          llm_max;
          llm_budget;
        })
    $ limit $ state $ out $ cache_dir $ max $ timeout $ time_budget $ llm_max
    $ llm_budget)

let negative_number_re = Rx.v "-[0-9]+"

(* cmdliner reads a negative number after an option as another option: join
   them ("--time-budget -1" as "--time-budget=-1"). Every option takes a
   value. *)
let join_negative_values argv =
  let rec go = function
    | o :: v :: rest
      when String.starts_with ~prefix:"--" o
           && (not (String.contains o '='))
           && Rx.fullmatch negative_number_re v <> None ->
        (o ^ "=" ^ v) :: go rest
    | x :: rest -> x :: go rest
    | [] -> []
  in
  Array.of_list (go (Array.to_list argv))

let parse_args argv =
  let argv = join_negative_values argv in
  let open Cmdliner in
  let info =
    Cmd.info "triage-open-prs"
      ~doc:
        "Analyse the open PRs of opam-repository whose CI failed, for the \
         triage page."
  in
  match Cmd.eval_value ~argv (Cmd.v info args_term) with
  | Ok (`Ok a) -> a
  | Ok (`Help | `Version) -> exit Cmd.Exit.ok
  | Error _ -> exit Cmd.Exit.cli_error

(** What happened to an open PR in a run of [main]: each is counted in the run's
    statistics (see [counter]). *)
type outcome =
  | Unchanged
      (** Nothing to do again (same commit, CI run and tool, or attempts used
          up): the previous entry stays. *)
  | Deferred
      (** The run's time budget is spent: left for the next run, and marked
          pending unless a result for the same commit is shown meanwhile. *)
  | Summary_failed
      (** CI runs and its page could not be read: the previous entry stays (a
          result, or a failed analysis with its attempts). *)
  | No_failure_yet  (** CI runs and no job has failed yet: not listed. *)
  | Kept_after_error
      (** Analysing a commit again failed: its previous result stays, with the
          error beside it. *)
  | Analysed  (** A new result. *)
  | Failed  (** The analysis failed: its error, without a result. *)

let counter = function
  | Unchanged -> "unchanged"
  | Deferred -> "deferred"
  | Summary_failed -> "summary failed"
  | No_failure_yet -> "running, no failure yet"
  | Kept_after_error | Failed -> "failed"
  | Analysed -> "analysed"

type item = {
  p : Dict.t;
  key : string;
  sha : string;
  prev : Dict.t option;
  pv : Dict.t;
  same_sha : bool;
  entry : Dict.t;
}
(** An open PR in a run: [p] as listed, [prev] its entry in the previous state
    ([pv]: empty without one), [entry] its entry in the new state. *)

(** The previous analysis can be kept ([same]: see [reuse]). *)
let reusable it same = same && (has_result it.pv || keep_failed it.prev)

type before_suggest = {
  summary : Json.t option;
  digest : string option;
  same : bool;
  retry : bool;
  refresh : string list;
  refreshed : bool;
}
(** What is known when [suggest] is about to run: the CI page of a running PR
    ([summary], and the [digest] of its job statuses), whether the analysis
    repeats the previous one ([same], [retry]: see [reuse]), the options that
    refresh the cache ([refresh]: none once [summary] did) and whether it was
    refreshed for this CI run ([refreshed]). *)

(** What reading the CI page of a running PR leads to: [suggest] runs, or the PR
    is done with. *)
type running_page = Go_on of before_suggest | Stop of outcome

let counts_text (net : counts) =
  String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s=%d" k v) net)

let main ?(argv = Sys.argv) () =
  let args = parse_args argv in
  let start = Unix.gettimeofday () in
  let token =
    match !github_token_hook () with
    | Some t -> t
    | None ->
        Util.die
          "a GitHub token is needed (GITHUB_TOKEN, GH_TOKEN or `gh auth \
           login`): the PR listing uses the GraphQL API"
  in
  let old =
    match Json.load args.state with
    | Some (`Assoc _ as s) ->
        List.map (fun (k, v) -> (k, Dict.of_json v)) (Json.assoc "prs" s)
    | _ -> []
  in
  let prs, list_calls = !list_open_prs_hook token in
  let state_of p = Dict.str_opt p "ci_state" in
  (* Failed CI, and CI still running (shown if some jobs have already failed). *)
  let failing =
    List.filter
      (fun p -> state_of p = Some "FAILURE" || state_of p = Some "PENDING")
      prs
  in
  let nstate s = List.length (List.filter (fun p -> state_of p = Some s) prs) in
  Printf.eprintf
    "%d open PRs, %d with %s = failure, %d running (%d GitHub API call(s))\n%!"
    (List.length prs) (nstate "FAILURE") context (nstate "PENDING") list_calls;
  let failing =
    match args.limit with Some n -> Util.take n failing | None -> failing
  in
  let llm = !llm_available_hook () in
  let llm_left = ref (if llm then args.llm_budget else 0) in
  prerr_endline
    ("LLM fallback: "
    ^
    if llm then Printf.sprintf "on (at most %d calls)" args.llm_budget
    else "off (no ANTHROPIC_API_KEY)");
  let version = !tool_version_hook () in
  let new_state : Dict.t Odict.t = Odict.create () in
  let stats : int Odict.t = Odict.create () in
  let stat k = Option.value ~default:0 (Odict.get stats k) in
  let bump k = Odict.set stats k (stat k + 1) in
  let net_total : int Odict.t = Odict.create () in
  (* Transient LLM failures in a row, across the PRs of this run: after
     llm_breaker (an outage), the LLM is not asked again in this run. *)
  let llm_streak = ref 0 in
  let llm_off = ref None in
  (* why the LLM was turned off in this run: "credentials" or "outage" *)
  let merged_state () =
    let ns = Odict.to_list new_state in
    List.map
      (fun (k, e) ->
        (k, match List.assoc_opt k ns with Some e' -> e' | None -> e))
      old
    @ List.filter (fun (k, _) -> not (List.mem_assoc k old)) ns
  in
  let raw p k = Option.value ~default:`Null (Dict.raw p k) in
  let item_of p =
    let key = string_of_int (Dict.int p "number") in
    let sha = Dict.str p "sha" in
    let prev = List.assoc_opt key old in
    let pv = match prev with Some pv -> pv | None -> Dict.create () in
    let same_sha = prev <> None && Dict.str_opt pv "sha" = Some sha in
    let entry = Dict.copy pv in
    Dict.update entry
      (List.map
         (fun k -> (k, raw p k))
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
         ]);
    Dict.set_str entry "ci_url"
      (match Dict.str_opt p "ci_url" with
      | Some u when u <> "" -> u
      | _ -> Net.base ^ sha);
    { p; key; sha; prev; pv; same_sha; entry }
  in
  (* The time budget is spent: the PR is left for the next run. *)
  let defer it =
    let entry = it.entry in
    if it.same_sha && has_result it.pv then
      (* To redo (tool or CI changed, LLM, MCP errors): the latest result for
         this commit is shown meanwhile. *)
      Dict.remove entry "pending"
    else begin
      if it.prev <> None && not it.same_sha then begin
        (* an older result would describe another commit *)
        let base = since_base it.prev it.sha in
        List.iter (Dict.remove entry)
          ([
             "result";
             "error";
             "jobs";
             "analysed_at";
             "attempts";
             "tool_version";
             "ci_digest";
             "since_last_push";
             "since_base";
             "ci_created";
             "ci_unfinished";
             "llm";
             "llm_unanswered";
             "llm_calls";
             "mcp_errors";
             "net_errors";
             "git_unreachable";
             "seconds";
             "refresh_pending";
           ]
          @ last_error);
        (match base with Some b -> Dict.set entry "since_base" b | None -> ());
        (* compared with when the new commit's analysis lands *)
        Dict.set entry "ci_state" (raw it.p "ci_state")
        (* the new commit's (its CI may still run) *)
      end;
      Dict.set_bool entry "pending" true
    end;
    Deferred
  in
  (* A running PR is only worth showing once some job has failed: [summary]
     reads the CI page (not cached while CI runs) and gives the counts.
     [Stop]: nothing more to do for the PR. *)
  let read_running_page it ~refresh ~refreshed : running_page =
    let s, serr, snet =
      !run_tool_hook
        ([ "summary"; it.sha; "--json"; "--cache-dir"; args.cache_dir ]
        @ refresh)
        120
    in
    let refreshed = refreshed || count snet "refreshed" <> 0 in
    if s = None && it.same_sha then begin
      (* The CI page could not be read (timeout, network): nothing says the
         failures are gone, so the result is kept until the next run; a
         failed analysis too, with its attempts. *)
      Printf.eprintf "#%d %s summary failed: %s; the previous %s is kept\n%!"
        (Dict.int it.p "number")
        (Util.prefix_chars it.sha 12)
        (Option.value ~default:"None" serr)
        (if has_result it.pv then "result" else "analysis");
      Stop Summary_failed
    end
    else
      let failed =
        List.filter
          (fun j ->
            Json.str_opt "status" j = Some "failed"
            && not
                 (match Json.str_opt "category" j with
                 | Some c -> List.mem c noise
                 | None -> false))
          (match s with Some s -> Json.list "jobs" s | None -> [])
      in
      if failed = [] then Stop No_failure_yet
      else
        (* The same job statuses as at the previous analysis (opam-ci's
           status stays pending when a job never starts): nothing to redo. *)
        let d = ci_digest s in
        let same, _, retry =
          reuse ~digest:d it.prev it.p version (!llm_left > 0)
        in
        if reusable it same then Stop Unchanged
        else
          Go_on
            {
              summary = s;
              digest = Some d;
              same;
              retry;
              refresh = [];
              refreshed;
            }
  in
  (* Runs [suggest], and records its result in the PR's entry. *)
  let run_suggest it ~t0 ~asked_refresh (b : before_suggest) =
    let p = it.p and pv = it.pv and entry = it.entry in
    let attempts =
      (if b.same || b.retry then attempts_used it.prev else 0) + 1
    in
    let use_llm = !llm_left > 0 in
    let extra =
      if use_llm then
        [ "--llm"; "--llm-max"; string_of_int (min args.llm_max !llm_left) ]
      else []
    in
    let result, err, net =
      !run_tool_hook
        ([
           "suggest";
           it.sha;
           "--json";
           "--max";
           string_of_int args.max;
           "--cache-dir";
           args.cache_dir;
         ]
        @ extra @ b.refresh)
        args.timeout
    in
    List.iter
      (fun (k, v) ->
        Odict.set net_total k
          (v + Option.value ~default:0 (Odict.get net_total k)))
      net;
    let refreshed = b.refreshed || count net "refreshed" <> 0 in
    llm_left := !llm_left - count net "llm";
    if count net "timeout" <> 0 && use_llm then decr llm_left;
    (* an LLM call may have been under way: count it *)
    if use_llm then begin
      (* The tool turned the LLM off (credentials refused, an outage), or
         its API failed llm_breaker times in a row over the PRs analysed
         so far: not asked again in this run (a later run tries again). *)
      if List.mem_assoc "llm-streak" net then
        llm_streak :=
          count net "llm-streak"
          + if count net "llm" <> 0 then 0 else !llm_streak;
      if
        (count net "llm-off" <> 0 || !llm_streak >= llm_breaker)
        && !llm_left > 0
      then begin
        llm_off :=
          Some (if count net "llm-off" <> 0 then "credentials" else "outage");
        Printf.eprintf
          "LLM off for the rest of this run (%d API failures in a row%s)\n%!"
          !llm_streak
          (if count net "llm-off" <> 0 then ", or the tool turned it off"
           else "");
        llm_left := 0
      end
    end;
    let err_s = Option.value ~default:"None" err in
    let reqs = match counts_text net with "" -> "?" | s -> s in
    let seconds () = Float.round ((Unix.gettimeofday () -. t0) *. 10.) /. 10. in
    if result = None && it.same_sha && has_result pv then begin
      (* A failed redo (timeout, outage) of an analysis of the same commit:
         the previous result is kept, with the error beside it. Its fields
         (tool version, CI status) still say what it saw, so it is tried
         again, max_attempts times at most (see reuse); the CI run tried is
         recorded, so that the attempts at a rebuild add up too, and are
         made from the cache once it was refreshed. *)
      Dict.update entry
        [
          ("last_error", Json.opt_string err);
          ("last_error_at", `String (now ()));
          ("attempts", `Int attempts);
          ("last_error_ci_state", raw p "ci_state");
          ("last_error_ci_created", raw p "ci_created");
          ("last_error_refreshed", `Bool refreshed);
        ];
      Dict.remove entry "pending";
      Printf.eprintf
        "#%d %s ERROR: %s; the previous result is kept (%ss; requests: %s)\n%!"
        (Dict.int p "number")
        (Util.prefix_chars it.sha 12)
        err_s
        (Json.repr_float (seconds ()))
        reqs;
      Kept_after_error
    end
    else begin
      let summary =
        match b.summary with
        | None ->
            let s, _, _ =
              !run_tool_hook
                [ "summary"; it.sha; "--json"; "--cache-dir"; args.cache_dir ]
                120
            in
            s
        | s -> s
      in
      let base = since_base it.prev it.sha in
      (match base with
      | Some b when result <> None ->
          Dict.set entry "since_last_push"
            (since_last_push b (Option.get result));
          Dict.remove entry "since_base"
      | _ ->
          if it.prev <> None && not it.same_sha then begin
            Dict.remove entry "since_last_push";
            (* no comparable previous result yet *)
            match base with
            | Some b ->
                Dict.set entry "since_base"
                  b (* kept for when an analysis of this commit lands *)
            | None -> ()
          end);
      Dict.update entry
        [
          ("ci_state", raw p "ci_state");
          ("ci_created", raw p "ci_created");
          ("ci_digest", Json.opt_string b.digest);
          ( "ci_unfinished",
            match result with
            | Some r when Json.mem "ci_unfinished" r ->
                Json.member "ci_unfinished" r
            | _ -> `Int 0 );
        ];
      (* A failed analysis that was to refresh the cache and did not get that
         far: the next attempt must, whatever ci_created says (see
         needs_refresh). *)
      if result = None && asked_refresh && not refreshed then
        Dict.set_bool entry "refresh_pending" true
      else Dict.remove entry "refresh_pending";
      (* [llm]: every unexplained failure got its LLM answer (else the PR is
         analysed again when a run has LLM budget left, see reuse).
         [llm_calls]: the answers this analysis got from the API (none: the
         next attempt is a retry, see reuse). *)
      let unanswered : Json.t =
        if use_llm then
          match result with
          | Some r when Json.mem "llm_unanswered" r ->
              Json.member "llm_unanswered" r
          | _ -> `Int 0
        else `Null
      in
      let secs = seconds () in
      Dict.update entry
        [
          ("result", Option.value ~default:`Null result);
          ("error", Json.opt_string err);
          ("attempts", `Int attempts);
          ("tool_version", `String version);
          ("llm", `Bool (use_llm && not (Json.truthy unanswered)));
          ("llm_unanswered", unanswered);
          ("llm_calls", if use_llm then `Int (count net "llm") else `Null);
          ("mcp_errors", `Int (count net "mcp-errors"));
          ("net_errors", `Int (count net "net-errors" + count net "timeout"));
          (* A git host that failed earlier today: not asked again; the
             analysis is done again once the day has passed (see reuse). *)
          ("git_unreachable", `Int (count net "git-unreachable"));
          ("analysed_at", `String (now ()));
          ("seconds", `Float secs);
          ( "jobs",
            match summary with
            | Some s when Json.truthy s -> jobs_overview s
            | _ -> `Null );
        ];
      List.iter (Dict.remove entry) ("pending" :: last_error);
      Printf.eprintf "#%d %s %s (%ss; requests: %s)\n%!" (Dict.int p "number")
        (Util.prefix_chars it.sha 12)
        (if result <> None then "ok" else "ERROR: " ^ err_s)
        (Json.repr_float secs) reqs;
      if result <> None then Analysed else Failed
    end
  in
  (* Analyses the PR, reading its CI page first while CI runs. *)
  let analyse it ~same ~retry =
    let t0 = Unix.gettimeofday () in
    (* Jobs rebuilt since the previous analysis: the first run of the tool
       fetches the CI page again and drops the cached logs of its jobs. Also
       after an analysis made while CI was running (a job whose log was read
       then may have been rebuilt since), and without a previous analysis of
       this build (see needs_refresh). [refreshed]: the tool says it did
       ("refreshed" in its counts: a CI page that could not be fetched leaves
       the previous build's page and logs in the cache), or a failed attempt
       at this CI run did. *)
    let refresh = if needs_refresh it.prev it.p then [ "--refresh" ] else [] in
    let asked_refresh = refresh <> [] in
    let refreshed =
      tried_run it.prev it.p && Dict.truthy it.pv "last_error_refreshed"
    in
    if asked_refresh then bump "refreshed";
    let before =
      if state_of it.p = Some "PENDING" then
        read_running_page it ~refresh ~refreshed
      else
        Go_on { summary = None; digest = None; same; retry; refresh; refreshed }
    in
    match before with
    | Stop outcome -> outcome
    | Go_on b -> run_suggest it ~t0 ~asked_refresh b
  in
  (* One PR: its entry in the new state, and what happened to it. *)
  let triage p =
    let it = item_of p in
    (* Once the run's LLM budget is spent, a PR analysed without the LLM is
       not analysed again (without it): it waits for a run with budget left. *)
    let same, _same_commit, retry = reuse it.prev p version (!llm_left > 0) in
    let outcome =
      if reusable it same then Unchanged
      else if
        args.time_budget <> 0
        && Unix.gettimeofday () -. start > float_of_int args.time_budget
      then defer it
      else analyse it ~same ~retry
    in
    (it, outcome)
  in
  List.iter
    (fun p ->
      let it, outcome = triage p in
      bump (counter outcome);
      match outcome with
      | No_failure_yet -> ()
      | Unchanged | Deferred | Summary_failed ->
          Odict.set new_state it.key it.entry
      | Kept_after_error | Analysed | Failed ->
          Odict.set new_state it.key it.entry;
          (* keep the progress if the run is killed *)
          save_state args.state (merged_state ()))
    failing;
  let listed =
    List.map (fun p -> (string_of_int (Dict.int p "number"), p)) failing
  in
  List.iter
    (fun (key, entry) -> note_listed_ci entry (List.assoc key listed))
    (Odict.to_list new_state);
  mark_shared_failures (Odict.to_list new_state);
  Odict.set stats "dropped"
    (List.length (List.filter (fun (k, _) -> not (Odict.mem new_state k)) old));
  save_state args.state (Odict.to_list new_state);
  let entries = Odict.values new_state in
  let data =
    `Assoc
      [
        ("generated", `String (now ()));
        ("repository", `String (owner ^ "/" ^ name));
        ("open_prs", `Int (List.length prs));
        ("failing_prs", `Int (nstate "FAILURE"));
        (* opam-ci's status stays pending when a job never starts: running
           only while the CI page has unfinished jobs. *)
        ("running_prs", `Int (List.length (List.filter ci_running entries)));
        ("limit", match args.limit with Some n -> `Int n | None -> `Null);
        ("max_failures_per_pr", `Int args.max);
        ( "llm",
          `Assoc
            [
              ("enabled", `Bool llm);
              ( "calls",
                `Int (Option.value ~default:0 (Odict.get net_total "llm")) );
              ("off", Json.opt_string !llm_off);
            ] );
        ( "run",
          `Assoc
            ([
               ("github_api_calls", `Int list_calls);
               ( "tool_requests",
                 `Assoc
                   (List.map
                      (fun (k, v) -> (k, `Int v))
                      (Odict.to_list net_total)) );
             ]
            @ List.map (fun (k, v) -> (k, `Int v)) (Odict.to_list stats)) );
        ( "prs",
          `List
            (List.map Dict.to_json
               (Util.sort_by ~reverse:true
                  (fun e -> Dict.str e "updated")
                  entries)) );
        (* PRs whose CI passes: nothing to analyse, listed so that the page
           can say which PRs seem ready. *)
        ( "passing",
          `List
            (List.filter_map
               (fun p ->
                 if state_of p = Some "SUCCESS" then
                   Some
                     (`Assoc
                        (List.map
                           (fun k -> (k, raw p k))
                           [
                             "number";
                             "title";
                             "url";
                             "draft";
                             "updated";
                             "author";
                             "labels";
                             "stale_labels";
                           ]))
                 else None)
               prs) );
      ]
  in
  Util.makedirs (Filename.dirname (Util.abspath args.out));
  let oc = open_out_bin args.out in
  output_string oc (Json.dumps ~indent:1 data);
  close_out oc;
  Printf.eprintf
    "analysed %d, failed %d, unchanged %d, running without failure %d, \
     deferred %d, dropped %d; tool requests: %s; wrote %s\n\
     %!"
    (stat "analysed") (stat "failed") (stat "unchanged")
    (stat "running, no failure yet")
    (stat "deferred") (stat "dropped")
    (match counts_text (Odict.to_list net_total) with "" -> "none" | s -> s)
    args.out;
  let append path s =
    let oc = open_out_gen [ Open_append; Open_creat; Open_wronly ] 0o644 path in
    output_string oc s;
    close_out oc
  in
  (* In the run's summary on GitHub, so that an expired key is noticed. *)
  (match (Util.getenv_nonempty "GITHUB_STEP_SUMMARY", !llm_off) with
  | Some path, Some off ->
      append path
        (Printf.sprintf "**LLM diagnoses were turned off in this run:** %s\n\n"
           (llm_off_text off))
  | _ -> ());
  match Util.getenv_nonempty "GITHUB_OUTPUT" with
  | Some path ->
      append path
        (Printf.sprintf "changed=%s\n"
           (if stat "analysed" <> 0 || stat "failed" <> 0 || stat "dropped" <> 0
            then "true"
            else "false"))
  | None -> ()
