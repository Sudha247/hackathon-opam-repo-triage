open Net

module Status = struct
  type t = Passed | Failed | Active | Aborted | Not_started | Other of string

  let to_string = function
    | Passed -> "passed"
    | Failed -> "failed"
    | Active -> "active"
    | Aborted -> "aborted"
    | Not_started -> "not-started"
    | Other s -> s

  let of_string = function
    | "passed" -> Passed
    | "failed" -> Failed
    | "active" -> Active
    | "aborted" -> Aborted
    | "not-started" -> Not_started
    | s -> Other s
end

module Kind = struct
  type t =
    | Build
    | Tests
    | Lower_bounds
    | Revdeps
    | Revdeps_list
    | Lint
    | Analysis
    | Other of string

  let to_string = function
    | Build -> "build"
    | Tests -> "tests"
    | Lower_bounds -> "lower-bounds"
    | Revdeps -> "revdeps"
    | Revdeps_list -> "revdeps-list"
    | Lint -> "lint"
    | Analysis -> "analysis"
    | Other s -> s

  let of_string = function
    | "build" -> Build
    | "tests" -> Tests
    | "lower-bounds" -> Lower_bounds
    | "revdeps" -> Revdeps
    | "revdeps-list" -> Revdeps_list
    | "lint" -> Lint
    | "analysis" -> Analysis
    | s -> Other s
end

module Category = struct
  type t =
    | Build
    | Build_or_test
    | Dependency
    | Solver
    | Network
    | Cancelled
    | Ignored
    | Not_available
    | Sources
    | Lint
    | Analysis
    | Status of Status.t

  let to_string = function
    | Build -> "build"
    | Build_or_test -> "build-or-test"
    | Dependency -> "dependency"
    | Solver -> "solver"
    | Network -> "network"
    | Cancelled -> "cancelled"
    | Ignored -> "ignored"
    | Not_available -> "not-available"
    | Sources -> "sources"
    | Lint -> "lint"
    | Analysis -> "analysis"
    | Status s -> Status.to_string s

  let of_string = function
    | "build" -> Build
    | "build-or-test" -> Build_or_test
    | "dependency" -> Dependency
    | "solver" -> Solver
    | "network" -> Network
    | "cancelled" -> Cancelled
    | "ignored" -> Ignored
    | "not-available" -> Not_available
    | "sources" -> Sources
    | "lint" -> Lint
    | "analysis" -> Analysis
    | s -> Status (Status.of_string s)

  let passed = Status Passed
end

type job = {
  variant : string;
  url : string;
  label : string;
  status : Status.t;
  reason : string option;
  category : Category.t;
  target : string option;
  kind : Kind.t;
  revdep : string option;
  platform : string list;
  experimental : bool;
  in_summary : bool;
  messages : string list;
}

type data = {
  pr : int option;
  jobs : job list;
  mutable unfinished : int;
  mutable unanalysed : int;
  mutable health_stale : Json.t option;
  mutable llm_unanswered : int option;
  mutable sha : string option;
}

let job_to_json j : Json.t =
  `Assoc
    [
      ("variant", `String j.variant);
      ("url", `String j.url);
      ("label", `String j.label);
      ("status", `String (Status.to_string j.status));
      ("reason", Json.opt_string j.reason);
      ("category", `String (Category.to_string j.category));
      ("target", Json.opt_string j.target);
      ("kind", `String (Kind.to_string j.kind));
      ("revdep", Json.opt_string j.revdep);
      ("platform", Json.strings j.platform);
      ("experimental", `Bool j.experimental);
      ("in_summary", `Bool j.in_summary);
      ("messages", Json.strings j.messages);
    ]

let data_fields d : (string * Json.t) list =
  [
    ("pr", match d.pr with Some n -> `Int n | None -> `Null);
    ("jobs", `List (List.map job_to_json d.jobs));
    ("unfinished", `Int d.unfinished);
  ]

let failing_pkg j =
  match j.revdep with Some r when r <> "" -> Some r | _ -> j.target

(* Job statuses that will not change without a rebuild. *)
let finished : Status.t -> bool = function
  | Passed | Failed | Aborted -> true
  | Active | Not_started | Other _ -> false

let stopped : Status.t -> bool = function
  | Failed | Aborted -> true
  | _ -> false

let unfinished_jobs jobs =
  let left = List.filter (fun j -> not (finished j.status)) jobs in
  let stuck = List.filter (fun j -> j.status = Not_started) left in
  let analysis_failed =
    List.exists (fun j -> j.kind = Analysis && stopped j.status) jobs
  in
  let some_failed = List.exists (fun j -> stopped j.status) jobs in
  if
    stuck <> []
    && (analysis_failed || (List.length stuck = List.length left && some_failed))
  then List.filter (fun j -> j.status <> Not_started) left
  else left

(* ------------------------------------------------------------------------ *)
(* Commit page parsing

   Structure of the page:
     <h2>Summary of errors</h2><ul class="statuses"> ... </ul>
     <details><summary><b>Main results</b></summary><ul class="statuses"> ... </ul></details>
     <details><summary><b>Reverse dependencies</b></summary><ul class="statuses"> ... </ul></details>
   Each node is <li class="none|passed|failed|skipped|..."> containing either
   a plain text label (a grouping node, e.g. "compilers", "4.14",
   "debian-13") or <a href=".../variant/<comma,separated,path>">label
   (status[: reason])</a>, optionally followed by a nested <ul class="statuses">. *)

type raw_job = {
  sec : string;
  href : string;
  cls : string;
  text : string;
  ancestors : string list;
  mutable msgs : string list;
}

(* The text of the nodes, with the collapsed text of [link] in its place. *)
let rec own_text ~link nodes =
  String.concat ""
    (List.map
       (fun n ->
         match Soup.element n with
         | Some e when Some e == link ->
             collapse_space (String.concat "" (Soup.texts e))
         | Some e -> own_text ~link (Soup.to_list (Soup.children e))
         | None -> Option.value ~default:"" (Soup.leaf_text n))
       nodes)

and collapse_space s = String.concat " " (Util.split s)

let is_list n = Option.map Soup.name (Soup.element n) = Some "ul"

(* The jobs of the [li] items of list [ul] (and of their sub-lists), in
   section [sec], under the items labelled [ancestors] (outermost first);
   [owner] is the job of the nearest enclosing item that has one, which a
   class-less item under it is a message of (e.g. lint errors). *)
let rec list_jobs ~sec ~ancestors ~owner ul =
  Soup.children ul |> Soup.elements
  |> Soup.filter (fun li -> Soup.name li = "li")
  |> Soup.to_list
  |> List.concat_map (fun li ->
      let content, sublists =
        List.partition
          (fun n -> not (is_list n))
          (Soup.to_list (Soup.children li))
      in
      let link =
        List.find_map
          (fun n ->
            match Soup.element n with
            | Some e
              when Soup.name e = "a"
                   && Util.contains
                        (Option.value ~default:"" (Soup.attribute "href" e))
                        "/variant/" ->
                Some e
            | _ -> None)
          content
      in
      let cls = Soup.attribute "class" li in
      let label = String.trim (own_text ~link content) in
      let job =
        Option.map
          (fun a ->
            {
              sec;
              href = Option.value ~default:"" (Soup.attribute "href" a);
              cls = Option.value ~default:"" cls;
              text = collapse_space (String.concat "" (Soup.texts a));
              ancestors;
              msgs = [];
            })
          link
      in
      (match (job, owner) with
      | None, Some j when cls = None && collapse_space label <> "" ->
          j.msgs <- j.msgs @ [ collapse_space label ]
      | _ -> ());
      let owner = if job <> None then job else owner in
      Option.to_list job
      @ List.concat_map
          (fun ul ->
            list_jobs ~sec ~ancestors:(ancestors @ [ label ]) ~owner
              (Option.get (Soup.element ul)))
          sublists)

let pull_re = Rx.v {|/pull/(\d+)$|}

let parse_page page =
  let soup = Soup.parse page in
  let pr =
    Soup.fold
      (fun pr a ->
        match
          Rx.search pull_re (Option.value ~default:"" (Soup.attribute "href" a))
        with
        | Some m -> Some (int_of_string (Rx.group m 1))
        | None -> pr)
      None (Soup.select "a" soup)
  in
  let lists_after sec node =
    Soup.next_siblings node |> Soup.elements |> Soup.to_list
    |> List.filter (fun e -> Soup.name e = "ul")
    |> List.concat_map (list_jobs ~sec ~ancestors:[] ~owner:None)
  in
  (* The summary: the lists after the "Summary of errors" heading, up to the
     first section. *)
  let summary =
    Soup.select "h2" soup |> Soup.to_list
    |> List.concat_map (fun h2 ->
        if String.trim (String.concat "" (Soup.texts h2)) = "Summary of errors"
        then
          Soup.next_siblings h2 |> Soup.elements |> Soup.to_list
          |> List.fold_left
               (fun (stop, acc) e ->
                 if stop || Soup.name e = "details" then (true, acc)
                 else if Soup.name e = "ul" then
                   ( false,
                     acc @ list_jobs ~sec:"summary" ~ancestors:[] ~owner:None e
                   )
                 else (false, acc))
               (false, [])
          |> snd
        else [])
  in
  (* The sections ("Main results", "Reverse dependencies", ...): the lists
     after the summary of each [details]. *)
  let main =
    Soup.select "details > summary" soup
    |> Soup.to_list
    |> List.concat_map (lists_after "main")
  in
  (pr, summary @ main)

let label_re = Rx.v ~flags:[ Rx.S ] {|^(.*?) \((\w+)(?::\s*(.*))?\)$|}

(** Split a variant path into (platform, target, kind, revdep).

    Examples:
    - compilers,4.14,lwt.5.10.2,revdeps,osc.0.1.4
    - day10,debian-13,x86_64,5.5,lwt.5.10.2,tests
    - compilers,4.11,foo.1.0,lower-bounds
    - distributions,centos-10-ocaml-4.14,foo.1.0
    - (lint) *)
let split_variant variant : string list * string option * Kind.t * string option
    =
  if String.starts_with ~prefix:"(" variant then
    ([], None, Kind.of_string (Util.strip ~chars:"()" variant), None)
  else
    let parts = String.split_on_char ',' variant in
    let n = List.length parts in
    match List.find_index (( = ) "revdeps") parts with
    | Some i ->
        if i = n - 1 then (* the job computing the list of revdeps *)
          ( Util.take (i - 1) parts,
            List.nth_opt parts (i - 1),
            Revdeps_list,
            None )
        else
          ( Util.take (i - 1) parts,
            List.nth_opt parts (i - 1),
            Revdeps,
            Some (String.concat "," (Util.drop (i + 1) parts)) )
    | None ->
        let last = List.nth parts (n - 1) in
        if (last = "lower-bounds" || last = "tests") && n >= 2 then
          ( Util.take (n - 2) parts,
            Some (List.nth parts (n - 2)),
            Kind.of_string last,
            None )
        else (Util.take (n - 1) parts, Some last, Build, None)

let curl_failed_http_re =
  Rx.v
    {|curl failed|HTTP error code|Could not resolve|Connection (refused|reset|timed out)|}

let failed_build_dependency_re =
  Rx.v {|\bfailed to build\b|a dependency failed|^\s*\S+ failed:$|}

let solution_conflict_re = Rx.v ~flags:[ Rx.I ] {|No solution|conflict|}
let failed_runtest_test_re = Rx.v {|failed at ".*(@runtest|test)|}

let categorize (status : Status.t) reason : Category.t =
  let r = Option.value ~default:"" reason in
  if status <> Failed then Status status (* passed, active, ... *)
  else if String.starts_with ~prefix:"[SKIP] Failure ignored" r then
    Ignored (* x-ci-accept-failures *)
  else if String.starts_with ~prefix:"[SKIP]" r then
    Not_available (* package not installable on that variant *)
  else if r = "Cancelled" || Util.contains (Util.prefix_chars r 20) "Cancelled"
  then Cancelled
  else if Rx.test curl_failed_http_re r then Network
  else if Util.contains r "Failed to get sources" then
    Sources
    (* fetched but could not be extracted/verified (e.g. symlinks on msys2) *)
  else if Rx.test failed_build_dependency_re r then Dependency
  else if Rx.test solution_conflict_re r then Solver
  else if Rx.test failed_runtest_test_re r then Build_or_test
  else Build

let parse_page_uncached page =
  let pr, raw = parse_page page in
  (* The links of the summary: a set, the page can list thousands of jobs. *)
  let summary_hrefs = Hashtbl.create 64 in
  List.iter
    (fun r -> if r.sec = "summary" then Hashtbl.replace summary_hrefs r.href ())
    raw;
  let jobs =
    List.filter_map
      (fun r ->
        if r.sec <> "main" then None
        else begin
          let variant =
            Http.unquote
              (match Util.partition r.href "/variant/" with _, _, v -> v)
          in
          let label, status, reason =
            match Rx.match_ label_re r.text with
            | Some m -> (Rx.group m 1, Rx.group m 2, Rx.group_opt m 3)
            | None -> (r.text, r.cls, None)
          in
          let status = Status.of_string status in
          let reason =
            match reason with
            | Some s when s <> "" -> Some (String.trim s)
            | _ -> None
          in
          let platform, target, kind, revdep = split_variant variant in
          Some
            {
              variant;
              url = Http.urljoin base r.href;
              label;
              status;
              reason;
              category =
                (match (kind, status) with
                | Lint, Failed -> Lint
                | Analysis, Failed -> Analysis
                | _ -> categorize status reason);
              target;
              kind;
              revdep;
              platform;
              experimental =
                List.exists
                  (fun a -> Util.contains a "(experimental)")
                  r.ancestors;
              in_summary = Hashtbl.mem summary_hrefs r.href;
              messages = r.msgs;
            }
        end)
      raw
  in
  {
    pr;
    jobs;
    unfinished = 0;
    unanalysed = 0;
    health_stale = None;
    llm_unanswered = None;
    sha = None;
  }

(* A run reads some pages several times (the baseline page of the previous
   release, by several checks), and parsing a page of thousands of jobs
   takes a while: each page is parsed once. Callers get their own copy of
   the record, whose analysis fields they set. *)
let parsed_pages : (Digest.t, data) Hashtbl.t = Hashtbl.create 4
let parsed_pages_lock = Mutex.create ()

let parse_commit_page page =
  let key = Digest.string page in
  let d =
    match
      Mutex.protect parsed_pages_lock (fun () ->
          Hashtbl.find_opt parsed_pages key)
    with
    | Some d -> d
    | None ->
        let d = parse_page_uncached page in
        Mutex.protect parsed_pages_lock (fun () ->
            Hashtbl.replace parsed_pages key d);
        d
  in
  { d with unfinished = d.unfinished }

let load_commit ?(refresh = false) sha cache_dir =
  let url = base ^ sha in
  let before = net_get "http" in
  let page = fetch ~refresh url cache_dir in
  let data = parse_commit_page page in
  if refresh then begin
    List.iter (fun j -> Util.remove (cache_path j.url cache_dir)) data.jobs;
    net_set "refreshed" 1
  end;
  let page, data =
    if
      unfinished_jobs data.jobs <> []
      && net_get "http" = before
      && not (is_offline ())
    then
      let page = fetch ~refresh:true url cache_dir in
      (page, parse_commit_page page)
    else (page, data)
  in
  data.unfinished <- List.length (unfinished_jobs data.jobs);
  (* A test fixture is a snapshot: keep the page when recording one, even if
     some jobs never finish (stuck experimental jobs on old commits). *)
  if data.unfinished > 0 && Sys.getenv_opt "OPAM_CI_RECORD" <> Some "1" then
    Util.remove (cache_path url cache_dir);
  (page, data)

let full_sha_re = Rx.v "([0-9a-f]{40})"

let resolve_sha target pr =
  match pr with
  | Some pr -> (
      (* One REST call (no token needed); the head can move, so not cached on disk. *)
      let d =
        gh_api ~cache:false (Printf.sprintf "repos/%s/pulls/%d" repo pr)
      in
      match
        Option.bind d (fun d -> Json.str_opt "sha" (Json.member "head" d))
      with
      | Some sha when sha <> "" -> sha
      | _ ->
          (* Fallback without the API: refs/pull/N/head (slow: lists every ref). *)
          net_incr "git";
          let out =
            try
              Proc.run
                [
                  "git";
                  "ls-remote";
                  "https://github.com/" ^ repo;
                  Printf.sprintf "refs/pull/%d/head" pr;
                ]
            with _ -> { Proc.code = 1; stdout = ""; stderr = "git failed" }
          in
          if String.trim out.stdout = "" then
            Util.die
              (Printf.sprintf "cannot resolve PR #%d: %s" pr
                 (Util.prefix_chars (String.trim out.stderr) 200));
          List.hd (Util.split out.stdout))
  | None -> (
      match Rx.search full_sha_re (Option.value ~default:"" target) with
      | Some m -> Rx.group m 1
      | None ->
          Util.die
            (Printf.sprintf "cannot find a commit sha in %s"
               (match target with Some t -> Util.repr t | None -> "None")))

(* ------------------------------------------------------------------------ *)
(* Log excerpt extraction *)

let day10_indent_re = Rx.v ~flags:[ Rx.M ] "^ {8}(- [^\\s])?"

(* day10 prints opam's output indented by 8 spaces, and the build output with
   an extra "- " prefix; remove both so that day10 logs read like classic
   ones (the "#=== ERROR while ..." block, "-> installed ...", compiler
   errors). Python: (?m)^ {8}(?:- (?=\S))? *)
let remove_day10_indent text =
  Rx.subf day10_indent_re
    (fun m ->
      match Rx.group_opt m 1 with Some g -> String.sub g 2 1 | None -> "")
    text

let pre_pre_re = Rx.v ~flags:[ Rx.S ] "<pre[^>]*>(.*?)</pre>"
let tag_re = Rx.v "<[^>]+>"
let error_while_re = Rx.v ~flags:[ Rx.M ] "^ {8}#=== ERROR while "

let log_text page =
  let raw =
    match Rx.search pre_pre_re page with Some m -> Rx.group m 1 | None -> page
  in
  let text = Html.unescape (Rx.sub tag_re "" raw) in
  if Util.contains text "Building with day10" || Rx.test error_while_re text
  then remove_day10_indent text
  else text

let error_line = Rx.v {|^# ?(Error\b|.*\b(error|Error):)|}
let env_output_file_re = Rx.v "# (env|output)-file"

let error_package_conflict_re =
  Rx.v {|\[ERROR\] Package conflict|No solution|Sorry, no solution|}

let timestamp_re = Rx.v {|^\d{4}-\d\d-\d\d \d\d:\d\d\.\d\d: |}
let two_words_re = Rx.v {|^\S+ \S+ |}
let job_failed_re = Rx.v {|^\d{4}-\d\d-\d\d \d\d:\d\d\.\d\d: Job failed: (.*)|}
let new_job_re = Rx.v "New job: (.*)"

let git_merge_edit_re =
  Rx.v "git merge --no-edit ([0-9a-f]{40})|onto master ([0-9a-f]{40})"

let excerpt ?(max_output_lines = 40) text : (string * string) list =
  let lines = Array.of_list (Util.splitlines text) in
  let nl = Array.length lines in
  let out = Odict.create () in
  (* 1. First "ERROR while compiling" block (opam repeats it in the error report). *)
  (match
     Array.find_index (String.starts_with ~prefix:"#=== ERROR while ") lines
   with
  | None -> ()
  | Some i ->
      (* compiling, installing, fetching ...: up to an empty line or the next section *)
      let rec block_end k =
        if
          k < nl
          && not
               (String.trim lines.(k) = ""
               || String.starts_with ~prefix:"<><>" lines.(k))
        then block_end (k + 1)
        else k
      in
      let block = Array.to_list (Array.sub lines i (block_end (i + 1) - i)) in
      let header =
        List.filter
          (fun b ->
            (not (String.starts_with ~prefix:"### output" b))
            && not (Rx.test_match env_output_file_re b))
          block
      in
      let block =
        match List.find_index (( = ) "### output ###") block with
        | Some k ->
            let body = Util.drop (k + 1) block in
            let header = Util.take k block in
            let nb = List.length body in
            let body =
              if nb > max_output_lines then begin
                let cut = nb - max_output_lines in
                let tail = Util.drop (nb - max_output_lines) body in
                (* Keep the first error too when it is before the tail: dune
                    prints errors first and pages of warnings can follow. *)
                let first =
                  List.find_index (Rx.test error_line) (Util.take cut body)
                in
                match first with
                | None ->
                    Printf.sprintf "# [... %d lines elided ...]" cut :: tail
                | Some first ->
                    let lo = max 0 (first - 5) and hi = min (first + 15) cut in
                    (if lo > 0 then
                       [ Printf.sprintf "# [... %d lines elided ...]" lo ]
                     else [])
                    @ Util.sublist ~start:lo ~stop:hi body
                    @ (if cut > hi then
                         [
                           Printf.sprintf "# [... %d lines elided ...]"
                             (cut - hi);
                         ]
                       else [])
                    @ tail
              end
              else body
            in
            header @ [ "### output ###" ] @ body
        | None -> header
      in
      Odict.set out "error_block" (String.concat "\n" block));
  (* 2. Solver failure. *)
  (match Array.find_index (Rx.test error_package_conflict_re) lines with
  | Some i ->
      Odict.set out "solver"
        (String.concat "\n"
           (Array.to_list (Array.sub lines i (min 30 (nl - i)))))
  | None -> ());
  (* 3. "Log analysis" chosen message = last line of the log, after the timestamp. *)
  let tail =
    List.filter (fun l -> Rx.test_match timestamp_re l) (Array.to_list lines)
  in
  (match Util.last_opt tail with
  | Some l -> Odict.set out "final" (Rx.sub ~count:1 two_words_re "" l)
  | None -> ());
  Array.iter
    (fun l ->
      match Rx.match_ job_failed_re l with
      | Some m -> Odict.set out "job_failed" (Rx.group m 1)
      | None -> ())
    lines;
  (* 4. Builder and base commit info (useful to reproduce and compare). *)
  Array.iter
    (fun l ->
      (match Rx.search new_job_re l with
      | Some m when not (Odict.mem out "job") ->
          Odict.set out "job" (String.trim (Rx.group m 1))
      | _ -> ());
      (match Rx.search git_merge_edit_re l with
      | Some m when not (Odict.mem out "base") ->
          Odict.set out "base"
            (match Rx.group_opt m 1 with Some g -> g | None -> Rx.group m 2)
      | _ -> ());
      if Util.contains l "Building with day10" then
        Odict.set out "builder" "day10")
    lines;
  ignore (Odict.setdefault out "builder" "classic");
  Odict.to_list out

(* ------------------------------------------------------------------------ *)
(* Rendering *)

let platform_str job =
  let p = match String.concat "/" job.platform with "" -> "-" | p -> p in
  p ^ if job.experimental then " (exp)" else ""

let opam_vcmp a b =
  let order c =
    if c = '~' then -1
    else if Util.is_alpha c then Char.code c
    else Char.code c + 256
  in
  let cmp_nondigit x y =
    let n = max (String.length x) (String.length y) in
    let rec go i =
      if i >= n then 0
      else
        let ox = if i < String.length x then order x.[i] else 0 in
        let oy = if i < String.length y then order y.[i] else 0 in
        if ox <> oy then if ox < oy then -1 else 1 else go (i + 1)
    in
    go 0
  in
  let span p s =
    let i = ref 0 in
    while !i < String.length s && p s.[!i] do
      incr i
    done;
    !i
  in
  let rec loop a b =
    if a = "" && b = "" then 0
    else
      let ka = span (fun c -> not (Util.is_digit c)) a
      and kb = span (fun c -> not (Util.is_digit c)) b in
      let c = cmp_nondigit (String.sub a 0 ka) (String.sub b 0 kb) in
      if c <> 0 then c
      else
        let a = Util.drop_chars a ka and b = Util.drop_chars b kb in
        let da = span Util.is_digit a and db = span Util.is_digit b in
        (* Compare the numbers without overflowing: no leading zeros, then length, then digits. *)
        let strip0 s =
          let s = Util.lstrip ~chars:"0" s in
          s
        in
        let na = strip0 (String.sub a 0 da)
        and nb = strip0 (String.sub b 0 db) in
        let c =
          if String.length na <> String.length nb then
            compare (String.length na) (String.length nb)
          else compare na nb
        in
        if c <> 0 then if c < 0 then -1 else 1
        else
          loop
            (String.sub a da (String.length a - da))
            (String.sub b db (String.length b - db))
  in
  loop a b

let split_pkg full =
  match Util.partition full "." with name, _, version -> (name, version)

let ocaml_version job =
  match job.platform with
  | [ "compilers"; v ] -> Some v
  | [ "day10"; _; "x86_64"; v ] -> Some v
  | _ -> None
(* other platforms (distributions, extras, macos, ...) are not compared *)

let failing : Category.t list = [ Build; Build_or_test; Dependency; Solver ]

type failure_key = string option * string option * Kind.t * string

let command_noise_re = Rx.v {|[`"']|-j \d+|}

let failure_key j : failure_key =
  ( j.target,
    j.revdep,
    j.kind,
    Rx.sub command_noise_re "" (Option.value ~default:"" j.reason) )

let merge_jobs a b = Util.dedup (a @ b)

let arches =
  [
    "x86_64";
    "amd64";
    "arm64";
    "aarch64";
    "riscv64";
    "s390x";
    "ppc64";
    "ppc64le";
    "x86_32";
    "i386";
    "arm32";
  ]

let build_tests_revdeps_re =
  Rx.v {|^(\S+) \[(build|tests|revdeps|lower-bounds)(?:, revdep of (\S+))?\]$|}

let solver_line_re = Rx.v {|^(\S+) \[[^\]]*\]$|}
let packages_re = Rx.v {|^(\d+ packages) \[|}
let ocaml_re = Rx.v {|^(?:(.+?)-)?ocaml-(\d+\.\d+)(?:-(.+))?$|}

let describe_job variant =
  let parts = String.split_on_char ',' variant in
  let kind_suffix = function
    | "tests" -> " (tests)"
    | "lower-bounds" -> " (lower bounds)"
    | x -> " (" ^ x ^ ")"
  in
  match Rx.match_ build_tests_revdeps_re variant with
  | Some m -> (
      (* a pattern over several jobs (from matrix_rules) *)
      let pkg = Rx.group m 1
      and kind = Rx.group m 2 in
      match Rx.group_opt m 3 with
      | Some target -> pkg ^ ", reverse dependency of " ^ target
      | None -> (
          pkg
          ^
          match kind with
          | "tests" -> " (tests)"
          | "lower-bounds" -> " (lower bounds)"
          | _ -> ""))
  | None -> (
      match Rx.match_ solver_line_re variant with
      | Some m ->
          Rx.group m 1
          (* a label such as "pkg.1 [all build jobs skipped]": the package *)
      | None -> (
          if variant = "(lint)" then "opam lint"
          else
            match Rx.match_ packages_re variant with
            | Some m -> Rx.group m 1
            | None -> (
                let builder = List.hd parts in
                let n = List.length parts in
                let plat_rest =
                  if builder = "day10" && n >= 5 then
                    Some
                      ( Printf.sprintf "%s %s, OCaml %s (day10 builder)"
                          (List.nth parts 1) (List.nth parts 2)
                          (List.nth parts 3),
                        Util.drop 4 parts )
                  else if builder = "compilers" && n >= 3 then
                    Some ("OCaml " ^ List.nth parts 1, Util.drop 2 parts)
                  else if
                    List.mem builder
                      [ "distributions"; "extras"; "freebsd"; "macos" ]
                    && n >= 3
                  then begin
                    let p1 = List.nth parts 1 in
                    let plat =
                      match Rx.match_ ocaml_re p1 with
                      | Some m ->
                          let pre = Rx.group_opt m 1
                          and ver = Rx.group m 2
                          and post = Rx.group_opt m 3 in
                          let is_arch =
                            match post with
                            | Some p -> List.mem p arches || p = "amd64"
                            | None -> false
                          in
                          if is_arch then
                            Printf.sprintf "%s %s, OCaml %s"
                              (match pre with Some p -> p | None -> "None")
                              (Option.get post) ver
                          else
                            String.concat ", "
                              (List.filter
                                 (fun x -> x <> "")
                                 [
                                   Util.replace
                                     (Option.value ~default:"" pre)
                                     "opam-" "opam ";
                                   ("OCaml " ^ ver
                                   ^
                                   match post with
                                   | Some p when p <> "" -> " " ^ p
                                   | _ -> "");
                                 ])
                      | None -> p1
                    in
                    Some (plat, Util.drop 2 parts)
                  end
                  else None
                in
                match plat_rest with
                | None -> variant
                | Some (plat, rest) ->
                    let pkg = List.hd rest and extra = List.tl rest in
                    let what, extra =
                      match extra with
                      | "revdeps" :: r :: more ->
                          (r ^ ", reverse dependency of " ^ pkg, more)
                      | _ -> (pkg, extra)
                    in
                    let what =
                      what ^ String.concat "" (List.map kind_suffix extra)
                    in
                    what ^ " on " ^ plat)))

(* ------------------------------------------------------------------------ *)
(* Rules on the job matrix (no fetch), on log text, on lint messages, and on
   packages that install nowhere. *)

let neutral : Category.t list =
  [ Not_available; Ignored; Network; Cancelled; Sources ]

let failing_cats = failing
let classic_distro = "debian-13"

type dims = {
  ocaml : string option;
  os : string;
  distro : string option;
  arch : string;
}

let ocaml_version_re = Rx.v {|ocaml-(\d+\.\d+)|}
let ocaml_suffix_re = Rx.v "-ocaml-.*$"
let amd64_arm64_x86_re = Rx.v "-(amd64|arm64|x86_64)$"

let job_dims job =
  let p = job.platform in
  let joined = String.concat "," p in
  let named_ocaml =
    Option.map (fun m -> Rx.group m 1) (Rx.search ocaml_version_re joined)
  in
  let ocaml, distro, arch =
    match p with
    | "compilers" :: v :: _ -> (Some v, Some classic_distro, "x86_64")
    | "day10" :: d :: a :: v :: _ -> (Some v, Some d, a)
    | "distributions" :: d :: _ ->
        (named_ocaml, Some (Rx.sub ocaml_suffix_re "" d), "x86_64")
    | "extras" :: e :: _
      when List.mem (List.hd (String.split_on_char '-' e)) arches ->
        (named_ocaml, None, List.hd (String.split_on_char '-' e))
    | ("freebsd" | "macos") :: d :: _ ->
        let arch =
          match Rx.search amd64_arm64_x86_re d with
          | Some m -> Rx.group m 1
          | None -> "x86_64"
        in
        (named_ocaml, Some (Rx.sub ocaml_suffix_re "" d), arch)
    | _ -> (named_ocaml, None, "x86_64")
  in
  let dist = Option.value ~default:"" distro in
  let os =
    if
      String.starts_with ~prefix:"cygwin" dist
      || String.starts_with ~prefix:"msys2" dist
    then "win32"
    else if String.starts_with ~prefix:"freebsd" dist then "freebsd"
    else if String.starts_with ~prefix:"macos" dist then "macos"
    else "linux"
  in
  { ocaml; os; distro; arch }

let number_re = Rx.v {|\d+|}

let vkey v =
  List.map
    (fun d -> Option.value ~default:max_int (int_of_string_opt d))
    (Rx.findall number_re v)

let platform_words job =
  Util.last (Util.rsplit1 " on " (describe_job job.variant))

let failed_re = Rx.v {|\s*([\w+-]+\.[^\s:]+)(?: [\w+-]+\.[^\s:]+)* failed|}

let failing_dep reason =
  match Rx.match_ failed_re (Option.value ~default:"" reason) with
  | Some m -> Some (Rx.group m 1)
  | None -> None
