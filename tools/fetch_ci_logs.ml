(* Download the individual job logs from an opam-repository CI
   (opam.ci.ocaml.org) commit page.

   Usage:
       fetch-ci-logs https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/<sha>

   By default only jobs with status "failed" are fetched (skipped/passed
   jobs are ignored). Logs are written as plain text to ./ci-logs/<sha>/,
   together with:
       jobs.json    - every job listed on the page (variant, status, label, url, log file, errors)
       summary.txt  - failing jobs grouped by root cause *)

open Triage

let base = "https://opam.ci.ocaml.org"

(* The site returns 403 to the default urllib/curl user agent. *)
let headers =
  [ ("User-Agent", "Mozilla/5.0 (compatible; opam-ci-log-fetcher)") ]

(* Shown (with HTTP 200) when the site is rate limiting. *)
let busy_marker = "The site is busy"

let job_re =
  {|<li class="(?P<status>[a-z_]+)"><a href="(?P<href>[^"]*/variant/[^"]*)">(?P<label>.*?)</a>|}

(* Log lines that indicate a CI worker problem rather than a package problem. *)
let infra_patterns =
  [
    "runc run failed";
    "special device overlay does not exist";
    "No space left on device";
  ]

let commit_url_re = {|^/github/[^/]+/[^/]+/commit/[0-9a-f]{7,40}/?$|}

(** GET a URL, retrying on errors and on the site's 'busy' page. *)
let fetch ?(retries = 6) ?(backoff = 3.0) ?(timeout = 60.) url =
  let fail last = failwith (Printf.sprintf "failed to fetch %s: %s" url last) in
  (* [last]: why the previous attempt failed. *)
  let rec attempt k last =
    if k >= retries then fail last
    else
      let retry last =
        Unix.sleepf (backoff *. float_of_int (k + 1));
        attempt (k + 1) last
      in
      match Http.urlopen ~headers ~timeout url with
      | body when not (Util.contains body busy_marker) -> body
      | _ -> retry "site busy (rate limited)"
      | exception (Http.Http_error { code = 404; _ } as e) ->
          fail (Http.error_text e)
      | exception ((Http.Http_error _ | Http.Net_error _) as e) ->
          retry (Http.error_text e)
  in
  attempt 0 "None"

(** Turn a job page into plain text: keep the log, drop markup. *)
let html_to_text page =
  (* Python: re.sub(r"<(script|style)\b.*?</\1>", "", page, flags=re.S | re.I) *)
  let lower = String.lowercase_ascii page in
  let b = Buffer.create (String.length page) in
  let rec go pos =
    match Rx.search ~flags:[ Rx.S; Rx.I ] ~pos {|<(script|style)\b|} page with
    | None ->
        Buffer.add_string b (String.sub page pos (String.length page - pos))
    | Some m ->
        let tag = String.lowercase_ascii (Rx.group m 1) in
        let close = Util.find ~from:(Rx.end_ m) lower ("</" ^ tag ^ ">") in
        if close < 0 then begin
          (* No closing tag: no match here; try after this position. *)
          let a = Rx.start m in
          Buffer.add_string b (String.sub page pos (a + 1 - pos));
          go (a + 1)
        end
        else begin
          Buffer.add_string b (String.sub page pos (Rx.start m - pos));
          go (close + String.length tag + 3)
        end
  in
  go 0;
  let page = Buffer.contents b in
  let page = Rx.sub ~flags:[ Rx.I ] {|<br\s*/?>|} "\n" page in
  String.trim (Html.unescape (Rx.sub "<[^>]+>" "" page)) ^ "\n"

let parse_jobs page =
  let jobs = Odict.create () in
  List.iter
    (fun m ->
      let g k = Option.value ~default:"" (Rx.named m k) in
      let href = Html.unescape (g "href") in
      let variant = Http.unquote (Util.last (Util.rsplit1 "/variant/" href)) in
      let label =
        String.concat " "
          (Util.split (Html.unescape (Rx.sub "<[^>]+>" "" (g "label"))))
      in
      if not (Odict.mem jobs variant) then
        Odict.set jobs variant
          (Dict.of_list
             [
               ("variant", `String variant);
               ("status", `String (g "status"));
               ("label", `String label);
               ("url", `String (base ^ href));
             ]))
    (Rx.finditer ~flags:[ Rx.S ] job_re page);
  Odict.values jobs

let safe_name variant = Rx.sub {|[^A-Za-z0-9._,+-]|} "_" variant ^ ".log"

(** Pull out the interesting lines: opam [ERROR]s, lint errors, the final 'Job
    failed' line. *)
let extract_errors text =
  let keep = ref [] in
  List.iter
    (fun line ->
      let s = String.trim line in
      if
        (String.starts_with ~prefix:"[ERROR]" s
        || String.starts_with ~prefix:"#=== ERROR while" s
        || String.starts_with ~prefix:"Error in " s
        || Util.contains s "Job failed:"
        || Util.contains s "command not found"
        || String.starts_with ~prefix:"[NOTE] Building " s
        || List.exists (fun p -> Util.contains s p) infra_patterns)
        && not (List.mem s !keep)
      then keep := !keep @ [ s ])
    (Util.splitlines text);
  !keep

let root_cause job =
  let label = Dict.str job "label" in
  let cause =
    match Rx.search {|\(failed: (.*)\)\s*$|} label with
    | Some m -> Rx.group m 1
    | None -> label
  in
  let default = cause in
  if Util.contains cause "a dependency failed" then begin
    let errors = Dict.strs job "errors" in
    match
      List.find_map
        (fun e ->
          match Rx.match_ {|\[ERROR\] (\S+) failed|} e with
          | Some m -> Some (Rx.group m 1)
          | None ->
              Option.map
                (fun m -> Rx.group m 1)
                (Rx.match_ {|#=== ERROR while \w+ (\S+)|} e))
        errors
    with
    | Some d -> d ^ " failed: (via dependency)"
    | None ->
        if
          List.exists
            (fun e -> List.exists (fun p -> Util.contains e p) infra_patterns)
            errors
        then begin
          let building =
            List.filter_map
              (fun e ->
                if String.starts_with ~prefix:"[NOTE] Building " e then
                  Some (Util.last (Util.split e))
                else None)
              errors
          in
          let where =
            match Util.last_opt building with
            | Some b -> " while building " ^ b
            | None -> ""
          in
          Printf.sprintf
            "CI infrastructure error%s (container/mount failure; likely needs \
             a retry)"
            where
        end
        else default
  end
  else default

let run ~url ~out ~status ~all ~jobs_n ~force () =
  let arg_url = url in
  let netloc, path =
    match
      Rx.match_ {|^[A-Za-z][A-Za-z0-9+.-]*://([^/?#]*)([^?#]*)|}
        (String.trim arg_url)
    with
    | Some m -> (Rx.group m 1, Rx.group m 2)
    | None -> ("", "")
  in
  if netloc <> "opam.ci.ocaml.org" || not (Rx.test_match commit_url_re path)
  then
    Util.die
      "expected a URL like \
       https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/<sha>";
  let path = Util.rstrip ~chars:"/" path in
  let sha = Util.last (Util.rsplit1 "/" path) in
  let out =
    match out with Some o -> o | None -> Filename.concat "ci-logs" sha
  in
  let out =
    if String.length out > 1 then Util.rstrip ~chars:"/" out else out
    (* as pathlib prints it *)
  in
  Util.makedirs out;
  let write name text =
    let oc = open_out_bin (Filename.concat out name) in
    output_string oc text;
    close_out oc
  in
  Printf.eprintf "Fetching summary page for %s ...\n%!" sha;
  let page = fetch (base ^ path) in
  write "commit.html" page;
  let jobs = parse_jobs page in
  if jobs = [] then
    Util.die "no jobs found on the page (format changed, or CI hasn't run yet?)";
  let counts =
    List.sort compare
      (Util.counter (List.map (fun j -> Dict.str j "status") jobs))
  in
  let counts_s =
    String.concat ", "
      (List.map (fun (k, v) -> Printf.sprintf "%s=%d" k v) counts)
  in
  Printf.eprintf "Jobs on page: %s\n%!" counts_s;
  let wanted = if status = [] then [ "failed" ] else status in
  let selected =
    List.filter (fun j -> all || List.mem (Dict.str j "status") wanted) jobs
  in
  Printf.eprintf "Downloading %d log(s) to %s/ ...\n%!" (List.length selected)
    out;
  let work job =
    let name = safe_name (Dict.str job "variant") in
    let p = Filename.concat out name in
    Dict.set_str job "log_file" name;
    let text =
      if Sys.file_exists p && not force then
        In_channel.with_open_bin p In_channel.input_all
      else begin
        let text = html_to_text (fetch (Dict.str job "url")) in
        write name text;
        text
      end
    in
    Dict.set job "errors" (Json.strings (extract_errors text))
  in
  let failures = ref [] and done_ = ref 0 and m = Mutex.create () in
  let queue = ref selected in
  let total = List.length selected in
  let worker () =
    let rec loop () =
      Mutex.lock m;
      let next =
        match !queue with
        | j :: rest ->
            queue := rest;
            Some j
        | [] -> None
      in
      Mutex.unlock m;
      match next with
      | None -> ()
      | Some j ->
          let err =
            try
              work j;
              None
            with e -> Some (Util.exn_text e)
            (* keep going; report at the end *)
          in
          Mutex.lock m;
          (match err with
          | Some e ->
              Dict.set_str j "fetch_error" e;
              failures := !failures @ [ j ]
          | None -> ());
          incr done_;
          Printf.eprintf "\r  %d/%d%!" !done_ total;
          Mutex.unlock m;
          loop ()
    in
    loop ()
  in
  List.iter Thread.join
    (List.init (max 1 jobs_n) (fun _ -> Thread.create worker ()));
  prerr_newline ();
  write "jobs.json"
    (Json.dumps ~indent:2 (`List (List.map Dict.to_json jobs)) ^ "\n");
  let groups =
    Util.group_by root_cause
      (List.filter (fun j -> Dict.str j "status" = "failed") selected)
    |> List.map (fun (c, js) ->
        (c, List.map (fun j -> Dict.str j "variant") js))
    |> List.stable_sort (fun (_, a) (_, b) ->
        compare (List.length b) (List.length a))
  in
  let lines =
    [ "CI summary for " ^ arg_url; "Jobs: " ^ counts_s; "" ]
    @ List.concat_map
        (fun (cause, variants) ->
          Printf.sprintf "[%d] %s" (List.length variants) cause
          :: List.map (fun v -> "    " ^ v) (List.sort compare variants)
          @ [ "" ])
        groups
  in
  write "summary.txt" (String.concat "\n" lines);
  print_endline
    (String.concat "\n"
       (Util.take 2 lines @ [ "" ]
       @ List.map
           (fun (c, v) -> Printf.sprintf "[%d] %s" (List.length v) c)
           groups));
  Printf.printf "\nLogs, jobs.json and summary.txt written to %s/\n" out;
  if !failures <> [] then begin
    Printf.eprintf "\n%d log(s) could not be downloaded (re-run to retry):\n"
      (List.length !failures);
    List.iter
      (fun j ->
        Printf.eprintf "  %s: %s\n" (Dict.str j "variant")
          (Dict.str j "fetch_error"))
      !failures;
    exit 1
  end

let () =
  let open Cmdliner in
  let url =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"URL"
          ~doc:"the commit page of the CI run (or a commit sha)")
  in
  let out =
    Arg.(
      value
      & opt (some string) None
      & info [ "o"; "out" ] ~docv:"OUT" ~doc:"output directory")
  in
  let status =
    Arg.(
      value & opt_all string []
      & info [ "s"; "status" ] ~docv:"STATUS"
          ~doc:"only the jobs with this status (repeatable; default: failed)")
  in
  let all =
    Arg.(
      value & flag
      & info [ "all" ] ~doc:"all the jobs, not only the failed ones")
  in
  let jobs_n =
    Arg.(
      value & opt int 3
      & info [ "j"; "jobs" ] ~docv:"JOBS" ~doc:"parallel downloads")
  in
  let force =
    Arg.(value & flag & info [ "force" ] ~doc:"download the logs again")
  in
  let term =
    Term.(
      const (fun url out status all jobs_n force ->
          run ~url ~out ~status ~all ~jobs_n ~force ())
      $ url $ out $ status $ all $ jobs_n $ force)
  in
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "fetch-ci-logs"
             ~doc:
               "Download the failed job logs of a CI run into ci-logs/<sha>/.")
          term))
