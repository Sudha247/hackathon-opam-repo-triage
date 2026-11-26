(* HTTP, GitHub API and git access, with their on-disk caches. *)

let base = "https://opam.ci.ocaml.org/github/ocaml/opam-repository/commit/"

let net : int Odict.t =
  Odict.of_list [ ("http", 0); ("mcp", 0); ("git", 0); ("github-api", 0) ]

let offline = Sys.getenv_opt "OPAM_CI_OFFLINE" = Some "1"
let offline_ref = ref offline
let is_offline () = !offline_ref
let write_atomic_hook : (string -> string -> unit) ref = ref (fun _ _ -> ())

let counts_json () : Json.t =
  `Assoc (List.map (fun (k, v) -> (k, `Int v)) (Odict.to_list net))

let net_set k v =
  Odict.set net k v;
  match Util.getenv_nonempty "OPAM_CI_NET_FILE" with
  | Some path -> (
      try !write_atomic_hook path (Json.dumps (counts_json ()))
      with Sys_error _ | Unix.Unix_error _ -> ())
  | None -> ()

let net_get k = Option.value ~default:0 (Odict.get net k)
let net_incr k = net_set k (net_get k + 1)

(** Count a failed request ("net-errors"): the analysis is incomplete, and the
    triage page analyses the PR again later. *)
let net_error () = net_incr "net-errors"

let offline_misses : string list ref = ref []
let offline_miss what = offline_misses := !offline_misses @ [ what ]
let user_agent = "opam-ci-triage"

(* ------------------------------------------------------------------------ *)
(* Fetching *)

(* Test seams. *)
let rename_hook = ref Sys.rename
let sleep_hook = ref Unix.sleepf

let write_atomic path data =
  if is_offline () && Some path <> Util.getenv_nonempty "OPAM_CI_NET_FILE" then
    ()
  else begin
    let d = Filename.dirname (Util.abspath path) in
    Util.makedirs d;
    let tmp = Filename.temp_file ~temp_dir:d ".tmp-" "" in
    try
      let oc = open_out_bin tmp in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc data);
      !rename_hook tmp path
    with e ->
      Util.remove tmp;
      raise e
  end

let () = write_atomic_hook := write_atomic

let fresh path max_age =
  match max_age with
  | None -> true
  | Some a -> is_offline () || Unix.gettimeofday () -. Util.getmtime path < a

let cached_json ?max_age path : Json.t option =
  let try_one p read =
    if not (Sys.file_exists p) then None
    else if not (fresh p max_age) then Some None
    else Some (try Some (Json.parse (read p)) with _ -> None)
  in
  match
    try_one path (fun p -> In_channel.with_open_bin p In_channel.input_all)
  with
  | Some r -> r
  | None -> (
      match try_one (path ^ ".gz") Util.read_gzip with
      | Some r -> r
      | None -> None)

let sha256_hex s = Digestif.SHA256.(to_hex (digest_string s))

let cache_path url cache_dir =
  Filename.concat cache_dir (String.sub (sha256_hex url) 0 32 ^ ".html")

(* A page that did not exist (404) is fetched again after a day. *)
let not_found_max_age = 24. *. 3600.

let fetch ?(refresh = false) ?max_age url cache_dir =
  let path = cache_path url cache_dir in
  if (not refresh) && Sys.file_exists path && fresh path max_age then
    In_channel.with_open_bin path In_channel.input_all
  else if (not refresh) && Sys.file_exists (path ^ ".gz") then
    Util.read_gzip (path ^ ".gz")
  else if
    (not refresh)
    && Sys.file_exists (path ^ ".404")
    && fresh (path ^ ".404")
         (Some
            (Float.min
               (match max_age with
               | Some a when a > 0. -> a
               | _ -> not_found_max_age)
               not_found_max_age))
  then
    raise
      (Http.Http_error
         { code = 404; url; headers = [ (Http.cached_marker, "") ]; body = "" })
  else if is_offline () then begin
    offline_miss url;
    failwith (Printf.sprintf "offline: %s is not in the cache" url)
  end
  else begin
    net_incr "http";
    (* A request that fails (after the retries) is counted in "net-errors",
       whatever the caller does with the exception: a log or a page that
       could not be read makes the analysis incomplete. A 404 is an answer,
       not counted. *)
    let body =
      try
        let rec attempt k =
          let r =
            Http.request
              ~headers:[ ("User-Agent", user_agent) ]
              ~timeout:120. url
          in
          if r.status >= 400 || r.status = 0 then begin
            if r.status = 404 then write_atomic (path ^ ".404") "";
            if (not (List.mem r.status [ 429; 502; 503 ])) || k = 5 then
              raise
                (Http.Http_error
                   {
                     code = r.status;
                     url;
                     headers = r.resp_headers;
                     body = r.data;
                   });
            let delay =
              match
                Option.bind
                  (Http.header r.resp_headers "Retry-After")
                  int_of_string_opt
              with
              | Some d when d > 0 -> d
              | _ -> 1 lsl (k + 1)
            in
            !sleep_hook (float_of_int (min delay 60));
            attempt (k + 1)
          end
          else r.data
        in
        attempt 0
      with
      | Http.Http_error { code; _ } as e ->
          if code <> 404 then net_error ();
          raise e
      | Http.Net_error _ as e ->
          net_error ();
          raise e
    in
    if body = "" then begin
      net_error ();
      failwith ("empty response for " ^ url)
    end;
    write_atomic path body;
    body
  end

(* ------------------------------------------------------------------------ *)
(* Baseline comparison

   Cheap baseline: the opam-repo-ci results of the PR that published the
   previous version of the same package. Its revdeps jobs built the same
   revdep versions against the previous version, so a failure present there
   is not caused by the new PR. Costs a few GitHub API calls and one page
   fetch. *)

let repo = "ocaml/opam-repository"

let github_token () =
  match Util.getenv_nonempty "GITHUB_TOKEN" with
  | Some t -> Some t
  | None -> (
      match Util.getenv_nonempty "GH_TOKEN" with
      | Some t -> Some t
      | None -> (
          try
            let out = Proc.run ~timeout:10. [ "gh"; "auth"; "token" ] in
            match String.trim out.stdout with "" -> None | t -> Some t
          with _ -> None))

let gh_cache : (string, Json.t) Hashtbl.t = Hashtbl.create 16

let gh_api ?(cache_dir = ".opam-ci-cache") ?(cache = true) path : Json.t option
    =
  match Hashtbl.find_opt gh_cache path with
  | Some v -> Some v
  | None -> (
      let disk =
        Filename.concat cache_dir
          ("api-" ^ String.sub (sha256_hex path) 0 24 ^ ".json")
      in
      let hit = if cache || is_offline () then cached_json disk else None in
      match hit with
      | Some v ->
          Hashtbl.replace gh_cache path v;
          Some v
      | None ->
          if is_offline () then (
            offline_miss ("github-api " ^ path);
            None)
          else begin
            net_incr "github-api";
            let tok = github_token () in
            let headers =
              [
                ("User-Agent", user_agent);
                ("Accept", "application/vnd.github+json");
              ]
              @
              match tok with
              | Some t -> [ ("Authorization", "Bearer " ^ t) ]
              | None -> []
            in
            match
              Http.urlopen ~headers ~timeout:60.
                ("https://api.github.com/" ^ path)
            with
            | body -> (
                match Json.parse_opt body with
                | None ->
                    net_error ();
                    None
                | Some v ->
                    if cache || Sys.getenv_opt "OPAM_CI_RECORD" = Some "1" then
                      write_atomic disk (Json.dumps v);
                    Hashtbl.replace gh_cache path v;
                    Some v)
            | exception Http.Http_error { code; _ } ->
                if (code = 403 || code = 429) && tok = None then
                  prerr_endline
                    "note: GitHub API rate limit reached (60/hour without a \
                     token); set GITHUB_TOKEN to raise it";
                if code <> 404 then net_error ();
                None
            | exception (Http.Net_error _ | Sys_error _ | Unix.Unix_error _) ->
                net_error ();
                None
          end)

(* ------------------------------------------------------------------------ *)
(* Bound suggestions

   Cheap rules only: CI logs, opam files and upstream git tags. Nothing is
   built. Upstream sources are read with plain git (no token, any host):
   [git ls-remote --tags], then a shallow blob-less fetch of one tag gives
   the file list, and single files are read from raw.githubusercontent.com
   (GitHub) or fetched on demand by git (other hosts). Each suggestion
   carries its evidence.

   Upstream git URLs come from the dev-repo and src fields of opam files
   (untrusted): only plain https URLs reach git, after [--] where git takes
   it, and git may only use the https transport (no ssh://, file://, ext::). *)
let safe_git_url_re = {|https://[A-Za-z0-9][A-Za-z0-9.-]*/[^\s]+|}

let git_protocols =
  [ "-c"; "protocol.allow=never"; "-c"; "protocol.https.allow=always" ]

let git_timeout = 60.

let git_fetch_timeout =
  180. (* a shallow blob-less fetch of a large repository *)

(* A fetch or ls-remote that failed because of the network or the server
   (counted in "net-errors": the tags or files are then unknown, not
   absent), as opposed to a clean negative answer: a missing tag ("couldn't
   find remote ref"), repository ("not found", 404) or a private one
   (credentials asked). *)
let git_net_failure =
  {|timed out|Could not resolve (host|proxy)|Failed to connect|Connection (refused|reset|timed out)|returned error: (5\d\d|429)|RPC failed|early EOF|unexpected disconnect|hung up unexpectedly|transfer closed|Recv failure|Empty reply|SSL|TLS|gnutls|Couldn't connect|}

let git_unreachable =
  {|timed out|Could not resolve host|Failed to connect|Couldn't connect|Connection (refused|timed out)|SSL|TLS|gnutls|certificate|}

let git_down_max_age = 24. *. 3600.

let safe_git_url url =
  match url with
  | Some u when u <> "" -> Rx.fullmatch safe_git_url_re u <> None
  | _ -> false

let git ?cwd ?(check = true) ?git_dir args : Proc.result =
  let cmd = List.hd args in
  if (cmd = "fetch" || cmd = "ls-remote") && is_offline () then begin
    offline_miss ("git " ^ String.concat " " (Util.take 4 args));
    { Proc.code = 1; stdout = ""; stderr = "offline" }
  end
  else begin
    if List.mem cmd [ "fetch"; "ls-remote"; "cat-file" ] then net_incr "git";
    let env =
      [ ("GIT_TERMINAL_PROMPT", "0") ]
      @ (match git_dir with Some d -> [ ("GIT_DIR", d) ] | None -> [])
      @ if is_offline () then [ ("GIT_NO_LAZY_FETCH", "1") ] else []
    in
    let timeout = if cmd = "fetch" then git_fetch_timeout else git_timeout in
    let out =
      try Proc.run ?cwd ~env ~timeout (("git" :: git_protocols) @ args) with
      | Proc.Timeout _ ->
          {
            Proc.code = 124;
            stdout = "";
            stderr = Printf.sprintf "timed out after %.0f s" timeout;
          }
      | Unix.Unix_error (e, _, _) ->
          { Proc.code = 127; stdout = ""; stderr = Unix.error_message e }
    in
    if
      (cmd = "fetch" || cmd = "ls-remote")
      && out.code <> 0
      && Rx.test ~flags:[ Rx.I ] git_net_failure out.stderr
    then net_error ();
    if check && out.code <> 0 then
      failwith
        (Printf.sprintf "git %s...: %s"
           (String.concat " " (Util.take 3 args))
           (Util.prefix_chars (String.trim out.stderr) 200));
    out
  end

let repo_url dev_repo =
  let u = Util.removeprefix (String.trim dev_repo) "git+" in
  let u = Rx.sub "^git://" "https://" u in
  let u = Rx.sub "^git@([^:]+):" {|https://\1/|} u in
  let u = List.hd (Util.split_on ~maxsplit:1 "#" u) in
  match
    Rx.match_ {|(https://github\.com/[^/]+/[^/]+?)(?:\.git)?(?:/.*)?$|} u
  with
  | Some m ->
      Some (Rx.group m 1)
      (* also archive URLs: https://github.com/o/r/releases/... -> repo *)
  | None ->
      let u = Rx.sub "/-/(archive|tags|tree)/.*$" "" u in
      (* GitLab archive URLs (nested groups kept) *)
      let u = Util.removesuffix u "/" in
      if safe_git_url (Some u) then Some u else None
