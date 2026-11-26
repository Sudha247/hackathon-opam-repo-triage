open Net

(* Test seam: git as the sources run it. *)
let git_hook = ref Net.git
let git ?cwd ?check ?git_dir args = !git_hook ?cwd ?check ?git_dir args
let split_pkg = Ci.split_pkg
let opam_vcmp = Ci.opam_vcmp
let sort_versions vs = List.stable_sort opam_vcmp vs

let max_version vs =
  List.fold_left
    (fun a v ->
      match a with
      | None -> Some v
      | Some b -> Some (if opam_vcmp v b > 0 then v else b))
    None vs

let min_version vs =
  List.fold_left
    (fun a v ->
      match a with
      | None -> Some v
      | Some b -> Some (if opam_vcmp v b < 0 then v else b))
    None vs

(* ------------------------------------------------------------------------ *)
(* Answers of ocaml.org's MCP tools *)

type history = {
  versions : (string * string) list;
  hmodule : string option;
  partial : bool;
}

type find_match = {
  package : string;
  module_path : string list;
  mversion : string option;
  library : string option;
}

type module_find = {
  matches : find_match list;
  checked : (string * string) list;
  no_docs : string list;
  distribution : bool;
  fpartial : bool;
}

type changelog = {
  file : string option;
  cversions : string list;
  text : string;
  truncated : bool;
}

let mcp_failures = ref 0
let mcp_unavailable : (string, unit) Hashtbl.t = Hashtbl.create 4

type t = {
  cache_dir : string;
  raw_mem : (string, string option) Hashtbl.t;
  gitdirs : (string, unit) Hashtbl.t;
  down : (string, bool) Hashtbl.t;
  ensure_mem : (string * string, string option) Hashtbl.t;
  tags_mem : (string, string list) Hashtbl.t;
  tree_mem : (string * string, string list) Hashtbl.t;
  mutable master_sha : string option option;
  mcp_mem : (string, Json.t) Hashtbl.t;
  versions_mem : (string, Json.t list option) Hashtbl.t;
  docpath : (string * string, string) Hashtbl.t;
  history_mem : (string, history option) Hashtbl.t;
  find_mem : (string, module_find option) Hashtbl.t;
  mem : (string, Json.t) Hashtbl.t;  (** for the rules' own memoisation *)
}

let create cache_dir =
  {
    cache_dir;
    raw_mem = Hashtbl.create 64;
    gitdirs = Hashtbl.create 4;
    down = Hashtbl.create 4;
    ensure_mem = Hashtbl.create 8;
    tags_mem = Hashtbl.create 8;
    tree_mem = Hashtbl.create 8;
    master_sha = None;
    mcp_mem = Hashtbl.create 64;
    versions_mem = Hashtbl.create 16;
    docpath = Hashtbl.create 8;
    history_mem = Hashtbl.create 8;
    find_mem = Hashtbl.create 8;
    mem = Hashtbl.create 8;
  }

let opam_repo_url = "https://github.com/" ^ repo

let raw ?max_age src url =
  match Hashtbl.find_opt src.raw_mem url with
  | Some v -> v
  | None ->
      let v =
        try Some (fetch ?max_age url src.cache_dir)
        with
        | Http.Http_error _ | Http.Net_error _ | Failure _ | Sys_error _
        | Unix.Unix_error _
        ->
          None
        (* HTTP or network error, offline cache miss, empty answer *)
      in
      Hashtbl.replace src.raw_mem url v;
      v

(* Test seam. *)
let raw_hook = ref raw
let raw ?max_age src url = !raw_hook ?max_age src url

(* ---- git plumbing (bare, shallow, blob-less mirrors under cache_dir/git) *)

(* Touched on each use: the CI workflow deletes the mirrors not used for a month. *)
let git_stamp = "opam-ci-last-used"
let short_hash n s = String.sub (sha256_hex s) 0 n

let gitdir src url =
  if not (safe_git_url (Some url)) then
    failwith
      (Printf.sprintf "not a plain https git URL: %s"
         (Util.repr (Util.prefix_chars url 100)));
  let d =
    Filename.concat (Filename.concat src.cache_dir "git") (short_hash 16 url)
  in
  if not (Hashtbl.mem src.gitdirs d) then begin
    (* A mirror whose files were deleted (cache pruning) is made again. *)
    let valid =
      Util.is_dir d
      && (git ~git_dir:d ~check:false [ "rev-parse"; "--git-dir" ]).code = 0
      && String.trim
           (git ~git_dir:d ~check:false
              [ "config"; "--get"; "remote.origin.url" ])
             .stdout
         = url
    in
    if (not valid) && not (is_offline ()) then begin
      Util.rmtree d;
      Util.makedirs d;
      ignore (git [ "init"; "-q"; "--bare"; d ]);
      ignore (git ~git_dir:d [ "remote"; "add"; "--"; "origin"; url ]);
      ignore (git ~git_dir:d [ "config"; "remote.origin.promisor"; "true" ]);
      ignore
        (git ~git_dir:d
           [ "config"; "remote.origin.partialclonefilter"; "blob:none" ]);
      ignore (git ~git_dir:d [ "config"; "extensions.partialClone"; "origin" ])
    end;
    write_atomic (Filename.concat d git_stamp) "";
    Hashtbl.replace src.gitdirs d ()
  end;
  d

let down_marker src url =
  Filename.concat src.cache_dir ("gitdown-" ^ short_hash 16 url)

(** Whether a fetch or listing of [url] failed in the last [git_down_max_age] in
    a way a dead host fails every time (see [Net.git_unreachable]): it is not
    asked again meanwhile. That failure was counted once in "net-errors" (the
    triage page retries the analysis a few times); a cached one is counted in
    "git-unreachable" instead, so that a dead dev-repo host does not make every
    analysis incomplete and retried, each after a timeout. *)
let git_down src url =
  match Hashtbl.find_opt src.down url with
  | Some d -> d
  | None ->
      let p = down_marker src url in
      let d = Sys.file_exists p && fresh p (Some git_down_max_age) in
      Hashtbl.replace src.down url d;
      if d then net_incr "git-unreachable";
      d

(* Remember a failure of git on [url] that a dead host gives every time (not
   offline: write_atomic writes nothing then). *)
let note_down src url (out : Proc.result) =
  if out.code <> 0 && Rx.test ~flags:[ Rx.I ] git_unreachable out.stderr then begin
    write_atomic (down_marker src url)
      (Util.prefix_chars (String.trim out.stderr) 300);
    Hashtbl.replace src.down url true
  end

let ensure src url ref is_tag =
  match Hashtbl.find_opt src.ensure_mem (url, ref) with
  | Some r -> r
  | None ->
      if not (safe_git_url (Some url)) then None
      else begin
        let d = gitdir src url in
        let rev = if is_tag then "refs/tags/" ^ ref else ref in
        let rev =
          if
            (git ~git_dir:d ~check:false
               [ "rev-parse"; "--verify"; "-q"; rev ^ "^{tree}" ])
              .code <> 0
          then begin
            let spec =
              if is_tag then Printf.sprintf "+refs/tags/%s:refs/tags/%s" ref ref
              else ref
            in
            if git_down src url then None
            else
              let r =
                git ~git_dir:d ~check:false
                  [
                    "fetch";
                    "-q";
                    "--depth";
                    "1";
                    "--filter=blob:none";
                    "origin";
                    spec;
                  ]
              in
              note_down src url r;
              if r.code <> 0 then None else Some rev
          end
          else Some rev
        in
        Hashtbl.replace src.ensure_mem (url, ref) rev;
        rev
      end

let tags src url =
  match Hashtbl.find_opt src.tags_mem url with
  | Some t -> t
  | None ->
      let disk =
        Filename.concat src.cache_dir ("tags-" ^ short_hash 16 url ^ ".json")
      in
      let tags =
        match cached_json ~max_age:86400. disk with
        | Some (`List (_ :: _ as l)) -> List.filter_map Json.to_str_opt l
        | _ ->
            if (not (safe_git_url (Some url))) || git_down src url then []
            else begin
              let r =
                git ~check:false [ "ls-remote"; "--tags"; "--refs"; "--"; url ]
              in
              let tags =
                List.filter_map
                  (fun l ->
                    match Util.partition l "refs/tags/" with
                    | _, "refs/tags/", t -> Some t
                    | _ -> None)
                  (Util.splitlines r.stdout)
              in
              (* A failed listing (network error, offline) is not kept on disk;
             an unreachable host is remembered for a day (git_down). *)
              note_down src url r;
              if r.code = 0 && tags <> [] then
                write_atomic disk (Json.dumps (Json.strings tags));
              tags
            end
      in
      Hashtbl.replace src.tags_mem url tags;
      tags

(* Test seam. *)
let tags_hook = ref tags
let tags src url = !tags_hook src url

let tag_for src url name version =
  let tags = tags src url in
  List.find_opt
    (fun c -> List.mem c tags)
    [
      version;
      "v" ^ version;
      name ^ "-" ^ version;
      name ^ "." ^ version;
      name ^ "-v" ^ version;
      "release-" ^ version;
      "Release-" ^ version;
      Util.replace version "~" "-";
      "v" ^ Util.replace version "~" "-";
      Util.replace version "~" "_";
      "v" ^ Util.replace version "~" "_";
    ]

(* Test seam. *)
let tag_for_hook = ref tag_for
let tag_for src url name version = !tag_for_hook src url name version

let tree ?(is_tag = true) src url ref =
  match Hashtbl.find_opt src.tree_mem (url, ref) with
  | Some p -> p
  | None ->
      let paths =
        match ensure src url ref is_tag with
        | Some rev ->
            Util.splitlines
              (git ~git_dir:(gitdir src url) ~check:false
                 [ "ls-tree"; "-r"; "--name-only"; rev ])
                .stdout
        | None -> []
      in
      Hashtbl.replace src.tree_mem (url, ref) paths;
      paths

(* Test seam. *)
let tree_hook = ref tree
let tree ?is_tag src url ref = !tree_hook ?is_tag src url ref

let file_at ?(is_tag = true) src url ref path =
  match Rx.match_ {|https://github\.com/([^/]+/[^/]+)$|} url with
  | Some m ->
      (* Upstream paths may hold spaces or non-ASCII characters: quoted
       (characters allowed in a URL path are kept, so are the cache keys). *)
      raw src
        (Printf.sprintf "https://raw.githubusercontent.com/%s/%s/%s"
           (Rx.group m 1) ref
           (Http.quote ~safe:"/!$&'()*+,;=:@~" path))
  | None -> (
      match ensure src url ref is_tag with
      | None -> None
      | Some rev ->
          let r =
            git ~git_dir:(gitdir src url) ~check:false
              [ "cat-file"; "-p"; rev ^ ":" ^ path ]
          in
          if r.code = 0 then Some r.stdout else None)

(* Test seam. *)
let file_at_hook = ref file_at
let file_at ?is_tag src url ref path = !file_at_hook ?is_tag src url ref path

(* ---- ocaml.org documentation, through its MCP endpoint (JSON, plain-text
   signatures; docs are built for every package version in opam-repository) *)

let mcp_url = "https://ocaml.org/mcp"
let mcp_timeout = 20.
let mcp_max_failures = 3

(* The tool's own errors (an unknown package, a version without docs yet)
   are asked again after a day: docs get built, a tool error may be transient. *)
let mcp_error_max_age = 24. *. 3600.

(* Whether an MCP answer is kept a day only ([mcp_error_max_age]): the tool's
   error (null), or documentation not built (yet): a [documentation_status]
   other than success ("unknown", "pending", "failed"...), which changes
   when ocaml.org builds it. *)
let short_lived (v : Json.t) =
  match v with
  | `Null -> true
  | `Assoc _ -> (
      match Json.member "documentation_status" v with
      | `Null | `String "success" -> false
      | _ -> true)
  | _ -> false

(* Tools whose callers need to know more than "no answer" (see [mcp_call]):
   their errors are answers kept as {"tool_error": message}; a partial answer
   (docs-ci could not be read for some versions or candidates, or the call
   ran out of time) is returned but neither cached nor counted as answered;
   a server that does not have the tool yet makes it unavailable
   ([mcp_unavailable]), which is not a failure: the callers have another way. *)
let mcp_strict_tools = [ "ocaml_module_changes"; "ocaml_module_find" ]

let mcp_key tool (args : (string * Json.t) list) =
  Json.dumps ~sort_keys:true (`List [ `String tool; `Assoc args ])

exception Mcp_failure of string

(** [mcp()]: (value, answered), [answered] false when the call failed (as
    opposed to the tool answering with an error). For the tools of
    [mcp_strict_tools], a partial answer is returned with [answered] false, and
    counted in "mcp-errors". The value is [`Null] for no answer. *)
let mcp_call ?max_age src tool args : Json.t * bool =
  let strict = List.mem tool mcp_strict_tools in
  if strict && Hashtbl.mem mcp_unavailable tool then (`Null, false)
  else begin
    let key = mcp_key tool args in
    match Hashtbl.find_opt src.mcp_mem key with
    | Some v -> (v, true)
    | None -> (
        let disk =
          Filename.concat src.cache_dir ("mcp-" ^ short_hash 24 key ^ ".json")
        in
        let hit = cached_json ?max_age disk in
        let hit =
          match hit with
          | Some v when short_lived v ->
              cached_json
                ~max_age:
                  (Float.min
                     (match max_age with
                     | Some a when a > 0. -> a
                     | _ -> mcp_error_max_age)
                     mcp_error_max_age)
                disk
          | h -> h
        in
        let result =
          match hit with
          | Some v -> `Value v
          | None ->
              if is_offline () then (
                offline_miss ("mcp " ^ key);
                `Return (`Null, false))
              else if !mcp_failures >= mcp_max_failures then (
                net_incr "mcp-errors";
                `Return (`Null, false))
              else begin
                net_incr "mcp";
                let body =
                  Json.dumps
                    (`Assoc
                       [
                         ("jsonrpc", `String "2.0");
                         ("id", `Int 1);
                         ("method", `String "tools/call");
                         ( "params",
                           `Assoc
                             [
                               ("name", `String tool); ("arguments", `Assoc args);
                             ] );
                       ])
                in
                try
                  let text =
                    Http.urlopen ~body ~timeout:mcp_timeout
                      ~headers:
                        [
                          ("User-Agent", user_agent);
                          ("Content-Type", "application/json");
                          ("Accept", "application/json, text/event-stream");
                        ]
                      mcp_url
                  in
                  let msg =
                    try Json.parse text
                    with _ -> raise (Mcp_failure "unreadable answer")
                  in
                  let err = if strict then Json.member "error" msg else `Null in
                  match err with
                  | `Assoc _
                    when String.starts_with ~prefix:"unknown tool"
                           (match Json.member "message" err with
                           | `String s -> s
                           | `Null -> ""
                           | v -> Json.dumps v) ->
                      Hashtbl.replace mcp_unavailable tool ();
                      (* not deployed (yet): the callers do without it *)
                      `Return (`Null, false)
                  | _ ->
                      let res =
                        match msg with
                        | `Assoc kv when List.mem_assoc "result" kv ->
                            List.assoc "result" kv
                        | _ -> raise (Mcp_failure "KeyError: 'result'")
                      in
                      (match res with
                      | `Assoc _ -> ()
                      | _ ->
                          raise
                            (Mcp_failure
                               (Printf.sprintf "no result: %s"
                                  (Util.prefix_chars (Json.dumps res) 100))));
                      let first_text () =
                        match Json.member "content" res with
                        | `List (c :: _) -> Json.member "text" c
                        | _ -> raise (Mcp_failure "no content")
                      in
                      let v =
                        if Json.truthy (Json.member "isError" res) then begin
                          let t = first_text () in
                          if strict then
                            `Assoc
                              [
                                ( "tool_error",
                                  `String
                                    (match t with
                                    | `String s -> s
                                    | `Null -> "None"
                                    | v -> Json.dumps v) );
                              ]
                          else `Null
                        end
                        else
                          match first_text () with
                          | `String s -> (
                              try Json.parse s
                              with _ ->
                                raise (Mcp_failure "unreadable content"))
                          | _ -> raise (Mcp_failure "content is not text")
                      in
                      let meta_partial =
                        match Json.member "_meta" res with
                        | `Assoc _ as m ->
                            Json.member "org.ocaml/partial" m = `Bool true
                        | _ -> false
                      in
                      let v_partial =
                        match v with
                        | `Assoc _ -> Json.member "partial" v = `Bool true
                        | _ -> false
                      in
                      if strict && (meta_partial || v_partial) then begin
                        (* Some versions could not be read: what was read is used,
                     and the analysis is incomplete (asked again later). *)
                        net_incr "mcp-errors";
                        `Return (v, false)
                      end
                      else begin
                        write_atomic disk (Json.dumps v);
                        `Value v
                      end
                with e ->
                  net_incr "mcp-errors";
                  incr mcp_failures;
                  if !mcp_failures = mcp_max_failures then
                    Printf.eprintf
                      "note: ocaml.org's MCP server failed %d times (last: \
                       %s); not called again in this run\n\
                       %!"
                      !mcp_failures
                      (Util.prefix_chars (Util.exn_text e) 100);
                  `Return (`Null, false)
                (* transient: not cached *)
              end
        in
        match result with
        | `Return r -> r
        | `Value v ->
            Hashtbl.replace src.mcp_mem key v;
            (v, true))
  end

let mcp ?max_age src tool args = fst (mcp_call ?max_age src tool args)

(* Version lists change with each release: their answers are kept a day. *)
let versions_max_age = 24. *. 3600.

(** [{version, published, statuses}] of [name], newest first, from
    ocaml_package_versions (one call per 500 versions): [] if [name] is not a
    package, None if the server did not answer. *)
let mcp_versions src name =
  match Hashtbl.find_opt src.versions_mem name with
  | Some v -> v
  | None ->
      let rec loop out offset =
        let args =
          [ ("package", `String name); ("limit", `Int 500) ]
          @ if offset > 0 then [ ("offset", `Int offset) ] else []
        in
        let d, answered =
          mcp_call ~max_age:versions_max_age src "ocaml_package_versions" args
        in
        if not answered then None
        else
          match Json.member "versions" d with
          | `List vs ->
              let out =
                out @ List.filter (function `Assoc _ -> true | _ -> false) vs
              in
              if
                (not (Json.truthy (Json.member "truncated" d)))
                || vs = []
                || List.length out >= 5000
              then Some out
              else loop out (List.length out)
          | _ -> Some out (* not a package (the tool's error) *)
      in
      let out = loop [] 0 in
      Hashtbl.replace src.versions_mem name out;
      out

let versions_git src name ref =
  let ref =
    if ref = "master" then begin
      (match src.master_sha with
      | Some _ -> ()
      | None ->
          let r =
            git ~check:false [ "ls-remote"; opam_repo_url; "refs/heads/master" ]
          in
          src.master_sha <-
            Some
              (match Util.split r.stdout with s :: _ -> Some s | [] -> None));
      Option.join src.master_sha
    end
    else Some ref
  in
  match ref with
  | None | Some "" -> []
  | Some ref -> (
      let d = gitdir src opam_repo_url in
      match ensure src opam_repo_url ref false with
      | None -> []
      | Some rev ->
          let r =
            git ~git_dir:d ~check:false
              [
                "ls-tree";
                "--name-only";
                Printf.sprintf "%s:packages/%s" rev name;
              ]
          in
          sort_versions
            (List.filter_map
               (fun l ->
                 if String.starts_with ~prefix:(name ^ ".") l then
                   Some (snd (split_pkg l))
                 else None)
               (Util.splitlines r.stdout)))

let versions ?(ref = "master") src name =
  if ref = "master" then
    match mcp_versions src name with
    | None -> None
    | Some vs ->
        Some
          (sort_versions
             (List.sort_uniq compare
                (List.filter_map (fun v -> Json.str_opt "version" v) vs)))
  else Some (versions_git src name ref)

(* Test seam. *)
let versions_hook = ref versions
let versions ?ref src name = !versions_hook ?ref src name

let versions_or_empty ?ref src name =
  Option.value ~default:[] (versions ?ref src name)

let changelog ?since ?until src pkg =
  let args =
    [ ("package", `String pkg) ]
    @ (match since with
      | Some s when s <> "" -> [ ("since", `String s) ]
      | _ -> [])
    @
    match until with
    | Some u when u <> "" -> [ ("until", `String u) ]
    | _ -> []
  in
  let d = mcp src "ocaml_package_changelog" args in
  match (d, Json.member "section_versions" d, Json.member "content" d) with
  | `Assoc _, (`List (_ :: _) as vs), `String content ->
      Some
        {
          file = Json.str_opt "file" d;
          cversions =
            List.map
              (function `String s -> s | v -> Json.dumps v)
              (Json.to_list vs);
          text = content;
          truncated = Json.truthy (Json.member "truncated" d);
        }
  | _ -> None

let doc_modules src pkg version =
  let d =
    mcp src "ocaml_package_documentation"
      [ ("package", `String pkg); ("version", `String version) ]
  in
  if not (Json.truthy d) then None
  else
    match Json.member "documentation_status" d with
    | `Null | `String "success" -> (
        let mods = Odict.create () in
        List.iter
          (fun m ->
            match Json.str_opt "path" m with
            | Some p when p <> "" ->
                ignore
                  (Odict.setdefault mods (Json.str "name" m)
                     (Util.removesuffix p "/index.html"))
            | _ -> ())
          (List.concat_map
             (fun lib -> Json.list "modules" lib)
             (Json.list "libraries" d)
          @ Json.list "modules" d);
        match Odict.to_list mods with [] -> None | l -> Some l)
    | _ -> None

(* Test seam. *)
let doc_modules_hook = ref doc_modules
let doc_modules src pkg version = !doc_modules_hook src pkg version

(** Plain-text documentation (signatures) of one module page, or None. *)
let doc_page src pkg version path =
  let d =
    mcp src "ocaml_module_documentation"
      [
        ("package", `String pkg);
        ("version", `String version);
        ("path", `String (path ^ "/index.html"));
      ]
  in
  if Json.truthy d then Json.str_opt "content" d else None

let defines text kind ident =
  let i = Rx.escape ident in
  let pats =
    match kind with
    | "value" ->
        [ Printf.sprintf {|\b(val|external|let|and)\s+(rec\s+)?%s\b|} i ]
    | "constructor" ->
        [
          Printf.sprintf {|(\||=|\[)\s*%s\b|} i;
          Printf.sprintf {|\b%s\s+of\b|} i;
        ]
    (* a record field starts a line or follows { or ; (not `val x :`, not `~x:`) *)
    | "field" ->
        [ Printf.sprintf {|(?m)(^|[{;])[ \t]*(mutable\s+)?%s\s*:([^:]|$)|} i ]
    | "type" -> [ Printf.sprintf {|\btype\s+(nonrec\s+)?([^=]*\s)?%s\b|} i ]
    | "module" -> [ Printf.sprintf {|\bmodule\s+(type\s+)?%s\b|} i ]
    | "module-type" -> [ Printf.sprintf {|\bmodule\s+type\s+%s\b|} i ]
    | "class" ->
        [ Printf.sprintf {|\bclass\s+(virtual\s+)?(\[[^\]]*\]\s*)?%s\b|} i ]
    | "class-type" ->
        [
          Printf.sprintf {|\bclass\s+type\s+(virtual\s+)?(\[[^\]]*\]\s*)?%s\b|}
            i;
        ]
    | "exception" -> [ Printf.sprintf {|\bexception\s+%s\b|} i ]
    | k -> raise (Invalid_argument ("defines: " ^ k))
  in
  List.exists (fun p -> Rx.test p text) pats

let doc_present src pkg version mods kind ident =
  let top0 = match mods with m :: _ -> m | [] -> ident in
  let cached = Hashtbl.find_opt src.docpath (pkg, top0) in
  let via_cache =
    match (cached, mods) with
    | Some c, _ :: rest -> (
        (* Same module path as in another version: skip the package index. *)
        match doc_page src pkg version (String.concat "/" (c :: rest)) with
        | Some page -> Some (Some (defines page kind ident))
        | None -> None)
    | _ -> None
  in
  match via_cache with
  | Some r -> r
  | None -> (
      match doc_modules src pkg version with
      | None -> None
      | Some tops -> (
          let first3 = List.map snd (Util.take 3 tops) in
          if kind = "module" && mods = [] then
            if List.mem_assoc ident tops then Some true
            else (* maybe wrapped: Wrapper.ident *)
              Some
                (List.exists
                   (fun b ->
                     defines
                       (Option.value ~default:"" (doc_page src pkg version b))
                       "module" ident)
                   first3)
          else
            let m0 = List.hd mods and rest = List.tl mods in
            let base =
              match List.assoc_opt m0 tops with
              | Some b -> Some b
              | None ->
                  (* reached through an open of a wrapper module (e.g. Term from Why3) *)
                  List.find_map
                    (fun b ->
                      let p = b ^ "/" ^ m0 in
                      match doc_page src pkg version p with
                      | Some x when x <> "" -> Some p
                      | _ -> None)
                    first3
            in
            match base with
            | None ->
                None (* page missing: ocaml.org docs are sometimes partial *)
            | Some base -> (
                match
                  doc_page src pkg version (String.concat "/" (base :: rest))
                with
                | None -> None
                | Some page ->
                    Hashtbl.replace src.docpath (pkg, top0) base;
                    Some (defines page kind ident))))

(* Test seam. *)
let doc_present_hook = ref doc_present

let doc_present src pkg version mods kind ident =
  !doc_present_hook src pkg version mods kind ident

(* API histories change when a release is published or its docs are built. *)
let api_history_max_age = 24. *. 3600.

(* The most versions ocaml_module_changes checks in one call. *)
let api_history_max_versions = 60

(* ocaml_module_changes's kinds for the identifier kinds of find_unbound; a
   constructor may be an exception's (asked second). *)
let api_kinds = function
  | "value" -> [ "value" ]
  | "type" -> [ "type" ]
  | "constructor" -> [ "constructor"; "exception" ]
  | "field" -> [ "field" ]
  | "module" -> [ "module" ]
  | "module-type" -> [ "module-type" ]
  | "exception" -> [ "exception" ]
  | "class" -> [ "class" ]
  | "class-type" -> [ "class-type" ]
  | _ -> [ "any" ]

(* The tool's error when the module is documented but the item is in none of
   the versions checked: '... "x" not found in M in versions 1.0 to 2.0'. *)
let api_not_found = {|^[\w-]+ ".*" not found in \S+ in (versions? |no version)|}

let api_history src pkg mods kind ident versions =
  if mods = [] || versions = [] then None
  else begin
    let lo = Option.get (min_version versions)
    and hi = Option.get (max_version versions) in
    let key = Json.dumps (Json.strings ([ pkg; kind; ident; lo; hi ] @ mods)) in
    match Hashtbl.find_opt src.history_mem key with
    | Some h -> h
    | None ->
        let path = String.concat "." mods in
        (* [absent]: the history of a kind the item is in no version of; the
         next kinds may still have it. *)
        let rec try_kinds absent = function
          | [] -> absent
          | k :: kinds -> (
              let args =
                [
                  ("package", `String pkg);
                  ("module", `String path);
                  ("identifier", `String ident);
                  ("kind", `String k);
                  ("from_version", `String lo);
                  ("to_version", `String hi);
                  ("max_versions", `Int api_history_max_versions);
                ]
              in
              let d, answered =
                mcp_call ~max_age:api_history_max_age src "ocaml_module_changes"
                  args
              in
              match d with
              | `Assoc _ when Json.mem "tool_error" d ->
                  let e =
                    match Json.member "tool_error" d with
                    | `String s -> s
                    | v -> Json.dumps v
                  in
                  if Rx.test_match api_not_found e then try_kinds absent kinds
                  (* maybe of the next kind *)
                    else absent
              | `Assoc _ -> (
                  match Json.member "versions" d with
                  | `List entries ->
                      (* The module path names several modules ("List" in
                    ocaml-base-compiler's docs: compiler-libs'
                    Parameter.List and Stdlib.List): the tool used one of
                    them, maybe not the code's. No answer: the callers look
                    version by version, through the module path as written.
                    Unless the tool checked them all and only one has the
                    item: that one is the code's. *)
                      if
                        Json.truthy (Json.member "ambiguous_with" d)
                        && Json.member "resolved_module" d <> `String path
                        && not
                             (Json.member "resolved_by" d = `String "identifier"
                             && not (Json.truthy (Json.member "unchecked" d)))
                      then None
                      else
                        let statuses = Odict.create () in
                        List.iter
                          (fun e ->
                            match
                              (Json.member "version" e, Json.member "status" e)
                            with
                            | `String v, `String s -> Odict.set statuses v s
                            | _ -> ())
                          entries;
                        let hist =
                          {
                            versions = Odict.to_list statuses;
                            hmodule = Json.str_opt "resolved_module" d;
                            partial = not answered;
                          }
                        in
                        if Json.member "found" d = `Bool false then
                          (* in no version checked: maybe of the next kind *)
                          try_kinds
                            (if absent = None then Some hist else absent)
                            kinds
                        else Some hist
                  | _ -> absent)
              | _ -> absent (* not deployed, or the call failed *))
        in
        let out = try_kinds None (api_kinds kind) in
        Hashtbl.replace src.history_mem key out;
        out
  end

(* Test seam. *)
let api_history_hook = ref api_history

let api_history src pkg mods kind ident versions =
  !api_history_hook src pkg mods kind ident versions

(* Which candidates document a module changes when a release is published
   or its docs are built. *)
let module_find_max_age = 24. *. 3600.

(* The most candidates ocaml_module_find searches in one call. *)
let module_find_max_packages = 40
let module_name = {|[A-Z][\w']*|}

let module_providers src name packages =
  let packages = Util.take module_find_max_packages (Util.dedup packages) in
  if name = "" || packages = [] then None
  else begin
    let key = Json.dumps (Json.strings (name :: packages)) in
    match Hashtbl.find_opt src.find_mem key with
    | Some r -> r
    | None ->
        let d, answered =
          mcp_call ~max_age:module_find_max_age src "ocaml_module_find"
            [
              ("name", `String name);
              ("kind", `String "module");
              ("packages", Json.strings packages);
            ]
        in
        let out =
          match d with
          | `Assoc _
            when (not (Json.mem "tool_error" d))
                 &&
                 match Json.member "matches" d with
                 | `List _ -> true
                 | _ -> false ->
              let field m k rx =
                match Json.member k m with
                | `String s when Rx.fullmatch rx s <> None -> Some s
                | _ -> None
              in
              let matches =
                List.filter_map
                  (fun m ->
                    match m with
                    | `Assoc _ when Json.member "kind" m = `String "module" ->
                        let path =
                          String.split_on_char '.'
                            (Option.value ~default:""
                               (field m "module" {|[\w'.]+|}))
                        in
                        if
                          field m "package" {|[\w+-]+|} = None
                          || not
                               (List.for_all
                                  (fun c -> Rx.fullmatch module_name c <> None)
                                  path)
                        then None
                        else
                          Some
                            {
                              package = Json.str "package" m;
                              module_path = path;
                              mversion = field m "version" {|[\w+.~-]+|};
                              library = field m "library" {|[\w+.-]+|};
                            }
                    | _ -> None (* a library, a module type *))
                  (Json.list "matches" d)
              in
              let checked = Odict.create () in
              List.iter
                (fun c ->
                  match (Json.member "package" c, Json.member "status" c) with
                  | `String p, `String s -> Odict.set checked p s
                  | _ -> ())
                (Json.list "checked" d);
              let checked = Odict.to_list checked in
              Some
                {
                  matches;
                  checked;
                  no_docs =
                    List.filter_map
                      (fun (p, s) -> if s = "no-docs" then Some p else None)
                      checked;
                  distribution =
                    (match Json.member "note" d with
                    | `String _ -> true
                    | _ -> false);
                  fpartial = not answered;
                }
          | _ -> None
        in
        Hashtbl.replace src.find_mem key out;
        out
  end

(* Test seam. *)
let module_providers_hook = ref module_providers

let module_providers src name packages =
  !module_providers_hook src name packages

let published src pkg version =
  List.find_map
    (fun v ->
      if Json.str_opt "version" v = Some version then
        match Json.member "published" v with
        | `String p when Rx.fullmatch {|\d{4}-\d\d-\d\d|} p <> None -> Some p
        | _ -> None
      else None)
    (Option.value ~default:[] (mcp_versions src pkg))

(* Test seam. *)
let published_hook = ref published
let published src pkg version = !published_hook src pkg version

(* An opam file on master can be edited (bounds, x-maintained): fetched again after a day. *)
let master_max_age = 24. *. 3600.

let opam_file ?(ref = "master") src name version =
  raw
    ?max_age:(if ref = "master" then Some master_max_age else None)
    src
    (Printf.sprintf
       "https://raw.githubusercontent.com/%s/%s/packages/%s/%s.%s/opam" repo ref
       name name version)

(* Test seam. *)
let opam_file_hook = ref opam_file
let opam_file ?ref src name version = !opam_file_hook ?ref src name version

let upstream ?(ref = "master") src name version =
  let text =
    match opam_file ~ref src name version with
    | Some t when t <> "" -> Some t
    | _ -> opam_file src name version
  in
  match text with
  | None | Some "" -> None
  | Some text -> (
      match Rx.search ~flags:[ Rx.M ] {|^dev-repo:\s*"([^"]+)"|} text with
      | Some m -> repo_url (Rx.group m 1)
      | None -> (
          match
            Rx.search {|src:\s*"(https://(?:github|gitlab)\.com/[^"]+)"|} text
          with
          | Some m -> repo_url (Rx.group m 1)
          | None -> None))

(* Test seam. *)
let upstream_hook = ref upstream
let upstream ?ref src name version = !upstream_hook ?ref src name version

(* ------------------------------------------------------------------------ *)

(** The PR that published name.version: [{"pr": N, "sha": head sha}], or None.
    Uses 2 GitHub REST calls (token optional). *)
let find_publication ?(cache_dir = ".opam-ci-cache") name version =
  let commits =
    Json.to_list
      (Option.value ~default:`Null
         (gh_api ~cache_dir
            (Printf.sprintf
               "repos/%s/commits?path=packages/%s/%s.%s/opam&per_page=100" repo
               name name version)))
  in
  match List.rev commits with
  | [] -> None
  | last :: _ -> (
      let first = Json.str "sha" last in
      (* oldest commit touching the file = the one adding it *)
      let pulls =
        Json.to_list
          (Option.value ~default:`Null
             (gh_api ~cache_dir
                (Printf.sprintf "repos/%s/commits/%s/pulls" repo first)))
      in
      match pulls with
      | pr :: _ ->
          Some
            [
              ("pr", Json.member "number" pr);
              ("sha", Json.member "sha" (Json.member "head" pr));
            ]
      | [] -> Some [ ("pr", `Null); ("sha", `String first) ])

let find_baseline_commit ?(cache_dir = ".opam-ci-cache") target =
  let name, version = split_pkg target in
  let vs = versions_or_empty (create cache_dir) name in
  let older = List.filter (fun v -> opam_vcmp v version < 0) vs in
  match max_version older with
  | None -> None
  | Some prev ->
      Some
        (Dict.of_list
           (("prev", `String (name ^ "." ^ prev))
           :: Option.value ~default:[] (find_publication ~cache_dir name prev)))

let ocaml_repo = "https://github.com/ocaml/ocaml"

let ocaml_releases src =
  let rel =
    List.filter
      (fun t -> Rx.fullmatch {|[45]\.\d+\.0|} t <> None)
      (tags src ocaml_repo)
  in
  sort_versions (List.filter (fun t -> opam_vcmp t "4.08.0" >= 0) rel)

let module_files paths modname =
  let m = String.lowercase_ascii modname in
  let out =
    List.filter
      (fun p ->
        let base = Util.last (Util.rsplit1 "/" p) in
        let stem =
          String.lowercase_ascii (List.hd (String.split_on_char '.' base))
        in
        stem = m
        && (String.ends_with ~suffix:".mli" base
           || String.ends_with ~suffix:".ml" base))
      paths
  in
  Util.sort_by
    (fun p ->
      ( not (String.ends_with ~suffix:".mli" p),
        Util.contains p "/test",
        String.length p ))
    out

let present ?(stdlib = false) src repo ref path_mods kind ident =
  let paths = tree src repo ref in
  if paths = [] then None
  else begin
    let paths =
      if stdlib then
        List.filter (fun p -> String.starts_with ~prefix:"stdlib/" p) paths
      else paths
    in
    if kind = "module" && path_mods = [] then
      Some (module_files paths ident <> [])
    else
      (* Try the innermost module that has its own file, then outer ones. *)
      let mods = Array.of_list path_mods in
      let rec try_k k =
        if k < 0 then Some false
        else
          let files = module_files paths mods.(k) in
          if files = [] then try_k (k - 1)
          else
            let inner =
              Array.to_list (Array.sub mods (k + 1) (Array.length mods - k - 1))
            in
            Some
              (List.exists
                 (fun f ->
                   let text =
                     Option.value ~default:"" (file_at src repo ref f)
                   in
                   if inner <> [] && not (defines text "module" (List.hd inner))
                   then false
                   else kind = "module" || defines text kind ident)
                 (Util.take 3 files))
      in
      try_k (Array.length mods - 1)
  end

type presence = Yes | No | No_docs | Unknown

let history_presence hist version =
  match hist with
  | None -> Unknown
  | Some h -> (
      match List.assoc_opt version h.versions with
      | Some "present" -> Yes
      | Some "absent" -> No
      | Some "module-not-found" ->
          if h.hmodule <> None && h.hmodule <> Some "" then No else No_docs
      | Some ("no-docs" | "docs-failed") -> No_docs
      | _ -> Unknown)

let history_has_module hist version =
  match hist with
  | None -> None
  | Some h -> (
      match List.assoc_opt version h.versions with
      | Some ("present" | "absent" | "page-truncated") -> Some true
      | Some "module-not-found" -> Some false
      | _ -> None)

let bisect_cap = 10
let api_history_source = "ocaml.org API history"
let module_find_source = "ocaml.org module search"

let bisect_presence ?(allowed = fun _ -> true) ?(cap = bisect_cap) check
    versions bad =
  let evidence = Odict.create () in
  let c v =
    match Odict.get evidence v with
    | Some r -> r
    | None ->
        let r = check v in
        Odict.set evidence v r;
        r
  in
  let newer =
    List.filter (fun v -> opam_vcmp v bad > 0 && allowed v) versions
  in
  match List.find_opt (fun v -> c v = Some true) (Util.take cap newer) with
  | Some v -> (Some (">=", v), Odict.to_list evidence)
  | None -> (
      let older =
        List.filter (fun v -> opam_vcmp v bad < 0 && allowed v) versions
      in
      match
        List.find_opt
          (fun v -> c v = Some true)
          (List.rev (Util.sublist ~start:(-cap) older))
      with
      | Some v ->
          let later = List.filter (fun w -> opam_vcmp w v > 0) versions in
          ( Some ("<", match later with w :: _ -> w | [] -> bad),
            Odict.to_list evidence )
      | None -> (None, Odict.to_list evidence))

let existing_bounds opam_text dep =
  match
    Rx.search ~flags:[ Rx.M ]
      ({|^\s*"|} ^ Rx.escape dep ^ {|"\s*\{([^}]*)\}|})
      (Option.value ~default:"" opam_text)
  with
  | Some m when not (String.contains (Rx.group m 1) '|') ->
      List.map
        (function [ op; v ] -> (op, v) | _ -> assert false)
        (Rx.findall_groups {|(>=|<=|>|<|=)\s*"([^"]+)"|} (Rx.group m 1))
  | _ -> []

let satisfies v bounds =
  List.for_all
    (fun (op, b) ->
      let c = opam_vcmp v b in
      match op with
      | ">=" -> c >= 0
      | ">" -> c > 0
      | "<=" -> c <= 0
      | "<" -> c < 0
      | "=" -> c = 0
      | _ -> false)
    bounds

let norm s = Rx.sub "[-_]" "" (String.lowercase_ascii s)

let constraint_edit opam_text dep op version =
  let m =
    Rx.search ~flags:[ Rx.M ]
      ({|^([ \t]*)"|} ^ Rx.escape dep ^ {|"[ \t]*(\{([^}]*)\})?[^\n]*$|})
      (Option.value ~default:"" opam_text)
  in
  let new_term = Printf.sprintf {|%s "%s"|} op version in
  match m with
  | None -> (None, Printf.sprintf {|"%s" {%s}|} dep new_term)
  | Some m ->
      let old = Rx.group0 m in
      let filt = String.trim (Rx.group m 3) in
      let terms =
        if filt = "" then []
        else if String.contains filt '|' || String.contains filt '(' then
          [ "(" ^ filt ^ ")" ]
        else
          let terms = List.map String.trim (String.split_on_char '&' filt) in
          let drop =
            if op = ">=" || op = ">" then {|(>=|>)\s*"|} else {|(<=|<)\s*"|}
          in
          List.filter (fun t -> not (Rx.test_match drop t)) terms
      in
      ( Some (String.trim old),
        Printf.sprintf {|"%s" {%s}|} dep
          (String.concat " & " (terms @ [ new_term ])) )

let license_id text =
  let t = Rx.sub {|\s+|} " " (Option.value ~default:"" text) in
  let has = Util.contains t in
  if has "Permission is hereby granted, free of charge" then Some "MIT"
  else if has "Apache License" && has "Version 2.0" then Some "Apache-2.0"
  else if has "Redistribution and use in source and binary forms" then
    Some
      (if Rx.test "Neither the name|names of its contributors" t then
         "BSD-3-Clause"
       else "BSD-2-Clause")
  else if
    has "Permission to use, copy, modify, and/or distribute"
    || has "Permission to use, copy, modify, and distribute"
  then Some "ISC"
  else if has "GNU LESSER GENERAL PUBLIC LICENSE" then
    Some
      (if has "2.1" then
         "LGPL-2.1-or-later (check: -only vs -or-later, OCaml linking \
          exception)"
       else "LGPL-3.0-or-later (check)")
  else if has "GNU GENERAL PUBLIC LICENSE" then
    Some
      (if has "Version 3" then "GPL-3.0-or-later (check)"
       else "GPL-2.0-or-later (check)")
  else if has "Mozilla Public License" && has "2.0" then Some "MPL-2.0"
  else None

let opam_field text field =
  match
    Rx.search ~flags:[ Rx.M ]
      ("^" ^ Rx.escape field ^ {|:\s*"([^"]*)"|})
      (Option.value ~default:"" text)
  with
  | Some m -> Some (Rx.group m 1)
  | None -> None

let url_src text =
  match
    Rx.search ~flags:[ Rx.M; Rx.S ] {|^url\s*\{.*?\bsrc:\s*"([^"]+)"|}
      (Option.value ~default:"" text)
  with
  | Some m -> Some (Rx.group m 1)
  | None -> None
