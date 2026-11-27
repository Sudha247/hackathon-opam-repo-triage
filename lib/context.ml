open Suggestion.T
open Ci
module SS = Set.Make (String)

module PS = Set.Make (struct
  type t = string * string

  let compare = compare
end)

let platform_vars = Filter.platform_vars
let eval_filter = Filter.eval_filter

(* ------------------------------------------------------------------------ *)
(* Seams: the functions the tests replace. *)

let fetch_hook : (string -> string -> string) ref =
  ref (fun url cache_dir -> Net.fetch url cache_dir)

let log_text_hook : (string -> string) ref = ref log_text
let parse_commit_page_hook : (string -> data) ref = ref parse_commit_page

let gh_api_hook : (string -> string -> Json.t option) ref =
  ref (fun path cache_dir -> Net.gh_api ~cache_dir path)

let find_baseline_commit_hook : (string -> string -> Dict.t option) ref =
  ref (fun target cache_dir -> Sources.find_baseline_commit ~cache_dir target)

(* The source lookups ([src.opam_file], [src.versions], ...). *)
let opam_file_hook :
    (?ref:string -> Sources.t -> string -> string -> string option) ref =
  ref (fun ?ref src name version -> Sources.opam_file ?ref src name version)

let versions_hook : (Sources.t -> string -> string list option) ref =
  ref (fun src name -> Sources.versions src name)

let mcp_hook :
    (?max_age:float -> Sources.t -> string -> (string * Json.t) list -> Json.t)
    ref =
  ref (fun ?max_age src tool args -> Sources.mcp ?max_age src tool args)

let opam_file ?ref src name version = !opam_file_hook ?ref src name version
let fetch url cache_dir = !fetch_hook url cache_dir

(* An optional string in a message: "None" when absent. *)
let s_none = function Some s -> s | None -> "None"

(* A JSON value in a message (a PR number, a string...), with the spelling
   of the messages stored in the state: "None", "True", "False". *)
let json_text (v : Json.t) =
  match v with
  | `Int n -> string_of_int n
  | `String s -> s
  | `Null -> "None"
  | `Bool b -> if b then "True" else "False"
  | v -> Json.dumps v

let picked match_ variant =
  match match_ with
  | None | Some "" -> true
  | Some m -> Rx.test (Rx.v m) variant

(* ------------------------------------------------------------------------ *)

(* A URL that can be pasted in a shell command between single quotes: https,
   no space, quote, `;|&$`, backtick, parenthesis, `<>`, `!`, `#`... *)
let shell_safe_url =
  Rx.v
    {|https://[A-Za-z0-9][A-Za-z0-9.-]*(?::\d{1,5})?(?:/[A-Za-z0-9._~%+=,:/?-]*)?|}

let checksum_command url =
  match url with
  | Some u when u <> "" && Rx.fullmatch shell_safe_url u <> None ->
      Some (Printf.sprintf "curl -sL '%s' | sha256sum" u)
  | _ -> None

let ci_accept_failures_re = Rx.v {|x-ci-accept-failures:\s*\[([^\]]*)\]|}
let quoted_re = Rx.v {|"([^"]+)"|}
let comment_re = Rx.v "#[^\n]*"

(** The platforms listed in x-ci-accept-failures. *)
let accepted_platforms opam_text =
  match
    Rx.search ci_accept_failures_re (Option.value ~default:"" opam_text)
  with
  | Some m ->
      SS.of_list (Rx.findall quoted_re (Rx.sub comment_re "" (Rx.group m 1)))
  | None -> SS.empty

let accepted_but_failing (results : Suggestion.t list) src sha pr_packages =
  List.iter
    (fun r ->
      let ds = r.Suggestion.distros
      and owner = Suggestion.str_field r.Suggestion.failing in
      let name, ver = split_pkg owner in
      if
        ds = [] || ver = "" || String.contains owner ' '
        || Titles.has_rule [ Rerun; Lint; Transient ] r
      then ()
      else begin
        let text =
          match
            if List.mem owner pr_packages then opam_file ~ref:sha src name ver
            else None
          with
          | Some t when t <> "" -> Some t
          | _ -> opam_file src name ver
        in
        if SS.subset (SS.of_list ds) (accepted_platforms text) then begin
          r.new_ <- Absent;
          r.old <- Absent;
          r.alternative <- None;
          r.candidates <- None;
          Suggestion.update r
            (Where.fix_where Ci "the CI environment (day10 builder)"
            @ [
                `Rule Rule.Accept_failures_ignored;
                `Confidence "high";
                `Note
                  (Printf.sprintf
                     "%s already lists %s in x-ci-accept-failures, but the \
                      day10 builder ignores that field there. Nothing to \
                      change in the package."
                     owner (String.concat ", " ds));
              ])
        end
      end)
    results

let opam_path_re = Rx.v {|packages/[^/]+/([^/]+)/opam$|}

let compare_files base sha cache_dir =
  let d =
    match base with
    | Some b when b <> "" ->
        !gh_api_hook
          (Printf.sprintf "repos/%s/compare/%s...%s" Net.repo b sha)
          cache_dir
    | _ -> None
  in
  match d with Some d -> Json.list "files" d | None -> []

let filtered_dependency_re = Rx.v {|"([\w+-]+)"\s*\{(.*)\}$|}
let dependency_re = Rx.v {|"([\w+-]+)"\s*(?:\{(.*)\})?$|}
let upper_bound_re = Rx.v {|<\s*"([^"]+)"|}

(** [name.version -> dep {< "V"}] for the opam files this PR modifies only by
    adding an upper bound that excludes a version the PR adds: their dependency
    solutions are the same as on master. *)
let bound_only_changes base sha cache_dir =
  let files = compare_files base sha cache_dir in
  let added =
    List.filter_map
      (fun f ->
        if Json.str "status" f = "added" then
          Option.map
            (fun m -> Rx.group m 1)
            (Rx.match_ opam_path_re (Json.str "filename" f))
        else None)
      files
  in
  let out = Odict.create () in
  List.iter
    (fun f ->
      match Rx.match_ opam_path_re (Json.str "filename" f) with
      | Some m when Json.str "status" f = "modified" -> (
          let lines = Util.splitlines (Json.str "patch" f) in
          let first c l = String.length l > 0 && l.[0] = c in
          let rem =
            List.filter_map
              (fun l ->
                if first '-' l && not (String.starts_with ~prefix:"---" l) then
                  Some (String.trim (Util.drop_chars l 1))
                else None)
              lines
          in
          let add =
            List.filter_map
              (fun l ->
                if first '+' l && not (String.starts_with ~prefix:"+++" l) then
                  Some (String.trim (Util.drop_chars l 1))
                else None)
              lines
          in
          if rem = [] || List.length rem <> List.length add then ()
          else
            let terms f =
              List.filter
                (fun t -> t <> "")
                (List.map String.trim
                   (String.split_on_char '&' (Option.value ~default:"" f)))
            in
            let rec go acc = function
              | [] -> Some (List.rev acc)
              | (r, a) :: rest -> (
                  let ma = Rx.match_ filtered_dependency_re a
                  and mr = Rx.match_ dependency_re r in
                  match (ma, mr) with
                  | Some ma, Some mr when Rx.group ma 1 = Rx.group mr 1 -> (
                      let ta = terms (Rx.group_opt ma 2)
                      and tr = terms (Rx.group_opt mr 2) in
                      let extra =
                        List.filter (fun t -> not (List.mem t tr)) ta
                      in
                      let mv =
                        match extra with
                        | [ e ] -> Rx.fullmatch upper_bound_re e
                        | _ -> None
                      in
                      match mv with
                      | Some mv
                        when List.length ta = List.length tr + 1
                             && List.mem
                                  (Rx.group ma 1 ^ "." ^ Rx.group mv 1)
                                  added ->
                          go
                            (Printf.sprintf {|%s {< "%s"}|} (Rx.group ma 1)
                               (Rx.group mv 1)
                            :: acc)
                            rest
                      | _ -> None)
                  | _ -> None)
            in
            match go [] (List.combine rem add) with
            | Some bounds ->
                Odict.set out (Rx.group m 1) (String.concat ", " bounds)
            | None -> ())
      | _ -> ())
    files;
  Odict.to_list out

(* opam-health-check's results for one package. *)
type health = {
  builds : (string * (string * string option)) list;
      (** compiler -> (status, log url) *)
  hversion : string option;
  run_date : string option;
}

(** opam-health-check's results for the newest version of a package, from
    ocaml.org's MCP, or None. It builds the newest version of every package on
    master, without tests; its results change daily, so they are cached for a
    day. *)
let health_check src name (cache : (string, health option) Hashtbl.t) =
  match Hashtbl.find_opt cache name with
  | Some h -> h
  | None ->
      let r =
        !mcp_hook ~max_age:86400. src "ocaml_package_build_information"
          [ ("package", `String name); ("limit", `Int 5) ]
      in
      let res =
        List.find_opt
          (fun x -> Json.str_opt "package" x = Some name)
          (Json.list "results" r)
      in
      let h =
        match res with
        | Some (`Assoc (_ :: _) as res) ->
            let builds = Odict.create () in
            List.iter
              (fun b ->
                Odict.set builds (Json.str "compiler" b)
                  (Json.str "status" b, Json.str_opt "log_url" b))
              (Json.list "builds" res);
            let run_date =
              match Json.member "date" (Json.member "run" r) with
              | `Null -> None
              | v -> Some (json_text v)
            in
            Some
              {
                builds = Odict.to_list builds;
                hversion =
                  Option.map json_text
                    (match Json.member "version" res with
                    | `Null -> None
                    | v -> Some v);
                run_date;
              }
        | _ -> None
      in
      Hashtbl.replace cache name h;
      h

(* opam-health-check runs about daily: a run older than this (the service is
   stuck, or ocaml.org's copy is) describes an older master, and its results
   are not used. *)
let health_max_age_days = 7

let health_run_stale ?today date =
  match date with
  | None | Some "" -> false
  | _ when Net.is_offline () || Sys.getenv_opt "OPAM_CI_RECORD" = Some "1" ->
      (* the tests replay a fixture at any date; recording one fetches what
         the replay reads *)
      false
  | Some d -> (
      match Date.days_of_date (Util.prefix_chars d 10) with
      | None -> false
      | Some d -> (
          let today = match today with Some t -> t | None -> Date.today () in
          match Date.days_of_date today with
          | Some t -> t - d > health_max_age_days
          | None -> false))

type notes = {
  new_ : (failure_key, string) Hashtbl.t;
  mutable unverified : failure_key list;
  good : (string, PS.t) Hashtbl.t;
  bad : (string, PS.t) Hashtbl.t;
  partial : (string, PS.t) Hashtbl.t;
  via : (string, SS.t) Hashtbl.t;
  mutable run : string option option;
  mutable added : SS.t option;
  mutable notes_health_stale : string option;
}

let new_notes () =
  {
    new_ = Hashtbl.create 8;
    unverified = [];
    good = Hashtbl.create 8;
    bad = Hashtbl.create 8;
    partial = Hashtbl.create 8;
    via = Hashtbl.create 8;
    run = None;
    added = None;
    notes_health_stale = None;
  }

let sorted_bindings tbl =
  List.sort
    (fun (a, _) (b, _) -> compare a b)
    (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

let revdeps_re = Rx.v ",([^,]+),revdeps,"

let health_summary ?src (notes : notes) =
  let run = Option.join notes.run in
  let when_ =
    match run with
    | Some r when r <> "" -> "opam-health-check, " ^ Words.short_date (Some r)
    | _ -> "opam-health-check"
  in
  let ocamls xs =
    String.concat ", " (List.sort_uniq compare (List.map fst (PS.elements xs)))
  in
  let listing d =
    String.concat ", "
      (List.map
         (fun (rd, xs) -> Printf.sprintf "%s (OCaml %s)" rd (ocamls xs))
         d)
  in
  let names ?(n = 6) xs =
    let k = List.length xs in
    if k > n then
      String.concat ", " (Util.take n xs) ^ Printf.sprintf " and %d more" (k - n)
    else if k = 1 then List.hd xs
    else String.concat ", " (Util.take (k - 1) xs) ^ " and " ^ Util.last xs
  in
  let out = ref [] in
  List.iter
    (fun (rd, xs) ->
      (* Broken by this PR: the usual fix in opam-repository is an upper
         bound on the new release in the reverse dependency, in the same PR. *)
      let targets =
        List.sort_uniq compare
          (List.filter_map
             (fun (_, v) ->
               Option.map (fun m -> Rx.group m 1) (Rx.search revdeps_re v))
             (PS.elements xs))
      in
      let target = match targets with t :: _ -> Some t | [] -> None in
      let via =
        match Hashtbl.find_opt notes.via rd with
        | Some s -> SS.elements s
        | None -> []
      in
      let ocaml = "OCaml " ^ ocamls xs in
      let error =
        if via <> [] then
          let one = List.length via = 1 in
          Printf.sprintf
            "%s fails with this PR on %s, and so %s %s, which %s on it and %s \
             on master (%s)."
            rd ocaml
            (if one then "does" else "do")
            (names via)
            (if one then "depends" else "depend")
            (if one then "builds" else "build")
            when_
        else
          Printf.sprintf
            "%s fails with this PR on %s but builds on master (%s)." rd ocaml
            when_
      in
      let r =
        Suggestion.make
          ([
             `Variant
               (rd ^ ", reverse dependency"
               ^ match target with Some t -> " of " ^ t | None -> "");
             `Rule Rule.Health_check_new;
           ]
          @ Where.fix_where Pr "this PR"
          @ [
              `Confidence "medium";
              `Failing (Some rd);
              `Error error;
              `Note
                "Probably broken by this PR: look at it first. \
                 opam-health-check builds the newest version of each package, \
                 without tests.";
              `Jobs (List.sort_uniq compare (List.map snd (PS.elements xs)));
            ])
      in
      let rn, rv = split_pkg rd in
      let added = notes.added in
      let opam =
        match (src, target) with
        | Some src, Some t
          when match added with None -> true | Some a -> SS.mem t a ->
            opam_file src rn rv
        | _ -> None
      in
      (match (target, added) with
      | Some t, Some a when not (SS.mem t a) ->
          r.note <-
            Some
              (Printf.sprintf
                 "This PR modifies %s rather than adding a new version: check \
                  that the change does not break %s. opam-health-check builds \
                  the newest version of each package, without tests."
                 t rd)
      | _ -> ());
      let opam =
        match (opam, target) with
        | Some o, Some t when o <> "" && String.starts_with ~prefix:"conf-" t ->
            r.note <-
              Some
                (Printf.sprintf
                   "%s is a new version of a conf package: when it breaks %s, \
                    the conf package is usually what is wrong (its depexts or \
                    its check), not %s."
                   t rd rd);
            None
        | o, _ -> o
      in
      (match (opam, target) with
      | Some opam, Some target when opam <> "" ->
          let tn, tv = split_pkg target in
          let old, new_, what =
            if List.mem tn (Loginfo.direct_deps (Some opam)) then
              let old, new_ = Sources.constraint_edit (Some opam) tn "<" tv in
              (old, new_, "upper bound")
            else
              ( None,
                Printf.sprintf
                  {|conflicts: [ "%s" {>= "%s"} ]   (added to any existing `conflicts`)|}
                  tn tv,
                "conflict" )
          in
          Suggestion.update r
            [
              `Dep tn;
              `Old old;
              `New (Some new_);
              `Note
                (Printf.sprintf
                   "Probably broken by %s. Usual fix: add this %s to %s's opam \
                    file in this PR, or release a fixed %s. opam-health-check \
                    builds the newest version of each package, without tests."
                   target what rd rn);
            ]
      | _ -> ());
      out := r :: !out)
    (sorted_bindings notes.good);
  let bad = Hashtbl.copy notes.bad in
  Hashtbl.iter (fun k v -> Hashtbl.replace bad k v) notes.partial;
  let bad = sorted_bindings bad in
  if bad <> [] then
    out :=
      Suggestion.make
        ([
           `Variant (Words.counted (List.length bad) "reverse dependency");
           `Rule Rule.Health_check_preexisting;
         ]
        @ Where.fix_where Other_package
            "the reverse dependencies (other packages)"
        @ [
            `Confidence "high";
            `Failing (Some (String.concat ", " (List.map fst bad)));
            `Error
              (Printf.sprintf "These fail on master too (%s): %s" when_
                 (listing bad));
            `Note
              "They, or one of their dependencies, fail without this PR. \
               Nothing to change in this PR.";
            `Jobs
              (List.sort_uniq compare
                 (List.concat_map
                    (fun (_, xs) -> List.map snd (PS.elements xs))
                    bad));
          ])
      :: !out;
  List.rev !out

let add_pair tbl k x =
  Hashtbl.replace tbl k
    (PS.add x (Option.value ~default:PS.empty (Hashtbl.find_opt tbl k)))

let command_noise_re = Rx.v {|[`"']|-j \d+|}

let compilation_installation_failed_re =
  Rx.v {|The (compilation|installation) of \S+ failed at |}

let preexisting_failures ?src ?notes (data : data) sha base cache_dir :
    (failure_key * string) list =
  let failing =
    List.filter
      (fun j -> j.status = Failed && List.mem j.category failing_cats)
      data.jobs
  in
  let per_job : (string, string) Hashtbl.t = Hashtbl.create 16 in
  let bound =
    match base with
    | Some b when b <> "" -> bound_only_changes base sha cache_dir
    | _ -> []
  in
  List.iter
    (fun j ->
      let pkg =
        if j.revdep = None || j.revdep = Some "" then j.target else None
      in
      match pkg with
      | Some pkg when List.mem_assoc pkg bound ->
          Hashtbl.replace per_job j.variant
            (Printf.sprintf
               "this PR only adds %s to %s, so its dependencies are the same \
                as on master"
               (List.assoc pkg bound) pkg)
      | _ -> ())
    failing;
  let added =
    List.sort_uniq compare
      (List.filter_map
         (fun j ->
           match j.target with
           | Some t when t <> "" && not (List.mem_assoc t bound) -> Some t
           | _ -> None)
         failing)
  in
  List.iter
    (fun target ->
      match !find_baseline_commit_hook target cache_dir with
      | Some info when Dict.mem info "sha" -> (
          let info_sha = Dict.str info "sha"
          and info_prev = Dict.str info "prev" in
          match
            !parse_commit_page_hook (fetch (Net.base ^ info_sha) cache_dir)
          with
          | exception _ -> () (* old runs are purged (404) *)
          | prev ->
              let idx = Hashtbl.create 64 in
              List.iter
                (fun b ->
                  if b.target = Some info_prev then
                    Hashtbl.replace idx (b.platform, b.kind, b.revdep) b)
                prev.jobs;
              let name = fst (split_pkg target) in
              (* The reason with the release's version left out, normalised as in failure_key. *)
              let reason job version =
                String.trim
                  (Rx.sub command_noise_re ""
                     (Util.replace
                        (Option.value ~default:"" job.reason)
                        version name))
              in
              List.iter
                (fun j ->
                  if j.target <> Some target || Hashtbl.mem per_job j.variant
                  then ()
                  else
                    match
                      Hashtbl.find_opt idx (j.platform, j.kind, j.revdep)
                    with
                    (* The same job failing is not enough: the new release may fail
                   differently. The failure must be the same. *)
                    | Some b
                      when b.category = j.category
                           && reason b info_prev = reason j target -> (
                        let pr =
                          match Dict.get info "pr" with
                          | Some pr -> "#" ^ json_text pr
                          | None -> Util.prefix_chars info_sha 12
                        in
                        let why =
                          Printf.sprintf
                            "%s failed the same job in the CI of %s" info_prev
                            pr
                        in
                        let own_build =
                          (j.revdep = None || j.revdep = Some "")
                          && Rx.test_match compilation_installation_failed_re
                               (Option.value ~default:"" j.reason)
                        in
                        if not own_build then
                          Hashtbl.replace per_job j.variant why
                        else
                          (* The release's own build, and the reason names only the
                       step that failed, not the error: its code changed, so
                       the error may be another one. Noted, but not counted as
                       happening without the PR ([preexisting_unverified]). *)
                          match notes with
                          | None -> ()
                          | Some n ->
                              let k = failure_key j in
                              if not (List.mem k n.unverified) then
                                n.unverified <- n.unverified @ [ k ];
                              Hashtbl.replace per_job j.variant
                                (why
                               ^ " (failing at the same step; the errors were \
                                  not compared)"))
                    | _ -> ())
                failing)
      | _ -> ())
    (Util.take 3 added);
  (* opam-health-check: master, newest version of each package, no tests, on
     the classic builder's default platform. *)
  (match (notes, base) with
  | Some n, Some b when b <> "" -> (
      match
        !gh_api_hook
          (Printf.sprintf "repos/%s/compare/%s...%s" Net.repo b sha)
          cache_dir
      with
      | Some (`Assoc kv as d) when List.mem_assoc "files" kv ->
          n.added <-
            Some
              (SS.of_list
                 (List.filter_map
                    (fun f ->
                      if Json.str "status" f = "added" then
                        Option.map
                          (fun m -> Rx.group m 1)
                          (Rx.match_ opam_path_re (Json.str "filename" f))
                      else None)
                    (Json.list "files" d)))
      | _ -> ())
  | _ -> ());
  (match src with
  | None -> ()
  | Some src ->
      let cache = Hashtbl.create 16
      and asked = Hashtbl.create 16
      and newest = Hashtbl.create 16 in
      let stale = ref SS.empty in
      let is_newest target =
        (* opam-health-check builds a reverse dependency with the newest
          versions; a job testing it against an older version of the target
          (a PR fixing old releases) is not comparable to that. *)
        match Hashtbl.find_opt newest target with
        | Some b -> b
        | None ->
            let tn, tv = split_pkg target in
            let b =
              match !versions_hook src tn with
              | None | Some [] -> true
              | Some vs -> List.for_all (fun v -> opam_vcmp tv v >= 0) vs
            in
            Hashtbl.replace newest target b;
            b
      in
      List.iter
        (fun j ->
          let d = job_dims j in
          match j.revdep with
          | Some rd
            when rd <> ""
                 && (not (Hashtbl.mem per_job j.variant))
                 && j.category <> Build_or_test
                 && j.kind <> Tests
                 && d.distro = Some classic_distro
                 && d.arch = "x86_64" && d.ocaml <> None && d.ocaml <> Some ""
            ->
              let ocaml = Option.get d.ocaml in
              let name, ver = split_pkg rd in
              if (not (Hashtbl.mem asked name)) && Hashtbl.length asked >= 25
              then ()
              else begin
                Hashtbl.replace asked name ();
                match health_check src name cache with
                | Some hc
                  when hc.hversion = Some ver && List.mem_assoc ocaml hc.builds
                  ->
                    if health_run_stale hc.run_date then
                      stale := SS.add (Option.get hc.run_date) !stale
                    else begin
                      let status, _log = List.assoc ocaml hc.builds in
                      let when_ =
                        match hc.run_date with
                        | Some r when r <> "" ->
                            Printf.sprintf "opam-health-check, OCaml %s, %s"
                              ocaml
                              (Words.short_date (Some r))
                        | _ ->
                            Printf.sprintf "opam-health-check, OCaml %s" ocaml
                      in
                      if status = "bad" then
                        Hashtbl.replace per_job j.variant
                          (Printf.sprintf
                             "%s also fails to build on master (%s)" rd when_)
                      else if status = "partial" then
                        Hashtbl.replace per_job j.variant
                          (Printf.sprintf
                             "a dependency of %s also fails on master (%s)" rd
                             when_);
                      if
                        status = "good"
                        && not (is_newest (Option.value ~default:"" j.target))
                      then ()
                      else begin
                        (* A reverse dependency failing because one of its
                      dependencies failed: the package to bound is that
                      dependency (awa 0.7.0 broke git-mirage, and 16 packages
                      failed through it). *)
                        let who =
                          match
                            if j.category = Dependency then failing_dep j.reason
                            else None
                          with
                          | Some w when w <> "" -> w
                          | _ -> rd
                        in
                        (match notes with
                        | Some n
                          when List.mem status [ "bad"; "partial"; "good" ] ->
                            add_pair
                              (match status with
                              | "bad" -> n.bad
                              | "partial" -> n.partial
                              | _ -> n.good)
                              who (ocaml, j.variant);
                            if who <> rd then
                              Hashtbl.replace n.via who
                                (SS.add rd
                                   (Option.value ~default:SS.empty
                                      (Hashtbl.find_opt n.via who)));
                            if n.run = None then n.run <- Some hc.run_date
                        | _ -> ());
                        match notes with
                        | Some n when status = "good" ->
                            Hashtbl.replace n.new_ (failure_key j)
                              (Printf.sprintf
                                 "%s builds on master (%s), so this PR \
                                  probably causes this failure."
                                 rd when_)
                        | _ -> ()
                      end
                    end
                | _ -> ()
              end
          | _ -> ())
        failing;
      if not (SS.is_empty !stale) then begin
        let latest = SS.max_elt !stale in
        (match notes with
        | Some n -> n.notes_health_stale <- Some latest
        | None -> ());
        Printf.eprintf
          "note: opam-health-check's latest results are from %s, more than %d \
           days ago: not used to tell pre-existing failures from new ones\n\
           %!"
          (Words.short_date (Some latest))
          health_max_age_days
      end);
  let out = Util.group_by failure_key failing in
  List.filter_map
    (fun (k, js) ->
      let whys = List.map (fun j -> Hashtbl.find_opt per_job j.variant) js in
      if List.mem None whys then None else Some (k, Option.get (List.hd whys)))
    out

let sources_re = Rx.v {|sources of (\S+?):|}

let cannot_create_symlink_re =
  Rx.v {|([^\s:]+): Cannot create symlink to '([^']+)'|}

let sources_rules ?match_ (data : data) cache_dir =
  let out = ref [] and seen = Hashtbl.create 8 in
  List.iter
    (fun j ->
      if
        j.status <> Failed || j.category <> Sources
        || not (picked match_ j.variant)
      then ()
      else begin
        let pkg =
          match Rx.search sources_re (Option.value ~default:"" j.reason) with
          | Some m -> Some (Rx.group m 1)
          | None -> (
              match j.revdep with
              | Some r when r <> "" -> Some r
              | _ -> j.target)
        in
        let dims = job_dims j in
        let key = (pkg, dims.os) in
        if Hashtbl.mem seen key then
          begin match !out with
          | last :: _ ->
              last.also_seen_on <-
                Some (Suggestion.list last.also_seen_on @ [ j.variant ]);
              last.jobs <- Some (Suggestion.list last.jobs @ [ j.variant ])
          | [] -> ()
          end
        else begin
          Hashtbl.replace seen key ();
          let text =
            try !log_text_hook (fetch j.url cache_dir) with _ -> ""
          in
          let m = Rx.search cannot_create_symlink_re text in
          let distro = s_none dims.distro in
          let r =
            Suggestion.make
              ([
                 `Variant j.variant;
                 `Url j.url;
                 `Rule Rule.Sources;
                 `Failing pkg;
                 `Jobs [ j.variant ];
               ]
              @ Where.fix_where Upstream (s_none pkg ^ " (upstream archive)")
              @ [
                  `Experimental j.experimental;
                  `Error
                    (Util.prefix_chars (Option.value ~default:"" j.reason) 200);
                  `Confidence "medium";
                ])
          in
          (match m with
          | Some m ->
              Suggestion.update r
                [
                  `Error
                    (Printf.sprintf
                       "tar cannot create the symlink %s -> %s on %s"
                       (Rx.group m 1) (Rx.group m 2) distro);
                  `Confidence "high";
                  `Note
                    (Printf.sprintf
                       "The source archive contains symlinks, which tar cannot \
                        create on %s. Replace them upstream by copies, or add \
                        `x-ci-accept-failures: [\"%s\"]` or `available: os != \
                        \"win32\"` to this release."
                       distro distro);
                ]
          | None ->
              r.note <-
                Some
                  "The archive was downloaded but could not be unpacked. Check \
                   the url and the checksum.");
          out := r :: !out
        end
      end)
    data.jobs;
  List.rev !out

(* Architectures whose CI machines abort jobs routinely (slow emulated or few
   workers): an aborted job there is listed but, like one on an experimental
   platform, does not hold a PR back. The analysis job is never one of them. *)
let routine_abort_re = Rx.v {|(?:^|[,-])(?:riscv64|s390x)(?:[,-]|$)|}

let routine_abort j =
  j.status = Aborted && j.kind <> Analysis
  && Rx.test routine_abort_re
       (String.concat "," (Util.take 2 (String.split_on_char ',' j.variant)))
(* the platform, not a package *)

let http_error_code_re =
  Rx.v {|HTTP error code (\d+)|curl failed|Could not resolve|}

let rerun_rules ?match_ (data : data) (results : Suggestion.t list) =
  let unfinished = List.map (fun j -> j.variant) (unfinished_jobs data.jobs) in
  let analysis_failed =
    List.exists (fun j -> j.kind = Analysis && j.status = Failed) data.jobs
  in
  let jobs =
    List.filter
      (fun j ->
        picked match_ j.variant
        && (j.status = Failed
            && List.mem j.category [ Network; Cancelled ]
            && not (List.mem j.kind [ Lint; Analysis ])
           || j.status = Aborted
           || j.status = Not_started
              && (not (List.mem j.variant unfinished))
              && not analysis_failed))
      data.jobs
  in
  let transient = List.filter (fun r -> r.rule = Some Transient) results in
  if jobs = [] && transient = [] then results
  else begin
    (* The kind of failure of a job: its status or category, and a detail. *)
    let kind j =
      if j.status <> Failed then (Some (Status.to_string j.status), None)
      else
        let m =
          Rx.search http_error_code_re (Option.value ~default:"" j.reason)
        in
        let detail =
          match m with
          | Some m -> (
              match Rx.group_opt m 1 with
              | Some g when g <> "" ->
                  Some ("HTTP " ^ g ^ if g = "429" then ", rate limit" else "")
              | _ -> Some (Rx.group0 m))
          | None -> None
        in
        (Some (Category.to_string j.category), detail)
    in
    (* The number of jobs of each kind, the most frequent first (the first
       seen first among equals). *)
    let most_common =
      List.map (fun j -> (kind j, 1)) jobs
      @ List.map
          (fun r ->
            ( (None, r.error),
              List.length
                (match Suggestion.list r.Suggestion.jobs with
                | [] -> [ Suggestion.str r.Suggestion.variant ]
                | l -> l) ))
          transient
      |> Util.group_by fst
      |> List.map (fun (k, l) -> (k, List.fold_left ( + ) 0 (List.map snd l)))
      |> List.stable_sort (fun (_, a) (_, b) -> compare b a)
    in
    let what (cat, detail) n =
      match cat with
      | None ->
          Printf.sprintf "%s (%s)" (s_none detail) (Words.counted n "job")
          (* a CI tool crash, from the log *)
      | Some "not-started" -> Words.counted n "job" ^ " not run"
      | Some cat -> (
          let noun =
            match cat with
            | "network" -> "network error"
            | "cancelled" -> "cancelled job"
            | "aborted" -> "aborted job"
            | c -> c ^ " job"
          in
          Words.counted n noun
          ^ match detail with Some d when d <> "" -> " (" ^ d ^ ")" | _ -> "")
    in
    let idle = List.filter (fun j -> j.status <> Failed) jobs in
    let variants =
      merge_jobs
        (List.map (fun j -> j.variant) jobs)
        (List.concat_map
           (fun r ->
             match Suggestion.list r.Suggestion.jobs with
             | [] -> [ Suggestion.str r.Suggestion.variant ]
             | l -> l)
           transient)
    in
    let exp = List.length (List.filter (fun j -> j.experimental) jobs) in
    let routine =
      List.length
        (List.filter (fun j -> routine_abort j && not j.experimental) jobs)
    in
    let nj = List.length jobs in
    let note =
      (if idle = [] then "These jobs failed because of the CI, not the packages"
       else if List.length idle = nj && transient = [] then
         "CI did not run these jobs to the end (aborted, or never started)"
       else
         "These jobs failed because of the CI, not the packages, or CI did not \
          run them to the end (aborted, or never started)")
      ^ ". An opam-repository maintainer can rerun them with \"Rebuild \
         failed\" on the CI page."
      ^ (if exp = 0 then ""
         else if exp = 1 then " One of them is on an experimental platform."
         else Printf.sprintf " %d of them are on experimental platforms." exp)
      ^
      if routine = 0 then ""
      else if routine = 1 then
        " One of them was aborted on riscv64 or s390x, where CI aborts jobs \
         routinely: it does not hold the PR back."
      else
        Printf.sprintf
          " %d of them were aborted on riscv64 or s390x, where CI aborts jobs \
           routinely: they do not hold the PR back."
          routine
    in
    let entry =
      Suggestion.make
        ([
           `Variant (Words.counted (List.length variants) "job");
           `Rule Rule.Rerun;
         ]
        @ Where.fix_where Rerun "CI (rerun)"
        @ [
            `Error
              (String.concat ", "
                 (List.map (fun (k, n) -> what k n) most_common));
            `Confidence "high";
            `Jobs variants;
            (* Only experimental jobs, and jobs aborted where aborts are
                routine, do not hold the PR back. *)
            `Experimental
              (exp + routine = nj
              && List.for_all
                   (fun r -> r.Suggestion.experimental = Some true)
                   transient);
            `Note note;
          ])
    in
    List.filter (fun r -> r.rule <> Some Transient) results @ [ entry ]
  end

(** [name.version -> (old available, new available)] for the opam files the
    commit [sha] modifies relative to master commit [base] (one GitHub API
    call). *)
let pr_opam_changes base sha cache_dir =
  List.fold_left
    (fun out f ->
      match Rx.match_ opam_path_re (Json.str "filename" f) with
      | Some m when Json.str "status" f = "modified" ->
          let lines = Util.splitlines (Json.str "patch" f) in
          (* The [available] field on the lines starting with [sign]: its
           first line, and the indented lines that follow it. *)
          let block sign =
            let _, got =
              List.fold_left
                (fun (inside, got) l ->
                  let c0 = if l = "" then None else Some l.[0] in
                  let rest = Util.drop_chars l 1 in
                  if
                    c0 = Some sign
                    && String.starts_with ~prefix:"available:" rest
                  then (true, String.trim rest :: got)
                  else if
                    inside && c0 = Some sign
                    && String.length l > 1
                    && (l.[1] = ' ' || l.[1] = '\t')
                  then (true, String.trim rest :: got)
                  else (false, got))
                (false, []) lines
            in
            String.trim
              (Util.removeprefix
                 (String.concat " " (List.rev got))
                 "available:")
          in
          let old = block '-' and new_ = block '+' in
          if old <> new_ then
            (Rx.group m 1, (old, new_)) :: List.remove_assoc (Rx.group m 1) out
          else out
      | _ -> out)
    []
    (compare_files base sha cache_dir)
  |> List.rev

let pr_opam_changes_hook :
    (string option -> string -> string -> (string * (string * string)) list) ref
    =
  ref pr_opam_changes

let upper_bound_term_re = Rx.v {|(<=|<)\s*"([^"]+)"|}

(** The lowest upper bound ([<] or [<=] version) of a dependency line, or None.
*)
let upper_bound line =
  let ups =
    List.map
      (function [ _; v ] -> v | _ -> assert false)
      (Rx.findall_groups upper_bound_term_re line)
  in
  Sources.min_version ups

let hunk_field_re = Rx.v {|@@[^@]*@@\s*([a-z][\w-]*):|}
let field_re = Rx.v {|[-+ ]?([a-z][\w-]*):|}
let dependency_change_re = Rx.v {|([-+])\s*"([\w+.-]+)"\s*(\{[^}]*\})?|}

let pr_added_bounds base sha cache_dir =
  List.fold_left
    (fun out f ->
      match Rx.match_ opam_path_re (Json.str "filename" f) with
      | Some m when Json.str "status" f = "modified" ->
          let old = Odict.create ()
          and new_ = Odict.create ()
          and field = ref None in
          List.iter
            (fun l ->
              if String.starts_with ~prefix:"@@" l then
                (* The hunk header names the field the hunk starts in (git's default function context). *)
                field :=
                  Option.map (fun h -> Rx.group h 1) (Rx.match_ hunk_field_re l)
              else begin
                (match Rx.match_ field_re l with
                | Some fm -> field := Some (Rx.group fm 1)
                | None -> ());
                match Rx.match_ dependency_change_re l with
                | Some dm when !field = Some "depends" ->
                    Odict.set
                      (if Rx.group dm 1 = "-" then old else new_)
                      (Rx.group dm 2)
                      (String.trim (Util.drop_chars l 1))
                | _ -> ()
              end)
            (Util.splitlines (Json.str "patch" f));
          let added =
            List.filter
              (fun (dep, line) ->
                match upper_bound line with
                | Some up -> (
                    match
                      upper_bound (Option.value ~default:"" (Odict.get old dep))
                    with
                    | None -> true
                    | Some was -> opam_vcmp up was < 0)
                | None -> false)
              (Odict.to_list new_)
          in
          if added <> [] then
            (Rx.group m 1, added) :: List.remove_assoc (Rx.group m 1) out
          else out
      | _ -> out)
    []
    (compare_files base sha cache_dir)
  |> List.rev

(* Python's re.search(rf"(?<![\w-]){n}(?![\w-])", f): [n] occurs in [f]
   neither preceded nor followed by a word character or a dash. *)
let uses_name f n =
  let is_w c = Util.is_alnum c || c = '_' || c = '-' || Char.code c >= 128 in
  let k = String.length n in
  let rec go from =
    let i = Util.find ~from f n in
    if i < 0 then false
    else if
      (i = 0 || not (is_w f.[i - 1]))
      && (i + k >= String.length f || not (is_w f.[i + k]))
    then true
    else go (i + 1)
  in
  k > 0 && go 0

let relaxed_available_rules (data : data) sha base cache_dir
    (_pr_packages : string list) =
  let changes =
    List.sort
      (fun (a, _) (b, _) -> compare a b)
      (!pr_opam_changes_hook base sha cache_dir)
  in
  let uses f names = List.exists (uses_name f) names in
  List.filter_map
    (fun (pkg, (old, new_)) ->
      if old = "" then None (* a filter was added: not a relaxation *)
      else begin
        let exact = ref true in
        let failing =
          List.filter
            (fun j ->
              if
                (not (List.mem j.category failing_cats))
                || not (j.target = Some pkg || j.revdep = Some pkg)
              then false
              else
                let env = Filter.platform_vars j in
                match
                  ( Filter.eval_filter (Some old) env,
                    Filter.eval_filter (Some new_) env )
                with
                | None, _ | _, None ->
                    (* Not evaluated: at least, the old filter restricts the
                     platform's os (or arch, on Linux) and the new one no longer does. *)
                    let names =
                      if List.assoc "os" env <> "linux" then
                        [ "os"; "os-distribution"; "os-family" ]
                      else [ "arch" ]
                    in
                    if not (uses old names && not (uses new_ names)) then false
                    else (
                      exact := false;
                      true)
                | Some was, Some now -> not (was || not now))
            data.jobs
        in
        if failing = [] then None
        else
          let plats =
            List.sort_uniq compare
              (List.map
                 (fun j ->
                   let d = job_dims j in
                   (match d.distro with Some s when s <> "" -> s | _ -> d.os)
                   ^ if j.experimental then " (experimental)" else "")
                 failing)
          in
          Some
            (Suggestion.make
               ([
                  `Variant (pkg ^ " [available changed by this PR]");
                  `Failing (Some pkg);
                ]
               @ Where.fix_where Pr "this PR"
               @ [
                   `Rule Rule.Pr_relaxed_available;
                   `Error
                     (Printf.sprintf
                        "This PR changes `available` of %s from `%s` to `%s`; \
                         %s now fails on %s"
                        pkg old
                        (if new_ = "" then "(removed)" else new_)
                        pkg (String.concat ", " plats));
                   `Old
                     (if new_ <> "" then Some ("available: " ^ new_) else None);
                   `New
                     (Some
                        (Printf.sprintf
                           "available: %s   (restore it, or narrow it to the \
                            platforms that build)"
                           old));
                   `Confidence (if !exact then "high" else "medium");
                   `Jobs (List.map (fun j -> j.variant) failing);
                   `Note
                     (if !exact then
                        "The previous filter excluded each of these platforms."
                      else
                        "Check that the previous filter excluded these \
                         platforms.");
                 ]))
      end)
    changes

(* A job that failed because of the CI rather than what it checks. *)
let ci_failure =
  Rx.v
    {|Cancelled|curl failed|HTTP error code|Could not resolve|Connection (?:refused|reset|timed out)|exited with status 128|}

let analysis_rules ?match_ (data : data) cache_dir =
  List.filter_map
    (fun j ->
      if
        j.kind <> Analysis || j.status <> Failed
        || not (picked match_ j.variant)
      then None
      else begin
        let why =
          match j.reason with
          | Some r when r <> "" -> r
          | _ ->
              Option.value ~default:""
                (List.find_opt (fun m -> String.trim m <> "") j.messages)
        in
        let why =
          if why <> "" then why
          else
            try
              let ex = excerpt (!log_text_hook (fetch j.url cache_dir)) in
              match List.assoc_opt "job_failed" ex with
              | Some s when s <> "" -> s
              | _ -> Option.value ~default:"" (List.assoc_opt "final" ex)
            with _ -> ""
        in
        let err = Util.prefix_chars (String.concat " " (Util.split why)) 300 in
        let r =
          Suggestion.make
            [
              `Variant j.variant;
              `Url j.url;
              `Rule Rule.Analysis;
              `Jobs [ j.variant ];
              `Experimental j.experimental;
              `Confidence "medium";
              `Error (if err = "" then "The CI page gives no reason" else err);
            ]
        in
        if Rx.test ci_failure why then
          Suggestion.update r
            (Where.fix_where Rerun "CI (rerun)"
            @ [
                `Note
                  "The analysis job failed because of the CI, so no build job \
                   ran. Rerun it.";
              ])
        else
          Suggestion.update r
            (Where.fix_where Pr "this PR"
            @ [
                `Note
                  "opam-repo-ci could not analyse this PR, so no build job \
                   ran. The cause is usually in the PR's files (an opam file \
                   that cannot be read, or a file in an unexpected place): see \
                   the reason above, or the job's log.";
              ]);
        Some r
      end)
    data.jobs

(* A finding of opam-ci-check: "Error in pkg.ver: …" or "Warning in pkg.ver: …".
   It fails the lint job on warnings too. *)
let lint_message = Rx.v {|(Error|Warning) in ([\w+-]+)\.(\S+?): (.*)$|}

(** (PR number, set of (package name, message)) of the lint errors in the CI run
    of the PR that published the previous version of a package of this PR. *)
let lint_baseline (data : data) cache_dir =
  let targets =
    List.sort_uniq compare
      (List.filter_map
         (fun j ->
           match j.target with Some t when t <> "" -> Some t | _ -> None)
         data.jobs)
  in
  let rec go = function
    | [] -> None
    | target :: rest -> (
        match !find_baseline_commit_hook target cache_dir with
        | Some info when Dict.mem info "sha" -> (
            match
              !parse_commit_page_hook
                (fetch (Net.base ^ Dict.str info "sha") cache_dir)
            with
            | exception (Http.Http_error _ | Failure _) -> None
            | base ->
                let msgs =
                  List.concat_map
                    (fun j ->
                      List.filter_map
                        (fun m ->
                          Option.map
                            (fun mm -> (Rx.group mm 2, Rx.group mm 4))
                            (Rx.match_ lint_message m))
                        j.messages)
                    base.jobs
                in
                Some (Option.value ~default:`Null (Dict.get info "pr"), msgs))
        | _ -> go rest)
  in
  go targets

let git_re = Rx.v {|"git"|}
let status_re = Rx.v "status 128"
let https_github_com_re = Rx.v {|https://github\.com/([^/]+/[^/]+)/blob/(.+)$|}
let github_repository_re = Rx.v {|https://github\.com/([^/]+/[^/]+)$|}
let field_start_re = Rx.v ~flags:[ Rx.M ] "^[a-z][\\w-]*:"
let md5_sha_re = Rx.v {|\[\s*"([^"]+)"\s+"(?:md5|sha\d+)=|}
let quoted_name_re = Rx.v {|"([\w+-]+)"|}
let depexts_npm_version_re = Rx.v {|depexts:[\s\S]*?npm-version|}
let missing_field_re = Rx.v {|Missing field '([\w-]+)'|}

let lint_rules ?src ?sha ?(cache_dir = ".opam-ci-cache") (data : data) =
  let out = ref [] in
  (* [src] is None in some tests: the lookups that need it then find nothing. *)
  let s = match src with Some s -> s | None -> Sources.create cache_dir in
  let baseline =
    if List.exists (fun j -> j.kind = Lint && j.status = Failed) data.jobs then
      lint_baseline data cache_dir
    else None
  in
  let pr_opam name ver =
    match sha with Some sha -> opam_file ~ref:sha s name ver | None -> None
  in
  List.iter
    (fun j ->
      if j.kind <> Lint || j.status <> Failed then ()
      else if
        not (List.exists (fun m -> Rx.test_match lint_message m) j.messages)
      then begin
        (* No finding: the job failed before linting (the CI could not fetch or check out the PR). *)
        let msg =
          match List.find_opt (fun m -> String.trim m <> "") j.messages with
          | Some m when m <> "" -> m
          | _ -> "no message"
        in
        let git = Rx.test git_re msg && Rx.test status_re msg in
        out :=
          !out
          @ [
              Suggestion.make
                ([ `Variant "(lint)" ]
                @ Where.fix_where Rerun "CI (rerun)"
                @ [
                    `Rule Rule.Lint_ci_error;
                    `Error (Util.prefix_chars msg 220);
                    `Url j.url;
                    `Note
                      ("The lint job failed before linting anything"
                      ^ (if git then ": git could not check out the PR's commit"
                         else "")
                      ^ ". Rerun it; if it fails again the same way, push the \
                         branch again.");
                  ]);
            ]
      end
      else
        List.iter
          (fun msg ->
            match Rx.match_ lint_message msg with
            | None -> ()
            | Some m -> (
                let level = Rx.group m 1
                and name = Rx.group m 2
                and ver = Rx.group m 3
                and text = Rx.group m 4 in
                let r =
                  Suggestion.make
                    ([ `Variant "(lint)"; `Failing (Some (name ^ "." ^ ver)) ]
                    @ Where.fix_where Pr "this PR"
                    @ [
                        `Rule Rule.Lint;
                        `Error (Util.prefix_chars text 220);
                        `Lint_level (String.lowercase_ascii level);
                      ])
                in
                let has = Util.contains text in
                let upd kv = Suggestion.update r kv in
                let note n = `Note n and conf c = `Confidence c in
                if has "warning 62" then begin
                  let v =
                    match List.rev (Rx.findall quoted_re text) with
                    | x :: _ -> x
                    | [] -> ""
                  in
                  let lid =
                    match Rx.match_ https_github_com_re v with
                    | Some gm ->
                        Sources.license_id
                          (Sources.raw s
                             (Printf.sprintf
                                "https://raw.githubusercontent.com/%s/%s"
                                (Rx.group gm 1) (Rx.group gm 2)))
                    | None -> None
                  in
                  match lid with
                  | Some lid ->
                      upd
                        [
                          `Old (Some (Printf.sprintf {|license: "%s"|} v));
                          `New
                            (Some
                               (Printf.sprintf {|license: "%s"|}
                                  (List.hd (String.split_on_char ' ' lid))));
                          conf
                            (if Util.contains lid "check" then "medium"
                             else "high");
                          note
                            (Printf.sprintf
                               "Detected from the license text (%s)." lid);
                        ]
                  | None ->
                      upd
                        [
                          note
                            "Use an SPDX identifier \
                             (https://spdx.org/licenses/).";
                          conf "medium";
                        ]
                end
                else if has "warning 68" && has "license" then begin
                  (* The license of another version in opam-repository, else
                   the one of the upstream LICENSE file: (license, origin). *)
                  let from_other_versions () =
                    let others =
                      Util.take 2
                        (List.filter
                           (fun v -> v <> ver)
                           (List.rev
                              (Option.value ~default:[] (!versions_hook s name))))
                    in
                    List.find_map
                      (fun v ->
                        match
                          Sources.opam_field (opam_file s name v) "license"
                        with
                        | Some l when l <> "" ->
                            Some
                              ( l,
                                Printf.sprintf "%s.%s in opam-repository" name v
                              )
                        | _ -> None)
                      others
                  in
                  let from_upstream () =
                    let up =
                      match sha with
                      | Some sha -> Sources.upstream ~ref:sha s name ver
                      | None -> None
                    in
                    match
                      Rx.match_ github_repository_re
                        (Option.value ~default:"" up)
                    with
                    | Some gm ->
                        List.find_map
                          (fun fn ->
                            Sources.license_id
                              (Sources.raw s
                                 (Printf.sprintf
                                    "https://raw.githubusercontent.com/%s/HEAD/%s"
                                    (Rx.group gm 1) fn))
                            |> Option.map (fun lid ->
                                ( List.hd (String.split_on_char ' ' lid),
                                  Printf.sprintf "%s in %s" fn (s_none up)
                                  ^
                                  if Util.contains lid "check" then " (check)"
                                  else "" )))
                          [ "LICENSE"; "LICENSE.md"; "LICENSE.txt"; "COPYING" ]
                    | None -> None
                  in
                  match
                    Util.first_some [ from_other_versions; from_upstream ]
                  with
                  | Some (lic, origin) ->
                      upd
                        [
                          `Old None;
                          `New (Some (Printf.sprintf {|license: "%s"|} lic));
                          conf
                            (if Util.contains origin "(check)" then "medium"
                             else "high");
                          note (Printf.sprintf "Taken from %s." origin);
                        ]
                  | None ->
                      upd
                        [
                          note "Add a `license:` field with an SPDX identifier.";
                          conf "medium";
                        ]
                end
                else if has "warning 47" then
                  begin match
                    Sources.opam_field (pr_opam name ver) "synopsis"
                  with
                  | Some syn when syn <> "" ->
                      let fixed = Util.rstrip ~chars:"." (String.trim syn) in
                      let fixed = String.capitalize_ascii fixed in
                      upd
                        [
                          `Old (Some (Printf.sprintf {|synopsis: "%s"|} syn));
                          `New (Some (Printf.sprintf {|synopsis: "%s"|} fixed));
                          conf "high";
                        ]
                  | _ ->
                      upd
                        [
                          note
                            "Start the synopsis with a capital letter, without \
                             a final dot.";
                          conf "high";
                        ]
                  end
                else if
                  has "warning 59" || (has "Weak checksum" && has "field url")
                then begin
                  let cmd =
                    checksum_command (Sources.url_src (pr_opam name ver))
                  in
                  upd
                    [
                      `New
                        (Some
                           {|checksum: ["sha256=<hash>" "sha512=<hash>"]   (in the url section)|});
                      note
                        (match cmd with
                        | Some c ->
                            Printf.sprintf
                              "Compute it with `%s` (or sha512sum)." c
                        | None ->
                            "Download the archive and compute its checksum \
                             with sha256sum (or sha512sum).");
                      conf "high";
                    ]
                end
                else if has "extra-files" || has "Unexpected file in packages/"
                then begin
                  let opam = Option.value ~default:"" (pr_opam name ver) in
                  let section =
                    match Util.partition opam "extra-files:" with
                    | _, "extra-files:", rest -> rest
                    | _ -> ""
                  in
                  let section =
                    List.hd (Rx.split ~maxsplit:1 field_start_re section)
                  in
                  (* up to the next field *)
                  let files = Rx.findall md5_sha_re section in
                  let f0 = match files with f :: _ -> f | [] -> "<file>" in
                  upd
                    [
                      `New
                        (Some
                           (Printf.sprintf
                              {|extra-source "%s" { src: "<URL where %s is published>" checksum: ["sha256=<hash>"] }|}
                              f0 f0));
                      note
                        ("opam-repository no longer accepts files/: publish \
                          the file (in the upstream repository, or as a \
                          release asset), refer to it with extra-source, and \
                          delete "
                        ^ Printf.sprintf
                            "extra-files and packages/%s/%s.%s/files/." name
                            name ver
                        ^
                        if files <> [] then
                          Printf.sprintf " Files: %s."
                            (String.concat ", " files)
                        else "");
                      conf "high";
                    ]
                end
                else if has "Weak checksum" then
                  upd
                    [
                      note
                        "Use SHA-256 or SHA-512 checksums (compute them with \
                         sha256sum or sha512sum).";
                      conf "high";
                    ]
                else if has "error 60" then begin
                  let cmd =
                    checksum_command (Sources.url_src (pr_opam name ver))
                  in
                  upd
                    [
                      note
                        ("The upstream archive no longer matches the checksum \
                          (the tag moved, or the archive was regenerated). "
                        ^ (match cmd with
                          | Some c -> Printf.sprintf "Recompute it with `%s`" c
                          | None ->
                              "Download the archive and compute its checksum \
                               again")
                        ^ ", and check with upstream that the release did not \
                           change.");
                      conf "high";
                    ]
                end
                else if has "error 46" then
                  upd
                    [
                      note
                        "Flagged \"conf\" but has url, build or install, while \
                         conf packages only check for system packages. Remove \
                         the `conf` flag (and the `conf-` prefix) if it really \
                         builds something, or drop url, build and install. A \
                         conf package that builds something needs the \
                         maintainers' agreement.";
                      conf "medium";
                    ]
                else if has "warning 41" then begin
                  let named = Rx.findall quoted_name_re text in
                  if List.mem name named then
                    upd
                      [
                        `Old (Some (Printf.sprintf "%%{%s:VAR}%%" name));
                        `New (Some "%{_:VAR}%   (in build/install/remove)");
                        conf "high";
                        note
                          "The package refers to its own variables by name: \
                           use the `_` prefix.";
                      ]
                  else
                    upd
                      [
                        note
                          (Printf.sprintf "Add %s to depends or depopts."
                             (String.concat ", " named));
                        conf "high";
                      ]
                end
                else if has "No package source directory" then
                  begin if Sources.url_src (pr_opam name ver) <> None then
                    upd
                      [
                        note
                          "The package has a url, but its source could not be \
                           fetched or verified (see the other lint errors, \
                           such as a checksum mismatch).";
                        conf "medium";
                      ]
                  else
                    upd
                      [
                        note
                          "Known false positive of opam-ci-check for packages \
                           without `url` (virtual packages): maintainers \
                           accept it.";
                        conf "high";
                      ]
                  end
                else if has "conf packages should always use" then
                  begin if
                    Rx.test depexts_npm_version_re
                      (Option.value ~default:"" (pr_opam name ver))
                  then
                    upd
                      [
                        note
                          "The depexts are npm packages (`npm-version` filter, \
                           checked by opam-check-npm-deps), the usual \
                           convention for Melange bindings. This lint error is \
                           a false positive: nothing to change, maintainers \
                           accept it.";
                        conf "high";
                      ]
                  else
                    upd
                      [
                        note
                          "This package has `depexts` but is not a conf \
                           package: move them into a conf-* package, or ask \
                           the maintainers to accept it (they often do).";
                        conf "medium";
                      ]
                  end
                else
                  r.note <-
                    Some
                      (if level = "Error" then
                         "The tool has no advice for this lint message."
                       else
                         "A warning, but the lint job fails on warnings too: \
                          follow the message.");
                (* A field also missing in the version on master (a PR that
                 deprecates or bounds old versions): the lint job reports it
                 again. Some authors add it then (#31205), others do not
                 (#30832): the change stays, with the note. *)
                let mf = Rx.search missing_field_re text in
                let on_master =
                  match (mf, src) with
                  | Some _, Some src -> opam_file src name ver
                  | _ -> None
                in
                (match (mf, on_master) with
                | Some mf, Some om
                  when om <> ""
                       && not
                            (Rx.test
                               (Rx.of_re
                                  (Re.seq
                                     [
                                       Rx.re ~flags:[ Rx.M ] "^";
                                       Re.str (Rx.group mf 1);
                                       Rx.re ~flags:[ Rx.M ] {|\s*:|};
                                     ]))
                               om) ->
                    let n = Suggestion.str r.note in
                    r.note <-
                      Some
                        ((if n <> "" then Util.rstrip ~chars:"." n ^ ". "
                          else "")
                        ^ "Already missing on master: this PR, which changes \
                           an existing version, did not remove it.")
                | _ -> ());
                (match baseline with
                | Some (pr, msgs) when List.mem (name, text) msgs ->
                    let prev = Util.rstrip ~chars:"." (Suggestion.str r.note) in
                    r.note <-
                      Some
                        ((if prev <> "" then prev ^ ". " else "")
                        ^ Printf.sprintf
                            "Already reported in the CI of #%s (the previous \
                             release), which was merged."
                            (json_text pr))
                | _ -> ());
                (* one entry for the same message and explanation on several packages *)
                let same =
                  List.find_opt
                    (fun o ->
                      (not (Suggestion.is_set r.Suggestion.new_))
                      && (not (Suggestion.is_set o.Suggestion.new_))
                      && Suggestion.str o.error = Suggestion.str r.error
                      && o.note = r.note)
                    !out
                in
                match same with
                | Some same ->
                    same.Suggestion.failing <-
                      Set
                        (Suggestion.str_field same.Suggestion.failing
                        ^ ", "
                        ^ Suggestion.str_field r.Suggestion.failing)
                | None -> (
                    (* one entry per (package, fix): several lint messages often share a fix *)
                    let prev =
                      if Suggestion.is_set r.Suggestion.new_ then
                        List.find_opt
                          (fun o ->
                            Suggestion.str_field o.Suggestion.failing
                            = Suggestion.str_field r.Suggestion.failing
                            && Suggestion.get o.Suggestion.new_
                               = Suggestion.get r.new_)
                          !out
                      else None
                    in
                    match prev with
                    | Some prev ->
                        prev.error <-
                          Some
                            (Suggestion.str prev.error ^ " | "
                            ^ Util.prefix_chars (Suggestion.str r.error) 120)
                    | None -> out := !out @ [ r ])))
          j.messages)
    data.jobs;
  !out

let skipped_rules ?src ?(bounds = []) (data : data) cache_dir pr_packages =
  List.filter_map
    (fun t ->
      let own =
        List.filter (fun j -> j.target = Some t && j.kind = Build) data.jobs
      in
      if
        List.length own < 3
        || List.exists (fun j -> j.category <> Not_available) own
      then None
      else begin
        let pick =
          Util.sublist ~start:(-1)
            (Util.sort_by
               (fun j ->
                 ( not (String.starts_with ~prefix:"compilers," j.variant),
                   j.variant ))
               own)
        in
        (* The solver's conflict chains, each with the package that ends it
           and the reason. *)
        let conflicts =
          List.concat_map
            (fun j ->
              match !log_text_hook (fetch j.url cache_dir) with
              | exception _ -> []
              | txt ->
                  (* opam writes the package with the constraint that pulled it
                   in, if any: "pkg = 1.0 -> …", "pkg >= 0.7 -> …" or "pkg -> …";
                   the line after a chain gives the reason, such as "unknown
                   package". *)
                  let pat =
                    let m = [ Rx.M ] in
                    Rx.of_re
                      (Re.seq
                         [
                           Rx.re ~flags:m {|^\s*- |};
                           Re.group
                             (Re.seq
                                [
                                  Re.str (List.hd (String.split_on_char '.' t));
                                  Rx.re ~flags:m {|(?: [<>=!]+ \S+)? -> .+|};
                                ]);
                           Rx.re ~flags:m {|\n\s*(.*)$|};
                         ])
                  in
                  List.filter_map
                    (function
                      | [ chain; why ] ->
                          Some
                            ( chain,
                              Util.last (Util.split_on " -> " chain),
                              String.trim why )
                      | _ -> None)
                    (Rx.findall_groups pat txt))
            pick
        in
        let chains = List.map (fun (chain, _, _) -> chain) conflicts in
        let unknown =
          SS.of_list
            (List.filter_map
               (fun (_, last, why) ->
                 if why = "unknown package" then
                   Some (List.hd (String.split_on_char ' ' last))
                 else None)
               conflicts)
        in
        let no_version =
          SS.of_list
            (List.filter_map
               (fun (_, last, why) ->
                 if
                   why = "no matching version"
                   && String.contains last ' '
                   && not (String.contains last '|')
                 then Some last
                 else None)
               conflicts)
        in
        if chains = [] then None
        else begin
          let firsts =
            SS.of_list
              (List.filter_map
                 (fun c ->
                   match Util.split_on " -> " c with
                   | _ :: b :: _ -> Some b
                   | _ -> None)
                 chains)
          in
          let r =
            Suggestion.make
              ([ `Variant (t ^ " [all build jobs skipped]"); `Failing (Some t) ]
              @ Where.fix_where Pr "this PR"
              @ [
                  `Rule Rule.Uninstallable;
                  `Error
                    ("Not installable on any tested platform. Solver: "
                    ^ String.concat "; "
                        (Util.take 3 (List.sort_uniq compare chains)));
                ])
          in
          let added = Option.value ~default:[] (List.assoc_opt t bounds) in
          let through =
            SS.of_list
              (List.map
                 (fun f -> List.hd (String.split_on_char ' ' f))
                 (SS.elements firsts))
          in
          let bounded =
            added <> []
            && (not (SS.is_empty through))
            && SS.for_all (fun d -> List.mem_assoc d added) through
            && SS.is_empty unknown
          in
          let lines =
            if bounded then
              String.concat "; "
                (List.map
                   (fun d -> "`" ^ List.assoc d added ^ "`")
                   (SS.elements through))
            else ""
          in
          (* A bound that no version satisfies ({>= "3.0" & < "2.0"}), or no
             published version (a typo such as {< "0.3"} for dune), is a
             mistake, not the point of the change. The versions come from
             ocaml.org (and this PR); when they are unknown (no answer,
             counted in "mcp-errors": the analysis is retried), nothing says
             the bound is intended. A disjunction is not checked, so nothing
             says it is intended either. *)
          let problem d =
            let line = List.assoc d added in
            let terms = Constraints.version_terms line in
            if String.contains line '|' then Some `Alternatives
            else if terms = [] then None
            else if Constraints.unsatisfiable terms then Some `Empty
            else
              match
                match src with Some src -> !versions_hook src d | None -> None
              with
              | None -> Some `Unchecked
              | Some vs ->
                  let vs =
                    vs
                    @ List.filter_map
                        (fun p ->
                          let n, v = split_pkg p in
                          if n = d then Some v else None)
                        pr_packages
                  in
                  if List.exists (fun v -> Constraints.allows v terms) vs then
                    None
                  else Some `Unpublished
          in
          let problems =
            if bounded then
              List.filter_map
                (fun d -> Option.map (fun p -> (p, d)) (problem d))
                (SS.elements through)
            else []
          in
          let with_problem p =
            List.filter_map
              (fun (p', d) -> if p' = p then Some d else None)
              problems
          in
          let empty = with_problem `Empty
          and unpublished = with_problem `Unpublished
          and unchecked = with_problem `Unchecked
          and alternatives = with_problem `Alternatives in
          let upd conf n = Suggestion.update r [ `Confidence conf; `Note n ] in
          let it_them l = if List.length l = 1 then "it" else "them" in
          if empty <> [] then
            upd "high"
              (Printf.sprintf
                 "This PR adds %s to %s, and no version of %s satisfies %s: a \
                  lower bound is above an upper bound. Fix the bound."
                 lines t (String.concat ", " empty) (it_them empty))
          else if unpublished <> [] then
            upd "high"
              (Printf.sprintf
                 "This PR adds %s to %s, and no published version of %s \
                  satisfies %s: check the bound for a typo."
                 lines t
                 (String.concat ", " unpublished)
                 (it_them unpublished))
          else if alternatives <> [] then
            upd "medium"
              (Printf.sprintf
                 "This PR adds %s to %s, which no tested platform satisfies, \
                  so CI cannot build it. The bound on %s has alternatives \
                  (`|`), which are not checked: check that some version \
                  satisfies it and that it is intended."
                 lines t
                 (String.concat ", " alternatives))
          else if unchecked <> [] then
            upd "medium"
              (Printf.sprintf
                 "This PR adds %s to %s, which no tested platform satisfies, \
                  so CI cannot build it. The versions of %s could not be read \
                  (ocaml.org did not answer), so nothing says some version \
                  satisfies the bound: check it."
                 lines t
                 (String.concat ", " unchecked))
          else if bounded then
            Suggestion.update r
              (Where.fix_where Nothing "nothing (this PR's bound)"
              @ [
                  `Rule Rule.Bounded_out;
                  `Confidence "high";
                  `Note
                    (Printf.sprintf
                       "This PR adds %s to %s, which no tested platform \
                        satisfies, so CI cannot build it. That is expected \
                        when the bound is the point of the change."
                       lines t);
                ])
          else if not (SS.is_empty unknown) then begin
            let one = SS.cardinal unknown = 1 in
            let names =
              String.concat ", "
                (List.map (fun u -> "`" ^ u ^ "`") (SS.elements unknown))
            in
            upd "high"
              (Printf.sprintf
                 "%s %s not in opam-repository. Add %s to this PR, or wait \
                  until %s published."
                 names
                 (if one then "is" else "are")
                 (if one then "it" else "them")
                 (if one then "it is" else "they are"))
          end
          else if not (SS.is_empty no_version) then begin
            let dep, _, cstr = Util.partition (SS.min_elt no_version) " " in
            upd "high"
              (Printf.sprintf
                 "No version of `%s` in opam-repository satisfies `%s`. \
                  Publish that version first (or in this PR), or relax the \
                  constraint if an existing version works."
                 dep cstr)
          end
          else if SS.cardinal firsts = 1 then begin
            let first = SS.choose firsts in
            let ends =
              List.sort_uniq compare
                (List.filter_map
                   (fun c ->
                     match Util.split_on " -> " c with
                     | _ :: _ :: e :: _ -> Some e
                     | _ -> None)
                   chains)
            in
            upd "high"
              (Printf.sprintf "Every solver conflict goes through `%s`" first
              ^ (if ends <> [] then
                   ", which requires " ^ String.concat " or " ends
                 else "")
              ^ ". Relax or drop that constraint, after checking which \
                 versions of the dependency really fail.")
          end;
          Some r
        end
      end)
    pr_packages

let accepted_elsewhere ?(max_versions = 3) (results : Suggestion.t list) src
    (_pr_packages : string list) =
  let cache = Hashtbl.create 8 in
  List.iter
    (fun r ->
      let new_ = Suggestion.str_field r.Suggestion.new_
      and failing = Suggestion.str_field r.Suggestion.failing in
      let name, ver = split_pkg failing in
      if ver = "" || String.contains failing ' ' then ()
      else begin
        let m = Rx.match_ ci_accept_failures_re new_ in
        let wanted =
          match m with
          | Some m -> Some (SS.of_list (Rx.findall quoted_re (Rx.group m 1)))
          | None ->
              if Util.contains new_ {|os != "win32"|} then
                Some (SS.of_list [ "cygwin"; "msys2" ])
              else None
        in
        match wanted with
        | None -> ()
        | Some wanted ->
            let accepted =
              match Hashtbl.find_opt cache name with
              | Some a -> a
              | None ->
                  let others =
                    Util.take max_versions
                      (List.filter
                         (fun v -> v <> ver)
                         (List.rev
                            (Option.value ~default:[] (!versions_hook src name))))
                  in
                  let a =
                    List.map
                      (fun v -> (v, accepted_platforms (opam_file src name v)))
                      others
                  in
                  Hashtbl.replace cache name a;
                  a
            in
            let found =
              List.filter_map
                (fun (v, acc) ->
                  let i = SS.inter acc wanted in
                  if SS.is_empty i then None else Some (v, i))
                accepted
            in
            if found <> [] then begin
              let found = List.sort (fun (a, _) (b, _) -> compare a b) found in
              let where =
                String.concat ", "
                  (List.map
                     (fun (v, ps) ->
                       Printf.sprintf "%s.%s (%s)" name v
                         (String.concat ", " (SS.elements ps)))
                     found)
              in
              let plats =
                SS.elements
                  (List.fold_left
                     (fun a (_, ps) -> SS.union a ps)
                     SS.empty found)
              in
              r.note <-
                Some
                  (String.trim
                     (Util.rstrip (Suggestion.str r.note)
                     ^ Printf.sprintf
                         " Other versions already accept failures there: %s."
                         where));
              if m = None then
                r.alternative <-
                  Some
                    (Printf.sprintf
                       "x-ci-accept-failures: [%s]   (as other versions of %s \
                        do)"
                       (String.concat " "
                          (List.map (fun p -> "\"" ^ p ^ "\"") plats))
                       name)
            end
      end)
    results
