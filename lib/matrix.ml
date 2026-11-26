open Suggestion.T
open Ci

let starts_day10 j = match j.platform with "day10" :: _ -> true | _ -> false

(* Python's str() of a (target, revdep, kind) tuple: the groups are sorted by it. *)
let key_str (target, revdep, kind) =
  let s = function Some x -> Util.repr x | None -> "None" in
  Printf.sprintf "(%s, %s, %s)" (s target) (s revdep) (Util.repr kind)

let matrix_groups jobs match_ =
  Util.group_by
    (fun j -> (j.target, j.revdep, j.kind))
    (List.filter
       (fun j ->
         List.mem j.kind [ "build"; "tests"; "revdeps" ]
         && (match j.target with Some t -> t <> "" | None -> false)
         &&
         match match_ with
         | None | Some "" -> true
         | Some m -> Rx.test m j.variant)
       jobs)

let sorted_strings l = List.sort_uniq compare l

(** Fails on every job of the older (or newer) OCaml versions, passes on the
    others -> ('>=' | '<', version, failing versions, passing versions). *)
let compiler_matrix js =
  let per = Odict.create () in
  List.iter
    (fun j ->
      if not (j.experimental || List.mem j.category neutral) then begin
        let v = (job_dims j).ocaml in
        let s =
          if List.mem j.category failing_cats then Some "fail"
          else if j.category = "passed" then Some "pass"
          else None
        in
        match (v, s) with
        | Some v, Some s when v <> "" ->
            let cur = Option.value ~default:[] (Odict.get per v) in
            if not (List.mem s cur) then Odict.set per v (s :: cur)
        | _ -> ()
      end)
    js;
  let kv = Odict.to_list per in
  if List.length kv < 2 || List.exists (fun (_, s) -> List.length s > 1) kv then
    None
  else begin
    let vs =
      List.stable_sort (fun (a, _) (b, _) -> compare (vkey a) (vkey b)) kv
    in
    let seq = List.map (fun (_, s) -> List.hd s) vs in
    let fails =
      List.filter_map
        (fun (v, s) -> if List.hd s = "fail" then Some v else None)
        vs
    in
    let passes =
      List.filter_map
        (fun (v, s) -> if List.hd s = "pass" then Some v else None)
        vs
    in
    if fails = [] || passes = [] then None
    else if seq = List.sort compare seq then (* fail ... fail pass ... pass *)
      Some (">=", List.hd passes, fails, passes)
    else if seq = List.rev (List.sort compare seq) then
      (* pass ... pass fail ... fail *)
      Some ("<", List.hd fails, fails, passes)
    else None
  end

(** Fails only on Windows, one arch, or some distros -> (dimension, values). *)
let platform_matrix js =
  let fails =
    List.filter
      (fun j -> (not j.experimental) && List.mem j.category failing_cats)
      js
  in
  let passes =
    List.filter (fun j -> (not j.experimental) && j.category = "passed") js
  in
  if fails = [] || passes = [] then None
  else begin
    let fd = List.map job_dims fails and pd = List.map job_dims passes in
    if
      List.for_all (fun d -> d.os = "win32") fd
      && List.exists (fun d -> d.os <> "win32") pd
    then
      Some
        ( "os",
          sorted_strings
            (List.map (fun d -> Option.value ~default:"" d.distro) fd) )
    else
      let archs = sorted_strings (List.map (fun d -> d.arch) fd) in
      let parchs = List.map (fun d -> d.arch) pd in
      if
        List.length archs = 1
        && archs <> [ "x86_64" ]
        && not (List.exists (fun a -> List.mem a parchs) archs)
      then Some ("arch", archs)
      else
        let dists = List.sort_uniq compare (List.map (fun d -> d.distro) fd) in
        let pdists = List.map (fun d -> d.distro) pd in
        let pocaml = List.map (fun d -> d.ocaml) pd in
        if
          (not (List.mem None dists))
          && (not (List.exists (fun d -> List.mem d pdists) dists))
          && List.for_all (fun d -> List.mem d.ocaml pocaml) fd
        then Some ("distro", List.map Option.get dists)
        else None
  end

(** The architecture of a job, with one name per architecture (the FreeBSD jobs
    say amd64). *)
let job_arch job =
  match (job_dims job).arch with
  | "amd64" -> "x86_64"
  | "aarch64" -> "arm64"
  | a -> a

let cygwin_pkg =
  [
    ("awk", "gawk");
    ("gawk", "gawk");
    ("gcc", "gcc-core");
    ("cc", "gcc-core");
    ("g++", "gcc-g++");
    ("make", "make");
    ("autoconf", "autoconf");
    ("automake", "automake");
    ("pkg-config", "pkgconf");
    ("m4", "m4");
    ("patch", "patch");
    ("perl", "perl");
    ("python3", "python3");
    ("bison", "bison");
    ("flex", "flex");
  ]

let keys_of js = List.map failure_key js
let variants js = List.map (fun j -> j.variant) js

let matrix_rules ?(failing_in_log = []) jobs (earlier : Suggestion.t list)
    match_ pr_packages =
  let fin j = List.assoc_opt (failure_key j) failing_in_log in
  let done_ =
    List.map (fun r -> (Suggestion.get r.Suggestion.failing, r.dep)) earlier
  in
  (* The failures for which a job's log gave a change: the pass/fail pattern
     adds only a workaround next to it (liquidsoap 2.4.6, #30906: its tests
     need base-bytes on OCaml 5, not to be disabled there). *)
  let from_log =
    List.filter_map
      (fun r ->
        match r.Suggestion.key with
        | Some k
          when Suggestion.nonempty_field r.Suggestion.new_
               || Suggestion.list r.candidates <> []
               || Suggestion.nonempty r.advice ->
            Some k
        | _ -> None)
      earlier
  in
  let out = ref [] in
  let groups =
    List.stable_sort
      (fun (a, _) (b, _) -> compare (key_str a) (key_str b))
      (matrix_groups jobs match_)
  in
  List.iter
    (fun ((target, revdep, kind), js) ->
      let target = Option.value ~default:"" target in
      let pkg = match revdep with Some r when r <> "" -> r | _ -> target in
      let fails =
        List.filter
          (fun j -> (not j.experimental) && List.mem j.category failing_cats)
          js
      in
      if fails <> [] then begin
        (* Who has to change: a failing dependency (named by CI, or found in
           the log's ERROR block), or the package itself. *)
        let named =
          Util.dedup
            (List.map
               (fun j ->
                 let d =
                   if j.category = "dependency" then failing_dep j.reason
                   else None
                 in
                 let d =
                   match d with Some x when x <> "" -> Some x | _ -> fin j
                 in
                 match d with
                 | Some d
                   when d <> "" && fst (split_pkg d) <> fst (split_pkg pkg) ->
                     Some d
                 | _ -> None)
               fails)
        in
        let culprit = match named with [ Some d ] -> Some d | _ -> None in
        let owner = Option.value ~default:pkg culprit in
        let tests_only =
          kind = "tests"
          || List.for_all (fun j -> j.category = "build-or-test") fails
        in
        let where = Where.pr_or_other owner (List.mem owner pr_packages) in
        let label =
          Printf.sprintf "%s [%s%s]" pkg kind
            (match revdep with
            | Some r when r <> "" -> ", revdep of " ^ target
            | _ -> "")
        in
        (* A day10 failure whose distribution, architecture and OCaml version
           pass on the classic builder is a problem of the day10 environment,
           not of the package: report it as such, and leave it out of the
           analyses below. Not when day10 passes on another architecture of
           the same distribution and OCaml version: the failure then depends
           on the architecture. *)
        let passes =
          List.filter
            (fun j -> (not j.experimental) && j.category = "passed")
            js
        in
        let classic_twin j =
          let d = job_dims j in
          let same =
            List.filter
              (fun p ->
                let dp = job_dims p in
                dp.distro = d.distro
                && (match d.distro with Some s -> s <> "" | None -> false)
                && dp.ocaml = d.ocaml)
              passes
          in
          if
            List.exists
              (fun p -> starts_day10 p && job_arch p <> job_arch j)
              same
          then None
          else
            List.find_opt
              (fun p -> (not (starts_day10 p)) && job_arch p = job_arch j)
              same
        in
        let twins =
          List.filter_map
            (fun j ->
              if starts_day10 j then
                Option.map (fun t -> (j, t)) (classic_twin j)
              else None)
            fails
        in
        let js, fails =
          if twins <> [] then begin
            let tj = List.map fst twins in
            let dists =
              sorted_strings
                (List.map
                   (fun j -> Option.value ~default:"" (job_dims j).distro)
                   tj)
            in
            let evidence =
              List.find_map
                (fun r ->
                  if
                    Suggestion.get r.Suggestion.failing = culprit
                    && r.rule = Some "missing-tool"
                  then Some r.note
                  else None)
                earlier
              |> Option.join
            in
            let note =
              Printf.sprintf
                "For example, it fails on %s and passes there on the main \
                 builder."
                (platform_words (List.hd tj))
              ^ (match evidence with Some e when e <> "" -> " " ^ e | _ -> "")
              ^
              if
                Util.contains
                  (Option.value ~default:"" evidence)
                  "Nothing to change"
              then ""
              else " Nothing to change in the package metadata."
            in
            out :=
              Suggestion.make
                ([
                   `Variant label;
                   `Failing (Some (Option.value ~default:pkg culprit));
                 ]
                @ Where.fix_where "ci" "the CI environment (day10 builder)"
                @ [
                    `Rule "builder-specific";
                    `Error
                      (Printf.sprintf
                         "Fails only on the day10 builder on %s; the main \
                          builder passes there."
                         (String.concat ", " dists));
                    `Confidence "high";
                    `Note note;
                    `Keys (keys_of tj);
                    `Jobs (variants tj);
                  ])
              :: !out;
            let gone j = List.exists (fun t -> t == j) tj in
            ( List.filter (fun j -> not (gone j)) js,
              List.filter (fun j -> not (gone j)) fails )
          end
          else (js, fails)
        in
        if
          fails <> []
          && not
               (List.for_all (fun j -> List.mem (failure_key j) from_log) fails)
        then begin
          let cm = compiler_matrix js in
          match cm with
          | Some (op, v, fv, pv)
            when not (List.mem (Some owner, Some "ocaml") done_) ->
              let filt =
                if tests_only then Printf.sprintf "with-test & %s \"%s\"" op v
                else Printf.sprintf "%s \"%s\"" op v
              in
              let gap =
                if op = ">=" then
                  Printf.sprintf
                    " OCaml versions between %s and %s were not tested, so the \
                     bound may be lower."
                    (Util.last fv) v
                else ""
              in
              let error =
                (if tests_only then
                   Printf.sprintf "Tests fail on OCaml %s, pass on %s"
                     (String.concat ", " fv) (String.concat ", " pv)
                 else
                   Printf.sprintf "Fails on OCaml %s, passes on %s"
                     (String.concat ", " fv) (String.concat ", " pv))
                ^
                match culprit with
                | Some c -> "; the failure is in the dependency " ^ c
                | None -> ""
              in
              out :=
                Suggestion.make
                  ([ `Variant label; `Failing (Some owner) ]
                  @ where
                  @ [
                      `Rule "compiler-matrix";
                      `Error error;
                      `Dep "ocaml";
                      `Old None;
                      `New
                        (Some
                           (Printf.sprintf "\"ocaml\" {%s}" filt
                           ^
                           if tests_only then "   (new line in depends)" else ""
                           ));
                      `Confidence "medium";
                      `Note ("Based only on which jobs pass and fail." ^ gap);
                      `Keys (keys_of fails);
                      `Jobs (variants fails);
                    ])
                :: !out
          | _ -> (
              match platform_matrix js with
              | None -> ()
              | Some (dim, vals) ->
                  let cond, what =
                    if dim = "os" then
                      ( Some "os != \"win32\"",
                        "Windows (" ^ String.concat ", " vals ^ ")" )
                    else if dim = "arch" then
                      ( Some
                          (String.concat " & "
                             (List.map
                                (fun a -> Printf.sprintf "arch != \"%s\"" a)
                                vals)),
                        String.concat ", " vals )
                    else (None, "distributions " ^ String.concat ", " vals)
                  in
                  let advice = ref None in
                  let new_ =
                    match culprit with
                    | Some culprit -> (
                        let tool = ref None in
                        List.iter
                          (fun r ->
                            if
                              Suggestion.get r.Suggestion.failing = Some culprit
                              && r.rule = Some "missing-tool"
                            then
                              tool :=
                                Rx.search {|(?:Program "?|")([\w.+-]+)|}
                                  (Suggestion.str r.error))
                          earlier;
                        let dep =
                          match !tool with
                          | Some m when dim = "os" ->
                              List.assoc_opt (Rx.group m 1) cygwin_pkg
                          | _ -> None
                        in
                        match dep with
                        | Some dep
                          when List.exists
                                 (fun v ->
                                   String.starts_with ~prefix:"cygwin" v)
                                 vals ->
                            Some
                              (Printf.sprintf
                                 "depexts: [\"%s\"] {os-distribution = \
                                  \"cygwin\"}   (in %s)"
                                 dep culprit)
                        | _ ->
                            (* Not an opam line: the change is described in the note. *)
                            advice :=
                              Some
                                (Printf.sprintf
                                   "Add the missing depext%s to %s, or exclude \
                                    %s there (`x-ci-accept-failures` or \
                                    `available`)."
                                   (if dim <> "arch" then
                                      " for " ^ String.concat "/" vals
                                    else "")
                                   culprit
                                   (if dim = "os" then "Windows"
                                    else String.concat "/" vals));
                            None)
                    | None -> (
                        match cond with
                        | Some cond when tests_only ->
                            Some
                              (Printf.sprintf
                                 "\"@runtest\" {with-test & %s}   (or the \
                                  equivalent filter on the test command)"
                                 cond)
                        | Some cond ->
                            Some
                              (Printf.sprintf
                                 "available: %s   (combined with any existing \
                                  `available`)"
                                 cond)
                        | None ->
                            Some
                              ("x-ci-accept-failures: ["
                              ^ String.concat " "
                                  (List.map (fun d -> "\"" ^ d ^ "\"") vals)
                              ^ "]"))
                  in
                  let reason =
                    List.hd
                      (sorted_strings
                         (List.map
                            (fun j ->
                              Util.prefix_chars
                                (Option.value ~default:"" j.reason)
                                120)
                            fails))
                  in
                  (* The CI's own reason is a sentence ("The compilation of …"): here it follows a colon. *)
                  let reason = Util.lstrip ~chars:": " reason in
                  let reason =
                    if String.starts_with ~prefix:"The " reason then
                      "t" ^ Util.drop_chars reason 1
                    else reason
                  in
                  let distros =
                    if List.for_all starts_day10 fails then
                      sorted_strings
                        (List.map
                           (fun j ->
                             let d = job_dims j in
                             match d.distro with
                             | Some s when s <> "" -> s
                             | _ -> d.os)
                           fails)
                    else []
                  in
                  let error =
                    (if tests_only then "Tests fail" else "Fails")
                    ^ " only on " ^ what
                    ^ (match culprit with
                      | Some c -> "; the failure is in the dependency " ^ c
                      | None -> "")
                    ^ ": " ^ reason
                  in
                  let note =
                    match culprit with
                    | Some _ -> (
                        Printf.sprintf "Nothing to change in %s." pkg
                        ^ match !advice with Some a -> " " ^ a | None -> "")
                    | None ->
                        if tests_only && cond <> None then
                          "This change only skips the tests on these \
                           platforms. Prefer fixing the failing tests."
                        else
                          "This change only hides the failure on these \
                           platforms. Prefer an upstream fix or the missing \
                           depext."
                  in
                  let r =
                    Suggestion.make
                      ([ `Variant label; `Failing (Some owner) ]
                      @ where
                      @ [
                          `Rule "platform-matrix";
                          `Distros distros;
                          `Error error;
                          `Old None;
                          `New new_;
                          `Confidence "medium";
                          `Note note;
                          `Keys (keys_of fails);
                          `Jobs (variants fails);
                        ])
                  in
                  (match !advice with
                  | Some a -> r.advice <- Some a
                  | None -> ());
                  out := r :: !out)
        end
      end)
    groups;
  List.rev !out

let collapse_systemic ?(threshold = 5) (out : Suggestion.t list) =
  let sig_ r =
    let rule = Suggestion.str r.rule in
    let pattern =
      if rule = "platform-matrix" then
        List.hd (Util.split_on ~maxsplit:1 ": " (Suggestion.str r.error))
      else Suggestion.str r.error
    in
    ( rule,
      Rx.sub {|the dependency \S+|} "a dependency" pattern,
      if rule = "compiler-matrix" then
        List.hd (Util.split_on "   " (Suggestion.str_field r.Suggestion.new_))
      else "" )
  in
  List.concat_map
    (fun ((rule, pattern, _), rs) ->
      if List.length rs < threshold then rs
      else begin
        let names =
          sorted_strings
            (List.map
               (fun r ->
                 List.hd
                   (Util.split_on ~maxsplit:1 " ["
                      (Suggestion.str r.Suggestion.variant)))
               rs)
        in
        let culprits =
          Util.counter
            (List.filter
               (fun f -> not (List.mem f names))
               (List.map
                  (fun r -> Suggestion.str_field r.Suggestion.failing)
                  rs))
        in
        let r0 = List.hd rs in
        let error, note =
          if rule = "builder-specific" then
            ( List.hd (Util.split_on ~maxsplit:1 ";" pattern) ^ ".",
              r0.Suggestion.note )
          else begin
            let error = Util.rstrip ~chars:"." pattern ^ "." in
            let code, _, hint =
              Util.partition (Suggestion.str_field r0.Suggestion.new_) "   "
            in
            let m = Rx.fullmatch {|\(in (\S+)\)|} (String.trim hint) in
            let fix =
              match m with
              | Some m when code <> "" ->
                  Printf.sprintf "Possible fix, in %s: `%s`." (Rx.group m 1)
                    code
              | _ ->
                  if code <> "" then
                    Printf.sprintf "Possible fix for each package: `%s`" code
                    ^
                    if String.trim hint <> "" then " " ^ String.trim hint ^ "."
                    else "."
                  else Suggestion.str r0.advice
            in
            let prefix =
              if culprits <> [] then
                (if List.length culprits = 1 then
                   "The failure is in a dependency: "
                 else "The failure is in dependencies: ")
                ^ String.concat ", "
                    (List.map
                       (fun (d, n) ->
                         Printf.sprintf "%s (%s)" d (Words.counted n "package"))
                       (Util.take 3 culprits))
                ^ ". "
              else ""
            in
            (error, Some (prefix ^ fix))
          end
        in
        let entry =
          Suggestion.make
            ([
               `Variant
                 (Printf.sprintf "%d packages [%s]" (List.length rs) rule);
               `Failing
                 (Some
                    (String.concat ", " (Util.take 6 names)
                    ^ if List.length names > 6 then " ..." else ""));
             ]
            @ (if rule = "builder-specific" then
                 Where.fix_where "ci" (Suggestion.str r0.fix_in)
               else
                 Where.fix_where "unknown"
                   "the PR, a shared dependency, or the CI")
            @ [
                `Rule (rule ^ "-systemic");
                `Error error;
                `Confidence "medium";
                `Keys (List.concat_map (fun r -> r.Suggestion.keys) rs);
                `Jobs
                  (Ci.merge_jobs []
                     (List.concat_map
                        (fun r -> Suggestion.list r.Suggestion.jobs)
                        rs));
              ])
        in
        entry.note <- note;
        (match culprits with
        | [ (d, n) ] when rule <> "builder-specific" && n = List.length rs ->
            (* Every package fails through the same dependency: the fix goes there (shown as such). *)
            entry.failing_dep <- Some d;
            entry.where_kind <- Some "other-package";
            entry.where_pkg <- Set d
        | _ -> ());
        [ entry ]
      end)
    (Util.group_by sig_ out)
