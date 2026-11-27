open Suggestion.T
open Ci

let starts_day10 j = match j.platform with "day10" :: _ -> true | _ -> false

(* Python's str() of a (target, revdep, kind) tuple: the groups are sorted by it. *)
let key_str (target, revdep, kind) =
  let s = function Some x -> Util.repr x | None -> "None" in
  Printf.sprintf "(%s, %s, %s)" (s target) (s revdep)
    (Util.repr (Kind.to_string kind))

let matrix_groups jobs match_ =
  Util.group_by
    (fun j -> (j.target, j.revdep, j.kind))
    (List.filter
       (fun j ->
         List.mem j.kind [ Kind.Build; Tests; Revdeps ]
         && (match j.target with Some t -> t <> "" | None -> false)
         &&
         match match_ with
         | None | Some "" -> true
         | Some m -> Rx.test (Rx.v m) j.variant)
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
          else if j.category = Category.passed then Some "pass"
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
    List.filter
      (fun j -> (not j.experimental) && j.category = Category.passed)
      js
  in
  if fails = [] || passes = [] then None
  else begin
    let fd = List.map job_dims fails and pd = List.map job_dims passes in
    (* Not when a Windows job passes (#29582: ocaml-compiler.5.3 fails on
       msys2, without diff, and passes on cygwin): see the distributions. *)
    if
      List.for_all (fun d -> d.os = "win32") fd
      && List.exists (fun d -> d.os <> "win32") pd
      && not (List.exists (fun d -> d.os = "win32") pd)
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
    ("m4", "m4");
    ("patch", "patch");
    ("perl", "perl");
    ("python3", "python3");
    ("bison", "bison");
    ("flex", "flex");
  ]

let keys_of js = List.map failure_key js
let variants js = List.map (fun j -> j.variant) js
let program_re = Rx.v {|(?:Program "?|")([\w.+-]+)|}

type run = { day : int option; base : string option }

(* A passing job of the main builder vouches for a day10 failure only if it
   ran on the same master commit, or within [twin_max_days] of it: master
   gains dozens of releases a week, and the builders' images change. One push
   starts its jobs within the hour, and a rebuild of failed jobs follows
   within a day or two. *)
let twin_max_days = 3

(* How much a passing main-builder job [t] vouches for the day10 failure [j]:
   [Some "high"] on the same master commit or the same push (a day apart at
   most: the builders merge the PR onto master when the job starts, so the
   commits of one push differ), [Some "medium"] within [twin_max_days], else
   [None] (an older run, or a run the logs do not date). Without [run], as
   before: the job list alone. *)
let twin_confidence run j t =
  match run with
  | None -> Some "high"
  | Some run -> (
      let a = run j and b = run t in
      match (a.base, b.base, a.day, b.day) with
      | Some x, Some y, _, _ when x = y -> Some "high"
      | _, _, Some x, Some y when abs (x - y) <= 1 -> Some "high"
      | _, _, Some x, Some y when abs (x - y) <= twin_max_days -> Some "medium"
      | _ -> None)

(* The builder (day10 or the main one) and distribution of a job: one image. *)
let builder_distro j = (starts_day10 j, (job_dims j).distro)

(* One run per builder and distribution: the jobs of a push run together. *)
let per_builder run =
  let memo = Hashtbl.create 8 in
  fun j ->
    let k = builder_distro j in
    match Hashtbl.find_opt memo k with
    | Some r -> r
    | None ->
        let r = run j in
        Hashtbl.replace memo k r;
        r

(* The main builder's pass, among [passes], on the distribution, architecture
   and OCaml version of the day10 failure [j]. Not when day10 passes on another
   architecture of the same distribution and OCaml version: the failure then
   depends on the architecture. *)
let classic_twin passes j =
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
  if List.exists (fun p -> starts_day10 p && job_arch p <> job_arch j) same then
    None
  else
    List.find_opt
      (fun p -> (not (starts_day10 p)) && job_arch p = job_arch j)
      same

let real_passes js =
  List.filter (fun j -> (not j.experimental) && j.category = Category.passed) js

let real_fails js =
  List.filter
    (fun j -> (not j.experimental) && List.mem j.category failing_cats)
    js

let stale_twins ~run ~among jobs match_ =
  List.concat_map
    (fun (_, js) ->
      let passes = real_passes js in
      List.filter
        (fun j ->
          starts_day10 j && List.memq j among
          &&
          match classic_twin passes j with
          | Some t -> twin_confidence (Some run) j t = None
          | None -> false)
        (real_fails js))
    (matrix_groups jobs match_)

(* A job the CI failed (a broken image, a crashed worker: see
   Rules.ci_failure) neither passes nor fails: it is left out of the
   patterns. But the platform it ran on is not known to pass either: a
   pattern "fails only on [vals]" holds only if each such job of the group,
   outside [vals], has a real pass on its platform (distribution and
   architecture). #28291: archive fails on cygwin and msys2, passes on the
   other distributions but fedora-45, whose image could not start a
   container: nothing says it does not fail there, and the log of the
   Windows jobs (archive.h missing) says more than the pattern. *)
let unknown_covered (dim, vals) passes unknown =
  let plat j =
    let d = job_dims j in
    (d.os, d.distro, job_arch j)
  in
  List.for_all
    (fun u ->
      let d = job_dims u in
      u.experimental
      || (match dim with
        | "os" -> d.os = "win32"
        | "arch" -> List.mem d.arch vals
        | _ -> (
            match d.distro with Some x -> List.mem x vals | None -> false))
      || List.exists (fun p -> plat p = plat u) passes)
    unknown

(* The compiler options of the CI's extras variants ([extras,ocaml-5.5-flambda]),
   the opam packages that select them (ocaml-option-NAME), and why a package
   fails with one: [`Unsupported] what it does not support, declared with a
   conflict on the option's package (owl 1.3 for no-flat-float-array,
   #31274); [`Upstream] what changes code that works otherwise (optimisation,
   instrumentation), for upstream to fix. *)
let compiler_options =
  let no_flat =
    `Unsupported
      "Without flat float arrays, code that reads a float array as an array of \
       doubles (C stubs, Obj.magic) breaks."
  and nnp =
    `Unsupported
      "Without naked pointers, code that stores pointers outside the OCaml \
       heap in OCaml values breaks."
  and flambda =
    `Upstream
      "Only flambda fails: its optimisations change what code relying on \
       unspecified behaviour sees (the physical equality `==` of immutable \
       values, their sharing, the evaluation order), or make deep recursion \
       use more stack."
  and slower what =
    `Upstream
      (Printf.sprintf
         "It fails only with %s: the same code runs slower there, and tests \
          that depend on time or speed fail."
         what)
  in
  [
    ("no-flat-float-array", ([ "ocaml-option-no-flat-float-array" ], no_flat));
    ("flambda", ([ "ocaml-option-flambda" ], flambda));
    ("flambda-fp", ([ "ocaml-option-flambda"; "ocaml-option-fp" ], flambda));
    ("fp", ([ "ocaml-option-fp" ], slower "frame pointers (fp)"));
    ("afl", ([ "ocaml-option-afl" ], slower "afl's instrumentation"));
    ("nnp", ([ "ocaml-option-nnp" ], nnp));
    ("nnpchecker", ([ "ocaml-option-nnpchecker" ], nnp));
  ]

let option_variant_re = Rx.v {|^ocaml-(\d+\.\d+)-([a-z-]+)$|}

(* The OCaml version and option of a job on a compiler-option variant. *)
let compiler_option j =
  match j.platform with
  | [ "extras"; e ] -> (
      match Rx.match_ option_variant_re e with
      | Some m when List.mem_assoc (Rx.group m 2) compiler_options ->
          Some (Rx.group m 1, Rx.group m 2)
      | _ -> None)
  | _ -> None

let matrix_rules ?(failing_in_log = []) ?(unknown = []) ?(unsure = []) ?run jobs
    (earlier : Suggestion.t list) match_ pr_packages =
  let fin j = List.assoc_opt (failure_key j) failing_in_log in
  let unknown = matrix_groups unknown match_ in
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
          when Suggestion.is_set r.Suggestion.new_
               || Suggestion.list r.candidates <> []
               || r.advice <> None ->
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
    (fun ((target', revdep, kind), js) ->
      let target = Option.value ~default:"" target' in
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
                   if j.category = Dependency then failing_dep j.reason
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
          kind = Kind.Tests
          || List.for_all (fun j -> j.category = Category.Build_or_test) fails
        in
        let where = Where.pr_or_other owner (List.mem owner pr_packages) in
        let label =
          Printf.sprintf "%s [%s%s]" pkg (Kind.to_string kind)
            (match revdep with
            | Some r when r <> "" -> ", revdep of " ^ target
            | _ -> "")
        in
        (* A day10 failure whose distribution, architecture and OCaml version
           pass on the classic builder is a problem of the day10 environment,
           not of the package: report it as such, and leave it out of the
           analyses below. *)
        let passes = real_passes js in
        let twins =
          List.filter_map
            (fun j ->
              if starts_day10 j then
                Option.bind (classic_twin passes j) (fun t ->
                    Option.map (fun c -> (j, c)) (twin_confidence run j t))
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
                    && r.rule = Some Missing_tool
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
              (* Not after a depext to add (conf-bison's {os-family =
                 "centos"}, which opam never matches: the main builder's
                 image has bison anyway). *)
              let e = Option.value ~default:"" evidence in
              if
                Util.contains e "Nothing to change" || Util.contains e "Add one"
              then ""
              else " Nothing to change in the package metadata."
            in
            out :=
              Suggestion.make
                ([
                   `Variant label;
                   `Failing (Some (Option.value ~default:pkg culprit));
                 ]
                @ Where.fix_where Ci "the CI environment (day10 builder)"
                @ [
                    `Rule Rule.Builder_specific;
                    `Error
                      (Printf.sprintf
                         "Fails only on the day10 builder on %s; the main \
                          builder passes there."
                         (String.concat ", " dists));
                    `Confidence
                      (if List.for_all (fun (_, c) -> c = "high") twins then
                         "high"
                       else "medium");
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
        (* The failures [unsure] of: unread, on a builder and distribution
           whose image the CI broke (#29582: ocaml-option-llvm.1's on day10's
           fedora-45, beyond --max, made a pattern of cygwin, fedora-45 and
           msys2). Not a failure of the package, nor a pass: unknown, as the
           CI's failures. After the day10 twins: a pass of the main builder
           still says the package builds there. *)
        let doubtful = List.filter (fun j -> List.memq j unsure) fails in
        let js, fails, group_unknown =
          let keep j = not (List.memq j doubtful) in
          ( List.filter keep js,
            List.filter keep fails,
            Option.value ~default:[]
              (List.assoc_opt (target', revdep, kind) unknown)
            @ doubtful )
        in
        (* A failure on a compiler-option variant (extras,ocaml-5.5-flambda)
           when the same job passes on the plain compiler of that version
           (compilers,5.5: the same builder and image): the option breaks
           it. Not when a rule explained the failure from its log (flambda's
           stack overflow, a timeout: the job of the log may be another of
           its failure's), and the failure is the option's only when all
           its failed jobs are on such variants (#31312: nx's tests are
           killed by SIGSEGV on no-flat-float-array, and fail differently on
           ppc64, riscv64 and FreeBSD under the same CI reason). *)
        let explained_in_log j =
          List.exists
            (fun r -> r.Suggestion.rule <> None && r.key = Some (failure_key j))
            earlier
        in
        let option_fails =
          List.filter_map
            (fun j ->
              match compiler_option j with
              | Some (v, opt)
                when (not (explained_in_log j))
                     && List.exists
                          (fun p -> p.platform = [ "compilers"; v ])
                          (real_passes js) ->
                  Some (opt, (v, j))
              | _ -> None)
            fails
        in
        List.iter
          (fun (opt, vjs) ->
            let ojs = List.map (fun (_, (_, j)) -> j) vjs
            and v = fst (snd (List.hd vjs)) in
            let keys =
              List.filter
                (fun k ->
                  List.for_all
                    (fun j ->
                      failure_key j <> k
                      || List.exists (fun (_, (_, o)) -> o == j) option_fails)
                    fails)
                (Util.dedup (keys_of ojs))
            in
            let packages, why = List.assoc opt compiler_options in
            let conflicts =
              Printf.sprintf "conflicts: [%s]"
                (String.concat " "
                   (List.map (fun p -> "\"" ^ p ^ "\"") packages))
              ^ match culprit with Some c -> "   (in " ^ c ^ ")" | None -> ""
            in
            let r =
              Suggestion.make
                ([ `Variant label; `Failing (Some owner) ]
                @ (match why with
                  | `Upstream _ ->
                      Where.fix_where Upstream "upstream (the package's source)"
                  | `Unsupported _ -> where)
                @ [
                    `Rule Rule.Compiler_option;
                    `Error
                      (Printf.sprintf
                         "%s only with OCaml %s built with %s (%s), %s on the \
                          plain OCaml %s%s"
                         (if tests_only then "Tests fail" else "Fails")
                         v opt
                         (String.concat ", " packages)
                         (if tests_only then "pass" else "passes")
                         v
                         (match culprit with
                         | Some c -> "; the failure is in the dependency " ^ c
                         | None -> ""));
                    `Confidence "medium";
                    `Keys keys;
                    `Jobs (variants ojs);
                  ]
                @
                match why with
                | `Upstream w ->
                    [
                      `Alternative
                        (conflicts
                       ^ "   (only hides it: prefer the upstream fix)");
                      `Note
                        (w
                       ^ " The fix is upstream; the conflict with the option's \
                          package in the alternative only hides it.");
                    ]
                | `Unsupported w ->
                    [
                      `Old None;
                      `New (Some conflicts);
                      `Note
                        (w
                        ^ Printf.sprintf
                            " If the package cannot support OCaml built with \
                             %s, declare the conflict with the option's \
                             package, as owl 1.3 does for no-flat-float-array; \
                             otherwise fix it upstream."
                            opt);
                    ])
            in
            out := r :: !out)
          (Util.group_by fst option_fails);
        let js, fails =
          let gone j = List.exists (fun (_, (_, o)) -> o == j) option_fails in
          ( List.filter (fun j -> not (gone j)) js,
            List.filter (fun j -> not (gone j)) fails )
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
                      `Rule Rule.Compiler_matrix;
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
              | Some p when not (unknown_covered p passes group_unknown) -> ()
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
                              && r.rule = Some Missing_tool
                            then
                              tool :=
                                Rx.search program_re (Suggestion.str r.error))
                          earlier;
                        let tool = Option.map (fun m -> Rx.group m 1) !tool in
                        (* A cygwin depext only for a failure on Windows only
                           (not cygwin, fedora-45 and msys2). *)
                        let windows_only =
                          dim = "os"
                          || dim = "distro"
                             && List.for_all
                                  (fun v ->
                                    String.starts_with ~prefix:"cygwin" v
                                    || String.starts_with ~prefix:"msys2" v)
                                  vals
                        in
                        let dep =
                          match tool with
                          | Some t when windows_only ->
                              List.assoc_opt t cygwin_pkg
                          | _ -> None
                        in
                        let excluded =
                          if dim = "os" then "Windows"
                          else String.concat "/" vals
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
                                (match tool with
                                | Some "pkg-config" when windows_only ->
                                    (* #31054: conf-pkg-config.5 is installed
                                       on cygwin, with its depext, and
                                       conf-glib-2.1 still finds no
                                       `pkg-config`. *)
                                    Printf.sprintf
                                      "On Windows, conf-pkg-config provides \
                                       `pkgconf`, and no `pkg-config` on \
                                       cygwin (a depext does not change that): \
                                       %s should run `pkgconf` on Windows, as \
                                       conf-zstd does, or exclude %s \
                                       (`x-ci-accept-failures` or \
                                       `available`)."
                                      culprit excluded
                                | _ ->
                                    Printf.sprintf
                                      "Add the missing depext%s to %s, or \
                                       exclude %s there \
                                       (`x-ci-accept-failures` or \
                                       `available`)."
                                      (if dim <> "arch" then
                                         " for " ^ String.concat "/" vals
                                       else "")
                                      culprit excluded);
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
                          `Rule Rule.Platform_matrix;
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

let dependency_re = Rx.v {|the dependency \S+|}
let in_re = Rx.v {|\(in (\S+)\)|}

let collapse_systemic ?(threshold = 5) (out : Suggestion.t list) =
  let sig_ r =
    let rule = r.rule in
    let pattern =
      if rule = Some Rule.Platform_matrix then
        List.hd (Util.split_on ~maxsplit:1 ": " (Suggestion.str r.error))
      else Suggestion.str r.error
    in
    ( rule,
      Rx.sub dependency_re "a dependency" pattern,
      if rule = Some Rule.Compiler_matrix then
        List.hd (Util.split_on "   " (Suggestion.str_field r.Suggestion.new_))
      else "" )
  in
  List.concat_map
    (fun ((rule, pattern, _), rs) ->
      (* Not compiler-option: the option's package and where the fix goes
         (upstream or the package) are each package's. *)
      if List.length rs < threshold || rule = Some Rule.Compiler_option then rs
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
          if rule = Some Rule.Builder_specific then
            ( List.hd (Util.split_on ~maxsplit:1 ";" pattern) ^ ".",
              r0.Suggestion.note )
          else begin
            let error = Util.rstrip ~chars:"." pattern ^ "." in
            let code, _, hint =
              Util.partition (Suggestion.str_field r0.Suggestion.new_) "   "
            in
            let m = Rx.fullmatch in_re (String.trim hint) in
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
                 (Printf.sprintf "%d packages [%s]" (List.length rs)
                    (Option.fold ~none:"" ~some:Rule.to_string rule));
               `Failing
                 (Some
                    (String.concat ", " (Util.take 6 names)
                    ^ if List.length names > 6 then " ..." else ""));
             ]
            @ (if rule = Some Rule.Builder_specific then
                 Where.fix_where Ci (Suggestion.str r0.fix_in)
               else
                 Where.fix_where Unknown
                   "the PR, a shared dependency, or the CI")
            @ [
                `Rule
                  (Rule.systemic
                     (Option.value ~default:(Rule.of_string "") rule));
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
        | [ (d, n) ]
          when rule <> Some Rule.Builder_specific && n = List.length rs ->
            (* Every package fails through the same dependency: the fix goes there (shown as such). *)
            entry.failing_dep <- Some d;
            entry.where_kind <- Some Other_package;
            entry.where_pkg <- Set d
        | _ -> ());
        [ entry ]
      end)
    (Util.group_by sig_ out)
