open Suggestion.T
open Ci
open Loginfo
module S = Sources

let installed_of (info : info) dep = List.assoc_opt dep info.installed
let is_installed (info : info) dep = List.mem_assoc dep info.installed

(* The forms of an unbound identifier in a compiler error. *)
(* The messages of unbound identifiers: the text before the name, its kind,
   and whether "type " may follow (Unbound class, Unbound module: then it is
   a class or module type, matched by its own line). *)
let unbound_res =
  List.map
    (fun (prefix, kind, not_type) ->
      (prefix, Rx.v (prefix ^ {|[`"']?([\w.']+)|}), kind, not_type))
    [
      ("Unbound value ", "value", false);
      ("Unbound constructor ", "constructor", false);
      ("Unbound record field ", "field", false);
      ("Unbound type constructor ", "type", false);
      ("Unbound module type ", "module-type", false);
      ("Unbound class type ", "class-type", false);
      ("Unbound class ", "class", true);
      ("Unbound module ", "module", true);
    ]

(* The standard library's sources, read for the releases ocaml.org's API history cannot tell. *)
let stdlib_sources = "OCaml's stdlib .mli files"

(* Dependencies not searched for the module of an unbound identifier (as in
   the guess from the dependencies), besides the conf-* packages: no OCaml
   module. Findlib libraries installed by an opam package of another name:
   [bytes] comes with OCaml 4 and is installed by base-bytes on OCaml 5
   (liquidsoap 2.4.6, #30906). *)
let findlib_packages = [ ("bytes", "base-bytes") ]
let not_searched = [ "ocaml"; "dune"; "ocamlfind" ]

(* How recent a dependency's release must be, before the job, to be
   suspected of removing a module. *)
let recent_release_days = 30
let log_date_re = Rx.v ~flags:[ Rx.M ] {|^(\d{4}-\d\d-\d\d) \d\d:\d\d|}

(** The date of a job (days since the epoch), from the first timestamp of its
    log (today if none). *)
let job_date text =
  let today () = Option.get (Date.days_of_date (Date.today ())) in
  match Rx.search log_date_re text with
  | Some m -> (
      match Date.days_of_date (Rx.group m 1) with
      | Some d -> d
      | None -> today ())
  | None -> today ()

(** [(package, previous version, installed version, release date)] of the
    dependencies among [deps] installed at a version released in the
    [recent_release_days] before [when_], whose previous version has [top] as a
    top-level module and whose installed version does not (from their ocaml.org
    docs). Top-level modules only: a wrapper's page may mention a submodule of
    that name (matrix 0.2.0's Matrix re-exports a Grid, not the Grid module of
    matrix 0.1.0). *)
let removed_module src top deps (info : info) when_ =
  List.filter_map
    (fun d ->
      match installed_of info d with
      | None | Some "" -> None
      | Some v -> (
          let pub = S.published src d v in
          let age =
            match pub with
            | Some p -> Option.map (fun x -> when_ - x) (Date.days_of_date p)
            | None -> None
          in
          match age with
          | Some age when 0 <= age && age <= recent_release_days -> (
              let older =
                List.filter
                  (fun x -> opam_vcmp x v < 0)
                  (S.versions_or_empty src d)
              in
              match Util.last_opt older with
              | None -> None
              | Some prev -> (
                  let before = S.doc_modules src d prev
                  and now = S.doc_modules src d v in
                  match (before, now) with
                  | Some b, Some n
                    when List.mem_assoc top b && not (List.mem_assoc top n) ->
                      Some (d, prev, v, Option.get pub)
                  | _ -> None))
          | _ -> None))
    deps

let module_find_note (found : S.module_find option) top (info : info) opam_text
    fname =
  match found with
  | None -> ""
  | Some f when f.fpartial && f.matches = [] -> ""
  | Some f ->
      if f.matches <> [] then begin
        let deps =
          dep_names opam_text "depends" @ dep_names opam_text "depopts"
        in
        let descs =
          List.map
            (fun (m : S.find_match) ->
              let p = m.package in
              let what =
                (if m.module_path <> [ top ] then
                   [ "as " ^ String.concat "." m.module_path ]
                 else [])
                @ (match m.library with
                  | Some l when l <> "" -> [ "library " ^ l ]
                  | _ -> [])
                @
                if p = fname then [ "the failing package" ]
                else if not (is_installed info p) then
                  [
                    (if List.mem p deps then "not installed in this job"
                     else "not a dependency of " ^ fname);
                  ]
                else []
              in
              (match m.mversion with
                | Some v when v <> "" -> p ^ " " ^ v
                | _ -> p)
              ^ if what <> [] then " (" ^ String.concat ", " what ^ ")" else "")
            (Util.take 3 f.matches)
        in
        let more = List.length f.matches - List.length descs in
        Printf.sprintf "ocaml.org's docs have module %s in %s%s." top
          (String.concat "; " descs)
          (if more > 0 then Printf.sprintf "; and %d more" more else "")
      end
      else if f.distribution then
        Printf.sprintf
          "ocaml.org: %s comes with the OCaml compiler, not with an opam \
           package."
          top
      else
        let no_docs = List.filter (is_installed info) f.no_docs in
        Printf.sprintf
          "ocaml.org's docs have no module %s in the latest documented version \
           of the dependencies%s."
          top
          (if no_docs <> [] then
             Printf.sprintf " (without docs: %s%s)"
               (String.concat ", " (Util.take 5 no_docs))
               (if List.length no_docs > 5 then ", ..." else "")
           else "")

let there_field_within_re =
  Rx.v {|There is no field (\w+) within type [`"']?([\w.]+)|}

let find_unbound lines =
  let text = String.concat "\n" lines in
  let found =
    List.find_map
      (fun (prefix, rx, kind, not_type) ->
        let ok m =
          not
            (not_type
            && String.starts_with ~prefix:"type "
                 (Util.slice ~start:(Rx.start m + String.length prefix) text))
        in
        match List.find_opt ok (Rx.finditer rx text) with
        | Some m ->
            let parts =
              String.split_on_char '.'
                (Util.strip ~chars:"`\"'." (Rx.group m 1))
            in
            let n = List.length parts in
            Some (kind, Util.take (n - 1) parts, List.nth parts (n - 1))
        | None -> None)
      unbound_res
  in
  match found with
  | Some r -> Some r
  | None -> (
      match Rx.search there_field_within_re text with
      | Some m ->
          let parts = String.split_on_char '.' (Rx.group m 2) in
          Some ("field", Util.take (List.length parts - 1) parts, Rx.group m 1)
      | None -> None)

let toplevel_line =
  Rx.v
    {|^[-+]\s*(val [\w'.]+ :|- : |type |module |exception |Line \d+, characters |Characters \d+-\d+|Error: |Warning \d+)|}

(** Changed lines of a test diff, if most of them are OCaml toplevel output (mdx
    [# expr;;] answers, types, error locations). *)
let toplevel_diff err =
  let changed l =
    (* Python: ^[-+](?![-+]{2}) *)
    String.length l >= 1
    && (l.[0] = '-' || l.[0] = '+')
    && not
         (String.length l >= 3
         && (l.[1] = '-' || l.[1] = '+')
         && (l.[2] = '-' || l.[2] = '+'))
  in
  let lines = List.filter changed err in
  let hits = List.filter (fun l -> Rx.test_match toplevel_line l) lines in
  if lines <> [] && List.length hits * 2 >= List.length lines then Some hits
  else None

(* ------------------------------------------------------------------------ *)
(* Known errors *)

(* C/C++ errors that newer compilers introduced: code that built with an
   older GCC fails on recent distributions (debian-13, fedora, archlinux, ...). *)
let newer_cc =
  List.map
    (fun (pattern, cause, fix) -> (Rx.v pattern, cause, fix))
    [
      ( {|error: .*\[-W(implicit-int|implicit-function-declaration|incompatible-pointer-types|int-conversion|return-mismatch|declaration-missing-parameter-type)\]|},
        "GCC 14 turned -W{0} into an error by default: the C code relies on \
         pre-C99 rules",
        "Fix the C code upstream. For an old release, add a patch or \
         `x-maintained: false`; adding -fpermissive or -Wno-error={0} to the C \
         flags also works." );
      ( {|'u?int(?:8|16|32|64)_t' (?:does not name a type|was not declared in this scope|has not been declared)|is defined in header '<cstdint>'|},
        "Since GCC 13, libstdc++ headers no longer include <cstdint> for you",
        "Add `#include <cstdint>` upstream, or a patch in the opam file for an \
         old release." );
      ( {|(?:cannot use keyword|expected identifier or '\(' before) '(?:bool|true|false)'|'bool' cannot be defined via 'typedef'|},
        "GCC 15 compiles C as C23 by default, where bool, true and false are \
         keywords",
        "Fix the C code upstream, or build it with -std=gnu17." );
      ( {|error: .*\[-Werror=([\w-]+?)=?\]|},
        "The package builds with -Werror, and a newer compiler emits a new \
         warning (-W{0})",
        "Drop -Werror from release builds upstream. For an old release, add a \
         patch or `x-maintained: false`." );
    ]

let trailing_number_re = Rx.v {|\d+$|}

(** Version of the system package providing [lib], from the depext installation
    lines of a log (apt, dnf/yum, apk, pacman, zypper). *)
let system_package_version text lib =
  (* each pattern is (flags, before, after): the package's name goes between *)
  let pats =
    [
      ([], {|Setting up (?:lib)?|}, {|[\w.+-]*?(?:-dev)?:\w+ \(([^)]+)\)|});
      (* apt *)
      ([ Rx.M ], {|^[-\s]*(?:lib)?|}, {|[\w+-]*?-devel\s+\w+\s+(\d\S*)\s|});
      (* dnf table *)
      ([], {|Installing\s*:\s*(?:lib)?|}, {|[\w+-]*?-devel-(\d[^\s]*)|});
      ([], {|Installing (?:lib)?|}, {|[\w+-]*? \(([^)]+)\)|});
      (* apk *)
      ([], {|installing (?:lib)?|}, {| \(([^)]+)\)|}) (* pacman *);
    ]
  in
  List.find_map
    (fun name ->
      List.find_map
        (fun (flags, before, after) ->
          let pat =
            Rx.of_re
              (Re.no_case
                 (Re.seq
                    [ Rx.re ~flags before; Re.str name; Rx.re ~flags after ]))
          in
          Option.map (fun m -> Rx.group m 1) (Rx.search pat text))
        pats)
    (Util.dedup [ lib; Rx.sub trailing_number_re "" lib ])

type ctx = {
  pr_packages : string list;
  full_text : string;
  job_variant : string;
}

let such_function_unixepoch_re = Rx.v "no such function: unixepoch"
let lib_meta_such_re = Rx.v {|/lib/([\w+-]+)/META: No such file or directory|}

let undefined_reference_re =
  Rx.v {|undefined reference to (?:[`']|‘)?([\w.$@]+)|}

let collect2_ld_returned_re =
  Rx.v {|collect2|ld returned|linker command failed|/ld:|}

let unsupported_option_target_re =
  Rx.v {|unsupported option '(-m[\w=-]+)' for target '(arm64|aarch64)[^']*'|}

let execve_such_file_re =
  Rx.v {|execve\((\./[\w./-]+)\): No such file or directory|}

let exception_stack_overflow_re = Rx.v "exception Stack overflow"
let could_find_python3_re = Rx.v {|Could NOT find Python3?\b[^\n]*|}
let fatal_git_repository_re = Rx.v "fatal: not a git repository"
let git_describe_rev_re = Rx.v "git (describe|rev-parse|log)"
let error_program_library_re = Rx.v "Error: (Program|Library) "

let cannot_create_regular_re =
  Rx.v
    {|cannot create (?:regular file|directory) '?(/usr/[^':\s]*)'?: Read-only file system|}

let known_cycle_counting_re = Rx.v "No known cycle-counting instruction"
let core_kernel_shadows_re = Rx.v {|Core_kernel shadows Unix|Core_kernel\.Unix|}
let unbound_constructor_value_re = Rx.v "Unbound (constructor|value)"
let unbound_value_conv_re = Rx.v {|Unbound value Conv\.(\w+)|}
let checking_ar_re = Rx.v {|checking for ar\.\.\. no|}

let notforredeclaration_member_clang_re =
  Rx.v
    {|'NotForRedeclaration' is not a member of 'clang::Sema'|has no member named 'getOriginalNamespace'|}

let libclang_dev_re = Rx.v {|libclang-(\d+)-dev|}

let expected_type_string_re =
  Rx.v
    {|the expected type is\s+(?:# )?string option \* Parsetree\.(expression|pattern|core_type)|}

(** Failures with a recognisable message and a known cause. *)
let known_errors text full fname opam_text (info : info) (ctx : ctx)
    (dims : dims) platform deps =
  let installed = info.installed in
  Util.first_some
    [
      (fun () ->
        if Rx.test such_function_unixepoch_re text then
          let have = system_package_version full "sqlite3" in
          Some
            [
              `Rule Rule.System_library_version;
              `Keep;
              `Confidence "high";
              `Plat platform;
              `Error
                (Printf.sprintf
                   "sqlite3 >= 3.38 required (unixepoch); %s has %s" platform
                   (Option.value ~default:"an older version" have));
              `Note
                (Printf.sprintf
                   "The SQL uses `unixepoch()`, added in SQLite 3.38, and the \
                    system SQLite is older on %s. Exclude these distributions \
                    (`x-ci-accept-failures`, or `available` on os-distribution \
                    and os-version), or avoid `unixepoch()` upstream."
                   platform);
            ]
        else None);
      (fun () ->
        match Rx.search lib_meta_such_re text with
        | Some m when Rx.group m 1 <> fname ->
            let pkg = Rx.group m 1 in
            Some
              [
                `Rule Rule.Broken_install;
                `Confidence "medium";
                `Keep;
                `Error (Rx.group0 m);
                `Note
                  (Printf.sprintf
                     "%s's directory in the switch has no META file: its \
                      installation was incomplete when %s was built (being \
                      removed or reinstalled by opam at the same time). This \
                      is not a missing dependency of %s (that gives `Library \
                      \"%s\" not found`): look at what opam did with %s in \
                      this job, or rerun it."
                     pkg fname fname pkg pkg);
              ]
        | _ -> None);
      (fun () ->
        (* Python: undefined reference to [`'‘]?([\w.$@]+) *)
        match Rx.search undefined_reference_re text with
        | Some m when Rx.test collect2_ld_returned_re text ->
            let sym = Rx.group m 1 in
            let hint =
              if sym = "core_unix_gettid" then
                "core's linux_ext needs `core_unix_gettid`, which jst-config \
                 only enables with some glibc versions: an old core/jst-config \
                 combination"
              else if String.starts_with ~prefix:"LAPACKE_" sym then
                "the LAPACKE functions are missing from the system LAPACK: a \
                 depext providing LAPACKE (or a newer one) is needed"
              else
                Printf.sprintf
                  "the library that provides `%s` is missing, too old, or not \
                   linked"
                  sym
            in
            Some
              [
                `Rule Rule.Link_error;
                `Error (Printf.sprintf "undefined reference to `%s`" sym);
                `Confidence "medium";
                `Keep;
                `Note (Printf.sprintf "Link failure on %s: %s." platform hint);
              ]
        | _ -> None);
      (fun () ->
        match Rx.search unsupported_option_target_re text with
        | Some m ->
            Some
              [
                `Rule Rule.Platform_unsupported;
                `Confidence "high";
                `Keep;
                `Error
                  (Printf.sprintf
                     "The build passes the x86-only flag %s to the %s compiler"
                     (Rx.group m 1) (Rx.group m 2));
                `Note
                  "The C flags are x86-specific: upstream should only pass \
                   them on x86 (dune's %{architecture}, or a configurator \
                   check). For this release, exclude arm64 with `available: \
                   arch != \"arm64\"` if every arm64 build fails, or \
                   x-ci-accept-failures for the platforms concerned.";
              ]
        | None -> None);
      (fun () ->
        match Rx.search execve_such_file_re text with
        | Some m when dims.os = "freebsd" ->
            let p = Rx.group m 1 in
            Some
              [
                `Rule Rule.Platform_unsupported;
                `Confidence "medium";
                `Keep;
                `Error (p ^ " cannot be run on FreeBSD");
                `Note
                  (Printf.sprintf
                     "%s exists, so its #! interpreter does not: usually \
                      `#!/bin/bash`, which FreeBSD does not have. Upstream: \
                      use `#!/usr/bin/env bash` (or sh), or run it as `bash \
                      %s` in the build; conf-bash provides bash."
                     p p);
              ]
        | _ -> None);
      (fun () ->
        if
          Rx.test exception_stack_overflow_re text
          && Util.contains ctx.job_variant "flambda"
        then
          Some
            [
              `Rule Rule.Test_platform;
              `Confidence "medium";
              `Keep;
              `Error "Stack overflow with flambda";
              `Note
                "Only the flambda compiler overflows: its inlining makes deep \
                 recursion use more stack. Upstream can make the recursion \
                 tail-recursive or less deep; otherwise this extras job's \
                 failure is usually accepted.";
            ]
        else None);
      (fun () ->
        match Rx.search could_find_python3_re text with
        | Some m ->
            let r =
              [
                `Rule Rule.Missing_tool;
                `Error (Util.prefix_chars (String.trim (Rx.group0 m)) 200);
                `Confidence "high";
              ]
            in
            Some
              (if List.mem "conf-python-3" deps then
                 r
                 @ [
                     `Note
                       (Printf.sprintf
                          "conf-python-3 is a dependency, but CMake cannot \
                           find Python on %s."
                          platform);
                   ]
               else
                 r
                 @ [
                     `Dep "conf-python-3";
                     `Old None;
                     `New
                       (Some {|"conf-python-3" {build}   (new line in depends)|});
                     `Note "The CMake build needs a Python 3 interpreter.";
                   ])
        | None -> None);
      (fun () ->
        if
          Rx.test fatal_git_repository_re text
          && Rx.test git_describe_rev_re text
          && not (Rx.test error_program_library_re text)
          (* another cause, with git noise *)
        then
          Some
            [
              `Rule Rule.Needs_git_checkout;
              `Confidence "high";
              `Error
                "The build runs git, but the release archive is not a git \
                 checkout";
              `Note
                "The build calls git (`git describe` or similar) to find the \
                 version. Release archives have no .git directory, so this \
                 fails everywhere. Fix upstream: take the version from the \
                 opam file (`%{version}%`) or from dune's `%%VERSION%%` \
                 substitution.";
            ]
        else None);
      (fun () ->
        match Rx.search cannot_create_regular_re text with
        | Some m ->
            Some
              [
                `Rule Rule.Install_outside_prefix;
                `Confidence "high";
                `Error
                  (Printf.sprintf
                     "The install step writes to %s, outside the opam switch"
                     (Rx.group m 1));
                `Note
                  "The install command ignores the switch's prefix. Pass the \
                   switch's directories to it in the opam file, for example \
                   `PREFIX=%{prefix}%`, `BINDIR=%{bin}%` or `LIBDIR=%{lib}%`, \
                   whichever variables the Makefile uses.";
              ]
        | None -> None);
      (fun () ->
        if Rx.test known_cycle_counting_re text then
          let arch = dims.arch in
          let fname =
            match info.failing with Some (n, v) -> n ^ "." ^ v | None -> fname
          in
          Some
            [
              `Rule Rule.Unsupported_arch;
              `Confidence "high";
              `Error
                (Printf.sprintf "%s has no cycle counter for %s" fname arch);
              `New
                (Some
                   (Printf.sprintf
                      {|available: arch != "%s"   (in %s, combined with its existing `available`)|}
                      arch fname));
              `Old None;
              `Note
                (Printf.sprintf
                   "This version of %s does not support %s; newer versions do."
                   fname arch);
            ]
        else None);
      (fun () ->
        if
          Rx.test core_kernel_shadows_re text
          && Rx.test unbound_constructor_value_re text
        then
          Some
            [
              `Rule Rule.Library_api;
              `Confidence "medium";
              `Keep;
              `Error
                "Core_kernel shadows the Unix module: Unix constructors and \
                 values are not found";
              `Note
                (Printf.sprintf
                   "%s opens Core_kernel, whose Unix module hides the standard \
                    one, then uses Unix. Fix upstream (use `Caml_unix` or \
                    `Stdlib.Unix`); if only an optional part (a depopt such as \
                    lwt) is affected, a `conflicts` entry for it avoids the \
                    build."
                   fname);
            ]
        else None);
      (fun () ->
        match Rx.search unbound_value_conv_re text with
        | Some m
          when match List.assoc_opt "cmdliner" installed with
               | Some v -> opam_vcmp v "2.0.0" >= 0
               | None -> false ->
            let old, new_ =
              S.constraint_edit opam_text "cmdliner" "<" "2.0.0"
            in
            Some
              [
                `Rule Rule.Library_api;
                `Dep "cmdliner";
                `Old old;
                `New (Some new_);
                `Confidence "high";
                `Error (Rx.group0 m);
                `Note
                  (Printf.sprintf
                     "`Arg.Conv.%s` does not exist in cmdliner %s: the \
                      conversion API changed in cmdliner 2.0."
                     (Rx.group m 1)
                     (List.assoc "cmdliner" installed));
              ]
        | _ -> None);
      (fun () ->
        if Rx.test checking_ar_re text && dims.os = "win32" then
          Some
            [
              `Rule Rule.Platform_unsupported;
              `Confidence "medium";
              `Error
                (Printf.sprintf "The configure script of %s finds no `ar` on %s"
                   fname platform);
              `New
                (Some
                   {|available: os != "win32"   (combined with any existing `available`)|});
              `Old None;
              `Note
                (Printf.sprintf
                   "%s's configure script is not ported to Windows." fname);
            ]
        else None);
      (fun () ->
        match Rx.search notforredeclaration_member_clang_re text with
        | Some m ->
            let clang =
              match Rx.search libclang_dev_re full with
              | Some c -> Some (Rx.group c 1)
              | None -> None
            in
            Some
              [
                `Rule Rule.System_library_version;
                `Keep;
                `Confidence "high";
                `Plat platform;
                `Error
                  (Rx.group0 m
                  ^
                  match clang with
                  | Some c -> Printf.sprintf " (Clang %s installed)" c
                  | None -> "");
                `Note
                  (Printf.sprintf
                     "%s uses Clang APIs that Clang 19 removed. A bound on \
                      conf-libclang does not choose the installed Clang \
                      (conf-libclang.N accepts any Clang up to N): it needs a \
                      conf-libclang version whose depexts install Clang 18 or \
                      older on %s, or an update to the newer Clang API."
                     fname platform);
              ]
        | None -> None);
      (fun () ->
        if Rx.test expected_type_string_re text then
          let old, new_ = S.constraint_edit opam_text "ocaml" "<" "5.4" in
          Some
            [
              `Rule Rule.Stdlib_change;
              `Dep "ocaml";
              `Old old;
              `New (Some new_);
              `Confidence "high";
              `Error
                "Pattern on Parsetree tuples: OCaml 5.4's labelled tuples \
                 changed their type";
              `Note
                "OCaml 5.4 added labelled tuples: the Parsetree's tuple nodes \
                 now hold (label option * node) pairs. Code matching them \
                 directly needs OCaml < 5.4, or an update (ppxlib's Ast \
                 versions abstract this).";
            ]
        else None);
    ]

let c_compiler_rule text platform =
  List.find_map
    (fun (rx, cause, fix) ->
      match Rx.search rx text with
      | Some m ->
          let arg = match Rx.groups m with Some g :: _ -> g | _ -> "" in
          let line =
            match
              List.find_opt (fun l -> Rx.test rx l) (Util.splitlines text)
            with
            | Some l -> l
            | None -> Rx.group0 m
          in
          Some
            [
              `Rule Rule.Newer_c_compiler;
              `Error (Util.prefix_chars (String.trim line) 200);
              `Confidence "high";
              `Keep;
              `Plat platform;
              `Note
                (Printf.sprintf "%s (seen on %s). %s"
                   (Util.replace cause "{0}" arg)
                   platform
                   (Util.replace fix "{0}" arg));
            ]
      | None -> None)
    newer_cc

let depexts_re = Rx.v ~flags:[ Rx.M ] {|^depexts:|}
let field_start_re = Rx.v ~flags:[ Rx.M ] {|^[a-z][\w-]*:|}
let quoted_re = Rx.v {|"([^"]+)"|}
let depext_entry_re = Rx.v {|(\[[^\]]*\]|"[^"]*")\s*(?:\{([^}]*)\})?|}

let depext_entries opam_text =
  let t = Option.value ~default:"" opam_text in
  match Rx.search depexts_re t with
  | None -> None
  | Some m ->
      let rest =
        String.trim
          (List.hd
             (Rx.split ~maxsplit:1 field_start_re
                (Util.slice ~start:(Rx.end_ m) t)))
      in
      let rest =
        if String.starts_with ~prefix:"[" rest then
          Util.slice ~start:1 ~stop:(Util.rfind rest "]") rest
        else rest
      in
      Some
        (List.map
           (function
             | [ pkgs; flt ] -> (Rx.findall quoted_re pkgs, flt)
             | _ -> assert false)
           (Rx.findall_groups depext_entry_re rest))

let conf_without_depext fname opam_text (info : info) (ctx : ctx) job platform =
  if
    (not (String.starts_with ~prefix:"conf-" fname))
    || info.action <> Some "compiling"
  then None
  else
    let m =
      Rx.search
        (Rx.of_re
           (Re.seq
              [
                Rx.re {|The compilation of |};
                Re.str fname;
                Rx.re {|\.\S+ failed at "([^"\n]*)"|};
              ]))
        ctx.full_text
    in
    match (m, depext_entries opam_text) with
    | None, _ | _, None -> None
    | Some m, Some entries ->
        let env = Filter.platform_vars job in
        if
          List.exists
            (fun (_, flt) -> Filter.eval_filter (Some flt) env <> Some false)
            entries
        then None (* a depext for this platform (true), or cannot tell (None) *)
        else begin
          (* [advice]: a change described in words (the depext to add); [keep]:
           not replaced by a workaround from the pattern of passes and failures. *)
          let r =
            [
              `Rule Rule.Conf_no_depext;
              `Confidence "medium";
              `Error
                (Printf.sprintf "%s failed at `%s` on %s" fname (Rx.group m 1)
                   platform);
              `Advice ("Add depexts to " ^ fname);
              `Keep;
              `Depexts entries;
              `Check (fname, Rx.group m 1);
            ]
          in
          if entries = [] then
            Some
              (r
              @ [
                  `Note
                    (Printf.sprintf
                       "%s has no depexts at all, so no system package was \
                        installed for its check to find. Add the system \
                        packages that provide it, for each platform it \
                        supports."
                       fname);
                ])
          else
            let distro = (job_dims job).distro in
            Some
              (r
              @ [
                  `Note
                    (Printf.sprintf
                       "%s has no depext for %s, so no system package was \
                        installed for its check to find. Add the %s package \
                        that provides it to the depexts of %s%s"
                       fname platform platform fname
                       (match distro with
                       | Some d when d <> "" ->
                           Printf.sprintf
                             ", or list \"%s\" in its `x-ci-accept-failures` \
                              if %s has no such package."
                             d platform
                       | _ -> "."));
                ])
        end

let conf_depext_platforms (r : Suggestion.t) (jobs : (string, job) Hashtbl.t) =
  let entries = Option.value ~default:[] r.depexts in
  let fname, cmd = Option.value ~default:("", "") r.check in
  r.depexts <- None;
  r.check <- None;
  if entries = [] then
    r.error <- Some (Printf.sprintf "%s failed at `%s`" fname cmd)
    (* on every platform *)
  else begin
    let without = ref [] and other = ref [] and misread = ref [] in
    let vs =
      match r.Suggestion.jobs with
      | Some (_ :: _ as l) -> l
      | _ -> [ Suggestion.str r.Suggestion.variant ]
    in
    List.iter
      (fun v ->
        match Hashtbl.find_opt jobs v with
        | None -> ()
        | Some j ->
            let d = job_dims j in
            let name =
              match d.distro with Some x when x <> "" -> x | _ -> d.os
            in
            let env = Filter.platform_vars j in
            if
              not
                (List.for_all
                   (fun (_, f) -> Filter.eval_filter (Some f) env = Some false)
                   entries)
            then other := name :: !other
            else begin
              without := name :: !without;
              (* An entry meant for this platform, whose filter does not hold
               there: os-family = "ubuntu" (conf-rust-llvm, #27642), while
               opam's os-family is "debian" on Ubuntu. *)
              match List.assoc_opt "os-distribution" env with
              | Some dist
                when dist <> ""
                     && List.exists
                          (fun (_, f) -> Util.contains f ("\"" ^ dist ^ "\""))
                          entries ->
                  misread :=
                    ( dist,
                      Printf.sprintf "os-distribution is \"%s\"%s" dist
                        (match List.assoc_opt "os-family" env with
                        | Some fam when fam <> "" ->
                            Printf.sprintf " and its os-family \"%s\"" fam
                        | _ -> "") )
                    :: List.remove_assoc dist !misread
              | _ -> ()
            end)
      vs;
    if !without <> [] then begin
      let names = List.sort_uniq compare !without in
      let one = List.length names = 1 in
      let jn = String.concat ", " names in
      r.error <- Some (Printf.sprintf "%s failed at `%s` on %s" fname cmd jn);
      r.advice <- Some (Printf.sprintf "Add depexts for %s to %s" jn fname);
      let note =
        ref
          (Printf.sprintf
             "%s has no depext for %s, so no system package was installed \
              there for its check to find. Add the packages that provide it to \
              the depexts of %s, or add %s to its `x-ci-accept-failures` if %s \
              no such package."
             fname jn fname
             (String.concat " " (List.map (fun n -> "\"" ^ n ^ "\"") names))
             (if one then "that platform has" else "these platforms have"))
      in
      List.iter
        (fun (dist, values) ->
          note :=
            !note
            ^ Printf.sprintf
                " An entry names %s, but its filter does not hold there: on \
                 %s, opam's %s."
                dist dist values)
        (List.sort compare !misread);
      if !other <> [] then
        note :=
          !note
          ^ Printf.sprintf
              " On %s a depext applies (or the tool cannot tell): the check \
               fails there for another reason."
              (String.concat ", " (List.sort_uniq compare !other));
      r.note <- Some !note
    end
  end

let implicit_declaration_function_re =
  Rx.v {|implicit declaration of function (?:'|‘)?(caml_\w+)|}

let maximum_version_llvm_re =
  Rx.v {|maximum_version=(\d+)[\s\S]*?llvm_version=(\d[\w.]*)|}

let usable_version_llvm_re =
  Rx.v {|No usable version of LLVM <= ([\d.x]+) found|}

let pkg_config_atleast_re =
  Rx.v
    {|pkg-config (?:--atleast-version[= ](\S+)|--exact-version[= ](\S+)|--max-version[= ](\S+)) ([\w.+-]+)\)?\s*\n(?:.*\n){0,2}?.*(?:exited with code|exit status) 1|}

let requested_but_version_re =
  Rx.v
    {|Requested '([\w.+-]+) (>=|<=|=) ([\w.~+-]+)' but version of .* is ([\w.~+-]+)|}

let version_dune_language_re =
  Rx.v {|Version (\d+\.\d+) of the dune language is not supported|}

let distro_version_re = Rx.v {|[-\d]|}
let filtered_list_re = Rx.v {|\[([^\]]*)\]\s*\{([^}]*)\}|}

let package_was_found_re =
  Rx.v {|Package (\S+) was not found in the pkg-config search path|}

let configure_error_pc_re = Rx.v {|configure: error: ([\w.+-]+)\.pc not found|}
let library_found_re = Rx.v {|Library "([\w.-]+)" not found|}
let package_found_re = Rx.v {|Package `([\w.-]+)' not found|}
let tests_re = Rx.v {|(^|/)tests?/|}

let fatal_error_such_re =
  Rx.v {|fatal error: ([\w/.+-]+): No such file or directory|}

(* The messages of a missing tool, its name in group 1. *)
let missing_tool_messages =
  List.map Rx.v
    [
      {|Program "?([\w.+-]+)"? not found in the tree or in PATH|};
      {|"([\w.+-]+)": command not found|};
      {|([\w.+-]+): command not found|};
      {|Command not found: ([\w.+-]+)|};
    ]
  @ [ Rx.v ~flags:[ Rx.M ] {|^\S*?([\w.+-]+): (?:command )?not found$|} ]

let log_rules src err fname opam_text (info : info) (ctx : ctx) job =
  let text = String.concat "\n" err in
  let deps = direct_deps opam_text in
  let dims = job_dims job in
  let platform =
    match dims.distro with
    | Some d when d <> "" -> d
    | _ -> if dims.os = "linux" then "debian (classic builder)" else dims.os
  in
  Util.first_some
    [
      (fun () ->
        (* Python: implicit declaration of function ['‘]?(caml_\w+) *)
        match Rx.search implicit_declaration_function_re text with
        | Some m ->
            let ocaml = Option.value ~default:"this OCaml" info.ocaml in
            let f = Rx.group m 1 in
            Some
              [
                `Rule Rule.Ocaml_runtime_api;
                `Confidence "medium";
                `Keep;
                `Error
                  (Printf.sprintf
                     "The C stubs call %s, which OCaml %s's runtime does not \
                      declare"
                     f ocaml);
                `Note
                  (Printf.sprintf
                     "%s is not part of this OCaml's C API: it exists only in \
                      other OCaml versions, or in another compiler variant \
                      such as OxCaml. The package needs an `ocaml` constraint \
                      to the versions that have it, or a fix upstream (not a \
                      GCC problem, although GCC 14 reports it as an error)."
                     f);
              ]
        | None -> None);
      (fun () -> c_compiler_rule text platform);
      (fun () ->
        known_errors text
          (if ctx.full_text <> "" then ctx.full_text else text)
          fname opam_text info ctx dims platform deps);
      (fun () ->
        match Rx.search maximum_version_llvm_re text with
        | Some m
          when let major = List.hd (String.split_on_char '.' (Rx.group m 2)) in
               match
                 (int_of_string_opt major, int_of_string_opt (Rx.group m 1))
               with
               | Some a, Some b -> a > b
               | _ -> false ->
            let want = Rx.group m 1 and have = Rx.group m 2 in
            let major = List.hd (String.split_on_char '.' have) in
            Some
              [
                `Rule Rule.System_library_version;
                `Keep;
                `Confidence "high";
                `Plat platform;
                `Error
                  (Printf.sprintf "LLVM <= %s required; %s has LLVM %s" want
                     platform have);
                `Note
                  (Printf.sprintf
                     "%s needs LLVM %s or older, but its depexts install the \
                      distribution's default LLVM, which is now %s on %s. It \
                      needs versioned depexts there (llvm%s and clang%s \
                      compatibility packages, for example), or a new \
                      conf-libclang version for LLVM %s."
                     fname want major platform want want major);
              ]
        | _ -> None);
      (fun () ->
        match Rx.search usable_version_llvm_re text with
        | Some m ->
            Some
              [
                `Rule Rule.System_library_version;
                `Keep;
                `Confidence "medium";
                `Error (Rx.group0 m);
                `Note
                  (Printf.sprintf
                     "No LLVM found on %s: %s has no depext for it. Add one, \
                      or x-ci-accept-failures if LLVM is not packaged there."
                     platform fname);
              ]
        | None -> None);
      (fun () ->
        let found =
          match Rx.search pkg_config_atleast_re text with
          | Some m ->
              let want =
                match Rx.group_opt m 1 with
                | Some w -> w
                | None -> (
                    match Rx.group_opt m 2 with
                    | Some w -> w
                    | None -> Rx.group m 3)
              in
              let op =
                if Rx.group_opt m 1 <> None then ">="
                else if Rx.group_opt m 2 <> None then "="
                else "<="
              in
              let lib = Rx.group m 4 in
              Some (lib, op, want, system_package_version ctx.full_text lib)
          | None -> (
              match Rx.search requested_but_version_re text with
              | Some m ->
                  Some
                    ( Rx.group m 1,
                      Rx.group m 2,
                      Rx.group m 3,
                      Some (Rx.group m 4) )
              | None -> None)
        in
        match found with
        | Some (lib, op, want, have) ->
            let too =
              if op = ">=" then "too old"
              else if op = "<=" then "too new"
              else "not the expected version"
            in
            Some
              [
                `Rule Rule.System_library_version;
                `Keep;
                `Confidence "high";
                `Plat platform;
                `Error
                  (Printf.sprintf "%s %s %s required; %s has %s" lib op want
                     platform
                     (match have with
                     | Some h when h <> "" -> h
                     | _ -> "an older version"));
                `Note
                  (Printf.sprintf
                     "The system %s is %s on %s, and no depext can change \
                      that. Exclude these distributions \
                      (`x-ci-accept-failures`, or `available` on \
                      os-distribution and os-version), or relax the \
                      requirement upstream."
                     lib too platform);
              ]
        | None -> None);
      (fun () ->
        match Rx.search version_dune_language_re text with
        | Some m ->
            let old, new_ =
              S.constraint_edit opam_text "dune" ">=" (Rx.group m 1)
            in
            Some
              [
                `Rule Rule.Dune_lang;
                `Error (Rx.group0 m);
                `Dep "dune";
                `Old old;
                `New (Some new_);
                `Confidence "high";
              ]
        | None -> None);
      (fun () ->
        let m =
          List.find_map (fun p -> Rx.search p text) missing_tool_messages
        in
        let m =
          match m with
          | Some m
            when let t = Rx.group m 1 in
                 String.starts_with ~prefix:"-" t
                 || (t = "brew" && dims.os <> "macos")
                 || String.starts_with ~prefix:"llvm-config" t ->
              None
              (* an empty variable expanded as a command, or a probe for another platform's tool *)
          | m -> m
        in
        match m with
        | None -> None
        | Some m ->
            let tool = Rx.group m 1 in
            let conf = "conf-" ^ tool in
            let r = [ `Rule Rule.Missing_tool; `Error (Rx.group0 m) ] in
            let distro = Option.value ~default:"" dims.distro in
            let family = List.hd (Rx.split distro_version_re distro) in
            let field_text name =
              let t = Option.value ~default:"" opam_text in
              if not (Util.contains t (name ^ ":")) then ""
              else
                let rest =
                  match Util.partition t (name ^ ":") with _, _, r -> r
                in
                List.hd (Rx.split ~maxsplit:1 field_start_re rest)
            in
            let depexts_text = field_text "depexts" in
            let accepted =
              Rx.findall quoted_re (field_text "x-ci-accept-failures")
            in
            let declared =
              List.filter_map
                (function
                  | [ pk; flt ] ->
                      if family <> "" && Util.contains flt family then
                        Some (String.trim pk)
                      else None
                  | _ -> None)
                (Rx.findall_groups filtered_list_re depexts_text)
            in
            let is_conf = String.starts_with ~prefix:"conf-" fname in
            Some
              (if is_conf && distro <> "" && List.mem distro accepted then
                 r
                 @ Where.fix_where Ci "the CI environment"
                 @ [
                     `Note
                       (Printf.sprintf
                          "%s lists %s in x-ci-accept-failures, so CI should \
                           have ignored this failure and the failures of the \
                           packages that depend on it."
                          fname distro);
                     `Confidence "high";
                   ]
               else if is_conf && declared <> [] then
                 r
                 @ Where.fix_where Ci "the CI environment"
                 @ [
                     `Note
                       (Printf.sprintf
                          "%s declares a depext for %s (%s), but `%s` is \
                           missing on %s: the CI did not install it. Nothing \
                           to change in %s."
                          fname family
                          (String.concat ", " declared)
                          tool platform fname);
                     `Confidence "high";
                   ]
               else if is_conf then
                 r
                 @ [
                     `Note
                       (Printf.sprintf
                          "%s does not provide `%s` on %s: its depexts have no \
                           entry for that platform. Add one, unless the CI \
                           image should already have it."
                          fname tool platform);
                     `Confidence "medium";
                   ]
               else if List.mem conf deps then
                 r
                 @ [
                     `Note
                       (Printf.sprintf
                          "%s is a dependency, but `%s` is missing on %s: %s \
                           lacks a depext for it."
                          conf tool platform conf);
                     `Confidence "medium";
                   ]
               else
                 match S.versions src conf with
                 | Some (_ :: _) ->
                     r
                     @ [
                         `Dep conf;
                         `Old None;
                         `New
                           (Some
                              (Printf.sprintf
                                 {|"%s" {build}   (new line in depends)|} conf));
                         `Confidence "medium";
                       ]
                 | None ->
                     r
                     @ [
                         `Note
                           (Printf.sprintf
                              "`%s` is missing on %s. The tool could not check \
                               whether a %s package exists (ocaml.org did not \
                               answer): if it does, add it to depends; \
                               otherwise add a depext providing `%s`."
                              tool platform conf tool);
                         `Confidence "low";
                       ]
                 | Some [] ->
                     r
                     @ [
                         `Note
                           (Printf.sprintf
                              "There is no %s package: add a depext providing \
                               `%s` on %s."
                              conf tool platform);
                         `Confidence "low";
                       ]));
      (fun () ->
        let m =
          match Rx.search package_was_found_re text with
          | Some m -> Some m
          | None -> Rx.search configure_error_pc_re text
        in
        match m with
        | None -> None
        | Some m ->
            let name = Rx.group m 1 in
            let conf = "conf-" ^ name in
            let r = [ `Rule Rule.Missing_pkg_config; `Error (Rx.group0 m) ] in
            Some
              (if List.mem conf deps then
                 r
                 @ [
                     `Note
                       (Printf.sprintf
                          "%s is a dependency, but its pkg-config file is \
                           missing on %s: %s lacks a depext for it."
                          conf platform conf);
                     `Confidence "medium";
                   ]
               else
                 match S.versions src conf with
                 | Some (_ :: _) ->
                     r
                     @ [
                         `Dep conf;
                         `Old None;
                         `New
                           (Some
                              (Printf.sprintf {|"%s"   (new line in depends)|}
                                 conf));
                         `Confidence "medium";
                       ]
                 | None ->
                     r
                     @ [
                         `Note
                           (Printf.sprintf
                              "%s.pc is missing on %s. The tool could not \
                               check whether a %s package exists (ocaml.org \
                               did not answer): if it does, add it to depends; \
                               otherwise add a depext providing %s.pc."
                              name platform conf name);
                         `Confidence "low";
                       ]
                 | Some [] ->
                     r
                     @ [
                         `Note
                           (Printf.sprintf "Add a depext providing %s.pc on %s."
                              name platform);
                         `Confidence "low";
                       ]));
      (fun () ->
        let m =
          match Rx.search library_found_re text with
          | Some m -> Some m
          | None -> Rx.search package_found_re text
        in
        match m with
        | None -> None
        | Some m ->
            let lib = Rx.group m 1 in
            let pkg0 = List.hd (String.split_on_char '.' lib) in
            let pkg =
              Option.value ~default:pkg0 (List.assoc_opt pkg0 findlib_packages)
            in
            let in_tests = Rx.test tests_re text in
            let r = [ `Rule Rule.Missing_library; `Error (Rx.group0 m) ] in
            let siblings =
              List.fold_left
                (fun acc p ->
                  let n, v = split_pkg p in
                  (n, v) :: List.remove_assoc n acc)
                [] ctx.pr_packages
            in
            let installed = installed_of info pkg in
            let sib = List.assoc_opt pkg siblings in
            Some
              (match (sib, installed) with
              | Some sv, Some inst
                when List.mem pkg deps && pkg <> fname && inst <> ""
                     && inst <> sv ->
                  (* e.g. a lower-bounds job installed an older release of the sibling,
                before the library was added (or made public) in the one of this PR. *)
                  let old, new_ = S.constraint_edit opam_text pkg ">=" sv in
                  r
                  @ [
                      `Dep pkg;
                      `Old old;
                      `New (Some new_);
                      `Confidence "medium";
                      `Note
                        (Printf.sprintf
                           "%s.%s was installed and does not provide `%s`; \
                            %s.%s is released in this PR"
                           pkg inst lib pkg sv);
                    ]
                  @
                  if
                    Some sv = Option.map snd info.failing
                    || (info.failing = None && sv = "")
                  then
                    [
                      `Alternative
                        (Printf.sprintf
                           {|"%s" {= version}  (released together, same version)|}
                           pkg);
                    ]
                  else []
              | Some _, _ when (not (List.mem pkg deps)) && pkg <> fname ->
                  r
                  @ [
                      `Note
                        (Printf.sprintf
                           "`%s` comes from %s, another package of this PR: \
                            the tests of %s use a sibling package. Restrict \
                            them (a dune `(package ...)` field on the test \
                            stanza, or no `@runtest` under `-p %s`) instead of \
                            adding a dependency, which would create a cycle."
                           lib pkg fname fname);
                      `Confidence "medium";
                    ]
              | _ when List.mem pkg deps || pkg = fname ->
                  r
                  @ [
                      `Note
                        (Printf.sprintf
                           "%s is already a dependency: `%s` may only exist in \
                            other versions of %s, or be an optional \
                            sub-library."
                           pkg lib pkg);
                      `Confidence "low";
                    ]
              | _ -> (
                  match S.versions src pkg with
                  | Some (_ :: _) ->
                      let filt = if in_tests then " {with-test}" else "" in
                      r
                      @ [
                          `Dep pkg;
                          `Old None;
                          `New
                            (Some
                               (Printf.sprintf
                                  {|"%s"%s   (new line in depends)|} pkg filt));
                          `Confidence "medium";
                        ]
                      @
                      if pkg <> pkg0 then
                        [
                          `Note
                            (Printf.sprintf
                               "The library `%s` is installed by the %s \
                                package."
                               lib pkg);
                        ]
                      else []
                  | None ->
                      r
                      @ [
                          `Note
                            (Printf.sprintf
                               "The tool could not check whether %s is an opam \
                                package (ocaml.org did not answer): if it is, \
                                add it to depends; otherwise `%s` is probably \
                                an internal library missing from the release \
                                archive, or excluded by `-p %s`."
                               pkg lib fname);
                          `Confidence "low";
                        ]
                  | Some [] ->
                      r
                      @ [
                          `Note
                            (Printf.sprintf
                               "`%s` is not an opam package. It is probably an \
                                internal library missing from the release \
                                archive, or excluded by `-p %s`."
                               lib fname);
                          `Confidence "medium";
                        ])));
      (fun () ->
        match Rx.search fatal_error_such_re text with
        | Some m ->
            Some
              [
                `Rule Rule.Missing_header;
                `Error (Rx.group0 m);
                `Confidence "low";
                `Note
                  (Printf.sprintf
                     "A system header is missing on %s. Add the depext (or \
                      conf-* dependency) that provides %s, or \
                      x-ci-accept-failures for the platforms without it."
                     platform (Rx.group m 1));
              ]
        | None -> None);
      (fun () -> conf_without_depext fname opam_text info ctx job platform);
    ]

(* ------------------------------------------------------------------------ *)
(* One job *)

let ext_of_path f =
  (* The extension of the file name, from its last dot (none for a dot
     file). *)
  match String.rindex_opt f '.' with
  | Some i
    when i > 0 && not (String.for_all (fun c -> c = '.') (String.sub f 0 i)) ->
      String.sub f i (String.length f - i)
  | _ -> ""

let version_ints r =
  List.map
    (fun x -> Option.value ~default:0 (int_of_string_opt x))
    (String.split_on_char '.' r)

let these_packages_are_re = Rx.v "These packages are still missing: (.+)"

let segmentation_fault_bus_re =
  Rx.v
    {|(Segmentation fault|Bus error|Illegal instruction)\s+(day10[\w-]*|obuilder[\w-]*)|}

let runc_run_failed_re = Rx.v {|runc run failed: [^\n]{0,120}|}

let http_error_code_re =
  Rx.v
    {|HTTP error code (5\d\d|429)|Could not resolve host|Connection (?:reset|timed out)|}

let day10_internal_error_re =
  Rx.v {|day10: internal error, uncaught exception:\s*([^\n]*)|}

let worker_internal_error_re =
  Rx.v
    {|Uncaught exception: ([^\n]{0,200})(?:[^\n]*\n){1,3}[^\n]*Job failed: Failed: Internal error|}

let windows_container_re = Rx.v {|ctr: hcs::System::Start [^:\n]*: ([^\n]*)|}

let base_image_re =
  Rx.v {|\[ERROR\] Building the base for (\S+) failed with status \d+|}

let sources_again_re =
  Rx.v {|\[ERROR\] Failed to get sources of (\S+): ([^\n]*)|}

(* A crash of the CI's tools or workers, a container or base image it could
   not start or build, sources it could not fetch again after installing
   the package: (error, what failed) when the log shows one. *)
let ci_crash text =
  Util.first_some
    [
      (fun () ->
        Option.map
          (fun m ->
            ( "day10 crashed: " ^ String.trim (Rx.group m 1),
              "the day10 builder crashed (an internal error, exit code 125)" ))
          (Rx.search day10_internal_error_re text));
      (fun () ->
        Option.map
          (fun m ->
            ( "Internal error of the CI worker: " ^ String.trim (Rx.group m 1),
              "the CI worker failed (\"Internal error\")" ))
          (Rx.search worker_internal_error_re text));
      (fun () ->
        Option.map
          (fun m ->
            ( "The Windows container did not start: "
              ^ String.trim (Rx.group m 1),
              "the Windows container did not start" ))
          (Rx.search windows_container_re text));
      (fun () ->
        Option.map
          (fun m ->
            ( "day10 could not build its base image for " ^ Rx.group m 1,
              "day10 could not build its base image (the distribution's \
               packages, before any opam package)" ))
          (Rx.search base_image_re text));
      (fun () ->
        (* day10 fetches the sources again after the build: a failure then
           is the network's, the package installed. *)
        List.find_map
          (fun m ->
            let pkg = Rx.group m 1 in
            if Util.contains text ("-> installed " ^ pkg ^ "\n") then
              Some
                ( Printf.sprintf
                    "Failed to get sources of %s again, after installing it: %s"
                    pkg
                    (String.trim (Rx.group m 2)),
                  "the CI could not download the sources a second time, after \
                   the package built and installed" )
            else None)
          (Rx.finditer sources_again_re text));
    ]

(* Failures of the CI rather than of the build: a system package missing
   from the distribution, a crash of the CI tool, a container that did not
   start, a network error. *)
let ci_failure job text (info : info) pr_packages =
  let owner = match failing_pkg job with Some o -> o | None -> "" in
  Util.first_some
    [
      (fun () ->
        match Rx.search these_packages_are_re text with
        | Some m when info.failing = None ->
            let names = Util.split (Rx.group m 1) in
            let dims = job_dims job in
            let plat =
              match dims.distro with
              | Some d when d <> "" -> d
              | _ -> if dims.os = "linux" then "debian-13" else dims.os
            in
            let is_conf =
              String.starts_with ~prefix:"conf-" (fst (split_pkg owner))
            in
            Some
              ([ `Rule Rule.Missing_system_package; `Failing (Some owner) ]
              @ (if is_conf then
                   Where.pr_or_other owner (List.mem owner pr_packages)
                 else
                   Where.fix_where Other_package
                     "the dependency declaring these depexts")
              @ [
                  `Error
                    (Printf.sprintf "System packages not available on %s: %s"
                       plat (String.concat ", " names));
                  `New
                    (Some
                       (Printf.sprintf
                          {|x-ci-accept-failures: ["%s"]   (or correct the depext names for %s)|}
                          plat plat));
                  `Confidence "medium";
                  `Note
                    "Either the depext name is wrong for this distribution, \
                     the package needs an extra repository (EPEL, CRB, ports), \
                     or it does not exist there.";
                ])
        | _ -> None);
      (fun () ->
        match ci_crash text with
        | Some (error, what) ->
            Some
              ([
                 `Rule Rule.Ci_failure;
                 `Failing (if owner = "" then None else Some owner);
               ]
              @ Where.fix_where Ci "the CI environment"
              @ [
                  `Error error;
                  `Confidence "high";
                  `Note
                    (Printf.sprintf
                       "Not the package: %s. Nothing to change in the PR; \
                        rerun the job (\"Rebuild failed\" on the CI page), and \
                        report the failure to the CI's maintainers if it \
                        persists."
                       what);
                ])
        | None -> None);
      (fun () ->
        match Rx.search segmentation_fault_bus_re text with
        | Some m ->
            Some
              [
                `Rule Rule.Transient;
                `Error
                  (Printf.sprintf "%s of the CI tool %s" (Rx.group m 1)
                     (Rx.group m 2));
                `Note "The CI tool crashed, not the package. Rerun the job.";
              ]
        | None -> None);
      (fun () ->
        if Rx.test runc_run_failed_re text then
          Some
            [
              `Rule Rule.Transient;
              `Error "The CI could not start the build container";
              `Note
                "A CI infrastructure failure (the container did not start). \
                 Rerun the job.";
            ]
        else None);
      (fun () ->
        match Rx.search http_error_code_re text with
        | Some m when info.failing = None ->
            Some
              [
                `Rule Rule.Transient;
                `Error (Rx.group0 m);
                `Note "Network error. Rerun the job.";
              ]
        | _ -> None);
    ]

(* A failed build of [fname.fver] (one of the PR's packages, or a reverse
   dependency), as the rules on its error see it. *)
type build = {
  src : S.t;
  text : string; (* the job's log *)
  info : info;
  pr_packages : string list;
  gref : string; (* the git ref [opam_text] comes from *)
  fname : string;
  fver : string;
  opam_text : string option;
}

let remove_re = Rx.v ~flags:[ Rx.M ] {|^remove:|}

(* opam reinstalls a package at the same version (a "recompile" in the
     plan, e.g. after downgrading its dependencies) and removes it first;
     for a package with a [remove:] field, the rebuild then runs in an empty
     source directory ("No rule to make target", dune building nothing).
     Seen with opam 2.6 on lower-bounds jobs; frama-c's own PR passed
     because frama-c was pinned there. (The filter on "pinned" stands for a
     negative lookahead, which ocaml-re does not have.) *)
let same_version_reinstall (b : build) =
  let { text; info; fname; fver; opam_text; _ } = b in
  let plan =
    List.find_opt
      (fun m -> not (Util.contains (Rx.group m 1) "pinned"))
      (Rx.finditer
         (Rx.of_re
            (Re.seq
               [
                 Rx.re ~flags:[ Rx.M ] {|^\s*- recompile\s+|};
                 Re.str fname;
                 Rx.re {|\s+|};
                 Re.str fver;
                 Rx.re ~flags:[ Rx.M ] {|\s+(.*)$|};
               ]))
         text)
  in
  let removed =
    match plan with
    | Some p ->
        Rx.test
          (Rx.of_re
             (Re.seq
                [
                  Rx.re ~flags:[ Rx.M ] {|^-> removed\s+|};
                  Re.str (fname ^ "." ^ fver);
                  Rx.re ~flags:[ Rx.M ] {|\s*$|};
                ]))
          (Util.slice ~start:(Rx.end_ p) text)
    | None -> false
  in
  if removed && Rx.test remove_re (Option.value ~default:"" opam_text) then
    Some
      ([ `Rule Rule.Opam_same_version_reinstall; `Confidence "high" ]
      @ Where.fix_where Ci "opam (the CI's opam version)"
      @ [
          `Error
            (Printf.sprintf
               "%s.%s is recompiled at the same version, removed, then fails \
                to %s"
               fname fver
               (if info.action = Some "installing" then "install" else "build"));
          `Note
            (Printf.sprintf
               "opam rebuilds %s.%s at the same version and also removes it, \
                which needs its sources (it has a `remove:` field). Since opam \
                2.6 (ocaml/opam#6440) both steps move the same source \
                directory, so the one that runs second gets an empty \
                directory. An opam bug, not a problem of the packages or their \
                bounds: the job can be ignored. A pinned %s (as in its own PR) \
                does not hit it."
               fname fver fname);
        ])
  else None

(* Install failures: the install command failed (its output is in the
   block), or files listed for installation were never built because an
   earlier build command failed without stopping the build. *)
type install_failure =
  | Not_built of string (* no earlier build error: the files not built *)
  | Earlier_error of
      string list
      * string (* the earlier build errors, and the failure in words *)

let to_re = Rx.v {|/([^/\s]+) to /|}
let error_error_parse_re = Rx.v {|\bError\b|error:|Parse error|fatal error|}

let error_report_error_re =
  Rx.v {|Error report|\[ERROR\] The (compilation|installation)|}

let output_mark_re = Rx.v "^[-#] ?"

let install_failure text (info : info) =
  let err = info.error_lines in
  if
    info.action = Some "installing"
    && List.exists (fun l -> Util.contains l "couldn't be installed") err
  then begin
    let missing = List.concat_map (fun l -> Rx.findall to_re l) err in
    let exts =
      Util.counter
        (List.map
           (fun f -> match ext_of_path f with "" -> f | e -> e)
           missing)
    in
    let cut = Util.find text "#=== ERROR while installing" in
    let earlier =
      List.filter_map
        (fun l ->
          if
            Rx.test error_error_parse_re l
            && not (Rx.test error_report_error_re l)
          then Some (String.trim (Rx.sub ~count:1 output_mark_re "" l))
          else None)
        (Util.splitlines (Util.slice ~stop:cut text))
    in
    let install =
      "Files listed for installation were not built: "
      ^ String.concat ", "
          (List.map (fun (x, n) -> Printf.sprintf "%d %s" n x) exts)
    in
    if earlier <> [] then
      let err = Util.sublist ~start:(-15) earlier in
      Some
        (Earlier_error
           ( err,
             install
             ^ Printf.sprintf "; earlier build error: %s"
                 (Util.prefix_chars (Util.last err) 150) ))
    else Some (Not_built install)
  end
  else None

let has_type_string_re =
  Rx.v ~flags:[ Rx.M ]
    {|has type\s+string\s+but an expression was expected of type\s*$|}

let bytes_re = Rx.v {|=?\s*bytes|}

(* A string used where bytes are expected: safe-string. *)
let safe_string err_text opam_text =
  if Rx.test has_type_string_re err_text && Rx.test bytes_re err_text then
    let old, new_ = S.constraint_edit opam_text "ocaml" "<" "4.06" in
    Some
      [
        `Rule Rule.Safe_string;
        `Error
          "String used where bytes is expected (safe-string, default since \
           OCaml 4.06)";
        `Dep "ocaml";
        `Old old;
        `New (Some new_);
        `Confidence "high";
      ]
  else None

let diff_git_git_re = Rx.v {|(diff --git|--- |\S*git --no-pager diff)|}

(* In a test diff (cram/mdx), errors are part of the expected or actual
   output: only the lines before the diff are compiler errors. *)
let compiler_errors (info : info) err =
  if info.test_diff then
    let cut =
      match List.find_index (Rx.test_match diff_git_git_re) err with
      | Some i -> i
      | None -> List.length err
    in
    Util.take cut err
  else err

let ocaml_re =
  Rx.v ~flags:[ Rx.M ] {|^[ \t]*"ocaml"[ \t]*(?:\{([^}]*)\})?[^\n]*$|}

let test_re = Rx.v {|\(<\s*"[^"]+"\s*\|\s*!with-test\)|}

(* A test output diff (cram/mdx). *)
let test_output (info : info) err opam_text =
  match (toplevel_diff err, info.ocaml) with
  | Some top, Some ocaml when ocaml <> "" ->
      (* Expected toplevel output (types, values, error locations) printed
        by an older compiler: the test depends on the OCaml version. *)
      let mm =
        String.concat "." (Util.take 2 (String.split_on_char '.' ocaml))
      in
      let m = Rx.search ocaml_re (Option.value ~default:"" opam_text) in
      let term = Printf.sprintf {|< "%s" | !with-test|} mm in
      let filt =
        match m with Some m -> String.trim (Rx.group m 1) | None -> ""
      in
      let new_ =
        match Rx.search test_re filt with
        | Some prev ->
            (* already disabled from a later version: lower it *)
            Printf.sprintf {|"ocaml" {%s}|}
              (Util.replace filt (Rx.group0 prev) ("(" ^ term ^ ")"))
        | None ->
            if filt <> "" then Printf.sprintf {|"ocaml" {%s & (%s)}|} filt term
            else Printf.sprintf {|"ocaml" {%s}|} term
      in
      [
        `Rule Rule.Test_output_ocaml;
        `Dep "ocaml";
        `Confidence "medium";
        `Error
          (Printf.sprintf "Toplevel output in the tests differs on OCaml %s: %s"
             ocaml
             (Util.prefix_chars (List.hd top) 100));
        `Old
          (match m with
          | Some m -> Some (String.trim (Rx.group0 m))
          | None -> None);
        `New
          (Some
             (new_
             ^ Printf.sprintf
                 "   (keeps the package installable, disables its tests on \
                  OCaml >= %s)"
                 mm));
        `Alternative
          (Printf.sprintf
             {|"@runtest" {with-test & ocaml:version < "%s"}   (on the test command)|}
             mm);
        `Note
          (Printf.sprintf
             "%s is the oldest OCaml version that fails in this run; the \
              output may have changed in an earlier release (5.4 prints \
              shorter type paths, for example). Updating the expected output \
              upstream fixes it for good."
             mm);
      ]
  | _ ->
      (* completed by the caller with a passing job's versions *)
      [ `Rule Rule.Test_output; `Error "Test output differs (cram/mdx)" ]

(* Whether [top], the top module of an unbound identifier, is a module of
   the standard library; if so, [out] gets the bound on OCaml that adds or
   removes the identifier. *)
let stdlib_bound (b : build) out kind mods ident top =
  let { src; info; opam_text; _ } = b in
  let rel = S.ocaml_releases src in
  (* [] if the OCaml tags could not be listed *)
  let latest = Util.last_opt rel and t414 = "4.14.0" in
  let top_m =
    match mods with "Stdlib" :: m :: _ when top = "Stdlib" -> m | _ -> top
  in
  let stdlib_mli r m =
    S.raw src
      (Printf.sprintf
         "https://raw.githubusercontent.com/ocaml/ocaml/%s/stdlib/%s.mli" r
         (String.lowercase_ascii m))
  in
  let nonempty = function Some x when x <> "" -> true | _ -> false in
  if
    (match latest with
      | Some l -> nonempty (stdlib_mli l top_m)
      | None -> false)
    || nonempty (stdlib_mli t414 top_m)
  then begin
    if rel = [] then
      out.note <-
        Some
          "Standard library identifier, but the OCaml releases could not be \
           listed (git ls-remote failed): the tool could not check which \
           releases have it."
    else begin
      let inner =
        if top = "Stdlib" then Util.drop 2 mods else Util.drop 1 mods
      in
      (* ocaml.org documents the standard library with the compiler:
       ocaml-base-compiler up to 5.2, ocaml-compiler from 5.3 (where
       the compiler's sources moved). One API-history call for each,
       with the module path in full (Stdlib.List: "List" alone also
       names compiler-libs modules there); the .mli of a release for
       what it cannot tell (no docs, a failed call). *)
      let path =
        if kind = "module" && mods = [] then [ "Stdlib" ]
        else [ "Stdlib"; top_m ] @ inner
      in
      let split = "5.3.0" in
      (* Only the releases ocaml.org lists for each package: the tool
       rejects a range that starts at a version it does not know
       (4.08.0 of ocaml-base-compiler, say). *)
      let listed pkg rs =
        let known = S.versions_or_empty src pkg in
        List.filter (fun r -> List.mem r known) rs
      in
      let older =
        listed "ocaml-base-compiler"
          (List.filter (fun r -> opam_vcmp r split < 0) rel)
      in
      let newer =
        listed "ocaml-compiler"
          (List.filter (fun r -> opam_vcmp r split >= 0) rel)
      in
      let h_old =
        if older = [] then None
        else S.api_history src "ocaml-base-compiler" path kind ident older
      in
      let h_new =
        if newer = [] then None
        else S.api_history src "ocaml-compiler" path kind ident newer
      in
      let used = ref [] in
      let use x = if not (List.mem x !used) then used := x :: !used in
      let check r =
        let h = if opam_vcmp r split < 0 then h_old else h_new in
        match S.history_presence h r with
        | S.Yes ->
            use S.api_history_source;
            Some true
        | S.No ->
            use S.api_history_source;
            Some false
        | _ -> (
            use stdlib_sources;
            let text = stdlib_mli r top_m in
            if kind = "module" && mods = [] then Some (text <> None)
            else
              match text with
              | None -> Some false
              | Some text ->
                  if
                    inner <> [] && not (S.defines text "module" (List.hd inner))
                  then Some false
                  else Some (S.defines text kind ident))
      in
      let bad = Option.value ~default:"0" info.ocaml in
      let cands =
        match List.filter (fun r -> opam_vcmp r bad <= 0) rel with
        | [] -> [ List.hd rel ]
        | l -> l
      in
      let bad_rel =
        List.fold_left
          (fun a r ->
            if compare (version_ints r) (version_ints a) > 0 then r else a)
          (List.hd cands) cands
      in
      let res, ev = S.bisect_presence check rel bad_rel in
      out.evidence <-
        Some
          (List.map
             (fun (v, r) -> ("ocaml " ^ v, r))
             (Util.sort_by (fun (v, _) -> version_ints v) ev));
      if !used <> [] then
        out.source <- Some (String.concat ", " (List.sort compare !used));
      match res with
      | Some (op, v) ->
          let v =
            String.concat "." (Util.take 2 (String.split_on_char '.' v))
          in
          let old, new_ = S.constraint_edit opam_text "ocaml" op v in
          Suggestion.update out
            [
              `Rule Rule.Stdlib_change;
              `Dep "ocaml";
              `Old old;
              `New (Some new_);
              `Confidence "high";
            ];
          (* The bound drops OCaml releases the package supported: some
          authors keep them and do without the identifier (grenier
          0.17, Float.log2). *)
          let name = String.concat "." (mods @ [ ident ]) in
          out.note <-
            Some
              (if op = ">=" then
                 Printf.sprintf
                   "`%s` is new in OCaml %s: to keep supporting older \
                    releases, do without it instead."
                   name v
               else
                 Printf.sprintf
                   "`%s` was removed in OCaml %s: to support it, do without it \
                    instead."
                   name v)
      | None ->
          out.note <-
            Some
              "Standard library identifier, but no OCaml release adds or \
               removes it."
    end;
    true
  end
  else false

let dependency_name_re = Rx.v ~flags:[ Rx.M ] {|^\s*"([\w+-]+)"|}
let module_prefix_re = Rx.v {|\b([A-Z]\w+)\.|}
let file_tests_re = Rx.v ~flags:[ Rx.M ] {|^File "(?:[^"]*/)?tests?/|}

(* An unbound identifier from a library: the bound on the dependency that
   adds or removes it. *)
let library_bound (b : build) out err_text kind mods ident top =
  let { src; text; info; pr_packages; gref; fname; fver; opam_text } = b in
  (* Candidate packages whose name matches the top module. *)
  let installed_names = List.map fst info.installed in
  let cands =
    List.filter
      (fun p ->
        String.starts_with ~prefix:(S.norm p) (S.norm top)
        && String.length (S.norm p) >= 2)
      installed_names
  in
  let cands = Util.sort_by (fun p -> -String.length p) cands in
  let by_name = cands <> [] in
  let cands =
    match (cands, opam_text) with
    | [], Some ot when ot <> "" ->
        (* The module may be reached through an [open] (e.g. Term from
         Why3). Rank the direct dependencies by the module names in
         the error text. *)
        let block = depends_block ~close:"]" ot in
        let deps = Rx.findall dependency_name_re block in
        let deps =
          List.filter
            (fun d ->
              is_installed info d
              && not (List.mem d [ "ocaml"; "dune"; "ocamlfind" ]))
            deps
        in
        let mentioned =
          List.map S.norm (Rx.findall module_prefix_re err_text)
        in
        Util.sort_by
          (fun d ->
            not
              (List.exists
                 (fun m -> String.starts_with ~prefix:(S.norm d) m)
                 mentioned))
          deps
    | c, _ -> c
  in
  (* ocaml.org's module search: which of these packages and of the
     other direct (and optional) dependencies installed document
     module [top], at any depth (Term in why3 is Why3.Term), in their
     latest documented version. Those come first, with the module's
     full path, and need not have it in the version installed (it may
     have appeared later); the guesses above follow, for when the
     search does not answer or its packages are not the cause. *)
  let searched =
    List.filter
      (fun d ->
        is_installed info d
        && (not (List.mem d not_searched))
        && not (String.starts_with ~prefix:"conf-" d))
      (Util.dedup
         (cands @ dep_names opam_text "depends" @ dep_names opam_text "depopts"))
  in
  let found =
    if searched <> [] then S.module_providers src top searched else None
  in
  let chosen = Odict.create () in
  (* package -> full path of [top] in it (None: several) *)
  List.iter
    (fun (m : S.find_match) ->
      if is_installed info m.package then
        Odict.set chosen m.package
          (if Odict.mem chosen m.package then None else Some m.module_path))
    (match found with Some f -> f.matches | None -> []);
  let cands =
    Odict.keys chosen @ List.filter (fun p -> not (Odict.mem chosen p)) cands
  in
  (* A top-level module that no dependency documents in its newest
     version may have been removed by a recent release of one:
     console-eio's tests used Grid, a top-level module of matrix
     0.1.0, renamed Matrix_grid in matrix 0.2.0 (#31170). The package
     may also use a module of a package it does not declare, installed
     through another dependency: matrix, through k7. *)
  let removed =
    kind = "module" && mods = []
    && Odict.length chosen = 0
    && (not by_name)
    && (match found with Some f -> not f.fpartial | None -> false)
    &&
    let others =
      List.filter
        (fun p ->
          (not (List.mem p searched))
          && p <> fname
          && (not (List.mem p not_searched))
          && (not
                (String.starts_with ~prefix:"conf-" p
                || String.starts_with ~prefix:"base-" p
                || String.starts_with ~prefix:"ocaml-" p))
          && not (List.mem p [ "seq"; "compiler-cloning" ]))
        installed_names
    in
    List.exists
      (fun (pkg, prev, bad, pub) ->
        S.satisfies prev
          (List.filter
             (fun (op, _) -> op <> "=")
             (S.existing_bounds opam_text pkg))
        && begin
          let old, new_ = S.constraint_edit opam_text pkg "<" bad in
          let note =
            Printf.sprintf
              "`%s` is a top-level module of %s.%s, not of %s.%s (released %s)."
              top pkg prev pkg bad pub
          in
          let new_, note =
            if old = None then
              let tests = Rx.test file_tests_re err_text in
              ( Printf.sprintf {|"%s" {%s< "%s"}   (new line in depends)|} pkg
                  (if tests then "with-test & " else "")
                  bad,
                note
                ^ Printf.sprintf
                    " %s uses it without declaring %s: declare it, with this \
                     bound."
                    fname pkg )
            else (new_, note)
          in
          Suggestion.update out
            [
              `Rule Rule.Library_api;
              `Dep pkg;
              `Old old;
              `New (Some new_);
              `Confidence "medium";
              `Evidence
                [
                  (pkg ^ " " ^ prev, Some true);
                  ( Printf.sprintf "%s %s (installed in failing job)" pkg bad,
                    Some false );
                ];
              `Source "ocaml.org docs";
              `Note note;
            ];
          let extra = module_find_note found top info opam_text fname in
          if extra <> "" then
            out.note <- Some (Suggestion.str out.note ^ " " ^ extra);
          true
        end)
      (removed_module src top
         (Util.take 40 (searched @ others))
         info (job_date text))
  in
  if not removed then begin
    let tried msg = out.tried <- Some (Suggestion.list out.tried @ [ msg ]) in
    let bound_found =
      List.exists
        (fun pkg ->
          match installed_of info pkg with
          | None | Some "" -> false
          | Some bad -> (
              let path = Option.join (Odict.get chosen pkg) in
              let doc_mods =
                match (path, mods) with
                | Some p, _ :: rest ->
                    p @ rest (* Term.t_loc in why3: ['Why3', 'Term'] *)
                | Some p, [] ->
                    Util.take
                      (List.length p - 1)
                      p (* a top-level module found in a wrapper: ['Why3'] *)
                | None, _ -> mods (* full path, e.g. ['Ortools', 'Sat'] *)
              in
              let git_path =
                match mods with
                | m0 :: rest
                  when String.starts_with ~prefix:(S.norm pkg) (S.norm m0)
                       && rest <> [] ->
                    rest
                | _ -> mods
              in
              let used =
                ref
                  (if Odict.mem chosen pkg then [ S.module_find_source ] else [])
              in
              let use x = if not (List.mem x !used) then used := x :: !used in
              let bounds = S.existing_bounds opam_text pkg in
              let allowed v =
                S.satisfies v (List.filter (fun (op, _) -> op <> "=") bounds)
              in
              (* ocaml.org's API history of the item: one call for every
           version bisect_presence may check (not for a top-level
           module: the tool tracks the items of a module). *)
              let hist =
                if doc_mods <> [] then
                  let vs = S.versions_or_empty src pkg in
                  let window =
                    [ bad ]
                    @ Util.take S.bisect_cap
                        (List.filter
                           (fun v -> opam_vcmp v bad > 0 && allowed v)
                           vs)
                    @ Util.sublist ~start:(-S.bisect_cap)
                        (List.filter
                           (fun v -> opam_vcmp v bad < 0 && allowed v)
                           vs)
                  in
                  S.api_history src pkg doc_mods kind ident window
                else None
              in
              let check v =
                (* 1. ocaml.org's API history (ocaml_module_changes), for the
             versions it could tell *)
                let h = S.history_presence hist v in
                match h with
                | S.Yes ->
                    use S.api_history_source;
                    Some true
                | S.No ->
                    use S.api_history_source;
                    Some false
                | _ -> (
                    (* 2. ocaml.org docs for pkg.v (no git, no token, any
               upstream host), unless the history says it has none *)
                    let r =
                      if h <> S.No_docs then
                        S.doc_present src pkg v doc_mods kind ident
                      else None
                    in
                    match r with
                    | Some r ->
                        use "ocaml.org docs";
                        Some r
                    | None -> (
                        (* 3. fallback: shallow blob-less git fetch of the matching tag *)
                        let repo =
                          match S.upstream src pkg v with
                          | Some r -> Some r
                          | None -> S.upstream src pkg bad
                        in
                        let t =
                          match repo with
                          | Some r when r <> "" -> S.tag_for src r pkg v
                          | _ -> None
                        in
                        match (repo, t) with
                        | Some repo, Some t when t <> "" ->
                            use ("git " ^ repo);
                            if kind = "module" && mods = [] then
                              Some
                                (S.module_files (S.tree src repo t) ident <> [])
                            else
                              S.present src repo t
                                (if git_path = [] then [ top ] else git_path)
                                kind ident
                        | _ -> None))
              in
              let skip =
                if (not by_name) && not (Odict.mem chosen pkg) then begin
                  (* Accept a dependency only if it actually provides that
               module. (The history tells about the module path of the
               call: [top] only when that path is [top] alone.) *)
                  let has_mod =
                    if List.length doc_mods = 1 then
                      S.history_has_module hist bad
                    else None
                  in
                  let has_mod =
                    match has_mod with
                    | None -> S.doc_present src pkg bad [] "module" top
                    | h -> h
                  in
                  let has_mod =
                    match has_mod with
                    | Some b -> b
                    | None -> (
                        match S.upstream src pkg bad with
                        | Some repo when repo <> "" -> (
                            match S.tag_for src repo pkg bad with
                            | Some t when t <> "" ->
                                S.module_files (S.tree src repo t) top <> []
                            | _ -> false)
                        | _ -> false)
                  in
                  not has_mod
                end
                else false
              in
              (not skip)
              &&
              match check bad with
              | None ->
                  tried
                    (Printf.sprintf
                       "%s.%s: no ocaml.org docs and no matching git tag" pkg
                       bad);
                  false
              | Some true ->
                  tried
                    (Printf.sprintf
                       "%s.%s defines it (%s): not a version problem of %s" pkg
                       bad
                       (String.concat ", " (List.sort compare !used))
                       pkg);
                  false
              | Some false -> (
                  let res, ev =
                    S.bisect_presence ~allowed check
                      (S.versions_or_empty src pkg)
                      bad
                  in
                  out.evidence <-
                    Some
                      (List.map
                         (fun (v, r) -> (pkg ^ " " ^ v, r))
                         (List.stable_sort
                            (fun (a, _) (b, _) -> opam_vcmp a b)
                            ev)
                      @ [
                          ( Printf.sprintf "%s %s (installed in failing job)"
                              pkg bad,
                            Some false );
                        ]);
                  out.source <-
                    Some (String.concat ", " (List.sort compare !used));
                  match res with
                  | Some (op, v) ->
                      let old, new_ = S.constraint_edit opam_text pkg op v in
                      Suggestion.update out
                        [
                          `Rule Rule.Library_api;
                          `Dep pkg;
                          `Old old;
                          `New (Some new_);
                          `Confidence "high";
                        ];
                      if
                        List.mem (pkg ^ "." ^ fver) pr_packages
                        && S.upstream ~ref:gref src fname fver
                           = S.upstream ~ref:gref src pkg fver
                      then
                        out.alternative <-
                          Some
                            (Printf.sprintf
                               {|"%s" {= version}  (same upstream repository and version)|}
                               pkg);
                      true
                  | None -> false)))
        (Util.take 5 cands)
    in
    if not bound_found then begin
      if out.note = None then
        Suggestion.update out
          [
            `Note
              "The identifier was not found in the history of any dependency \
               the tool checked.";
            `Unexplained;
          ];
      let extra = module_find_note found top info opam_text fname in
      if extra <> "" then
        out.note <- Some (Suggestion.str out.note ^ " " ^ extra)
    end
  end

(* An unbound identifier [mods.ident] of kind [kind]. *)
let unbound_identifier (b : build) out err_text (kind, mods, ident) =
  out.error <-
    Some
      (Printf.sprintf "Unbound %s %s"
         (Util.replace kind "-" " ")
         (String.concat "." (mods @ [ ident ])));
  let top =
    match mods with
    | m :: _ -> Some m
    | [] -> if kind = "module" then Some ident else None
  in
  match top with
  | None ->
      out.note <-
        Some
          "The identifier has no module path, so the tool cannot tell which \
           package it comes from.";
      out.unexplained <- true (* for the LLM fallback (reset by suggest) *)
  | Some top ->
      if not (stdlib_bound b out kind mods ident top) then
        library_bound b out err_text kind mods ident top

(* The rules on the error of a failed build of [fname.fver], in order. *)
let build_failure src job text (info : info) pr_packages pr_sha out (fname, fver)
    =
  let full = fname ^ "." ^ fver in
  out.Suggestion.failing <- Set full;
  let in_pr = List.mem full pr_packages in
  Suggestion.update out (Where.pr_or_other full in_pr);
  let gref =
    if in_pr then pr_sha
    else match info.base with Some b when b <> "" -> b | _ -> "master"
  in
  let opam_text =
    match S.opam_file ~ref:gref src fname fver with
    | Some t when t <> "" -> Some t
    | _ -> S.opam_file src fname fver
  in
  let b = { src; text; info; pr_packages; gref; fname; fver; opam_text } in

  match same_version_reinstall b with
  | Some fields -> Suggestion.update out fields
  | None -> (
      match install_failure text info with
      | Some (Not_built install) ->
          Suggestion.update out
            [
              `Rule Rule.Install_missing_files;
              `Error install;
              `Confidence "low";
              `Note
                "The build does not produce these files on this platform \
                 (.cmxs files need native dynlink, for example). Make them \
                 optional in the .install file or the build, or restrict \
                 `available`.";
            ]
      | install -> (
          let err =
            match install with
            | Some (Earlier_error (err, msg)) ->
                out.install <- Some msg;
                err
            | _ -> info.error_lines
          in
          let err_text = String.concat "\n" err in
          let ctx =
            { pr_packages; full_text = text; job_variant = job.variant }
          in
          match
            Util.first_some
              [
                (fun () -> safe_string err_text opam_text);
                (fun () -> log_rules src err fname opam_text info ctx job);
              ]
          with
          | Some fields -> Suggestion.update out fields
          | None -> (
              match find_unbound (compiler_errors info err) with
              | Some unbound -> unbound_identifier b out err_text unbound
              | None -> (
                  if info.test_diff then
                    Suggestion.update out (test_output info err opam_text)
                  else
                    match out.install with
                    | Some install when install <> "" ->
                        out.install <- None;
                        Suggestion.update out
                          [
                            `Error install;
                            `Note
                              "The tool does not recognise the earlier build \
                               error.";
                            `Unexplained;
                          ]
                    | _ ->
                        Suggestion.update out
                          [ `Note "Unrecognised error."; `Unexplained ]))))

let suggest_for_job src job text pr_packages pr_sha : Suggestion.t =
  let info = log_info text in
  let dd = job_dims job in
  let out =
    Suggestion.make
      [
        `Variant job.variant;
        `Url job.url;
        `Kind job.kind;
        `Experimental job.experimental;
      ]
  in
  if match job.platform with "day10" :: _ -> true | _ -> false then
    out.distros <-
      [ (match dd.distro with Some d when d <> "" -> d | _ -> dd.os) ]
    (* see accepted_but_failing *);
  (match ci_failure job text info pr_packages with
  | Some fields -> Suggestion.update out fields
  | None -> (
      match info.failing with
      | None ->
          out.note <-
            Some
              "The log has no build error from opam: a dependency failed, or \
               the network or the solver did."
      | Some failing ->
          build_failure src job text info pr_packages pr_sha out failing));
  out

(* Dependencies too common to suspect. *)
let trivial_deps =
  [
    "ocaml";
    "dune";
    "ocamlfind";
    "odoc";
    "ocaml-base-compiler";
    "ocaml-compiler";
    "base-unix";
    "base-threads";
    "base-bytes";
    "conf-pkg-config";
    "dune-configurator";
  ]

let changelog_excerpt_max = 1500

(** The changelog sections of [dep] between two versions (after [since], up to
    [until]), cut to [changelog_excerpt_max] characters, for a candidate of a
    test-output failure; None if ocaml.org has none. [until] is the first
    release after the passing one, whose changes the suggested bound excludes.
    ocaml.org escapes "<" and "&" in changelogs; the text is decoded here, and
    escaped again by each output (HTML page, Markdown code block). *)
let changelog_excerpt src dep since until : Suggestion.Changelog.t option =
  match S.changelog ?since ?until src dep with
  | None -> None
  | Some c ->
      let text = String.trim (Html.unescape c.text) in
      let long = Util.char_length text > changelog_excerpt_max in
      let cut = long || c.truncated in
      let text =
        if long then
          List.hd
            (Util.rsplit1 "\n" (Util.prefix_chars text changelog_excerpt_max))
        else text
      in
      Some
        {
          file = Suggestion.field_of_option c.file;
          versions = Some c.cversions;
          text = Some text;
          cut = Some cut;
          extra = [];
        }

let dependency_line_re = Rx.v ~flags:[ Rx.M ] {|^\s*"([\w+-]+)"([^\n]*)$|}
let build_doc_dev_re = Rx.v {|\b(build|with-doc|with-dev-setup)\b|}

(** No passing job to compare with: dependencies whose installed version was
    released after the failing package itself are candidates for having changed
    its test output. Bound: before their first release after that date. Cost:
    one ocaml_package_versions call per dependency looked at (at most
    [max_lookups], those mentioned in the error first; the binary search over
    release dates reuses it), and one ocaml_package_changelog call per
    candidate, for the changes between the two versions. *)
let dated_candidates ?(max_lookups = 8) src (r : Suggestion.t) (bad : info)
    fname fver =
  match S.published src fname fver with
  | None ->
      r.note <-
        Some
          "No passing job to compare with, and no release date for the package."
  | Some day ->
      let opam_text = Option.value ~default:"" (S.opam_file src fname fver) in
      let block = depends_block opam_text in
      let dep_lines =
        List.map
          (function [ d; rest ] -> (d, rest) | _ -> assert false)
          (Rx.findall_groups dependency_line_re block)
      in
      let blob = String.lowercase_ascii (String.concat "\n" bad.error_lines) in
      let deps =
        List.filter_map
          (fun (d, rest) ->
            if
              (not (List.mem d trivial_deps))
              && is_installed bad d
              && not (Rx.test build_doc_dev_re rest)
            then Some d
            else None)
          dep_lines
      in
      let not_named d = not (Util.contains blob (String.lowercase_ascii d)) in
      let deps = Util.sort_by not_named deps in
      (* mentioned in the output first *)
      let newer =
        List.filter_map
          (fun d ->
            let v = List.assoc d bad.installed in
            match S.published src d v with
            | Some dv when dv > day -> Some (not_named d, dv, d, v)
            | _ -> None)
          (Util.take max_lookups deps)
      in
      let newer = List.sort compare newer in
      let cands =
        List.map
          (fun (_, dv, d, v) ->
            (* first version released after [day]: binary search on release dates *)
            let vs =
              Array.of_list
                (List.filter
                   (fun w -> opam_vcmp w v <= 0)
                   (S.versions_or_empty src d))
            in
            let lo = ref 0 and hi = ref (Array.length vs - 1) in
            (* vs[hi] == v is after [day] *)
            while !lo < !hi do
              let mid = (!lo + !hi) / 2 in
              match S.published src d vs.(mid) with
              | Some dm when dm > day -> hi := mid
              | _ -> lo := mid + 1
            done;
            let n = Array.length vs in
            ( not_named d,
              dv,
              d,
              v,
              (if n > 0 then vs.(!lo) else v),
              if n > 0 && !lo > 0 then Some vs.(!lo - 1) else None ))
          (Util.take 3 newer)
      in
      let cands = Util.sort_by (fun (a, b, _, _, _, _) -> (a, b)) cands in
      r.compared_with <-
        Some (Printf.sprintf "release date of %s.%s (%s)" fname fver day);
      r.Suggestion.candidates <-
        Some
          (List.map
             (fun (_, _, d, v, first, before) ->
               Suggestion.candidate ~dep:d ~failing:v ~first_after:first
                 ~old:None
                 ?changelog:(changelog_excerpt src d before (Some first))
                 (Printf.sprintf
                    {|"%s" {with-test & < "%s"}   (new line in depends)|} d
                    first))
             cands);
      r.also_newer <-
        Some (List.map (fun (_, _, d, _) -> d) (Util.drop 3 newer));
      r.confidence <-
        Set
          (Printf.sprintf
             "low (dependencies released after %s.%s; check their changelogs)"
             fname fver);
      if cands = [] then
        r.note <-
          Some
            (Printf.sprintf
               "None of the %d dependencies checked was released after %s.%s \
                (%s)."
               (min (List.length deps) max_lookups)
               fname fver day)

let complete_test_output ?(fetch = fun url cache_dir -> Net.fetch url cache_dir)
    src (r : Suggestion.t) job text (data : data) cache_dir =
  let bad = log_info text in
  let fname, fver = Option.get bad.failing in
  let same =
    List.filter
      (fun k ->
        k.target = job.target && k.revdep = job.revdep && k.kind = job.kind
        && k.category = Ci.Category.passed)
      data.jobs
  in
  let where = "this run" in
  match same with
  | [] -> dated_candidates src r bad fname fver
  | s0 :: _ ->
      let ok = log_info (log_text (fetch s0.url cache_dir)) in
      let opam_text =
        Option.value ~default:""
          (S.opam_file
             ~ref:
               (match bad.base with Some b when b <> "" -> b | _ -> "master")
             src fname fver)
      in
      let deps = Rx.findall dependency_name_re opam_text in
      let diffs =
        List.filter_map
          (fun (p, v) ->
            match List.assoc_opt p ok.installed with
            | Some w
              when w <> "" && w <> v && List.mem p deps
                   && not
                        (List.mem p
                           [
                             "ocaml";
                             "dune";
                             "ocaml-base-compiler";
                             "ocaml-compiler";
                           ]) ->
                Some (p, w, v)
            | _ -> None)
          bad.installed
      in
      let blob = String.lowercase_ascii (String.concat "\n" bad.error_lines) in
      let diffs =
        Util.sort_by
          (fun (p, _, _) ->
            (not (Util.contains blob (String.lowercase_ascii p)), p))
          diffs
      in
      r.compared_with <- Some (String.concat "/" s0.platform ^ " in " ^ where);
      let cands =
        List.filter_map
          (fun (p, good_v, bad_v) ->
            if opam_vcmp bad_v good_v > 0 then begin
              let vs =
                List.filter
                  (fun v -> opam_vcmp v good_v > 0)
                  (S.versions_or_empty src p)
              in
              let first = match vs with v :: _ -> v | [] -> bad_v in
              (* Test-only: keep the main constraint, add a separate with-test line. *)
              Some
                (Suggestion.candidate ~dep:p ~passing:good_v ~failing:bad_v
                   ~old:None
                   ?changelog:
                     (changelog_excerpt src p (Some good_v) (Some first))
                   (Printf.sprintf
                      {|"%s" {with-test & < "%s"}   (new line in depends)|} p
                      first))
            end
            else None)
          (Util.take 4 diffs)
      in
      r.candidates <- Some cands;
      r.confidence <-
        Set
          "low (dependencies whose version differs between a passing job and \
           the failing one)"

(* opam's plan lines: "- downgrade dune 3.24.2 to 3.15.3 [required by miaou-core]". *)
let plan_line =
  Rx.v ~flags:[ Rx.M ]
    {|^\s*- (install|downgrade|upgrade|recompile)\s+([\w+-]+)\s+(\S+)(?:\s+to\s+(\S+))?[^\[\n]*(?:\[(?:required by|uses) ([^\]]*)\])?|}

(** (name -> (from, to) for the downgraded packages, name -> packages requiring
    it). *)
let opam_plan text =
  let downgraded = Odict.create () and required_by = Odict.create () in
  List.iter
    (fun m ->
      let action = Rx.group m 1 and name = Rx.group m 2 and v1 = Rx.group m 3 in
      (match Rx.group_opt m 4 with
      | Some v2 when action = "downgrade" && v2 <> "" ->
          Odict.set downgraded name (v1, v2)
      | _ -> ());
      match Rx.group_opt m 5 with
      | Some by when by <> "" && Util.contains (Rx.group0 m) "required by" ->
          let cur = Option.value ~default:[] (Odict.get required_by name) in
          let add =
            List.filter_map
              (fun p -> match String.trim p with "" -> None | p -> Some p)
              (String.split_on_char ',' (Util.replace by "etc." ""))
          in
          Odict.set required_by name (List.sort_uniq compare (cur @ add))
      | _ -> ())
    (Rx.finditer plan_line text);
  (Odict.to_list downgraded, Odict.to_list required_by)

let name_re = Rx.v {|\b([A-Za-z][\w-]+)|}

let lower_bounds_rule src job text (r : Suggestion.t) pr_packages pr_sha :
    Suggestion.t option =
  if job.kind <> Lower_bounds then None
  else
    let info = log_info text in
    match info.failing with
    | None -> None
    | Some (fname, fver) ->
        let downgraded, required_by = opam_plan text in
        let siblings =
          List.fold_left
            (fun acc p ->
              (fst (split_pkg p), p)
              :: List.remove_assoc (fst (split_pkg p)) acc)
            [] pr_packages
        in
        let target =
          match List.assoc_opt fname siblings with
          | Some t -> t
          | None -> Option.value ~default:"" (failing_pkg job)
        in
        let tname, tver = split_pkg target in
        let opam =
          match
            if List.mem target pr_packages then
              S.opam_file ~ref:pr_sha src tname tver
            else None
          with
          | Some t when t <> "" -> Some t
          | _ -> S.opam_file src tname tver
        in
        let deps =
          List.filter
            (fun d -> (not (List.mem d trivial_deps)) || d = "dune")
            (direct_deps opam)
        in
        if fname <> tname then begin
          (* A: a dependency failed to build. Walk "required by" up to a direct
           dependency of the package that was downgraded. *)
          let seen = Hashtbl.create 8 and path = Hashtbl.create 8 in
          Hashtbl.replace seen fname ();
          Hashtbl.replace path fname None;
          (* Breadth first: the first package of a level that is a downgraded
           direct dependency, else the packages requiring those of the level
           (each package with the one it was reached from, in [path]). *)
          let rec walk = function
            | [] -> None
            | level -> (
                match
                  List.find_opt
                    (fun p -> List.mem p deps && List.mem_assoc p downgraded)
                    level
                with
                | Some p -> Some p
                | None ->
                    walk
                      (List.concat_map
                         (fun p ->
                           List.filter
                             (fun q ->
                               (not (Hashtbl.mem seen q))
                               && begin
                                 Hashtbl.replace seen q ();
                                 Hashtbl.replace path q (Some p);
                                 true
                               end)
                             (Option.value ~default:[]
                                (List.assoc_opt p required_by)))
                         level))
          in
          match walk [ fname ] with
          | None -> None
          | Some found ->
              let rec chain p acc =
                match p with
                | None -> List.rev acc
                | Some p -> chain (Hashtbl.find path p) (p :: acc)
              in
              let chain = chain (Some found) [] in
              let used, down = List.assoc found downgraded in
              let old, new_ = S.constraint_edit opam found ">=" used in
              let via =
                if List.length chain > 2 then
                  Printf.sprintf " (through %s)"
                    (String.concat " -> "
                       (Util.sublist ~start:1 ~stop:(-1) chain))
                else ""
              in
              let what =
                if found = fname then fname ^ "." ^ fver
                else
                  Printf.sprintf "%s.%s, which pulls in %s.%s%s" found down
                    fname fver via
              in
              Some
                (Suggestion.make
                   ([
                      `Variant job.variant;
                      `Url job.url;
                      `Kind job.kind;
                      `Experimental job.experimental;
                      `Rule Rule.Lower_bound;
                      `Failing (Some target);
                    ]
                   @ Where.pr_or_other target (List.mem target pr_packages)
                   @ [
                       `Dep found;
                       `Old old;
                       `New (Some new_);
                       `Confidence "medium";
                       `Error
                         (Printf.sprintf
                            "The lower-bounds job installs %s, which fails to \
                             build"
                            what);
                       `Note
                         (Printf.sprintf
                            "%s.%s, used in the other jobs, builds. A version \
                             in between may build too: this is the bound the \
                             CI results support."
                            found used);
                     ]))
        end
        else if
          r.rule <> None
          || Suggestion.is_set r.Suggestion.new_
          || Suggestion.list r.candidates <> []
        then None
        else begin
          (* B: the package itself fails, and no rule explains the error. *)
          let err = String.concat "\n" info.error_lines in
          let mentioned =
            Util.dedup (List.map S.norm (Rx.findall name_re err))
          in
          let named d =
            let n = S.norm d in
            List.mem n mentioned
            || String.length n >= 3
               && List.exists
                    (fun m -> String.starts_with ~prefix:n m)
                    mentioned
          in
          let cands = List.filter (fun d -> List.mem_assoc d downgraded) deps in
          if cands = [] then None
          else begin
            let cands = Util.sort_by (fun d -> (not (named d), d)) cands in
            let out =
              List.map
                (fun d ->
                  let used, down = List.assoc d downgraded in
                  let old, new_ = S.constraint_edit opam d ">=" used in
                  ( d,
                    Suggestion.candidate ~dep:d ~passing:used ~failing:down ~old
                      new_ ))
                (Util.take 5 cands)
            in
            let names =
              List.filter_map
                (fun (d, _) -> if named d then Some d else None)
                out
            in
            Suggestion.update r
              ([
                 `Rule Rule.Lower_bound;
                 `Candidates (List.map snd out);
                 `Compared_with "the other jobs";
                 `Confidence "low";
                 `Failing (Some target);
               ]
              @ Where.pr_or_other target (List.mem target pr_packages)
              @ [
                  `Error
                    (match r.error with
                    | Some e when e <> "" -> e
                    | _ -> (
                        match
                          List.find_opt
                            (fun l -> Util.contains l "Error")
                            info.error_lines
                        with
                        | Some l -> l
                        | None -> "build error"));
                  `Note
                    ((if names <> [] then
                        "The error names " ^ String.concat ", " names
                        ^ ": most likely the lower bound to raise. "
                      else "")
                    ^ "These direct dependencies are older in the lower-bounds \
                       job than in the other jobs, which pass.");
                ]);
            None
          end
        end

let upper_bound_re = Rx.v {|(<=?)\s*"([^"]+)"|}

let filtered_dependency_re =
  Rx.v ~flags:[ Rx.M ] {|^[ \t]*"([\w+-]+)"[ \t]*\{([^}]*)\}[^\n]*$|}

(** dependency -> (line, [(op, version)...]) for the dependencies with an upper
    bound (< or <=) in an opam file's depends. *)
let upper_bounds opam_text =
  let block = depends_block (Option.value ~default:"" opam_text) in
  let out = Odict.create () in
  List.iter
    (fun m ->
      let ups =
        List.map
          (function [ a; b ] -> (a, b) | _ -> assert false)
          (Rx.findall_groups upper_bound_re (Rx.group m 2))
      in
      if ups <> [] then
        Odict.set out (Rx.group m 1) (String.trim (Rx.group0 m), ups))
    (Rx.finditer filtered_dependency_re block);
  Odict.to_list out

let error_context_path_re =
  Rx.v
    {|\s*(=== ERROR|context |path |command |exit-code |env-file |output-file |## output|\(cd _build|\[\.\.\.)|}

let dune_language_lang_re =
  Rx.v ~flags:[ Rx.I ] {|dune language|lang dune|dune-project|version of dune|}

let file_re = Rx.v {|File "[^"]*"|}
let path_re = Rx.v {|(?:[\w.+-]*/)+[\w.+-]+|}
let identifier_re = Rx.v {|[A-Za-z][\w-]+|}

(** Does the error itself (not the command that failed) name the package [dep],
    or a module of it? *)
let error_mentions (info : info) dep =
  let lines =
    List.filter
      (fun l ->
        (not (Rx.test_match error_context_path_re l))
        && not (Util.contains l "dune build"))
      info.error_lines
  in
  let text = String.concat "\n" lines in
  if dep = "dune" then
    (* named in many errors ("your dune file"): only a dune version problem counts *)
    Rx.test dune_language_lang_re text
  else
    let text = Rx.sub file_re "" text in
    (* dune's locations: File "src/dune", line 3 *)
    let text = Rx.sub path_re "" text in
    (* paths *)
    let words = List.map S.norm (Rx.findall identifier_re text) in
    let d = S.norm dep in
    List.mem d words
    || String.length d >= 4
       && List.exists (fun w -> String.starts_with ~prefix:d w) words

(* The previous release of a package of the PR. *)
type prev_release = {
  prev : string; (* its version *)
  cur : string; (* the opam file of the PR's release *)
  prev_up : (string * (string * (string * string) list)) list;
      (* the [upper_bounds] of each *)
  cur_up : (string * (string * (string * string) list)) list;
}

let dependency_re =
  Rx.v ~flags:[ Rx.M ] {|^[ \t]*"([\w+-]+)"[ \t]*(?:\{([^}]*)\})?|}

let lower_bound_re = Rx.v {|>=?\s*"([^"]+)"|}

let previous_release_bound src job (info : info) pr_packages pr_sha
    (cache : (string, prev_release option) Hashtbl.t) : Suggestion.t option =
  match info.failing with
  | None -> None
  | Some (fname, fver) ->
      let pkg = fname ^ "." ^ fver in
      if not (List.mem pkg pr_packages) then None
      else begin
        let release =
          match Hashtbl.find_opt cache pkg with
          | Some r -> r
          | None ->
              let older =
                List.filter
                  (fun v -> opam_vcmp v fver < 0)
                  (S.versions_or_empty src fname)
              in
              let prev = S.max_version older in
              let cur = S.opam_file ~ref:pr_sha src fname fver in
              let prev_text =
                match prev with
                | Some p -> S.opam_file src fname p
                | None -> None
              in
              let r =
                match (prev, cur) with
                | Some p, Some c when p <> "" && c <> "" ->
                    Some
                      {
                        prev = p;
                        cur = c;
                        prev_up = upper_bounds prev_text;
                        cur_up = upper_bounds cur;
                      }
                | _ -> None
              in
              Hashtbl.replace cache pkg r;
              r
        in
        match release with
        | None -> None
        | Some { prev; cur; prev_up; cur_up } ->
            let block = depends_block cur in
            let cur_lines = Odict.create () in
            List.iter
              (fun m -> Odict.set cur_lines (Rx.group m 1) (Rx.group m 2))
              (Rx.finditer dependency_re block);
            List.find_map
              (fun (dep, (line, ups)) ->
                if List.mem_assoc dep cur_up then None
                else
                  let cl = Option.value ~default:"" (Odict.get cur_lines dep) in
                  let mentioned = Rx.findall quoted_re cl in
                  let lowers = Rx.findall lower_bound_re cl in
                  if
                    List.exists (fun (_, v) -> List.mem v mentioned) ups
                    || List.exists
                         (fun lo ->
                           List.exists (fun (_, v) -> opam_vcmp lo v >= 0) ups)
                         lowers
                  then
                    None
                    (* the new release changed this constraint on purpose (e.g. needs a newer API) *)
                  else
                    match installed_of info dep with
                    | None | Some "" -> None
                    | Some _ when not (error_mentions info dep) -> None
                    | Some have -> (
                        let excluded =
                          List.filter
                            (fun (op, v) ->
                              if op = "<" then opam_vcmp have v >= 0
                              else opam_vcmp have v > 0)
                            ups
                        in
                        match excluded with
                        | [] -> None
                        | (op, v) :: _ ->
                            let old, new_ =
                              S.constraint_edit (Some cur) dep op v
                            in
                            Some
                              (Suggestion.make
                                 ([
                                    `Variant job.variant;
                                    `Url job.url;
                                    `Kind job.kind;
                                    `Experimental job.experimental;
                                    `Rule Rule.Previous_release_bound;
                                    `Failing (Some pkg);
                                  ]
                                 @ Where.fix_where Pr "this PR"
                                 @ [
                                     `Dep dep;
                                     `Old old;
                                     `New (Some new_);
                                     `Confidence "medium";
                                     `Error
                                       (Printf.sprintf
                                          "%s.%s is installed; the previous \
                                           release %s.%s excludes it on master \
                                           (`%s`)"
                                          dep have fname prev line);
                                     `Note
                                       (Printf.sprintf
                                          "The previous release has this bound \
                                           in opam-repository (often added \
                                           after a breakage) and this one does \
                                           not. If the new release really \
                                           supports %s.%s, the failure has \
                                           another cause."
                                          dep have);
                                   ]))))
              (List.sort (fun (a, _) (b, _) -> compare a b) prev_up)
      end
