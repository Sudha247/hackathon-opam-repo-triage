(* opam filters on the platforms of CI jobs (the [available] field). *)

open Ci

(* opam's os-distribution and os-family of the CI distributions (those known for sure). *)
let os_distribution =
  [
    ("debian", "debian");
    ("ubuntu", "ubuntu");
    ("fedora", "fedora");
    ("centos", "centos");
    ("alpine", "alpine");
    ("archlinux", "arch");
    ("cygwin", "cygwin");
    ("msys2", "msys2");
    ("macos-homebrew", "homebrew");
    ("freebsd", "freebsd");
    ("opensuse", "opensuse-leap");
    ("opensuse-tumbleweed", "opensuse-tumbleweed");
  ]

(* opam's os-family (OpamSysPoll.poll_os_family): on Linux the first word of
   ID_LIKE in /etc/os-release, else the distribution's ID; "bsd" and
   "windows"; elsewhere the os-distribution. Its os-distribution on Linux is
   ID. CentOS Stream 9 and 10 have ID_LIKE="rhel fedora"
   (centos-stream-release.spec, branches c9s and c10s on
   gitlab.com/redhat/centos-stream/rpms). openSUSE: the CI's "opensuse-16.0"
   (and Leap 15's "opensuse-15.N") is Leap, ID="opensuse-leap" and
   ID_LIKE="suse opensuse" (Leap-release.spec, 000release-packages of
   src.opensuse.org/openSUSE/Leap, branch leap-16.0; Leap 15.6's
   /etc/os-release is the same); Tumbleweed has ID="opensuse-tumbleweed" and
   ID_LIKE="opensuse suse" (openSUSE-release.spec, branch factory of
   src.opensuse.org/pool/000release-packages): os-family "suse" on Leap,
   "opensuse" on Tumbleweed. *)
let os_family =
  [
    ("debian", "debian");
    ("ubuntu", "debian");
    ("alpine", "alpine");
    ("archlinux", "arch");
    ("cygwin", "windows");
    ("msys2", "windows");
    ("freebsd", "bsd");
    ("fedora", "fedora");
    ("centos", "rhel");
    ("macos-homebrew", "homebrew");
    ("opensuse", "suse");
    ("opensuse-tumbleweed", "opensuse");
  ]

let unstable_testing_re = Rx.v {|-[\d.]+$|-(unstable|testing)$|}

let platform_vars job =
  let d = job_dims job in
  let dist =
    Rx.sub unstable_testing_re "" (Option.value ~default:"" d.distro)
  in
  let arch =
    match d.arch with "amd64" -> "x86_64" | "aarch64" -> "arm64" | a -> a
  in
  [ ("os", d.os); ("arch", arch) ]
  @ (match List.assoc_opt dist os_distribution with
    | Some v -> [ ("os-distribution", v) ]
    | None -> [])
  @
  match List.assoc_opt dist os_family with
  | Some v -> [ ("os-family", v) ]
  | None -> []

let filter_token =
  Rx.v {|\s*("(?:[^"\\]|\\.)*"|<=|>=|!=|[=<>&|!()]|[A-Za-z_][\w:+.-]*)|}

exception Bad

let eval_filter text env =
  let text = String.trim (Option.value ~default:"" text) in
  let text =
    if String.starts_with ~prefix:"[" text && String.ends_with ~suffix:"]" text
    then String.sub text 1 (String.length text - 2)
    else text
  in
  let rec tokens acc pos =
    if String.trim (Util.slice ~start:pos text) = "" then Some (List.rev acc)
    else
      match Rx.match_ ~pos filter_token text with
      | None -> None
      | Some m -> tokens (Rx.group m 1 :: acc) (Rx.end_ m)
  in
  match tokens [] 0 with
  | None -> None
  | Some [] -> Some true
  | Some toks -> (
      let toks = Array.of_list toks in
      let i = ref 0 in
      let peek () = if !i < Array.length toks then Some toks.(!i) else None in
      let ops = [ "="; "!="; "<"; "<="; ">"; ">="; "&"; "|"; "!"; "("; ")" ] in
      let value () =
        match peek () with
        | None -> raise Bad
        | Some t when List.mem t ops -> raise Bad
        | Some t ->
            incr i;
            if String.starts_with ~prefix:"\"" t then
              Some (String.sub t 1 (String.length t - 2))
            else if t = "true" || t = "false" then Some t
            else List.assoc_opt t env
      in
      let rec atom () =
        match peek () with
        | Some "!" ->
            incr i;
            Option.map not (atom ())
        | Some "(" ->
            incr i;
            let v = disj () in
            if peek () <> Some ")" then raise Bad;
            incr i;
            v
        | _ -> (
            let a = value () in
            match peek () with
            | Some (("=" | "!=" | "<" | "<=" | ">" | ">=") as op) -> (
                incr i;
                let b = value () in
                match (a, b) with
                | Some a, Some b ->
                    let c =
                      if op = "=" || op = "!=" then if a = b then 0 else 1
                      else opam_vcmp a b
                    in
                    Some
                      (match op with
                      | "=" -> c = 0
                      | "!=" -> c <> 0
                      | "<" -> c < 0
                      | "<=" -> c <= 0
                      | ">" -> c > 0
                      | _ -> c >= 0)
                | _ -> None)
            | _ -> (
                match a with
                | Some "true" -> Some true
                | Some "false" -> Some false
                | _ -> None))
      and conj () =
        let vs = ref [ atom () ] in
        while peek () = Some "&" do
          incr i;
          vs := atom () :: !vs
        done;
        if List.mem (Some false) !vs then Some false
        else if List.mem None !vs then None
        else Some true
      and disj () =
        let vs = ref [ conj () ] in
        while peek () = Some "|" do
          incr i;
          vs := conj () :: !vs
        done;
        if List.mem (Some true) !vs then Some true
        else if List.mem None !vs then None
        else Some false
      in
      match disj () with
      | v -> if !i = Array.length toks then v else None
      | exception Bad -> None)
