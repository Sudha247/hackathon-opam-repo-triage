(* Regular expressions with the API and syntax of Python's [re] module, on
   ocaml-re (Perl syntax). Patterns are compiled once and memoised.

   Supported: Python's syntax as ocaml-re's Perl parser reads it, with
   leading inline flags ((?m), (?i), (?s)), (?P<name>...) groups, \Z, and
   Python's meaning of $ (end, or before a final newline). Not supported:
   lookahead/lookbehind and backreferences in patterns (those raise
   [Invalid_argument]: such patterns are written another way by hand).

   Matching is on bytes: \w, \s and character classes do not see non-ASCII
   characters as single characters (write alternatives for those). *)

type flag = I | M | S

type t = {
  pattern : string;
  re : Re.re;  (** unanchored *)
  at_start : Re.re Lazy.t;  (** anchored at the search position: [match_] *)
  whole : Re.re Lazy.t;  (** anchored at both ends: [fullmatch] *)
  ngroups : int;
  names : (string * int) list;
}

(* Leading inline flags, as Python allows them. *)
let rec leading_flags flags s =
  let n = String.length s in
  if n >= 4 && s.[0] = '(' && s.[1] = '?' then
    match String.index_opt s ')' with
    | Some j
      when j > 2
           && String.for_all
                (fun c -> c = 'i' || c = 'm' || c = 's' || c = 'x')
                (String.sub s 2 (j - 2)) ->
        let fl = String.sub s 2 (j - 2) in
        let flags =
          (if String.contains fl 'i' then [ I ] else [])
          @ (if String.contains fl 'm' then [ M ] else [])
          @ (if String.contains fl 's' then [ S ] else [])
          @ flags
        in
        leading_flags flags (String.sub s (j + 1) (n - j - 1))
    | _ -> (flags, s)
  else (flags, s)

(* Python syntax -> ocaml-re's Perl syntax, outside and inside brackets. *)
let translate s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  let in_bracket = ref false in
  while !i < n do
    let c = s.[!i] in
    if c = '\\' && !i + 1 < n then begin
      let d = s.[!i + 1] in
      (match d with
      | 'Z' when not !in_bracket -> Buffer.add_string b "\\z"
      | '1' .. '9' when not !in_bracket ->
          invalid_arg ("Rx: backreference in " ^ s)
      | 'f' -> Buffer.add_string b (if !in_bracket then "\x0c" else "\\f")
      | 'v' -> Buffer.add_string b (if !in_bracket then "\x0b" else "\\x0b")
      | _ ->
          Buffer.add_char b '\\';
          Buffer.add_char b d);
      i := !i + 2
    end
    else if !in_bracket then begin
      if c = ']' then in_bracket := false;
      Buffer.add_char b c;
      incr i
    end
    else if c = '[' then begin
      in_bracket := true;
      Buffer.add_char b c;
      incr i;
      (* A ']' first in the class (after an optional '^') is a literal. *)
      if !i < n && s.[!i] = '^' then (
        Buffer.add_char b '^';
        incr i);
      if !i < n && s.[!i] = ']' then (
        Buffer.add_string b "\\]";
        incr i)
    end
    else if c = '(' && !i + 2 < n && s.[!i + 1] = '?' then
      begin match s.[!i + 2] with
      | 'P' when !i + 3 < n && s.[!i + 3] = '<' ->
          Buffer.add_string b "(?<";
          i := !i + 4
      | '=' | '!' -> invalid_arg ("Rx: lookahead in " ^ s)
      | '<' when !i + 3 < n && (s.[!i + 3] = '=' || s.[!i + 3] = '!') ->
          invalid_arg ("Rx: lookbehind in " ^ s)
      | _ ->
          Buffer.add_char b c;
          incr i
      end
    else begin
      Buffer.add_char b c;
      incr i
    end
  done;
  Buffer.contents b

let cache : (flag list * string, t) Hashtbl.t = Hashtbl.create 256
let lock = Mutex.create ()

let build flags pattern =
  let flags, body = leading_flags flags pattern in
  let multiline = List.mem M flags in
  let opts =
    (if multiline then [ `Multiline ] else [ `Dollar_endonly ])
    @ (if List.mem S flags then [ `Dotall ] else [])
    @ if List.mem I flags then [ `Caseless ] else []
  in
  let r =
    try Re.Perl.re ~opts (translate body)
    with Re.Perl.Parse_error | Re.Perl.Not_supported ->
      invalid_arg ("Rx: cannot parse " ^ pattern)
  in
  let re = Re.compile r in
  {
    pattern;
    re;
    at_start = lazy (Re.compile (Re.seq [ Re.start; r ]));
    whole = lazy (Re.compile (Re.seq [ Re.start; r; Re.eos ]));
    ngroups = Re.group_count re - 1;
    names = Re.group_names re;
  }

(** The compiled form of a pattern (memoised). *)
let compile ?(flags = []) pattern =
  Mutex.lock lock;
  let r =
    match Hashtbl.find_opt cache (flags, pattern) with
    | Some t -> Ok t
    | None -> (
        match build flags pattern with
        | t ->
            Hashtbl.add cache (flags, pattern) t;
            Ok t
        | exception e -> Error e)
  in
  Mutex.unlock lock;
  match r with Ok t -> t | Error e -> raise e

(* ------------------------------------------------------------------------ *)
(* Match objects *)

type m = { g : Re.Group.t; t : t }

let group_opt m i = Re.Group.get_opt m.g i
let group m i = Option.value ~default:"" (group_opt m i)
let group0 m = Re.Group.get m.g 0

let named m name =
  match List.assoc_opt name m.t.names with
  | Some i -> group_opt m i
  | None -> None

let groups m = List.init m.t.ngroups (fun i -> group_opt m (i + 1))
let start ?(i = 0) m = fst (Re.Group.offset m.g i)
let end_ ?(i = 0) m = snd (Re.Group.offset m.g i)
let span ?(i = 0) m = Re.Group.offset m.g i

(* ------------------------------------------------------------------------ *)
(* Searching *)

let wrap t = Option.map (fun g -> { g; t })

let search ?flags ?(pos = 0) pattern s =
  let t = compile ?flags pattern in
  if pos > String.length s then None else wrap t (Re.exec_opt ~pos t.re s)

let match_ ?flags ?(pos = 0) pattern s =
  let t = compile ?flags pattern in
  if pos > String.length s then None
  else wrap t (Re.exec_opt ~pos (Lazy.force t.at_start) s)

let fullmatch ?flags pattern s =
  let t = compile ?flags pattern in
  wrap t (Re.exec_opt (Lazy.force t.whole) s)

let test ?flags pattern s = Re.execp (compile ?flags pattern).re s
let test_match ?flags pattern s = Option.is_some (match_ ?flags pattern s)

let finditer ?flags pattern s =
  let t = compile ?flags pattern in
  List.of_seq (Seq.map (fun g -> { g; t }) (Re.Seq.all t.re s))

let findall ?flags pattern s =
  let t = compile ?flags pattern in
  List.map
    (fun m -> if t.ngroups = 0 then group0 m else group m 1)
    (finditer ?flags pattern s)

let findall_groups ?flags pattern s =
  List.map
    (fun m -> List.init m.t.ngroups (fun i -> group m (i + 1)))
    (finditer ?flags pattern s)

(* Python's replacement templates: \1, \g<1>, \g<name>, \n, \\ ... *)
let expand m template =
  let b = Buffer.create (String.length template) in
  let n = String.length template in
  let i = ref 0 in
  while !i < n do
    let c = template.[!i] in
    if c = '\\' && !i + 1 < n then begin
      let d = template.[!i + 1] in
      match d with
      | '0' .. '9' ->
          let j = ref (!i + 1) in
          while !j < n && !j < !i + 3 && Py.is_digit template.[!j] do
            incr j
          done;
          Buffer.add_string b
            (group m
               (int_of_string (String.sub template (!i + 1) (!j - !i - 1))));
          i := !j
      | 'g' when !i + 2 < n && template.[!i + 2] = '<' ->
          let j = String.index_from template (!i + 3) '>' in
          let name = String.sub template (!i + 3) (j - !i - 3) in
          let v =
            match int_of_string_opt name with
            | Some k -> group_opt m k
            | None -> named m name
          in
          Buffer.add_string b (Option.value ~default:"" v);
          i := j + 1
      | 'n' ->
          Buffer.add_char b '\n';
          i := !i + 2
      | 't' ->
          Buffer.add_char b '\t';
          i := !i + 2
      | 'r' ->
          Buffer.add_char b '\r';
          i := !i + 2
      | '\\' ->
          Buffer.add_char b '\\';
          i := !i + 2
      | _ ->
          Buffer.add_char b '\\';
          Buffer.add_char b d;
          i := !i + 2
    end
    else (
      Buffer.add_char b c;
      incr i)
  done;
  Buffer.contents b

(* The matches of [t] in [s], the first [limit] ones if [limit] is positive. *)
let matches ?limit t s =
  let all = Re.Seq.all t.re s in
  match limit with Some n when n > 0 -> Seq.take n all | _ -> all

let subf ?flags ?count pattern f s =
  let t = compile ?flags pattern in
  let b = Buffer.create (String.length s) in
  let last =
    Seq.fold_left
      (fun last g ->
        let m = { g; t } in
        let a, e = span m in
        Buffer.add_string b (String.sub s last (a - last));
        Buffer.add_string b (f m);
        e)
      0 (matches ?limit:count t s)
  in
  Buffer.add_string b (String.sub s last (String.length s - last));
  Buffer.contents b

let sub ?flags ?count pattern template s =
  subf ?flags ?count pattern (fun m -> expand m template) s

let split ?flags ?maxsplit pattern s =
  let t = compile ?flags pattern in
  let parts, last =
    Seq.fold_left
      (fun (parts, last) g ->
        let m = { g; t } in
        let a, e = span m in
        let groups = List.init t.ngroups (fun i -> group m (i + 1)) in
        (List.rev_append groups (String.sub s last (a - last) :: parts), e))
      ([], 0)
      (matches ?limit:maxsplit t s)
  in
  List.rev (String.sub s last (String.length s - last) :: parts)

let escape s =
  let b = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      if Py.is_alnum c || c = '_' || Char.code c >= 128 then Buffer.add_char b c
      else (
        Buffer.add_char b '\\';
        Buffer.add_char b c))
    s;
  Buffer.contents b
