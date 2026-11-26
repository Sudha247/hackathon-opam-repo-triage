(* Regular expressions on ocaml-re, in Perl syntax; see rx.mli. *)

type flag = I | M | S

type t = {
  re : Re.re;  (** unanchored *)
  at_start : Re.re Lazy.t;  (** anchored at the search position: [match_] *)
  whole : Re.re Lazy.t;  (** anchored at both ends: [fullmatch] *)
  ngroups : int;
  names : (string * int) list;
}

let options flags =
  (if List.mem M flags then [ `Multiline ] else [ `Dollar_endonly ])
  @ (if List.mem S flags then [ `Dotall ] else [])
  @ if List.mem I flags then [ `Caseless ] else []

let re ?(flags = []) pattern =
  try Re.Perl.re ~opts:(options flags) pattern with
  | Re.Perl.Parse_error ->
      invalid_arg (Printf.sprintf "Rx: cannot parse the pattern %S" pattern)
  | Re.Perl.Not_supported ->
      invalid_arg
        (Printf.sprintf "Rx: unsupported syntax in the pattern %S" pattern)

let of_re r =
  let re = Re.compile r in
  {
    re;
    at_start = lazy (Re.compile (Re.seq [ Re.start; r ]));
    whole = lazy (Re.compile (Re.seq [ Re.start; r; Re.eos ]));
    ngroups = Re.group_count re - 1;
    names = Re.group_names re;
  }

let v ?flags pattern = of_re (re ?flags pattern)

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

let search ?(pos = 0) t s =
  if pos > String.length s then None else wrap t (Re.exec_opt ~pos t.re s)

let match_ ?(pos = 0) t s =
  if pos > String.length s then None
  else wrap t (Re.exec_opt ~pos (Lazy.force t.at_start) s)

let fullmatch t s = wrap t (Re.exec_opt (Lazy.force t.whole) s)
let test t s = Re.execp t.re s
let test_match t s = Option.is_some (match_ t s)
let finditer t s = List.of_seq (Seq.map (fun g -> { g; t }) (Re.Seq.all t.re s))

let findall t s =
  List.map
    (fun m -> if t.ngroups = 0 then group0 m else group m 1)
    (finditer t s)

let findall_groups t s =
  List.map
    (fun m -> List.init m.t.ngroups (fun i -> group m (i + 1)))
    (finditer t s)

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
          while !j < n && !j < !i + 3 && Util.is_digit template.[!j] do
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

let subf ?count t f s =
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

let sub ?count t template s = subf ?count t (fun m -> expand m template) s

let split ?maxsplit t s =
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
