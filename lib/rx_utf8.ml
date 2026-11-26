(* Regular expressions on UTF-8 text, as Python's [re] sees it.

   Python's \w, \s and \b see characters, ocaml-re sees bytes. To match like
   Python on text with non-ASCII characters, the patterns run on a copy of
   the text where each non-ASCII character is one byte: \xc0 for a word
   character (a letter for ocaml-re), \x0b for whitespace, \x80 otherwise;
   the results are taken from the original text through the offsets. *)

(* Python's \w and \s on a character: a letter or a number (general category
   L*, or a numeric type), and white space. *)
let is_word u =
  let u = Uchar.of_int u in
  (match Uucp.Gc.general_category u with
    | `Lu | `Ll | `Lt | `Lm | `Lo -> true
    | _ -> false)
  || Uucp.Num.numeric_type u <> `None

let is_space u = Uucp.White.is_white_space (Uchar.of_int u)
let ascii s = String.for_all (fun c -> Char.code c < 0x80) s

(* The one-byte-per-character copy of [s], and the offset in [s] of each of its bytes (and of its end). *)
let encode s =
  let n = String.length s in
  let b = Buffer.create n and offs = ref [] in
  let i = ref 0 in
  while !i < n do
    let u, k = Json.utf8_at s !i in
    offs := !i :: !offs;
    Buffer.add_char b
      (if u < 0x80 then Char.chr u
       else if is_space u then '\x0b'
       else if is_word u then '\xc0'
       else '\x80');
    i := !i + k
  done;
  (Buffer.contents b, Array.of_list (List.rev (n :: !offs)))

type m = { m : Rx.m; s : string; offs : int array }

let group_opt m i =
  match Rx.group_opt m.m i with
  | None -> None
  | Some _ ->
      let a, e = Rx.span ~i m.m in
      Some (String.sub m.s m.offs.(a) (m.offs.(e) - m.offs.(a)))

let group m i = Option.value ~default:"" (group_opt m i)
let group0 m = group m 0
let wrap s offs = Option.map (fun x -> { m = x; s; offs })

let prepare s =
  if ascii s then (s, Array.init (String.length s + 1) Fun.id) else encode s

let search t s =
  let e, offs = prepare s in
  wrap s offs (Rx.search t e)

let match_ t s =
  let e, offs = prepare s in
  wrap s offs (Rx.match_ t e)

let subf t f s =
  let e, offs = prepare s in
  let b = Buffer.create (String.length s) in
  let last =
    List.fold_left
      (fun last x ->
        let a, z = Rx.span x in
        Buffer.add_string b (String.sub s last (offs.(a) - last));
        Buffer.add_string b (f { m = x; s; offs });
        offs.(z))
      0 (Rx.finditer t e)
  in
  Buffer.add_string b (String.sub s last (String.length s - last));
  Buffer.contents b

let group_ref = Rx.v {|\\(\d)|}

let sub t template s =
  subf t
    (fun m ->
      Rx.subf group_ref
        (fun d -> group m (int_of_string (Rx.group d 1)))
        template)
    s
