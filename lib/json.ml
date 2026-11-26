(* JSON values (Yojson.Basic), printed exactly as Python's [json.dumps]
   prints them (ASCII only, ", " and ": " separators, or [indent]), so that
   the files the tool and the page scripts write keep their format. *)

type t = Yojson.Basic.t

let parse s : t = Yojson.Basic.from_string s
let parse_opt s = try Some (parse s) with _ -> None

let utf8_at s i =
  let d = String.get_utf_8_uchar s i in
  (Uchar.to_int (Uchar.utf_decode_uchar d), Uchar.utf_decode_length d)

let add_string ?(ensure_ascii = true) b s =
  Buffer.add_char b '"';
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    match c with
    | '"' ->
        Buffer.add_string b "\\\"";
        incr i
    | '\\' ->
        Buffer.add_string b "\\\\";
        incr i
    | '\n' ->
        Buffer.add_string b "\\n";
        incr i
    | '\r' ->
        Buffer.add_string b "\\r";
        incr i
    | '\t' ->
        Buffer.add_string b "\\t";
        incr i
    | '\b' ->
        Buffer.add_string b "\\b";
        incr i
    | '\x0c' ->
        Buffer.add_string b "\\f";
        incr i
    | c when Char.code c < 0x20 || (ensure_ascii && Char.code c = 0x7f) ->
        Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c));
        incr i
    | c when Char.code c < 0x80 ->
        Buffer.add_char b c;
        incr i
    | _ ->
        let u, k = utf8_at s !i in
        if not ensure_ascii then
          if Uchar.utf_decode_is_valid (String.get_utf_8_uchar s !i) then
            Buffer.add_string b (String.sub s !i k)
          else Buffer.add_string b "\xef\xbf\xbd"
        else if u >= 0x10000 then begin
          let v = u - 0x10000 in
          Buffer.add_string b
            (Printf.sprintf "\\u%04x\\u%04x"
               (0xD800 lor (v lsr 10))
               (0xDC00 lor (v land 0x3FF)))
        end
        else Buffer.add_string b (Printf.sprintf "\\u%04x" u);
        i := !i + k
  done;
  Buffer.add_char b '"'

let dumps ?indent ?(sort_keys = false) ?(ensure_ascii = true) ?(compact = false)
    (v : t) =
  let b = Buffer.create 4096 in
  let item_sep, key_sep =
    if compact then (",", ":")
    else match indent with None -> (", ", ": ") | Some _ -> (",", ": ")
  in
  let rec go level (v : t) =
    match v with
    | `Null -> Buffer.add_string b "null"
    | `Bool true -> Buffer.add_string b "true"
    | `Bool false -> Buffer.add_string b "false"
    | `Int n -> Buffer.add_string b (string_of_int n)
    | `Float f ->
        Buffer.add_string b
          (if Float.is_nan f then "NaN"
           else if f = Float.infinity then "Infinity"
           else if f = Float.neg_infinity then "-Infinity"
           else Py.repr_float f)
    | `String s -> add_string ~ensure_ascii b s
    | `List [] -> Buffer.add_string b "[]"
    | `Assoc [] -> Buffer.add_string b "{}"
    | `List l ->
        Buffer.add_char b '[';
        List.iteri
          (fun i x ->
            if i > 0 then Buffer.add_string b item_sep;
            newline (level + 1);
            go (level + 1) x)
          l;
        newline level;
        Buffer.add_char b ']'
    | `Assoc kv ->
        let kv =
          if sort_keys then
            List.stable_sort (fun (a, _) (b, _) -> compare a b) kv
          else kv
        in
        Buffer.add_char b '{';
        List.iteri
          (fun i (k, x) ->
            if i > 0 then Buffer.add_string b item_sep;
            newline (level + 1);
            add_string ~ensure_ascii b k;
            Buffer.add_string b key_sep;
            go (level + 1) x)
          kv;
        newline level;
        Buffer.add_char b '}'
  and newline level =
    match indent with
    | Some k ->
        Buffer.add_char b '\n';
        Buffer.add_string b (String.make (k * level) ' ')
    | None -> ()
  in
  go 0 v;
  Buffer.contents b

(* ------------------------------------------------------------------------ *)
(* Access, with Python's [d.get(k)] leniency: a missing key, a null and a
   value of another type all read as absent. *)

let member k (v : t) : t =
  match v with
  | `Assoc kv -> ( match List.assoc_opt k kv with Some x -> x | None -> `Null)
  | _ -> `Null

let ( |. ) v k = member k v
let to_str_opt (v : t) = match v with `String s -> Some s | _ -> None
let to_str (v : t) = match v with `String s -> s | _ -> ""

let to_int_opt (v : t) =
  match v with
  | `Int n -> Some n
  | `Float f -> Some (int_of_float f)
  | _ -> None

let to_int ?(default = 0) (v : t) = Option.value ~default (to_int_opt v)

let to_float_opt (v : t) =
  match v with
  | `Float f -> Some f
  | `Int n -> Some (float_of_int n)
  | _ -> None

let to_bool (v : t) = match v with `Bool b -> b | _ -> false
let to_list (v : t) = match v with `List l -> l | _ -> []
let to_assoc (v : t) = match v with `Assoc kv -> kv | _ -> []
let to_str_list (v : t) = List.filter_map to_str_opt (to_list v)
let str_opt k v = to_str_opt (member k v)
let str k v = to_str (member k v)
let int_opt k v = to_int_opt (member k v)
let bool k v = to_bool (member k v)
let list k v = to_list (member k v)
let assoc k v = to_assoc (member k v)
let mem k (v : t) = match v with `Assoc kv -> List.mem_assoc k kv | _ -> false

let truthy (v : t) =
  match v with
  | `Null | `Bool false | `Int 0 | `String "" | `List [] | `Assoc [] -> false
  | `Float f -> f <> 0.
  | _ -> true

let opt_string = function Some s -> `String s | None -> `Null
let strings l : t = `List (List.map (fun s -> `String s) l)

(* Python's repr and str of a value. *)
let rec py_repr (v : t) =
  match v with
  | `String s -> Py.repr s
  | `List l -> "[" ^ String.concat ", " (List.map py_repr l) ^ "]"
  | `Assoc kv ->
      "{"
      ^ String.concat ", "
          (List.map (fun (k, v) -> Py.repr k ^ ": " ^ py_repr v) kv)
      ^ "}"
  | v -> py_str v

and py_str (v : t) =
  match v with
  | `String s -> s
  | `Null -> "None"
  | `Bool b -> if b then "True" else "False"
  | `Int n -> string_of_int n
  | `Float f -> Py.repr_float f
  | v -> py_repr v

let load path = try Some (parse (Py.read_file path)) with _ -> None
