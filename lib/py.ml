(* Small helpers with the semantics of the Python built-ins the tool was first
   written with (string methods, list helpers, files, time). Strings are
   UTF-8 bytes; the functions that count characters say so. *)

(* ------------------------------------------------------------------------ *)
(* Strings *)

let is_space c =
  c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\x0b' || c = '\x0c'

let strip_with p s =
  let n = String.length s in
  let i = ref 0 and j = ref (n - 1) in
  while !i < n && p s.[!i] do
    incr i
  done;
  while !j >= !i && p s.[!j] do
    decr j
  done;
  String.sub s !i (!j - !i + 1)

let lstrip_with p s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && p s.[!i] do
    incr i
  done;
  String.sub s !i (n - !i)

let rstrip_with p s =
  let j = ref (String.length s - 1) in
  while !j >= 0 && p s.[!j] do
    decr j
  done;
  String.sub s 0 (!j + 1)

let in_chars chars c = String.contains chars c

let strip ?chars s =
  match chars with
  | None -> strip_with is_space s
  | Some c -> strip_with (in_chars c) s

let lstrip ?chars s =
  match chars with
  | None -> lstrip_with is_space s
  | Some c -> lstrip_with (in_chars c) s

let rstrip ?chars s =
  match chars with
  | None -> rstrip_with is_space s
  | Some c -> rstrip_with (in_chars c) s

let find ?(from = 0) s sub =
  let n = String.length s and k = String.length sub in
  let rec go i =
    if i + k > n then -1 else if String.sub s i k = sub then i else go (i + 1)
  in
  if k = 0 then if from <= n then from else -1 else go from

let rfind s sub =
  let n = String.length s and k = String.length sub in
  let rec go i =
    if i < 0 then -1 else if String.sub s i k = sub then i else go (i - 1)
  in
  go (n - k)

let contains s sub = find s sub >= 0

let split_on ?maxsplit sep s =
  if sep = "" then invalid_arg "Py.split_on: empty separator";
  let k = String.length sep in
  let rec go acc from count =
    match maxsplit with
    | Some m when count >= m ->
        List.rev (String.sub s from (String.length s - from) :: acc)
    | _ ->
        let i = find ~from s sep in
        if i < 0 then
          List.rev (String.sub s from (String.length s - from) :: acc)
        else go (String.sub s from (i - from) :: acc) (i + k) (count + 1)
  in
  go [] 0 0

let split ?maxsplit s =
  let n = String.length s in
  let rec go acc i count =
    let i = ref i in
    while !i < n && is_space s.[!i] do
      incr i
    done;
    if !i >= n then List.rev acc
    else
      match maxsplit with
      | Some m when count >= m ->
          List.rev (rstrip (String.sub s !i (n - !i)) :: acc)
      | _ ->
          let j = ref !i in
          while !j < n && not (is_space s.[!j]) do
            incr j
          done;
          go (String.sub s !i (!j - !i) :: acc) !j (count + 1)
  in
  go [] 0 0

let rsplit1 sep s =
  let i = rfind s sep in
  if i < 0 then [ s ]
  else
    [
      String.sub s 0 i;
      String.sub s
        (i + String.length sep)
        (String.length s - i - String.length sep);
    ]

let partition s sep =
  let i = find s sep in
  if i < 0 then (s, "", "")
  else
    ( String.sub s 0 i,
      sep,
      String.sub s
        (i + String.length sep)
        (String.length s - i - String.length sep) )

let replace ?count s old by =
  if old = "" then s
  else begin
    let b = Buffer.create (String.length s) in
    let k = String.length old in
    let rec go from n =
      let i =
        if match count with Some c -> n >= c | None -> false then -1
        else find ~from s old
      in
      if i < 0 then
        Buffer.add_string b (String.sub s from (String.length s - from))
      else begin
        Buffer.add_string b (String.sub s from (i - from));
        Buffer.add_string b by;
        go (i + k) (n + 1)
      end
    in
    go 0 0;
    Buffer.contents b
  end

let removeprefix s p =
  if String.starts_with ~prefix:p s then
    String.sub s (String.length p) (String.length s - String.length p)
  else s

let removesuffix s p =
  if p <> "" && String.ends_with ~suffix:p s then
    String.sub s 0 (String.length s - String.length p)
  else s

let splitlines s =
  let n = String.length s in
  (* The length of the line boundary at [i], or 0. *)
  let boundary i =
    match s.[i] with
    | '\r' -> if i + 1 < n && s.[i + 1] = '\n' then 2 else 1
    | '\n' | '\x0b' | '\x0c' | '\x1c' | '\x1d' | '\x1e' -> 1
    | '\xc2' when i + 1 < n && s.[i + 1] = '\x85' -> 2
    | '\xe2'
      when i + 2 < n
           && s.[i + 1] = '\x80'
           && (s.[i + 2] = '\xa8' || s.[i + 2] = '\xa9') ->
        3
    | _ -> 0
  in
  let rec go acc start i =
    if i >= n then
      List.rev
        (if start < n then String.sub s start (n - start) :: acc else acc)
    else
      match boundary i with
      | 0 -> go acc start (i + 1)
      | k -> go (String.sub s start (i - start) :: acc) (i + k) (i + k)
  in
  go [] 0 0

let is_digit c = c >= '0' && c <= '9'
let isdigit s = s <> "" && String.for_all is_digit s
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_alnum c = is_alpha c || is_digit c

let slice ?(start = 0) ?stop s =
  let n = String.length s in
  let norm i = if i < 0 then max 0 (n + i) else min i n in
  let a = norm start and b = match stop with None -> n | Some j -> norm j in
  if b <= a then "" else String.sub s a (b - a)

(* UTF-8: Python strings count code points. *)

let is_cont c = Char.code c land 0xC0 = 0x80
let length s = String.fold_left (fun n c -> if is_cont c then n else n + 1) 0 s

let byte_index s k =
  let n = String.length s in
  let i = ref 0 and c = ref 0 in
  while !i < n && !c < k do
    incr i;
    while !i < n && is_cont s.[!i] do
      incr i
    done;
    incr c
  done;
  !i

let prefix s k = if k <= 0 then "" else String.sub s 0 (byte_index s k)

let drop s k =
  if k <= 0 then s
  else
    let i = byte_index s k in
    String.sub s i (String.length s - i)

let suffix s k =
  let n = length s in
  if k >= n then s else drop s (n - k)

let sub_chars s i j = prefix (drop s i) (j - i)

let repr_float f =
  if Float.is_integer f && Float.abs f < 1e16 then Printf.sprintf "%.1f" f
  else if Float.is_nan f then "nan"
  else if f = Float.infinity then "inf"
  else if f = Float.neg_infinity then "-inf"
  else begin
    (* Shortest representation that reads back, in Python's layout. *)
    let rec digits p =
      let s = Printf.sprintf "%.*e" p f in
      if p >= 16 || float_of_string s = f then s else digits (p + 1)
    in
    let e = digits 0 in
    let mant, exp =
      match String.index_opt e 'e' with
      | Some i ->
          ( String.sub e 0 i,
            int_of_string (String.sub e (i + 1) (String.length e - i - 1)) )
      | None -> (e, 0)
    in
    let neg = mant.[0] = '-' in
    let mant =
      if neg then String.sub mant 1 (String.length mant - 1) else mant
    in
    let ds = String.concat "" (String.split_on_char '.' mant) in
    let sign = if neg then "-" else "" in
    if exp < -4 || exp >= 16 then
      let m =
        if String.length ds = 1 then ds
        else String.sub ds 0 1 ^ "." ^ String.sub ds 1 (String.length ds - 1)
      in
      Printf.sprintf "%s%se%s%02d" sign m
        (if exp < 0 then "-" else "+")
        (abs exp)
    else if exp < 0 then sign ^ "0." ^ String.make (-exp - 1) '0' ^ ds
    else
      let nd = String.length ds in
      if nd <= exp + 1 then sign ^ ds ^ String.make (exp + 1 - nd) '0' ^ ".0"
      else
        sign
        ^ String.sub ds 0 (exp + 1)
        ^ "."
        ^ String.sub ds (exp + 1) (nd - exp - 1)
  end

let repr s =
  let q =
    if String.contains s '\'' && not (String.contains s '"') then '"' else '\''
  in
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b q;
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when c = q ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b q;
  Buffer.contents b

(* ------------------------------------------------------------------------ *)
(* Lists *)

let dedup l =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then false
      else (
        Hashtbl.add seen x ();
        true))
    l

let take n l =
  let rec go n acc = function
    | x :: r when n > 0 -> go (n - 1) (x :: acc) r
    | _ -> List.rev acc
  in
  go n [] l

let rec drop_list n l =
  if n <= 0 then l else match l with [] -> [] | _ :: r -> drop_list (n - 1) r

let lslice ?(start = 0) ?stop l =
  let n = List.length l in
  let norm i = if i < 0 then max 0 (n + i) else min i n in
  let a = norm start and b = match stop with None -> n | Some j -> norm j in
  if b <= a then [] else take (b - a) (drop_list a l)

let last l = List.nth l (List.length l - 1)
let last_opt l = match List.rev l with [] -> None | x :: _ -> Some x

let sort_by ?(reverse = false) f l =
  let c a b = compare (f a) (f b) in
  List.stable_sort (if reverse then fun a b -> c b a else c) l

let sum = List.fold_left ( + ) 0
let range a b = List.init (max 0 (b - a)) (fun i -> a + i)
let enumerate l = List.mapi (fun i x -> (i, x)) l
let first_some fs = List.find_map (fun f -> f ()) fs

let index_of x l =
  let rec go i = function
    | [] -> None
    | y :: r -> if y = x then Some i else go (i + 1) r
  in
  go 0 l

let group_by f l =
  let order = ref [] and tbl = Hashtbl.create 16 in
  List.iter
    (fun x ->
      let k = f x in
      match Hashtbl.find_opt tbl k with
      | Some r -> r := x :: !r
      | None ->
          order := k :: !order;
          Hashtbl.add tbl k (ref [ x ]))
    l;
  List.rev_map (fun k -> (k, List.rev !(Hashtbl.find tbl k))) !order

let counter l =
  let g = group_by Fun.id l in
  List.stable_sort
    (fun (_, a) (_, b) -> compare b a)
    (List.map (fun (k, xs) -> (k, List.length xs)) g)

(* ------------------------------------------------------------------------ *)
(* Ordered string tables (insertion order) *)

module Odict = struct
  type 'a t = { mutable kv : (string * 'a) list }

  let create () = { kv = [] }
  let of_list kv = { kv }
  let get t k = List.assoc_opt k t.kv
  let mem t k = List.mem_assoc k t.kv

  let set t k v =
    if List.mem_assoc k t.kv then
      t.kv <-
        List.map (fun (k', v') -> if k' = k then (k', v) else (k', v')) t.kv
    else t.kv <- t.kv @ [ (k, v) ]

  let remove t k = t.kv <- List.remove_assoc k t.kv
  let keys t = List.map fst t.kv
  let values t = List.map snd t.kv
  let to_list t = t.kv
  let length t = List.length t.kv

  let setdefault t k d =
    match get t k with
    | Some v -> v
    | None ->
        set t k d;
        d
end

(* ------------------------------------------------------------------------ *)
(* Files *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let read_gzip path =
  let ic = Gzip.open_in path in
  Fun.protect ~finally:(fun () -> Gzip.close_in ic) @@ fun () ->
  let b = Buffer.create 65536 and chunk = Bytes.create 65536 in
  let rec go () =
    let n = Gzip.input ic chunk 0 (Bytes.length chunk) in
    if n > 0 then (
      Buffer.add_subbytes b chunk 0 n;
      go ())
  in
  go ();
  Buffer.contents b

let write_gzip path data =
  let oc = Gzip.open_out ~level:9 path in
  Fun.protect
    ~finally:(fun () -> Gzip.close_out oc)
    (fun () -> Gzip.output_substring oc data 0 (String.length data))

(* A path for Bos, None for one Fpath rejects (empty, or with a NUL). *)
let fpath path = Result.to_option (Fpath.of_string path)

let is_dir path =
  match Option.map Bos.OS.Dir.exists (fpath path) with
  | Some (Ok b) -> b
  | _ -> false

let makedirs path =
  match fpath path with
  | None -> ()
  | Some p -> (
      match Bos.OS.Dir.create ~path:true p with
      | Ok _ -> ()
      | Error (`Msg m) -> raise (Sys_error m))

let remove path = try Sys.remove path with Sys_error _ -> ()
let getmtime path = (Unix.stat path).Unix.st_mtime

let listdir path =
  match
    Option.map (Bos.OS.Dir.contents ~dotfiles:true ~rel:true) (fpath path)
  with
  | Some (Ok l) -> List.sort compare (List.map Fpath.to_string l)
  | _ -> []

let rmtree path =
  Option.iter
    (fun p -> ignore (Bos.OS.Path.delete ~must_exist:false ~recurse:true p))
    (fpath path)

let abspath p =
  if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

let getenv_nonempty k = match Sys.getenv_opt k with Some "" -> None | v -> v

let exn_text = function
  | Failure m | Invalid_argument m | Sys_error m -> m
  | Not_found -> "not found"
  | e -> Printexc.to_string e

let die msg =
  prerr_endline msg;
  exit 1

(* ------------------------------------------------------------------------ *)
(* Time *)

let sleep s = if s > 0. then Unix.sleepf s

let today () =
  let t = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday

let days_of_date s =
  try
    Scanf.sscanf s "%4d-%2d-%2d%!" (fun y m d ->
        match Ptime.of_date (y, m, d) with
        | Some t -> Some (fst (Ptime.Span.to_d_ps (Ptime.to_span t)))
        | None -> None)
  with _ -> None

let date_of_days n =
  match Ptime.of_span (Ptime.Span.of_int_s (n * 86400)) with
  | Some t ->
      let y, m, d = Ptime.to_date t in
      Printf.sprintf "%04d-%02d-%02d" y m d
  | None -> "1970-01-01"

let parse_iso s =
  let s = strip s in
  let s = if String.length s = 10 then s ^ "T00:00:00Z" else s in
  let s =
    if
      String.ends_with ~suffix:"Z" s
      || String.length s > 19
         && (String.contains (String.sub s 19 (String.length s - 19)) '+'
            || String.contains (String.sub s 19 (String.length s - 19)) '-')
    then s
    else s ^ "Z"
  in
  match Ptime.of_rfc3339 ~strict:false s with
  | Ok (t, _, _) -> Some (Ptime.to_float_s t)
  | Error _ -> None
