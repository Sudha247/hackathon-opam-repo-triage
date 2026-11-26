(* A mutable JSON object with insertion order: the records built member by
   member and printed as JSON that are not suggestions (see dict.mli). In
   the getters, a missing key and a null both read as absent. *)

module O = Odict

type t = Json.t O.t

let create = O.create
let of_list = O.of_list
let of_json (v : Json.t) = O.of_list (match v with `Assoc kv -> kv | _ -> [])
let to_json d : Json.t = `Assoc (O.to_list d)
let copy d = O.of_list (O.to_list d)
let get d k = match O.get d k with Some `Null | None -> None | v -> v
let raw = O.get
let mem = O.mem
let set = O.set
let remove = O.remove
let update d kv = List.iter (fun (k, v) -> set d k v) kv

(* Typed getters. *)

let str_opt d k = match get d k with Some (`String s) -> Some s | _ -> None
let str d k = Option.value ~default:"" (str_opt d k)

let int_opt d k =
  match get d k with Some v -> Json.to_int_opt v | None -> None

let int d k = Option.value ~default:0 (int_opt d k)
let list d k = match get d k with Some (`List l) -> l | _ -> []
let strs d k = List.filter_map Json.to_str_opt (list d k)

let strs_opt d k =
  match get d k with
  | Some (`List l) -> Some (List.filter_map Json.to_str_opt l)
  | _ -> None

let dict d k =
  match get d k with Some (`Assoc kv) -> Some (of_list kv) | _ -> None

let truthy d k = match get d k with Some v -> Json.truthy v | None -> false

(* Typed setters. *)

let set_str d k s = set d k (`String s)
let set_bool d k b = set d k (`Bool b)
let keys = O.keys
