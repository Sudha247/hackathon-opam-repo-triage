(* Mutable tables with string keys that keep the order in which the keys were
   added. *)

type 'a t = { mutable kv : (string * 'a) list }

let create () = { kv = [] }
let of_list kv = { kv }
let get t k = List.assoc_opt k t.kv
let mem t k = List.mem_assoc k t.kv

let set t k v =
  if List.mem_assoc k t.kv then
    t.kv <- List.map (fun (k', v') -> if k' = k then (k', v) else (k', v')) t.kv
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
