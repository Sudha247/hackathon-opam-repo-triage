(** Mutable tables with string keys that keep the order in which the keys were
    added: the order they are printed and iterated in (JSON objects, counters,
    the outputs' groups). *)

type 'a t = { mutable kv : (string * 'a) list }

val create : unit -> 'a t
val of_list : (string * 'a) list -> 'a t
val get : 'a t -> string -> 'a option
val mem : 'a t -> string -> bool

val set : 'a t -> string -> 'a -> unit
(** Replaces the value of a key in place, or adds the key at the end. *)

val remove : 'a t -> string -> unit
val keys : 'a t -> string list
val values : 'a t -> 'a list
val to_list : 'a t -> (string * 'a) list
val length : 'a t -> int

val setdefault : 'a t -> string -> 'a -> 'a
(** The value of a key, after setting it to the default if absent. *)
