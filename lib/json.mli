(** JSON values, printed exactly as Python's [json.dumps] prints them (ASCII
    only, [", "] and [": "] separators, or [indent]), so that the state, cache
    and page files keep their format.

    The accessors are lenient: a missing key, a null and a value of another type
    all read as absent ([None], [""], [0], [false] or [[]]). *)

type t = Yojson.Basic.t

val parse : string -> t
(** Raises [Yojson.Json_error] on invalid JSON. *)

val parse_opt : string -> t option

val load : string -> t option
(** The contents of a JSON file, None if it cannot be read or parsed. *)

val dumps :
  ?indent:int ->
  ?sort_keys:bool ->
  ?ensure_ascii:bool ->
  ?compact:bool ->
  t ->
  string
(** [dumps v] prints [v] on one line, or with [indent] spaces per level.
    [sort_keys] sorts the members of objects; [ensure_ascii] (the default)
    escapes non-ASCII characters; [compact] uses [","] and [":"]. *)

val utf8_at : string -> int -> int * int
(** The UTF-8 character at byte [i]: its code point and length in bytes. An
    invalid byte reads as U+FFFD, of length 1. *)

(** {1 Reading} *)

val member : string -> t -> t
(** [member k v]: member [k] of object [v], or [`Null]. *)

val ( |. ) : t -> string -> t
(** [v |. k] is [member k v]. *)

val mem : string -> t -> bool
val to_str_opt : t -> string option
val to_str : t -> string

val to_int_opt : t -> int option
(** An integer, or a float truncated. *)

val to_int : ?default:int -> t -> int
val to_float_opt : t -> float option
val to_bool : t -> bool
val to_list : t -> t list
val to_assoc : t -> (string * t) list

val to_str_list : t -> string list
(** The strings of a list. *)

val str_opt : string -> t -> string option
(** [str_opt k v] is [to_str_opt (member k v)], and so on. *)

val str : string -> t -> string
val int_opt : string -> t -> int option
val bool : string -> t -> bool
val list : string -> t -> t list
val assoc : string -> t -> (string * t) list

val truthy : t -> bool
(** False for null, false, 0, 0.0, [""], [[]] and [{}]; true otherwise. *)

(** {1 Printing as Python does}

    For the messages and fields printed as the tool has always printed them (in
    the outputs, the state and the LLM's error messages). *)

val py_repr : t -> string
(** Python's [repr]: [None], [True], [3], ['text'], [[1, 'a']], [{'k': 1}]. *)

val py_str : t -> string
(** Python's [str]: as [py_repr], but a string as it is. *)

(** {1 Building} *)

val opt_string : string option -> t
val strings : string list -> t
