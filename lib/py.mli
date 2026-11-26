(** Helpers on strings, lists, files and dates, with the semantics of the Python
    built-ins the tool was first written with, so that its outputs (cut lengths,
    splits, orders) stay the same.

    Strings are UTF-8 bytes. The functions that count characters (code points)
    rather than bytes say so. *)

(** {1 Strings} *)

val is_space : char -> bool
(** Space, tab, newline, carriage return, vertical tab and form feed. *)

val is_digit : char -> bool

val is_alpha : char -> bool
(** ASCII letters. *)

val is_alnum : char -> bool

val isdigit : string -> bool
(** A non-empty string of ASCII digits. *)

val strip : ?chars:string -> string -> string
(** [strip s] removes the surrounding [is_space] characters; [strip ~chars s]
    the surrounding characters of [chars]. *)

val lstrip : ?chars:string -> string -> string
val rstrip : ?chars:string -> string -> string

val find : ?from:int -> string -> string -> int
(** Index of the first occurrence of [sub] in [s] at or after [from], or -1. *)

val rfind : string -> string -> int
(** Index of the last occurrence of [sub] in [s], or -1. *)

val contains : string -> string -> bool
(** [contains s sub]: does [sub] occur in [s]? *)

val split_on : ?maxsplit:int -> string -> string -> string list
(** [split_on sep s]: the parts of [s] between the occurrences of [sep] (at most
    [maxsplit] splits). [sep] must not be empty. *)

val split : ?maxsplit:int -> string -> string list
(** The words of [s], separated by runs of [is_space] characters (at most
    [maxsplit] splits, the rest stripped on the right). *)

val rsplit1 : string -> string -> string list
(** [rsplit1 sep s]: [s] split at the last occurrence of [sep] ([[s]] if none).
*)

val partition : string -> string -> string * string * string
(** [partition s sep]: the part before the first [sep], [sep], and the part
    after; [(s, "", "")] if [sep] does not occur. *)

val replace : ?count:int -> string -> string -> string -> string
(** [replace s old by]: the occurrences of [old] replaced with [by] (the first
    [count] ones). *)

val removeprefix : string -> string -> string
val removesuffix : string -> string -> string

val splitlines : string -> string list
(** The lines of [s], without their ends. Line ends are \n, \r\n, \r, \x0b,
    \x0c, \x1c, \x1d, \x1e, U+0085, U+2028 and U+2029. A final line end does not
    start an empty line. *)

val slice : ?start:int -> ?stop:int -> string -> string
(** [slice ~start ~stop s]: the bytes from [start] to [stop], clamped; negative
    indices count from the end. *)

(** {2 Counting characters} *)

val length : string -> int
(** The number of characters. *)

val byte_index : string -> int -> int
(** The byte offset of character [k] (or the length of [s]). *)

val prefix : string -> int -> string
(** [prefix s k]: the first [k] characters. *)

val drop : string -> int -> string
(** [drop s k]: all but the first [k] characters. *)

val suffix : string -> int -> string
(** [suffix s k]: the last [k] characters. *)

val sub_chars : string -> int -> int -> string
(** [sub_chars s i j]: characters [i] to [j] (excluded). *)

(** {2 Printing} *)

val repr_float : float -> string
(** The shortest representation of a float that reads back, as Python prints it
    (["1.0"], ["0.25"], ["1e-05"]). *)

val repr : string -> string
(** An ASCII string quoted as Python's [repr] does (single quotes unless the
    string contains one and no double quote). *)

(** {1 Lists} *)

val dedup : 'a list -> 'a list
(** The first occurrence of each element, in order. *)

val take : int -> 'a list -> 'a list
(** The first [n] elements (all if fewer). *)

val drop_list : int -> 'a list -> 'a list
(** All but the first [n] elements. *)

val lslice : ?start:int -> ?stop:int -> 'a list -> 'a list
(** Elements [start] to [stop] (excluded), clamped; negative indices count from
    the end. *)

val last : 'a list -> 'a
(** The last element. Raises [Invalid_argument] on an empty list. *)

val last_opt : 'a list -> 'a option

val sort_by : ?reverse:bool -> ('a -> 'b) -> 'a list -> 'a list
(** Stable sort by a key. *)

val sum : int list -> int

val range : int -> int -> int list
(** [range a b]: [a; a + 1; ...; b - 1]. *)

val enumerate : 'a list -> (int * 'a) list
(** The elements with their index. *)

val index_of : 'a -> 'a list -> int option

val first_some : (unit -> 'a option) list -> 'a option
(** The result of the first function that returns one: the functions are called
    in order, until one returns [Some]. *)

val group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list
(** The elements grouped by key: the keys in the order they first appear, each
    with its elements in order. *)

val counter : 'a list -> ('a * int) list
(** Each distinct element with its number of occurrences, the most frequent
    first (the first seen first among equals). *)

(** {1 Ordered tables}

    Mutable tables with string keys that keep the order in which the keys were
    added (the order they are printed and iterated in). *)
module Odict : sig
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
end

(** {1 Files} *)

val read_file : string -> string
val read_gzip : string -> string
val write_gzip : string -> string -> unit

val is_dir : string -> bool
(** Is it a directory (false if it does not exist)? *)

val makedirs : string -> unit
(** Creates a directory and its missing parents. *)

val remove : string -> unit
(** Removes a file, if it exists. *)

val getmtime : string -> float

val listdir : string -> string list
(** The sorted entries of a directory ([[]] if it cannot be read). *)

val rmtree : string -> unit
(** Removes a file or a directory tree, ignoring errors. *)

val abspath : string -> string

val getenv_nonempty : string -> string option
(** An environment variable, an empty value reading as unset. *)

val exn_text : exn -> string
(** The message of an exception: the argument of [Failure], [Invalid_argument]
    and [Sys_error], else the registered printers' text. *)

val die : string -> 'a
(** Exits with status 1 after printing the message on stderr. *)

(** {1 Time} *)

val sleep : float -> unit
(** [Unix.sleepf], doing nothing for a non-positive delay. *)

val today : unit -> string
(** Today's date (local time), "YYYY-MM-DD". *)

val days_of_date : string -> int option
(** Days since 1970-01-01 of a "YYYY-MM-DD" date, or None. *)

val date_of_days : int -> string

val parse_iso : string -> float option
(** Seconds since the epoch of an ISO 8601 date-time ("2026-11-17T10:00:00Z",
    "+00:00" offsets, fractions), or of a date alone (midnight UTC). *)
