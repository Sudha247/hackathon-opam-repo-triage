(** Helpers on strings, lists, files and processes that the standard library
    lacks (OCaml 5.1).

    Some keep, on purpose, the exact behaviour of the Python functions of the
    same name (the tool was first written in Python): their results are in the
    outputs, the state and the cache keys, which must not change. Each one says
    so. Strings are UTF-8 bytes; the functions that count characters rather than
    bytes have [chars] in their name. *)

(** {1 Characters} *)

val is_digit : char -> bool
val is_alpha : char -> bool

val is_alnum : char -> bool
(** ASCII letters and digits. *)

val isdigit : string -> bool
(** A non-empty string of ASCII digits. *)

(** {1 Strings} *)

val strip : chars:string -> string -> string
(** [strip ~chars s] removes the characters of [chars] around [s]. *)

val lstrip : ?chars:string -> string -> string
(** Removes the characters of [chars] (default: white space, vertical tab
    included) at the start of [s]. *)

val rstrip : ?chars:string -> string -> string
(** The same at the end. *)

val find : ?from:int -> string -> string -> int
(** [find s sub]: the index of the first occurrence of [sub] in [s] at or after
    [from], or -1. *)

val rfind : string -> string -> int
(** The index of the last occurrence of [sub] in [s], or -1. *)

val contains : string -> string -> bool
(** [contains s sub]: does [sub] occur in [s]? *)

val split_on : ?maxsplit:int -> string -> string -> string list
(** [split_on sep s]: the parts of [s] between the occurrences of [sep], a
    non-empty string (at most [maxsplit] splits). *)

val split : ?maxsplit:int -> string -> string list
(** The words of [s], separated by runs of white space (at most [maxsplit]
    splits, the rest stripped on the right), as Python's [str.split()]: the
    words of log lines and error messages. *)

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
(** The lines of [s], without their ends, as Python's [str.splitlines]: the line
    ends are \n, \r\n, \r, \x0b, \x0c, \x1c, \x1d, \x1e, U+0085, U+2028 and
    U+2029, so that logs are cut into the same lines as the recorded results. A
    final line end does not start an empty line. *)

val slice : ?start:int -> ?stop:int -> string -> string
(** The bytes from [start] to [stop], clamped; negative indices count from the
    end. *)

val repr : string -> string
(** An ASCII string quoted as Python's [repr] quotes it (single quotes unless
    the string contains one and no double quote), as in the messages and texts
    the tool has always printed. *)

(** {2 Counting characters}

    Lengths and cuts in UTF-8 characters (code points), not bytes: the texts are
    cut to a number of characters in the outputs, as when they were first
    recorded. *)

val char_length : string -> int

val byte_index : string -> int -> int
(** The byte offset of character [k] (or the length of [s]). *)

val prefix_chars : string -> int -> string
(** [prefix_chars s k]: the first [k] characters. *)

val drop_chars : string -> int -> string
(** [drop_chars s k]: all but the first [k] characters. *)

val suffix_chars : string -> int -> string
(** [suffix_chars s k]: the last [k] characters. *)

val sub_chars : string -> int -> int -> string
(** [sub_chars s i j]: characters [i] to [j] (excluded). *)

(** {1 Lists} *)

val take : int -> 'a list -> 'a list
(** The first [n] elements (all if fewer). *)

val drop : int -> 'a list -> 'a list
(** All but the first [n] elements. *)

val sublist : ?start:int -> ?stop:int -> 'a list -> 'a list
(** Elements [start] to [stop] (excluded), clamped; negative indices count from
    the end. *)

val last : 'a list -> 'a
(** The last element. Raises [Invalid_argument] on an empty list. *)

val last_opt : 'a list -> 'a option

val dedup : 'a list -> 'a list
(** The first occurrence of each element, in order. *)

val sort_by : ?reverse:bool -> ('a -> 'b) -> 'a list -> 'a list
(** Stable sort by a key. *)

val group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list
(** The elements grouped by key: the keys in the order they first appear, each
    with its elements in order. *)

val counter : 'a list -> ('a * int) list
(** Each distinct element with its number of occurrences, the most frequent
    first, the first seen first among equals (the order of the outputs' counts).
*)

val first_some : (unit -> 'a option) list -> 'a option
(** The result of the first function that returns one: the functions are called
    in order, until one returns [Some]. *)

(** {1 Files} *)

val read_gzip : string -> string
val write_gzip : string -> string -> unit

val is_dir : string -> bool
(** Is it a directory (false if it does not exist)? *)

val makedirs : string -> unit
(** Creates a directory and its missing parents. Raises [Sys_error]. *)

val remove : string -> unit
(** Removes a file, if it exists. *)

val getmtime : string -> float

val listdir : string -> string list
(** The sorted entries of a directory ([[]] if it cannot be read). *)

val rmtree : string -> unit
(** Removes a file or a directory tree, ignoring errors. *)

val abspath : string -> string

(** {1 Processes} *)

val getenv_nonempty : string -> string option
(** An environment variable, an empty value reading as unset. *)

val exn_text : exn -> string
(** The message of an exception: the argument of [Failure], [Invalid_argument]
    and [Sys_error], else the registered printers' text. *)

val die : string -> 'a
(** Exits with status 1 after printing the message on stderr. *)
