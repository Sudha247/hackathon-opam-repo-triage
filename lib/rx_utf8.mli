(** Regular expressions on UTF-8 text where [\w], [\s] and [\b] see characters,
    as Python's [re] does on [str], rather than bytes.

    The patterns run on a copy of the text where each non-ASCII character is one
    byte: \xc0 for a word character (a letter for ocaml-re), \x0b for
    whitespace, \x80 otherwise. Groups and offsets are taken from the original
    text. *)

type m

val is_word : int -> bool
(** Whether a code point is a word character for Python's [\w]: a letter or a
    number in Unicode's sense (the underscore aside). *)

val is_space : int -> bool
(** Whether a code point is white space (Python's [\s]). *)

(** A match. *)

val search : ?flags:Rx.flag list -> string -> string -> m option
val match_ : ?flags:Rx.flag list -> string -> string -> m option

val group_opt : m -> int -> string option
(** The value of group [i], None if it did not take part in the match. *)

val group : m -> int -> string
(** The value of group [i], [""] if it did not take part in the match. *)

val group0 : m -> string

val sub : ?flags:Rx.flag list -> string -> string -> string -> string
(** The matches replaced with a template where [\1] .. [\9] are groups. *)

val prepare : string -> string * int array
(** The text the patterns run on, and the offset in [s] of each of its bytes
    (and of its end). *)
