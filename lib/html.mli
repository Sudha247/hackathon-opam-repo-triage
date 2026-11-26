(** Reading and writing HTML. *)

val escape : ?quote:bool -> string -> string
(** Escapes [&], [<], [>] and, with [quote] (the default), both quotes. *)

val unescape : string -> string
(** Decodes the numeric and the common named character references. *)
