(** Regular expressions with the syntax and matching rules of Python's [re]
    module (the syntax the rules and their tests are written in), on ocaml-re.

    Patterns are strings, compiled on first use and memoised. Supported:
    Python's syntax as ocaml-re's Perl parser reads it, with leading inline
    flags ([(?m)], [(?i)], [(?s)]), [(?P<name>...)] groups, [\Z], and [$]
    matching at the end or before a final newline. Lookaround and backreferences
    are not supported: they raise [Invalid_argument].

    Matching is on bytes: [\w], [\s] and character classes do not see a
    non-ASCII character as one character (see {!Rx_utf8}). *)

type flag =
  | I  (** case-insensitive *)
  | M  (** multi-line: [^] and [$] match at line boundaries *)
  | S  (** [.] matches a newline *)

type m
(** A match. *)

(** {1 Searching} *)

val search : ?flags:flag list -> ?pos:int -> string -> string -> m option
(** The first match at or after [pos]. *)

val match_ : ?flags:flag list -> ?pos:int -> string -> string -> m option
(** A match starting at [pos]. *)

val fullmatch : ?flags:flag list -> string -> string -> m option
(** A match of the whole string. *)

val test : ?flags:flag list -> string -> string -> bool
(** Does the pattern occur in the string? *)

val test_match : ?flags:flag list -> string -> string -> bool
(** Does the string start with a match of the pattern? *)

val finditer : ?flags:flag list -> string -> string -> m list
(** The non-overlapping matches, in order. *)

val findall : ?flags:flag list -> string -> string -> string list
(** The non-overlapping matches: the whole match if the pattern has no group,
    else the first group's value ([""] when it did not take part). *)

val findall_groups : ?flags:flag list -> string -> string -> string list list
(** The non-overlapping matches, as the values of all the groups. *)

(** {1 Groups} *)

val group_opt : m -> int -> string option
(** The value of group [i], None if it did not take part in the match. *)

val group : m -> int -> string
(** The value of group [i], [""] if it did not take part in the match. *)

val group0 : m -> string
(** The whole match. *)

val named : m -> string -> string option
(** The value of a named group. *)

val groups : m -> string option list
(** The values of all the groups. *)

val start : ?i:int -> m -> int
(** The byte offsets of the match (or of group [i]). *)

val end_ : ?i:int -> m -> int
val span : ?i:int -> m -> int * int

(** {1 Substituting and splitting} *)

val subf :
  ?flags:flag list -> ?count:int -> string -> (m -> string) -> string -> string
(** The matches replaced with [f m] (the first [count] ones, if positive). *)

val sub : ?flags:flag list -> ?count:int -> string -> string -> string -> string
(** The matches replaced with a template: [\1], [\g<1>], [\g<name>] stand for
    groups; [\n], [\t], [\r] for those characters, and a doubled backslash for a
    backslash. *)

val split : ?flags:flag list -> ?maxsplit:int -> string -> string -> string list
(** The parts between the matches, each followed by the values of the pattern's
    groups (at most [maxsplit] splits, if positive). *)

val escape : string -> string
(** The string with every character but ASCII letters, digits, [_] and non-ASCII
    bytes escaped: a pattern matching it literally. *)
