(** Regular expressions on ocaml-re, in Perl syntax as [Re.Perl] reads it.

    Static patterns are top-level values, compiled when their module is
    initialised ([v]): a pattern [Re.Perl] cannot parse (lookaround,
    backreferences) fails then, with a message naming it. Patterns that include
    data (a package or module name) are built with Re's combinators, the data as
    [Re.str], which needs no escaping ([re] and [of_re]).

    [$] matches at the end of the text or before a final newline, as in Perl and
    Python, unless the flag [M] makes it match at every line end. The functions
    follow Python's [re] module, which the tool was first written with: [match_]
    anchors at the start, [findall] returns the groups, [sub] takes a [\1] /
    [\g<name>] template.

    Matching is on bytes: [\w], [\s] and character classes do not see a
    non-ASCII character as one character (see {!Rx_utf8}). *)

type flag =
  | I  (** case-insensitive *)
  | M  (** multi-line: [^] and [$] match at line boundaries *)
  | S  (** [.] matches a newline *)

type t
(** A compiled pattern. *)

type m
(** A match. *)

(** {1 Compiling} *)

val v : ?flags:flag list -> string -> t
(** [v pattern] compiles a pattern; static patterns are compiled once, as
    top-level values. Raises [Invalid_argument] naming the pattern if it cannot
    be parsed. *)

val re : ?flags:flag list -> string -> Re.t
(** A pattern as an [Re.t], to combine with others (e.g. with [Re.str] for a
    name taken from the data, which then needs no escaping). *)

val of_re : Re.t -> t
(** Compiles a combined pattern. *)

(** {1 Searching} *)

val search : ?pos:int -> t -> string -> m option
(** The first match at or after [pos]. *)

val match_ : ?pos:int -> t -> string -> m option
(** A match starting at [pos]. *)

val fullmatch : t -> string -> m option
(** A match of the whole string. *)

val test : t -> string -> bool
(** Does the pattern occur in the string? *)

val test_match : t -> string -> bool
(** Does the string start with a match of the pattern? *)

val finditer : t -> string -> m list
(** The non-overlapping matches, in order. *)

val findall : t -> string -> string list
(** The non-overlapping matches: the whole match if the pattern has no group,
    else the first group's value ([""] when it did not take part). *)

val findall_groups : t -> string -> string list list
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

val subf : ?count:int -> t -> (m -> string) -> string -> string
(** The matches replaced with [f m] (the first [count] ones, if positive). *)

val sub : ?count:int -> t -> string -> string -> string
(** The matches replaced with a template: [\1], [\g<1>], [\g<name>] stand for
    groups; [\n], [\t], [\r] for those characters, and a doubled backslash for a
    backslash. *)

val split : ?maxsplit:int -> t -> string -> string list
(** The parts between the matches, each followed by the values of the pattern's
    groups (at most [maxsplit] splits, if positive). *)
