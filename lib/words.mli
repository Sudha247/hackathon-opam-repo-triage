(** Counts and dates in words. *)

val counted : ?many:string -> int -> string -> string
(** ["1 job"], ["2 jobs"], ["2 reverse dependencies"]: [n] and the noun, plural
    unless [n] is 1 ([many] for an irregular plural). *)

val short_date : ?year:int -> string option -> string
(** ["2026-11-17…"] -> ["17 Nov"], with the year when it is not [year] (by
    default the current year). Anything else is returned unchanged. *)
