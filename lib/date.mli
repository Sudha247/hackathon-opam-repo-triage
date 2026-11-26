(** Dates: "YYYY-MM-DD" days and ISO 8601 date-times. *)

val today : unit -> string
(** Today's date (local time), "YYYY-MM-DD". *)

val days_of_date : string -> int option
(** Days since 1970-01-01 of a "YYYY-MM-DD" date, or None. *)

val date_of_days : int -> string

val parse_iso : string -> float option
(** Seconds since the epoch of an ISO 8601 date-time ("2026-11-17T10:00:00Z",
    "+00:00" offsets, fractions), or of a date alone (midnight UTC). *)
