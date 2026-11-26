(** Where the fix of a suggestion goes: the [where_kind] of a {!Suggestion.t}
    (see {!Where}). *)

type t =
  | Pr  (** "pr": this PR *)
  | Other_package  (** "other-package" *)
  | Upstream  (** "upstream" *)
  | Ci  (** "ci": the CI's setup *)
  | Rerun  (** "rerun": jobs to run again *)
  | Nothing  (** "none": nothing to fix *)
  | Unknown  (** "unknown" *)

val all : t list

val to_string : t -> string
(** The name printed in [where_kind]: "pr", "other-package", "upstream", "ci",
    "rerun", "none" or "unknown". *)

val of_string : string -> t option
(** The kind of a name; None for another string (a suggestion stored with one
    keeps it, see {!Suggestion.of_json}). *)
