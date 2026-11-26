(** Mutable JSON objects that keep the order of their members: the records built
    member by member and printed as JSON that are not suggestions (those are
    {!Suggestion.t}), nor the triage page's data (which [scripts/page_data.ml]
    reads into records), nor the LLM's cached answers ({!Llm.Answer.t}): the
    baseline of [compare], the accuracy history's line, the jobs of
    [fetch-ci-logs] and [classify-logs].

    In the getters, a missing member and a null both read as absent. *)

type t

val create : unit -> t
val of_list : (string * Json.t) list -> t

val of_json : Json.t -> t
(** The members of an object ([create ()] for another value). *)

val to_json : t -> Json.t

val copy : t -> t
(** A new dict with the same members. *)

val keys : t -> string list
val mem : t -> string -> bool

val get : t -> string -> Json.t option
(** The value of a member, None if it is null. *)

val raw : t -> string -> Json.t option
(** The value of a member, null included. *)

(** {1 Typed getters} *)

val str_opt : t -> string -> string option

val str : t -> string -> string
(** The string, or [""]. *)

val int : t -> string -> int
(** The integer (a float truncated), or 0. *)

val list : t -> string -> Json.t list
(** The list, or [[]]. *)

val strs : t -> string -> string list
(** The strings of the list. *)

val strs_opt : t -> string -> string list option
(** The strings of the list, None if the member is not a list. *)

val dict : t -> string -> t option

(** {1 Changing} *)

val set : t -> string -> Json.t -> unit
(** Replaces the value of a member in place, or adds it at the end. *)

val update : t -> (string * Json.t) list -> unit
(** [set] for each member, in order. *)

val remove : t -> string -> unit
val set_str : t -> string -> string -> unit
val set_bool : t -> string -> bool -> unit
