(** Version constraints of opam formulas. *)

val version_terms : string -> (string * string) list
(** The (operator, version) terms of a formula that constrain the package's own
    version ([>= "1.0"]), not those comparing a variable ([os = "linux"],
    [ocaml:version < "5.0"]). *)

val unsatisfiable : (string * string) list -> bool
(** Do these terms (a conjunction) exclude every version? *)

val allows : string -> (string * string) list -> bool
(** Does [version] satisfy every term? *)
