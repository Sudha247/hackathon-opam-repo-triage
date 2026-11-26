(** The version of the tool, generated at build time (see version/dune). *)

val digest : string
(** A digest of the tool's sources (the library's [.ml] files and the entry
    point), 12 hex digits: the triage page analyses the PRs again when it
    changes. Sources, not the executable: a rebuild with another compiler
    release or dependency version is the same tool. *)
