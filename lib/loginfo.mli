(** Facts from a job log, and the dependencies of an opam file: used by the
    rules, the PR-level checks and the LLM fallback. *)

type info = {
  installed : (string * string) list;
      (** package -> version, in the order of the log ("-> installed", or
          day10's dependency graph) *)
  action : string option;  (** compiling, installing, removing *)
  failing : (string * string) option;
      (** the package whose step failed: name, version *)
  ocaml : string option;  (** the version of the compiler package installed *)
  error_lines : string list;
      (** opam's ["#=== ERROR while ..."] block, or the build output before
          opam's ["[ERROR]"] line in older day10 logs *)
  test_diff : bool;
      (** a test output differs (cram/mdx, a [.corrected] file) *)
  base : string option;  (** the master commit the PR was merged onto *)
}
(** What a job's log says about its failure. *)

val log_info : string -> info
(** The facts of a log's text. *)

val dep_names : string option -> string -> string list
(** [dep_names opam_text field]: the package names of an opam file's [depends]
    or [depopts] field, in order. *)

val depends_block : ?close:string -> string -> string
(** The [depends] field of an opam file, from "depends:" (or the start of the
    file if it has none) to the first [close] (by default, a line starting with
    a closing bracket). *)

val direct_deps : string option -> string list
(** The direct dependencies of an opam file (the names starting the lines of
    [depends]). *)
