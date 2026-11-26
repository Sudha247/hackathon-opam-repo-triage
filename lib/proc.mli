(** Running commands, capturing their output. *)

type result = { code : int; stdout : string; stderr : string }

exception Timeout of float
(** Raised when a command runs longer than its timeout (in seconds). *)

val which : string -> string option
(** The path of a program on the PATH, if any. *)

val run :
  ?cwd:string ->
  ?env:(string * string) list ->
  ?input:string ->
  ?timeout:float ->
  string list ->
  result
(** [run args] runs [args] (the program is looked up on the PATH) with the
    environment variables [env] added, [input] on its standard input, and
    returns its exit code (minus the signal number if killed) and output. Raises
    [Timeout] after [timeout] seconds (the process is killed), and
    [Unix.Unix_error] when the program cannot be started. *)

val real_run :
  ?cwd:string ->
  ?env:(string * string) list ->
  ?input:string ->
  ?timeout:float ->
  string list ->
  result
(** The implementation of [run]. *)

val run_hook :
  (?cwd:string ->
  ?env:(string * string) list ->
  ?input:string ->
  ?timeout:float ->
  string list ->
  result)
  ref
(** What [run] calls: tests replace it. *)
