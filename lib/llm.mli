(** Optional LLM fallback for the failures no rule explains ([--llm]).

    Calls the Claude Messages API over HTTPS (the request the Anthropic SDK
    sends, and its error messages), with the usual credentials:
    ANTHROPIC_API_KEY, ANTHROPIC_AUTH_TOKEN, or an [ant auth login] profile
    (through [ant auth print-credentials]). The model is given the job's log
    excerpt, the failing package's opam file and the job's results on the other
    platforms, and answers in a JSON schema close to a suggestion's. Answers are
    cached (the key is the prompt). *)

val apply_llm :
  Suggestion.t list ->
  Ci.data ->
  Sources.t ->
  string ->
  string list ->
  int ->
  string ->
  bool ->
  int
(** [apply_llm results data src cache_dir pr_packages limit model dry_run]: ask
    the LLM about the failures no rule explains, [limit] API calls at most:
    cached answers do not count, so that each run adds answers to the ones
    before. Returns how many distinct failures are left without an answer that a
    later call may give: beyond [limit], a transient API error, or the LLM off
    (credentials refused, {!llm_breaker} failures in a row); not a log without
    an error, a refusal or an unusable answer: asking again would give the same.
    With [dry_run], no call is made: the suggestions get the prompts instead. *)

val llm_todo : Suggestion.t list -> Ci.data -> Suggestion.t list
(** The failures to ask the LLM about: those no rule explains. *)

val llm_sig : ?key_line:string -> Suggestion.t -> string
(** Failures with the same signature share one answer: the same error from the
    rules, digits aside (the same error on other platforms); without one, the
    same failing package (exactly: not another version of it) and the same
    [key_line] of the log, digits aside. *)

(** {1 Calls} *)

val llm_timeout : float
(** Seconds per API request. *)

val llm_max_retries : int
(** Retries of a failed request (not the SDK's defaults, 10 minutes and 2
    retries: the hourly job gives the whole analysis 10 minutes). *)

val llm_breaker : int
(** Transient API failures in a row (an outage: each costs up to two timeouts)
    after which the LLM is not called again in this process; the failures it
    leaves are asked about in a later run. The triage page counts them across
    its PRs the same way ("llm-streak" in the request counts). *)

type state = { mutable streak : int; mutable off : string option }
(** This process's LLM calls: the transient failures in a row, and why the LLM
    is off (credentials refused, {!llm_breaker} failures), or None. "llm-off" in
    the request counts tells the triage page to stop for the rest of its run
    too. *)

val llm_state : state

(** The outcome of one [messages.create] call, after the SDK's retry. *)
type api_result =
  | Message of Json.t  (** the response body *)
  | Status_error of int * string  (** HTTP status, the SDK's message *)
  | Connection_error of string
      (** a network error or a timeout: the SDK's message *)
  | Sdk_error of string * string
      (** another error (no credentials...): its class name and message *)

val messages_create : model:string -> prompt:string -> api_result
(** [client.beta.messages.create(...)] with the SDK's timeout and retry. *)

(** {1 Answers} *)

(** An answer of the model, as the cache keeps it ([llm-KEY.json] in the cache
    directory, or [.json.gz]): the members of {!llm_schema}, then [llm_usage],
    added by the tool. *)
module Answer : sig
  type t = {
    diagnosis : string Suggestion.field;
    fix_in : string Suggestion.field;
        (** one of "this-pr", "other-package", "upstream", "ci", "unknown" *)
    package : string Suggestion.field;
        (** the package to change ([name.version]), or [""] *)
    change_old : string Suggestion.field;
        (** the line(s) of the opam file to replace, [""] to add a line *)
    change_new : string Suggestion.field;  (** the new line(s), or [""] *)
    confidence : string Suggestion.field;  (** "low", "medium" or "high" *)
    verify : string Suggestion.field;  (** what a human should check *)
    extra : (string * Json.t) list;
        (** members of another name, or of another type than a string, kept as
            they were read *)
    llm_usage : Json.t Suggestion.field;
        (** the tokens and the model of the call:
            [{"input": n, "output": n, "model": name}] *)
  }
  (** A member is absent, [null] or a string; an answer that {!llm_suggest} uses
      has all of them (it checks them on the JSON first), and [llm_usage]. *)

  val of_json : Json.t -> t
  (** Reads any JSON object; never fails. A member of another name, a repeated
      one, or a string member of another type goes to [extra], so that
      [to_json (of_json v)] has the members of [v]. *)

  val to_json : t -> Json.t
  (** The string members in the order of {!llm_schema}, then [extra], then
      [llm_usage] (the order the tool writes them in). *)
end

(** What {!llm_suggest} gives for one failure. *)
type reply =
  | Answer of { answer : Answer.t; names : string list option; cached : bool }
      (** the model's answer; [names]: the package names it was given
          ({!prompt_names}; None: not known); [cached]: from the cache, not a
          call *)
  | Failed of { message : string; called : bool; later : bool }
      (** no answer: why; [called]: an API call was made (it counts toward the
          limit: not for an unusable cached answer, nor an error before the
          call); [later]: a later call may give one (a transient API error, or
          the LLM off: credentials refused) *)
  | Skipped of string
      (** not asked (beyond the limit, the LLM off, or offline without a cached
          answer): why *)
  | Prompt of string  (** with [dry_run]: the prompt (it counts as a call) *)

val llm_suggest :
  ?call:bool ->
  Sources.t ->
  Suggestion.t ->
  Ci.job ->
  string ->
  Ci.data ->
  string list ->
  string ->
  string ->
  bool ->
  reply
(** [llm_suggest src r job log data pr_packages cache_dir model dry_run]: the
    model's answer for one failure (cached), or why there is none, or the prompt
    with [dry_run]. Without [call], or once the LLM is off ({!llm_state}), only
    a cached answer is given, else [Skipped]. Offline ([OPAM_CI_OFFLINE]), an
    answer not in the cache is [Skipped] too, never a call. *)

(** {1 Prompt and answer} *)

val llm_system : string
(** The system prompt. *)

val llm_schema : Json.t
(** The JSON schema of an answer. *)

val llm_prompt :
  Sources.t ->
  Suggestion.t ->
  Ci.job ->
  string ->
  Ci.data ->
  string list ->
  string
(** [llm_prompt src r job log data pr_packages]: the prompt for one failure. *)

val prompt_names : string -> string list
(** The package names the model was given: those of the PR, of the failing
    package, the dependencies with their installed versions, and the words of
    the opam file and of the error excerpt (a library or a tool the error
    names). A dependency the answer adds must be one of them to be shown as a
    diff. *)

val llm_change_as_diff : ?names:string list -> string option -> string -> bool
(** [llm_change_as_diff ~names old new_]: whether the model's change ([old]
    replaced by [new_]) has one of the shapes shown as a diff (a dependency
    without a version, possibly with [{with-test}], [{build}] or [{with-doc}]
    filters, and the [available], [x-ci-accept-failures] and [x-maintained]
    fields; without a depext), its old lines set the same dependencies or fields
    as the new ones, and each dependency it names is one of [names], the package
    names the model was given ({!prompt_names}; None: not known, so none is). *)

val key_error_line : string -> string option
(** The line of a job log that best names the error, for grouping the failures
    the LLM answered (the LLM digest). *)

(** {1 Test seams} *)

val fetch_hook : (string -> string -> string) ref
val log_text_hook : (string -> string) ref

val excerpt : ?max_output_lines:int -> string -> (string * string) list
(** {!Ci.excerpt} with the lines that mark a failure ([marks]): the log's part
    of the prompt. *)

val excerpt_hook :
  (?max_output_lines:int -> string -> (string * string) list) ref

val llm_prompt_hook :
  (Sources.t ->
  Suggestion.t ->
  Ci.job ->
  string ->
  Ci.data ->
  string list ->
  string)
  ref

val create_hook : (model:string -> prompt:string -> api_result) ref
(** The API call. *)

val llm_suggest_hook :
  (?call:bool ->
  Sources.t ->
  Suggestion.t ->
  Ci.job ->
  string ->
  Ci.data ->
  string list ->
  string ->
  string ->
  bool ->
  reply)
  ref

val key_error_line_hook : (string -> string option) ref
