(** Suggestions: one analysed failure of an opam-repo-ci run on an
    opam-repository pull request, with what failed, why, and what to change.

    The rules ({!Rules}, {!Matrix}, {!Context}, {!Llm}) build a suggestion
    member by member and change it in place while {!Suggest} merges and
    annotates the results; {!Titles} adds the members for people (title, job in
    words, where the fix goes). [suggest --json] prints the suggestions, the
    triage page stores them (in [state/prs.json] and [site/data.json], under
    [prs[].result.suggestions]) and reads them back.

    Two suggestions are the same one when they are physically equal ([==]):
    merging keeps the first and extends it.

    A member that the JSON may hold as [null] (["old": null]: a dependency line
    added, not changed) is a {!field}, which keeps [null] apart from an absent
    member so that both print as they were read. The others are options: absent
    or set. [confidence] is kept as it comes, an open set ("high", "low
    (dependencies released after ...)", ""); [rule] and [kind] are variants that
    keep names they do not know ([Other]); [where_kind] is a closed set (missing
    in suggestions stored before it existed: see {!Where}). *)

module T : sig
  (** A member that can be absent, [null], or set. *)
  type 'a field = Absent | Null | Set of 'a

  (** The changelog sections of a dependency between two versions (from
      ocaml.org), shown with a candidate. *)
  module Changelog : sig
    type t = {
      file : string field;  (** the changelog's file name *)
      versions : string list option;  (** the versions it covers *)
      text : string option;  (** the sections, decoded *)
      cut : bool option;  (** whether [text] was cut *)
      extra : (string * Json.t) list;
          (** members of another name or type, kept as they were read *)
    }
  end

  (** A dependency that may have caused a test-output failure or a lower-bounds
      failure: its version in the failing job and the passing one, and the bound
      that would exclude it. *)
  module Candidate : sig
    type t = {
      dep : string option;  (** the dependency (an opam package name) *)
      passing : string option;  (** its version in a passing job *)
      failing : string option;  (** its version in the failing job *)
      first_after : string option;
          (** its first release after the failing package's *)
      old : string field;
          (** the line of [depends] to change; null: a new line *)
      new_ : string field;  (** the new line, then a comment ("new") *)
      changelog : Changelog.t option;
      extra : (string * Json.t) list;
    }
  end

  type t = {
    mutable variant : string option;
        (** the CI job's variant, e.g.
            [compilers,4.14,lwt.5.10.2,revdeps,osc.0.1.4]; for a pattern over
            several packages, a summary *)
    mutable url : string option;  (** the job's page *)
    mutable kind : Ci.Kind.t option;  (** the job's kind *)
    mutable experimental : bool option;
        (** whether the job runs on an experimental platform (it does not block
            the PR) *)
    mutable failing : string field;
        (** the failing package ([name.version]), or several, comma-separated *)
    mutable fix_in : string option;  (** where the fix goes, in words *)
    mutable where_kind : Where_kind.t option;
        (** where the fix goes; missing in old results ({!Where.where_kind}
            derives it). A stored value that is not a {!Where_kind.t} is kept in
            [extra], and the member is absent. *)
    mutable where_pkg : string field;
        (** the other package where the fix goes ([where_kind] "other-package")
        *)
    mutable rule : Rule.t option;  (** the rule that recognised the failure *)
    mutable error : string option;  (** the error, in short *)
    mutable lint_level : string option;  (** "error" or "warning" for lint *)
    mutable dep : string option;  (** the dependency the change is about *)
    mutable failing_dep : string option;
        (** the shared dependency through which a pattern's packages fail *)
    mutable old : string field;
        (** the opam file's text to replace; null: a line to add *)
    mutable new_ : string field;  (** [new]: the suggested opam text *)
    mutable confidence : string field;
    mutable diagnosis : string option;  (** the LLM's diagnosis *)
    mutable llm_usage : Json.t option;  (** the LLM's tokens and model *)
    mutable log_error : string field;
        (** for an LLM answer: the log's line that best names the error *)
    mutable llm_error : string option;
        (** why the LLM gave no answer (the failure stays unrecognised) *)
    mutable llm_prompt : string option;  (** with [--llm-dry-run]: the prompt *)
    mutable note : string option;  (** the explanation, for people *)
    mutable install : string option;
        (** an install failure caused by an earlier build error: the message of
            the install step *)
    mutable evidence : (string * bool option) list option;
        (** whether each version ("pkg 1.2") defines the identifier, in order
            (None: not known) *)
    mutable source : string option;  (** where the versions were looked up *)
    mutable compared_with : string option;
        (** what the failing job was compared with *)
    mutable candidates : Candidate.t list option;
    mutable also_newer : string list option;
        (** other dependencies released after the failing package *)
    mutable tried : string list option;  (** the lookups that found nothing *)
    mutable checked : string option;
        (** why the change is not shown as a diff (it failed a check) *)
    mutable jobs : string list option;
        (** the variants of the jobs it stands for *)
    mutable also_seen_on : string list option;
        (** other platforms where the same failure was seen *)
    mutable preexisting : string option;
        (** why the failure happens without the PR *)
    mutable preexisting_unverified : bool option;
        (** the pre-existing failure's reason is not the same error *)
    mutable new_on_master : string option;
        (** opam-health-check sees the package build on master *)
    mutable alternative : string option;  (** another change *)
    mutable advice : string option;  (** a change described in words *)
    mutable title : string option;  (** the rule in words *)
    mutable job : string option;  (** the job in words *)
    mutable where : string field;  (** where the fix goes, for people *)
    mutable class_ : string option;
        (** [class]: fix, workaround, elsewhere or explanation *)
    mutable also_failing_in : int list option;
        (** on the page: the other open PRs where the same jobs fail *)
    mutable merged : int option;
        (** on the page: how many similar suggestions this one merges *)
    mutable packages : string list option;  (** on the page: their packages *)
    mutable errors : string list option;  (** on the page: their errors *)
    mutable notes : string list option;  (** on the page: their notes *)
    mutable extra : (string * Json.t) list;
    mutable key : Ci.failure_key option;  (** the failure it was found from *)
    mutable keys : Ci.failure_key list;  (** a pattern's failures *)
    mutable keep : bool;
        (** kept even when a pattern over the jobs explains its failure *)
    mutable plat : string option;
        (** the platform in [note], for merging the same note on several *)
    mutable distros : string list;  (** a day10 job's distributions *)
    mutable depexts : (string list * string) list option;
        (** a conf package's depexts: packages, filter *)
    mutable check : (string * string) option;
        (** a conf package's name and its failing command *)
    mutable installed : (string * string) list option;
        (** the versions the failing job installed *)
    mutable unexplained : bool;
        (** no rule explains the failure: the LLM may be asked *)
    mutable placed : bool;
        (** the review placed it on a line of the PR's diff *)
  }
  (** A suggestion. The members printed (JSON names in brackets when they
      differ) come first; [extra] keeps the members of another name, and the
      values of another type than their member's (such a member reads as
      absent); the last ones are internal and never printed. *)
end

include module type of struct
  include T
end

(** {1 Reading} *)

val get : 'a field -> 'a option
(** The value, None when absent or null. *)

val field_of_option : 'a option -> 'a field
(** [Set], or [Null] for None. *)

val str : string option -> string
(** The string, or [""]. *)

val nonempty : string option -> bool
(** Whether the string is set and not [""]. Only for the members that can be set
    to [""]: the texts the rules compute ([error], [note], [source], from lists
    that may be empty), and [confidence] (stored as [""] by older versions). The
    other string members are never [""] (the rules set them to names, variants
    and fixed words, and no stored suggestion has one): an option match says
    whether they are set. *)

val str_field : string field -> string

val nonempty_field : string field -> bool
(** The same, for [new], [old] and [failing] (computed opam text and package
    lists). *)

val list : 'a list option -> 'a list
(** The list, or [[]]. *)

val changelog_is_empty : Changelog.t -> bool

(** {1 Building} *)

val create : unit -> t
(** A suggestion without members. *)

val copy : t -> t
(** A new suggestion with the same members. *)

val candidate :
  ?passing:string ->
  ?first_after:string ->
  ?changelog:Changelog.t ->
  old:string option ->
  dep:string ->
  failing:string ->
  string ->
  Candidate.t
(** [candidate ~old ~dep ~failing new_] ([old]: None for a new line). *)

type set =
  [ `Variant of string
  | `Url of string
  | `Kind of Ci.Kind.t
  | `Experimental of bool
  | `Failing of string option
  | `Fix_in of string
  | `Where_kind of Where_kind.t
  | `Where_pkg of string option
  | `Rule of Rule.t
  | `Error of string
  | `Lint_level of string
  | `Dep of string
  | `Failing_dep of string
  | `Old of string option
  | `New of string option
  | `Confidence of string
  | `Diagnosis of string
  | `Llm_usage of Json.t
  | `Log_error of string option
  | `Llm_error of string
  | `Llm_prompt of string
  | `Note of string
  | `Install of string
  | `Evidence of (string * bool option) list
  | `Source of string
  | `Compared_with of string
  | `Candidates of Candidate.t list
  | `Also_newer of string list
  | `Tried of string list
  | `Checked of string
  | `Jobs of string list
  | `Also_seen_on of string list
  | `Preexisting of string
  | `Preexisting_unverified of bool
  | `New_on_master of string
  | `Alternative of string
  | `Advice of string
  | `Title of string
  | `Job of string
  | `Where of string option
  | `Class of string
  | `Also_failing_in of int list
  | `Key of Ci.failure_key
  | `Keys of Ci.failure_key list
  | `Keep
  | `Plat of string
  | `Distros of string list
  | `Depexts of (string list * string) list
  | `Check of string * string
  | `Installed of (string * string) list
  | `Unexplained ]
(** One member to set, as the rules list them. An optional string sets a member
    that can be null ([None]: null). *)

val set : t -> set -> unit

val update : t -> set list -> unit
(** [set] for each member, in order. *)

val make : set list -> t
(** [update] on a new suggestion. *)

(** {1 JSON} *)

val to_json : t -> Json.t
(** The printed members, in a fixed order, then those of [extra] (but for a
    member set since it was read, which prints its new value). *)

val of_json : Json.t -> t
(** Reads any suggestion stored so far; never fails. A member of another name,
    or whose value has another type than the member's, goes to [extra] (and the
    member is absent), so that [to_json (of_json v)] has the members of [v]. *)
