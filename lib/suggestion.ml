(* One analysed CI failure: what failed, why, and what to change. See
   suggestion.mli. *)

module T = struct
  type 'a field = Absent | Null | Set of 'a

  module Changelog = struct
    type t = {
      file : string field;
      versions : string list option;
      text : string option;
      cut : bool option;
      extra : (string * Json.t) list;
    }
  end

  module Candidate = struct
    type t = {
      dep : string option;
      passing : string option;
      failing : string option;
      first_after : string option;
      old : string field;
      new_ : string field;
      changelog : Changelog.t option;
      extra : (string * Json.t) list;
    }
  end

  type t = {
    mutable variant : string option;
    mutable url : string option;
    mutable kind : Ci.Kind.t option;
    mutable experimental : bool option;
    mutable failing : string field;
    mutable fix_in : string option;
    mutable where_kind : Where_kind.t option;
    mutable where_pkg : string field;
    mutable rule : Rule.t option;
    mutable error : string option;
    mutable lint_level : string option;
    mutable dep : string option;
    mutable failing_dep : string option;
    mutable old : string field;
    mutable new_ : string field;
    mutable confidence : string field;
    mutable diagnosis : string option;
    mutable llm_usage : Json.t option;
    mutable log_error : string field;
    mutable llm_error : string option;
    mutable llm_prompt : string option;
    mutable note : string option;
    mutable install : string option;
    mutable evidence : (string * bool option) list option;
    mutable source : string option;
    mutable compared_with : string option;
    mutable candidates : Candidate.t list option;
    mutable also_newer : string list option;
    mutable tried : string list option;
    mutable checked : string option;
    mutable jobs : string list option;
    mutable also_seen_on : string list option;
    mutable preexisting : string option;
    mutable preexisting_unverified : bool option;
    mutable new_on_master : string option;
    mutable alternative : string option;
    mutable advice : string option;
    mutable title : string option;
    mutable job : string option;
    mutable where : string field;
    mutable class_ : string option;
    mutable also_failing_in : int list option;
    mutable merged : int option;
    mutable packages : string list option;
    mutable errors : string list option;
    mutable notes : string list option;
    mutable extra : (string * Json.t) list;
    (* Internal, never printed. *)
    mutable key : Ci.failure_key option;
    mutable keys : Ci.failure_key list;
    mutable keep : bool;
    mutable plat : string option;
    mutable distros : string list;
    mutable depexts : (string list * string) list option;
    mutable check : (string * string) option;
    mutable installed : (string * string) list option;
    mutable unexplained : bool;
    mutable broken_platform : bool;
    mutable placed : bool;
  }
end

include T

let field_of_option = function Some x -> Set x | None -> Null
let get = function Set x -> Some x | Absent | Null -> None
let str = Option.value ~default:""
let nonempty = function Some s -> s <> "" | None -> false
let str_field f = str (get f)
let nonempty_field f = nonempty (get f)
let is_set = function Set _ -> true | Absent | Null -> false
let list = function Some l -> l | None -> []

let changelog_is_empty (c : Changelog.t) =
  c.file = Absent && c.versions = None && c.text = None && c.cut = None
  && c.extra = []

let candidate ?passing ?first_after ?changelog ~old ~dep ~failing new_ :
    Candidate.t =
  {
    dep = Some dep;
    passing;
    failing = Some failing;
    first_after;
    old = (match old with Some o -> Set o | None -> Null);
    new_ = Set new_;
    changelog;
    extra = [];
  }

let create () =
  {
    variant = None;
    url = None;
    kind = None;
    experimental = None;
    failing = Absent;
    fix_in = None;
    where_kind = None;
    where_pkg = Absent;
    rule = None;
    error = None;
    lint_level = None;
    dep = None;
    failing_dep = None;
    old = Absent;
    new_ = Absent;
    confidence = Absent;
    diagnosis = None;
    llm_usage = None;
    log_error = Absent;
    llm_error = None;
    llm_prompt = None;
    note = None;
    evidence = None;
    source = None;
    compared_with = None;
    candidates = None;
    also_newer = None;
    tried = None;
    checked = None;
    jobs = None;
    also_seen_on = None;
    preexisting = None;
    preexisting_unverified = None;
    new_on_master = None;
    alternative = None;
    advice = None;
    title = None;
    job = None;
    where = Absent;
    class_ = None;
    also_failing_in = None;
    merged = None;
    packages = None;
    errors = None;
    notes = None;
    extra = [];
    key = None;
    keys = [];
    keep = false;
    plat = None;
    distros = [];
    depexts = None;
    check = None;
    installed = None;
    install = None;
    unexplained = false;
    broken_platform = false;
    placed = false;
  }

(* Every nested value is immutable: a copy of the record is a deep copy. *)
let copy r = { r with variant = r.variant }

(* {1 Building} *)

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
  | `Unexplained
  | `Broken_platform ]

let set r (x : set) =
  match x with
  | `Variant v -> r.variant <- Some v
  | `Url v -> r.url <- Some v
  | `Kind v -> r.kind <- Some v
  | `Experimental v -> r.experimental <- Some v
  | `Failing v -> r.failing <- field_of_option v
  | `Fix_in v -> r.fix_in <- Some v
  | `Where_kind v -> r.where_kind <- Some v
  | `Where_pkg v -> r.where_pkg <- field_of_option v
  | `Rule v -> r.rule <- Some v
  | `Error v -> r.error <- Some v
  | `Lint_level v -> r.lint_level <- Some v
  | `Dep v -> r.dep <- Some v
  | `Failing_dep v -> r.failing_dep <- Some v
  | `Old v -> r.old <- field_of_option v
  | `New v -> r.new_ <- field_of_option v
  | `Confidence v -> r.confidence <- Set v
  | `Diagnosis v -> r.diagnosis <- Some v
  | `Llm_usage v -> r.llm_usage <- Some v
  | `Log_error v -> r.log_error <- field_of_option v
  | `Llm_error v -> r.llm_error <- Some v
  | `Llm_prompt v -> r.llm_prompt <- Some v
  | `Note v -> r.note <- Some v
  | `Install v -> r.install <- Some v
  | `Evidence v -> r.evidence <- Some v
  | `Source v -> r.source <- Some v
  | `Compared_with v -> r.compared_with <- Some v
  | `Candidates v -> r.candidates <- Some v
  | `Also_newer v -> r.also_newer <- Some v
  | `Tried v -> r.tried <- Some v
  | `Checked v -> r.checked <- Some v
  | `Jobs v -> r.jobs <- Some v
  | `Also_seen_on v -> r.also_seen_on <- Some v
  | `Preexisting v -> r.preexisting <- Some v
  | `Preexisting_unverified v -> r.preexisting_unverified <- Some v
  | `New_on_master v -> r.new_on_master <- Some v
  | `Alternative v -> r.alternative <- Some v
  | `Advice v -> r.advice <- Some v
  | `Title v -> r.title <- Some v
  | `Job v -> r.job <- Some v
  | `Where v -> r.where <- field_of_option v
  | `Class v -> r.class_ <- Some v
  | `Also_failing_in v -> r.also_failing_in <- Some v
  | `Key v -> r.key <- Some v
  | `Keys v -> r.keys <- v
  | `Keep -> r.keep <- true
  | `Plat v -> r.plat <- Some v
  | `Distros v -> r.distros <- v
  | `Depexts v -> r.depexts <- Some v
  | `Check (a, b) -> r.check <- Some (a, b)
  | `Installed v -> r.installed <- Some v
  | `Unexplained -> r.unexplained <- true
  | `Broken_platform -> r.broken_platform <- true

let update r l = List.iter (set r) l

let make l =
  let r = create () in
  update r l;
  r

(* {1 JSON}

   Each member is described by how to read it from a suggestion ([get]: None
   when absent) and how to set it ([put]: None clears it; false when the
   value does not have the member's type, the member is then absent). *)

type desc = {
  name : string;
  get : t -> Json.t option;
  put : t -> Json.t option -> bool;
}

let assoc_set k v kv =
  if List.mem_assoc k kv then
    List.map (fun (k', v') -> if k' = k then (k, v) else (k', v')) kv
  else kv @ [ (k, v) ]

(* Readers of the members' values: None for a value of another type. *)
let read_str = function `String s -> Some s | _ -> None

let read_nstr = function
  | `String s -> Some (Set s)
  | `Null -> Some Null
  | _ -> None

let read_bool = function `Bool b -> Some b | _ -> None

let read_all f = function
  | `List l ->
      let l' = List.filter_map f l in
      if List.length l' = List.length l then Some l' else None
  | _ -> None

let read_strs = read_all read_str
let read_ints = read_all (function `Int n -> Some n | _ -> None)

let read_evidence = function
  | `Assoc kv ->
      let l =
        List.filter_map
          (fun (k, v) ->
            match v with
            | `Bool b -> Some (k, Some b)
            | `Null -> Some (k, None)
            | _ -> None)
          kv
      in
      if List.length l = List.length kv then Some l else None
  | _ -> None

let nstr_json = function
  | Set s -> Some (`String s)
  | Null -> Some `Null
  | Absent -> None

let changelog_to_json (c : Changelog.t) : Json.t =
  `Assoc
    (List.filter_map
       (fun x -> x)
       [
         Option.map (fun v -> ("file", v)) (nstr_json c.file);
         Option.map (fun v -> ("versions", Json.strings v)) c.versions;
         Option.map (fun v -> ("text", `String v)) c.text;
         Option.map (fun v -> ("cut", `Bool v)) c.cut;
       ]
    @ c.extra)

let changelog_of_json : Json.t -> Changelog.t option = function
  | `Assoc kv ->
      let c =
        ref
          {
            Changelog.file = Absent;
            versions = None;
            text = None;
            cut = None;
            extra = [];
          }
      in
      List.iter
        (fun (k, v) ->
          let c0 = !c in
          let ok =
            match k with
            | "file" -> Option.map (fun x -> { c0 with file = x }) (read_nstr v)
            | "versions" ->
                Option.map
                  (fun x -> { c0 with versions = Some x })
                  (read_strs v)
            | "text" ->
                Option.map (fun x -> { c0 with text = Some x }) (read_str v)
            | "cut" ->
                Option.map (fun x -> { c0 with cut = Some x }) (read_bool v)
            | _ -> None
          in
          c :=
            match ok with
            | Some c' -> c'
            | None -> { c0 with extra = assoc_set k v c0.extra })
        kv;
      Some !c
  | _ -> None

let candidate_to_json (c : Candidate.t) : Json.t =
  let str k = Option.map (fun v -> (k, `String v)) in
  `Assoc
    (List.filter_map
       (fun x -> x)
       [
         str "dep" c.dep;
         str "passing" c.passing;
         str "failing" c.failing;
         str "first_after" c.first_after;
         Option.map (fun v -> ("old", v)) (nstr_json c.old);
         Option.map (fun v -> ("new", v)) (nstr_json c.new_);
         Option.map (fun v -> ("changelog", changelog_to_json v)) c.changelog;
       ]
    @ c.extra)

let candidate_of_json : Json.t -> Candidate.t option = function
  | `Assoc kv ->
      let c =
        ref
          {
            Candidate.dep = None;
            passing = None;
            failing = None;
            first_after = None;
            old = Absent;
            new_ = Absent;
            changelog = None;
            extra = [];
          }
      in
      List.iter
        (fun (k, v) ->
          let c0 = !c in
          let ok =
            match k with
            | "dep" ->
                Option.map (fun x -> { c0 with dep = Some x }) (read_str v)
            | "passing" ->
                Option.map (fun x -> { c0 with passing = Some x }) (read_str v)
            | "failing" ->
                Option.map (fun x -> { c0 with failing = Some x }) (read_str v)
            | "first_after" ->
                Option.map
                  (fun x -> { c0 with first_after = Some x })
                  (read_str v)
            | "old" -> Option.map (fun x -> { c0 with old = x }) (read_nstr v)
            | "new" -> Option.map (fun x -> { c0 with new_ = x }) (read_nstr v)
            | "changelog" ->
                Option.map
                  (fun x -> { c0 with changelog = Some x })
                  (changelog_of_json v)
            | _ -> None
          in
          c :=
            match ok with
            | Some c' -> c'
            | None -> { c0 with extra = assoc_set k v c0.extra })
        kv;
      Some !c
  | _ -> None

(* Descriptors, by the type of the member. *)
let dopt name read write get set =
  {
    name;
    get = (fun r -> Option.map write (get r));
    put =
      (fun r v ->
        match v with
        | None ->
            set r None;
            true
        | Some v -> (
            match read v with
            | Some x ->
                set r (Some x);
                true
            | None ->
                set r None;
                false));
  }

let dstr name = dopt name read_str (fun s -> `String s)
let dbool name = dopt name read_bool (fun b -> `Bool b)
let dstrs name = dopt name read_strs Json.strings

let dnstr name get set =
  {
    name;
    get = (fun r -> nstr_json (get r));
    put =
      (fun r v ->
        match v with
        | None ->
            set r Absent;
            true
        | Some v -> (
            match read_nstr v with
            | Some x ->
                set r x;
                true
            | None ->
                set r Absent;
                false));
  }

(* The members, in the order they are printed. *)
let descs =
  [
    dstr "variant" (fun r -> r.variant) (fun r v -> r.variant <- v);
    dstr "url" (fun r -> r.url) (fun r v -> r.url <- v);
    dopt "kind"
      (fun v -> Option.map Ci.Kind.of_string (read_str v))
      (fun k -> `String (Ci.Kind.to_string k))
      (fun r -> r.kind)
      (fun r v -> r.kind <- v);
    dbool "experimental"
      (fun r -> r.experimental)
      (fun r v -> r.experimental <- v);
    dnstr "failing" (fun r -> r.failing) (fun r v -> r.failing <- v);
    dstr "fix_in" (fun r -> r.fix_in) (fun r v -> r.fix_in <- v);
    dopt "where_kind"
      (fun v -> Option.bind (read_str v) Where_kind.of_string)
      (fun k -> `String (Where_kind.to_string k))
      (fun r -> r.where_kind)
      (fun r v -> r.where_kind <- v);
    dnstr "where_pkg" (fun r -> r.where_pkg) (fun r v -> r.where_pkg <- v);
    dopt "rule"
      (fun v -> Option.map Rule.of_string (read_str v))
      (fun r -> `String (Rule.to_string r))
      (fun r -> r.rule)
      (fun r v -> r.rule <- v);
    dstr "error" (fun r -> r.error) (fun r v -> r.error <- v);
    dstr "lint_level" (fun r -> r.lint_level) (fun r v -> r.lint_level <- v);
    dstr "dep" (fun r -> r.dep) (fun r v -> r.dep <- v);
    dstr "failing_dep" (fun r -> r.failing_dep) (fun r v -> r.failing_dep <- v);
    dnstr "old" (fun r -> r.old) (fun r v -> r.old <- v);
    dnstr "new" (fun r -> r.new_) (fun r v -> r.new_ <- v);
    dnstr "confidence" (fun r -> r.confidence) (fun r v -> r.confidence <- v);
    dstr "diagnosis" (fun r -> r.diagnosis) (fun r v -> r.diagnosis <- v);
    dopt "llm_usage" Option.some Fun.id
      (fun r -> r.llm_usage)
      (fun r v -> r.llm_usage <- v);
    dnstr "log_error" (fun r -> r.log_error) (fun r v -> r.log_error <- v);
    dstr "llm_error" (fun r -> r.llm_error) (fun r v -> r.llm_error <- v);
    dstr "llm_prompt" (fun r -> r.llm_prompt) (fun r v -> r.llm_prompt <- v);
    dstr "note" (fun r -> r.note) (fun r v -> r.note <- v);
    dstr "install" (fun r -> r.install) (fun r v -> r.install <- v);
    dopt "evidence" read_evidence
      (fun l ->
        `Assoc
          (List.map
             (fun (k, b) ->
               (k, match b with Some b -> `Bool b | None -> `Null))
             l))
      (fun r -> r.evidence)
      (fun r v -> r.evidence <- v);
    dstr "source" (fun r -> r.source) (fun r v -> r.source <- v);
    dstr "compared_with"
      (fun r -> r.compared_with)
      (fun r v -> r.compared_with <- v);
    dopt "candidates"
      (read_all candidate_of_json)
      (fun l -> `List (List.map candidate_to_json l))
      (fun r -> r.candidates)
      (fun r v -> r.candidates <- v);
    dstrs "also_newer" (fun r -> r.also_newer) (fun r v -> r.also_newer <- v);
    dstrs "tried" (fun r -> r.tried) (fun r v -> r.tried <- v);
    dstr "checked" (fun r -> r.checked) (fun r v -> r.checked <- v);
    dstrs "jobs" (fun r -> r.jobs) (fun r v -> r.jobs <- v);
    dstrs "also_seen_on"
      (fun r -> r.also_seen_on)
      (fun r v -> r.also_seen_on <- v);
    dstr "preexisting" (fun r -> r.preexisting) (fun r v -> r.preexisting <- v);
    dbool "preexisting_unverified"
      (fun r -> r.preexisting_unverified)
      (fun r v -> r.preexisting_unverified <- v);
    dstr "new_on_master"
      (fun r -> r.new_on_master)
      (fun r v -> r.new_on_master <- v);
    dstr "alternative" (fun r -> r.alternative) (fun r v -> r.alternative <- v);
    dstr "advice" (fun r -> r.advice) (fun r v -> r.advice <- v);
    dstr "title" (fun r -> r.title) (fun r v -> r.title <- v);
    dstr "job" (fun r -> r.job) (fun r v -> r.job <- v);
    dnstr "where" (fun r -> r.where) (fun r v -> r.where <- v);
    dstr "class" (fun r -> r.class_) (fun r v -> r.class_ <- v);
    dopt "also_failing_in" read_ints
      (fun l -> `List (List.map (fun n -> `Int n) l))
      (fun r -> r.also_failing_in)
      (fun r v -> r.also_failing_in <- v);
    dopt "merged"
      (function `Int n -> Some n | _ -> None)
      (fun n -> `Int n)
      (fun r -> r.merged)
      (fun r v -> r.merged <- v);
    dstrs "packages" (fun r -> r.packages) (fun r v -> r.packages <- v);
    dstrs "errors" (fun r -> r.errors) (fun r v -> r.errors <- v);
    dstrs "notes" (fun r -> r.notes) (fun r v -> r.notes <- v);
  ]

let desc_of name = List.find_opt (fun d -> d.name = name) descs

let to_json r : Json.t =
  let known =
    List.filter_map (fun d -> Option.map (fun v -> (d.name, v)) (d.get r)) descs
  in
  `Assoc
    (known @ List.filter (fun (k, _) -> not (List.mem_assoc k known)) r.extra)

let of_json (v : Json.t) =
  let r = create () in
  (match v with
  | `Assoc kv ->
      List.iter
        (fun (k, v) ->
          match desc_of k with
          | Some d when d.put r (Some v) ->
              r.extra <- List.filter (fun (k', _) -> k' <> k) r.extra
          | _ -> r.extra <- assoc_set k v r.extra)
        kv
  | _ -> ());
  r
