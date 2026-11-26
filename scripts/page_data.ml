(* The triage page's data: state/prs.json and site/data.json. See
   page_data.mli. *)

open Triage

type 'a field = 'a Suggestion.field = Absent | Null | Set of 'a

(* ------------------------------------------------------------------------ *)
(* Members: how to read one from JSON into a record, and print it from one.
   [put] gives None for a value of another type than the member's: the value
   is then kept in the record's [extra], and the member is absent. *)

type 'r member = {
  name : string;
  get : 'r -> Json.t option;
  put : 'r -> Json.t -> 'r option;
  clear : 'r -> 'r;  (** makes it absent *)
}

let read_str = function `String s -> Some s | _ -> None
let read_int = function `Int n -> Some n | _ -> None
let read_float = function `Float f -> Some f | _ -> None
let read_bool = function `Bool b -> Some b | _ -> None

let read_all f l =
  let l' = List.filter_map f l in
  if List.length l' = List.length l then Some l' else None

let read_list f = function `List l -> read_all f l | _ -> None

let read_assoc f = function
  | `Assoc kv -> read_all (fun (k, v) -> Option.map (fun x -> (k, x)) (f v)) kv
  | _ -> None

let read_obj of_json = function `Assoc _ as v -> Some (of_json v) | _ -> None
let ints kv : Json.t = `Assoc (List.map (fun (k, n) -> (k, `Int n)) kv)

(* An optional member: absent or set. *)
let opt name read write get set =
  {
    name;
    get = (fun r -> Option.map write (get r));
    put = (fun r v -> Option.map (fun x -> set r (Some x)) (read v));
    clear = (fun r -> set r None);
  }

(* A member that can also be null. *)
let fld name read write get set =
  {
    name;
    get =
      (fun r ->
        match get r with
        | Absent -> None
        | Null -> Some `Null
        | Set x -> Some (write x));
    put =
      (fun r v ->
        match v with
        | `Null -> Some (set r Null)
        | v -> Option.map (fun x -> set r (Set x)) (read v));
    clear = (fun r -> set r Absent);
  }

let str name = opt name read_str (fun s -> `String s)
let int name = opt name read_int (fun n -> `Int n)
let bool name = opt name read_bool (fun b -> `Bool b)
let strs name = opt name (read_list read_str) Json.strings
let nstr name = fld name read_str (fun s -> `String s)
let nint name = fld name read_int (fun n -> `Int n)

let assoc_set k v kv =
  if List.mem_assoc k kv then
    List.map (fun (k', v') -> if k' = k then (k, v) else (k', v')) kv
  else kv @ [ (k, v) ]

let read_record members ~extra ~set_extra empty (v : Json.t) =
  match v with
  | `Assoc kv ->
      List.fold_left
        (fun r (k, v) ->
          match List.find_opt (fun m -> m.name = k) members with
          | Some m -> (
              match m.put r v with
              | Some r -> set_extra r (List.remove_assoc k (extra r))
              | None -> set_extra r (assoc_set k v (extra r)))
          | None -> set_extra r (assoc_set k v (extra r)))
        empty kv
  | _ -> empty

let print_record members extra r : Json.t =
  let known =
    List.filter_map
      (fun m -> Option.map (fun v -> (m.name, v)) (m.get r))
      members
  in
  `Assoc (known @ List.filter (fun (k, _) -> not (List.mem_assoc k known)) extra)

let get = Suggestion.get

(* ------------------------------------------------------------------------ *)

module Revdep_job = struct
  type t = {
    variant : string field;
    url : string field;
    category : Ci.Category.t field;
    target : string field;
    extra : (string * Json.t) list;
  }

  let empty =
    {
      variant = Absent;
      url = Absent;
      category = Absent;
      target = Absent;
      extra = [];
    }

  let members =
    [
      nstr "variant" (fun r -> r.variant) (fun r variant -> { r with variant });
      nstr "url" (fun r -> r.url) (fun r url -> { r with url });
      fld "category"
        (fun v -> Option.map Ci.Category.of_string (read_str v))
        (fun c -> `String (Ci.Category.to_string c))
        (fun r -> r.category)
        (fun r category -> { r with category });
      nstr "target" (fun r -> r.target) (fun r target -> { r with target });
    ]

  let of_json =
    read_record members
      ~extra:(fun r -> r.extra)
      ~set_extra:(fun r extra -> { r with extra })
      empty

  let to_json r = print_record members r.extra r
end

module Jobs = struct
  type t = {
    total : int option;
    failed : (Ci.Category.t * int) list option;
    failing_revdeps : (string * Revdep_job.t list) list option;
    extra : (string * Json.t) list;
  }

  let empty =
    { total = None; failed = None; failing_revdeps = None; extra = [] }

  let members =
    [
      int "total" (fun r -> r.total) (fun r total -> { r with total });
      opt "failed"
        (fun v ->
          Option.map
            (List.map (fun (k, n) -> (Ci.Category.of_string k, n)))
            (read_assoc read_int v))
        (fun l ->
          ints (List.map (fun (c, n) -> (Ci.Category.to_string c, n)) l))
        (fun r -> r.failed)
        (fun r failed -> { r with failed });
      opt "failing_revdeps"
        (read_assoc (read_list (read_obj Revdep_job.of_json)))
        (fun l ->
          `Assoc
            (List.map
               (fun (k, js) -> (k, `List (List.map Revdep_job.to_json js)))
               l))
        (fun r -> r.failing_revdeps)
        (fun r failing_revdeps -> { r with failing_revdeps });
    ]

  let of_json =
    read_record members
      ~extra:(fun r -> r.extra)
      ~set_extra:(fun r extra -> { r with extra })
      empty

  let to_json r = print_record members r.extra r
end

module Result = struct
  type t = {
    sha : string option;
    pr : int field;
    ci_unfinished : int option;
    ci_jobs : int option;
    unanalysed : int option;
    incomplete : bool option;
    errors : (string * int) list option;
    llm_unanswered : int option;
    health_stale : string option;
    suggestions : Suggestion.t list option;
    extra : (string * Json.t) list;
  }

  let empty =
    {
      sha = None;
      pr = Absent;
      ci_unfinished = None;
      ci_jobs = None;
      unanalysed = None;
      incomplete = None;
      errors = None;
      llm_unanswered = None;
      health_stale = None;
      suggestions = None;
      extra = [];
    }

  let members =
    [
      str "sha" (fun r -> r.sha) (fun r sha -> { r with sha });
      nint "pr" (fun r -> r.pr) (fun r pr -> { r with pr });
      int "ci_unfinished"
        (fun r -> r.ci_unfinished)
        (fun r ci_unfinished -> { r with ci_unfinished });
      int "ci_jobs" (fun r -> r.ci_jobs) (fun r ci_jobs -> { r with ci_jobs });
      int "unanalysed"
        (fun r -> r.unanalysed)
        (fun r unanalysed -> { r with unanalysed });
      bool "incomplete"
        (fun r -> r.incomplete)
        (fun r incomplete -> { r with incomplete });
      opt "errors" (read_assoc read_int) ints
        (fun r -> r.errors)
        (fun r errors -> { r with errors });
      int "llm_unanswered"
        (fun r -> r.llm_unanswered)
        (fun r llm_unanswered -> { r with llm_unanswered });
      str "health_stale"
        (fun r -> r.health_stale)
        (fun r health_stale -> { r with health_stale });
      opt "suggestions"
        (function `List l -> Some (List.map Suggestion.of_json l) | _ -> None)
        (fun l -> `List (List.map Suggestion.to_json l))
        (fun r -> r.suggestions)
        (fun r suggestions -> { r with suggestions });
    ]

  let of_json =
    read_record members
      ~extra:(fun r -> r.extra)
      ~set_extra:(fun r extra -> { r with extra })
      empty

  let to_json r = print_record members r.extra r
  let suggestions r = Option.value ~default:[] r.suggestions
end

module Since_last_push = struct
  type t = {
    previous_sha : string field;
    fixed : string list option;
    new_ : string list option;
    still : int option;
    extra : (string * Json.t) list;
  }

  let empty =
    {
      previous_sha = Absent;
      fixed = None;
      new_ = None;
      still = None;
      extra = [];
    }

  let members =
    [
      nstr "previous_sha"
        (fun r -> r.previous_sha)
        (fun r previous_sha -> { r with previous_sha });
      strs "fixed" (fun r -> r.fixed) (fun r fixed -> { r with fixed });
      strs "new" (fun r -> r.new_) (fun r new_ -> { r with new_ });
      int "still" (fun r -> r.still) (fun r still -> { r with still });
    ]

  let of_json =
    read_record members
      ~extra:(fun r -> r.extra)
      ~set_extra:(fun r extra -> { r with extra })
      empty

  let to_json r = print_record members r.extra r
end

module Entry = struct
  type t = {
    mutable number : int field;
    mutable title : string field;
    mutable url : string field;
    mutable draft : bool field;
    mutable updated : string field;
    mutable sha : string field;
    mutable author : string field;
    mutable labels : string list field;
    mutable stale_labels : string list field;
    mutable ci_url : string field;
    mutable pending : bool option;
    mutable since_last_push : Since_last_push.t option;
    mutable since_base : t option;
    mutable ci_state : string field;
    mutable ci_created : string field;
    mutable ci_digest : string field;
    mutable ci_unfinished : int field;
    mutable refresh_pending : bool option;
    mutable result : Result.t field;
    mutable error : string field;
    mutable attempts : int option;
    mutable tool_version : string option;
    mutable llm : bool option;
    mutable llm_unanswered : int field;
    mutable llm_calls : int field;
    mutable mcp_errors : int option;
    mutable net_errors : int option;
    mutable git_unreachable : int option;
    mutable analysed_at : string option;
    mutable seconds : float option;
    mutable jobs : Jobs.t field;
    mutable last_error : string field;
    mutable last_error_at : string option;
    mutable last_error_ci_state : string field;
    mutable last_error_ci_created : string field;
    mutable last_error_refreshed : bool option;
    mutable ci_state_now : string field;
    mutable extra : (string * Json.t) list;
  }

  let create () =
    {
      number = Absent;
      title = Absent;
      url = Absent;
      draft = Absent;
      updated = Absent;
      sha = Absent;
      author = Absent;
      labels = Absent;
      stale_labels = Absent;
      ci_url = Absent;
      pending = None;
      since_last_push = None;
      since_base = None;
      ci_state = Absent;
      ci_created = Absent;
      ci_digest = Absent;
      ci_unfinished = Absent;
      refresh_pending = None;
      result = Absent;
      error = Absent;
      attempts = None;
      tool_version = None;
      llm = None;
      llm_unanswered = Absent;
      llm_calls = Absent;
      mcp_errors = None;
      net_errors = None;
      git_unreachable = None;
      analysed_at = None;
      seconds = None;
      jobs = Absent;
      last_error = Absent;
      last_error_at = None;
      last_error_ci_state = Absent;
      last_error_ci_created = Absent;
      last_error_refreshed = None;
      ci_state_now = Absent;
      extra = [];
    }

  let copy e = { e with number = e.number }

  (* Setters that change the record in place (and return it, for
     [read_record]). *)
  let ( <-- ) f =
   fun r x ->
    f r x;
    r

  let rec members () =
    [
      nint "number" (fun r -> r.number) (( <-- ) (fun r x -> r.number <- x));
      nstr "title" (fun r -> r.title) (( <-- ) (fun r x -> r.title <- x));
      nstr "url" (fun r -> r.url) (( <-- ) (fun r x -> r.url <- x));
      fld "draft" read_bool
        (fun b -> `Bool b)
        (fun r -> r.draft)
        (( <-- ) (fun r x -> r.draft <- x));
      nstr "updated" (fun r -> r.updated) (( <-- ) (fun r x -> r.updated <- x));
      nstr "sha" (fun r -> r.sha) (( <-- ) (fun r x -> r.sha <- x));
      nstr "author" (fun r -> r.author) (( <-- ) (fun r x -> r.author <- x));
      fld "labels" (read_list read_str) Json.strings
        (fun r -> r.labels)
        (( <-- ) (fun r x -> r.labels <- x));
      fld "stale_labels" (read_list read_str) Json.strings
        (fun r -> r.stale_labels)
        (( <-- ) (fun r x -> r.stale_labels <- x));
      nstr "ci_url" (fun r -> r.ci_url) (( <-- ) (fun r x -> r.ci_url <- x));
      bool "pending" (fun r -> r.pending) (( <-- ) (fun r x -> r.pending <- x));
      opt "since_last_push"
        (read_obj Since_last_push.of_json)
        Since_last_push.to_json
        (fun r -> r.since_last_push)
        (( <-- ) (fun r x -> r.since_last_push <- x));
      opt "since_base" (read_obj of_json) to_json
        (fun r -> r.since_base)
        (( <-- ) (fun r x -> r.since_base <- x));
      nstr "ci_state"
        (fun r -> r.ci_state)
        (( <-- ) (fun r x -> r.ci_state <- x));
      nstr "ci_created"
        (fun r -> r.ci_created)
        (( <-- ) (fun r x -> r.ci_created <- x));
      nstr "ci_digest"
        (fun r -> r.ci_digest)
        (( <-- ) (fun r x -> r.ci_digest <- x));
      nint "ci_unfinished"
        (fun r -> r.ci_unfinished)
        (( <-- ) (fun r x -> r.ci_unfinished <- x));
      bool "refresh_pending"
        (fun r -> r.refresh_pending)
        (( <-- ) (fun r x -> r.refresh_pending <- x));
      fld "result" (read_obj Result.of_json) Result.to_json
        (fun r -> r.result)
        (( <-- ) (fun r x -> r.result <- x));
      nstr "error" (fun r -> r.error) (( <-- ) (fun r x -> r.error <- x));
      int "attempts"
        (fun r -> r.attempts)
        (( <-- ) (fun r x -> r.attempts <- x));
      str "tool_version"
        (fun r -> r.tool_version)
        (( <-- ) (fun r x -> r.tool_version <- x));
      bool "llm" (fun r -> r.llm) (( <-- ) (fun r x -> r.llm <- x));
      nint "llm_unanswered"
        (fun r -> r.llm_unanswered)
        (( <-- ) (fun r x -> r.llm_unanswered <- x));
      nint "llm_calls"
        (fun r -> r.llm_calls)
        (( <-- ) (fun r x -> r.llm_calls <- x));
      int "mcp_errors"
        (fun r -> r.mcp_errors)
        (( <-- ) (fun r x -> r.mcp_errors <- x));
      int "net_errors"
        (fun r -> r.net_errors)
        (( <-- ) (fun r x -> r.net_errors <- x));
      int "git_unreachable"
        (fun r -> r.git_unreachable)
        (( <-- ) (fun r x -> r.git_unreachable <- x));
      str "analysed_at"
        (fun r -> r.analysed_at)
        (( <-- ) (fun r x -> r.analysed_at <- x));
      opt "seconds" read_float
        (fun f -> `Float f)
        (fun r -> r.seconds)
        (( <-- ) (fun r x -> r.seconds <- x));
      fld "jobs" (read_obj Jobs.of_json) Jobs.to_json
        (fun r -> r.jobs)
        (( <-- ) (fun r x -> r.jobs <- x));
      nstr "last_error"
        (fun r -> r.last_error)
        (( <-- ) (fun r x -> r.last_error <- x));
      str "last_error_at"
        (fun r -> r.last_error_at)
        (( <-- ) (fun r x -> r.last_error_at <- x));
      nstr "last_error_ci_state"
        (fun r -> r.last_error_ci_state)
        (( <-- ) (fun r x -> r.last_error_ci_state <- x));
      nstr "last_error_ci_created"
        (fun r -> r.last_error_ci_created)
        (( <-- ) (fun r x -> r.last_error_ci_created <- x));
      bool "last_error_refreshed"
        (fun r -> r.last_error_refreshed)
        (( <-- ) (fun r x -> r.last_error_refreshed <- x));
      nstr "ci_state_now"
        (fun r -> r.ci_state_now)
        (( <-- ) (fun r x -> r.ci_state_now <- x));
    ]

  and of_json v =
    read_record (members ())
      ~extra:(fun r -> r.extra)
      ~set_extra:(( <-- ) (fun r x -> r.extra <- x))
      (create ()) v

  and to_json r = print_record (members ()) r.extra r

  let members = members ()

  let number_opt e =
    match e.number with
    | Set n -> Some n
    | Absent | Null -> (
        (* a number written as a string, by hand *)
        match List.assoc_opt "number" e.extra with
        | Some (`String s) -> int_of_string_opt (String.trim s)
        | _ -> None)

  let number e =
    match number_opt e with Some n -> n | None -> failwith "not a number"

  let str f = Option.value ~default:"" (get f)
  let list f = Option.value ~default:[] (get f)
  let result e = get e.result
  let has_result e = result e <> None

  let suggestions e =
    match e.result with Set r -> Result.suggestions r | _ -> []

  let remove e names =
    List.iter
      (fun name ->
        (match List.find_opt (fun m -> m.name = name) members with
        | Some m -> ignore (m.clear e)
        | None -> ());
        e.extra <- List.remove_assoc name e.extra)
      names
end

(* ------------------------------------------------------------------------ *)

module Llm_info = struct
  type t = {
    enabled : bool option;
    calls : int option;
    off : string field;
    extra : (string * Json.t) list;
  }

  let empty = { enabled = None; calls = None; off = Absent; extra = [] }

  let members =
    [
      bool "enabled" (fun r -> r.enabled) (fun r enabled -> { r with enabled });
      int "calls" (fun r -> r.calls) (fun r calls -> { r with calls });
      nstr "off" (fun r -> r.off) (fun r off -> { r with off });
    ]

  let of_json =
    read_record members
      ~extra:(fun r -> r.extra)
      ~set_extra:(fun r extra -> { r with extra })
      empty

  let to_json r = print_record members r.extra r
end

module Run = struct
  type t = {
    github_api_calls : int option;
    tool_requests : (string * int) list option;
    counts : (string * int) list;
    extra : (string * Json.t) list;
  }

  let empty =
    { github_api_calls = None; tool_requests = None; counts = []; extra = [] }

  let members =
    [
      int "github_api_calls"
        (fun r -> r.github_api_calls)
        (fun r github_api_calls -> { r with github_api_calls });
      opt "tool_requests" (read_assoc read_int) ints
        (fun r -> r.tool_requests)
        (fun r tool_requests -> { r with tool_requests });
    ]

  (* The other members are the run's counts: integers. *)
  let of_json v =
    let r =
      read_record members
        ~extra:(fun r -> r.extra)
        ~set_extra:(fun r extra -> { r with extra })
        empty v
    in
    let counts, extra =
      List.partition_map
        (fun (k, v) -> match v with `Int n -> Left (k, n) | v -> Right (k, v))
        r.extra
    in
    { r with counts; extra }

  let to_json r =
    print_record members
      (List.map (fun (k, n) -> (k, `Int n)) r.counts @ r.extra)
      r

  let count r k = List.assoc_opt k r.counts
end

type t = {
  generated : string option;
  repository : string option;
  open_prs : int option;
  failing_prs : int option;
  running_prs : int option;
  limit : int field;
  max_failures_per_pr : int field;
  llm : Llm_info.t option;
  run : Run.t option;
  prs : Entry.t list option;
  passing : Entry.t list option;
  extra : (string * Json.t) list;
}

let empty =
  {
    generated = None;
    repository = None;
    open_prs = None;
    failing_prs = None;
    running_prs = None;
    limit = Absent;
    max_failures_per_pr = Absent;
    llm = None;
    run = None;
    prs = None;
    passing = None;
    extra = [];
  }

let entries = function
  | `List l -> read_all (read_obj Entry.of_json) l
  | _ -> None

let entries_json l = `List (List.map Entry.to_json l)

let members =
  [
    str "generated"
      (fun r -> r.generated)
      (fun r generated -> { r with generated });
    str "repository"
      (fun r -> r.repository)
      (fun r repository -> { r with repository });
    int "open_prs" (fun r -> r.open_prs) (fun r open_prs -> { r with open_prs });
    int "failing_prs"
      (fun r -> r.failing_prs)
      (fun r failing_prs -> { r with failing_prs });
    int "running_prs"
      (fun r -> r.running_prs)
      (fun r running_prs -> { r with running_prs });
    nint "limit" (fun r -> r.limit) (fun r limit -> { r with limit });
    nint "max_failures_per_pr"
      (fun r -> r.max_failures_per_pr)
      (fun r max_failures_per_pr -> { r with max_failures_per_pr });
    opt "llm"
      (read_obj Llm_info.of_json)
      Llm_info.to_json
      (fun r -> r.llm)
      (fun r llm -> { r with llm });
    opt "run" (read_obj Run.of_json) Run.to_json
      (fun r -> r.run)
      (fun r run -> { r with run });
    opt "prs" entries entries_json
      (fun r -> r.prs)
      (fun r prs -> { r with prs });
    opt "passing" entries entries_json
      (fun r -> r.passing)
      (fun r passing -> { r with passing });
  ]

let of_json =
  read_record members
    ~extra:(fun r -> r.extra)
    ~set_extra:(fun r extra -> { r with extra })
    empty

let to_json r = print_record members r.extra r
let prs d = Option.value ~default:[] d.prs

(* ------------------------------------------------------------------------ *)

module State = struct
  type t = {
    updated : string option;
    prs : (string * Entry.t) list option;
    extra : (string * Json.t) list;
  }

  let empty = { updated = None; prs = None; extra = [] }

  let members =
    [
      str "updated" (fun r -> r.updated) (fun r updated -> { r with updated });
      opt "prs"
        (* an entry that is not an object reads as an empty one *)
        (read_assoc (fun v -> Some (Entry.of_json v)))
        (fun l -> `Assoc (List.map (fun (k, e) -> (k, Entry.to_json e)) l))
        (fun r -> r.prs)
        (fun r prs -> { r with prs });
    ]

  let of_json =
    read_record members
      ~extra:(fun r -> r.extra)
      ~set_extra:(fun r extra -> { r with extra })
      empty

  let to_json r = print_record members r.extra r
end
