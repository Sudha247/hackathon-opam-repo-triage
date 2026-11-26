open Triage
open Suggestion.T

let base = Net.base
let repo_url = "https://github.com/ocaml/opam-repository"

(* One verdict per PR: the first that applies, in this order (see [verdict]). *)
let verdicts =
  [
    ("ready", "Not this PR's fault");
    ("change", "Change this PR");
    ("blocked", "Blocked elsewhere");
    ("ci", "On hold");
    ("rerun", "Needs a rerun");
    ("other", "Needs a look");
    ("error", "Not analysed");
  ]

let verdict_label k = List.assoc k verdicts

let verdict_help = function
  | "ready" ->
      "CI fails, but none of the failures is this PR's fault: each one also \
       fails without this PR, comes "
      ^ "from the CI's setup, or is on an experimental platform; CI has \
         finished, and the jobs it did not "
      ^ "run to the end are on experimental platforms or aborted on riscv64 or \
         s390x (routine there)"
  | "change" ->
      "A change to this PR's opam files is suggested: a fix, or a workaround \
       that skips platforms or tests"
  | "blocked" ->
      "At least one failure must be fixed in another package (failures that \
       also happen without this PR do not count)"
  | "ci" ->
      "No failure is this PR's fault (each one also fails without this PR, \
       comes from the CI's setup, or is "
      ^ "on an experimental platform), but the PR is a draft, has a label that \
         keeps it out of the ready list, " ^ "or its CI is still running"
  | "rerun" ->
      "Some jobs did not run to the end because of the CI (network errors, \
       cancelled or aborted jobs, jobs "
      ^ "that never started, a crash of the CI's tools), and nothing else \
         points at this PR: rerun them " ^ "before merging"
  | "other" ->
      "Failures of this PR's packages without a suggested change, failures \
       whose cause is not known, or " ^ "failures the tool cannot explain"
  | _ -> "The analysis is pending or failed"

let change_classes = [ "fix"; "workaround" ]

(* Labels of ocaml/opam-repository: "consider for merge" is a filter, and a
   PR with one of [not_ready_labels] is never listed as ready, unless the
   label may be stale (the author acted after it was added, see
   triage_open_prs). *)
let merge_label = "consider for merge"
let no_merge_label = "do not merge"
let not_ready_labels = [ no_merge_label; "needs reporter action" ]

(* Where the fix goes ([where_kind], see where.ml) for failures that do not
   come from the PR: the CI's setup (or its opam), nothing to fix (a bound
   the PR adds on purpose). Not "rerun": CI did not test those jobs. Never
   for an LLM answer (see [ci_side]). *)
let ci_side_kinds = [ "ci"; "none" ]

(* Rules whose failures also happen on master (opam-health-check). *)
let preexisting_rules = [ "health-check-preexisting" ]

(* Job categories (from [summary]), in words. *)
let job_categories =
  [
    ("build", "build failed");
    ("build-or-test", "build or tests failed");
    ("dependency", "a dependency failed");
    ("solver", "no solution");
    ("network", "network error");
    ("cancelled", "cancelled");
    ("sources", "sources not unpacked");
    ("lint", "lint");
    ("not-available", "skipped (not installable there)");
    ("ignored", "ignored (accepted failure)");
    ("analysis", "analysis failed");
  ]

let job_category c = Option.value ~default:c (List.assoc_opt c job_categories)
let noise = [ "passed"; "network"; "cancelled"; "not-available"; "ignored" ]

let months =
  [|
    "Jan";
    "Feb";
    "Mar";
    "Apr";
    "May";
    "Jun";
    "Jul";
    "Aug";
    "Sep";
    "Oct";
    "Nov";
    "Dec";
  |]

(* ------------------------------------------------------------------------ *)
(* Lenient JSON access: a missing key, a null and a value of another type
   read as absent *)

let ( |. ) v k = Json.member k v
let truthy = Json.truthy
let has k (v : Json.t) = Json.mem k v

(** Python's str() of a value. *)
let str = Json.py_str

(** [e(s)]: HTML-escaped [str(s)], "" for None. *)
let e_s s = Html.escape s

let e (v : Json.t) = match v with `Null -> "" | v -> Html.escape (str v)
let ei n = string_of_int n

(** The value as a string, "" when absent ([v or ""]). *)
let sget v k =
  match v |. k with
  | `String s -> s
  | `Null -> ""
  | x -> if truthy x then str x else ""

let num (v : Json.t) =
  match v with
  | `Int n -> n
  | `Float f -> int_of_float f
  | `String s -> int_of_string (Py.strip s)
  | _ -> failwith "not a number"

let sugg_of (v : Json.t) = Suggestion.of_json v

(* A lower-case version for the search field (Unicode's full lower-case
   mapping, as Python's str.lower, but for a final sigma). *)
let lower s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let k = Uchar.utf_decode_length d in
      (match Uucp.Case.Map.to_lower (Uchar.utf_decode_uchar d) with
      | `Uchars us when Uchar.utf_decode_is_valid d ->
          List.iter (Buffer.add_utf_8_uchar b) us
      | _ -> Buffer.add_string b (String.sub s i k));
      go (i + k)
    end
  in
  go 0;
  Buffer.contents b

(* ------------------------------------------------------------------------ *)

(** An <a> only for https URLs; plain escaped text otherwise. *)
let link ?cls (url : Json.t) text =
  let c =
    match cls with Some c -> Printf.sprintf " class=\"%s\"" c | None -> ""
  in
  match url with
  | `String u
    when String.starts_with ~prefix:"https://" u
         && not (Rx.test {|[\s"'<>`]|} u) ->
      Printf.sprintf "<a href=\"%s\"%s rel=\"noopener noreferrer\">%s</a>"
        (e_s u) c (e_s text)
  | _ -> Printf.sprintf "<span%s>%s</span>" c (e_s text)

let links url text = link (`String url) text

(** '2026-11-20T…' -> '20 Nov', with the year when it is not [year]. *)
let day ?year iso =
  let iso = Option.value ~default:"" iso in
  match Rx.match_ {|(\d{4})-(\d\d)-(\d\d)|} iso with
  | Some m
    when let mo = int_of_string (Rx.group m 2) in
         1 <= mo && mo <= 12 ->
      let out =
        Printf.sprintf "%d %s"
          (int_of_string (Rx.group m 3))
          months.(int_of_string (Rx.group m 2) - 1)
      in
      if Some (Rx.group m 1) = year then out else out ^ " " ^ Rx.group m 1
  | _ -> iso

let day_v ?year (v : Json.t) =
  day ?year
    (match v with
    | `String s -> Some s
    | `Null -> None
    | v -> if truthy v then Some (str v) else None)

let shorten s n =
  let s = String.concat " " (Py.split s) in
  if Py.length s <= n then s
  else Py.rstrip (Py.prefix s (n - 1)) ^ "\xe2\x80\xa6"

let plural n = if n = 1 then "" else "s"

(** Whether the fix of suggestion [r] goes in another package (see where.ml:
    stored suggestions may predate [where_kind]). *)
let other_package r = Where.where_kind r = "other-package"

let suggestions_of (res : Json.t) =
  List.map sugg_of (Json.to_list (res |. "suggestions"))

(** The suggestions of an analysed PR, by class (classes in their first-seen
    order). *)
let groups (pr : Json.t) =
  if truthy (pr |. "pending") then []
  else
    Py.group_by
      (fun r ->
        match r.class_ with Some c when c <> "" -> c | _ -> "explanation")
      (suggestions_of (pr |. "result"))

let group g k = Option.value ~default:[] (List.assoc_opt k g)

let ci_running pr =
  if has "ci_state_now" pr then pr |. "ci_state_now" = `String "PENDING"
  else pr |. "ci_state" = `String "PENDING" && pr |. "ci_unfinished" <> `Int 0

(** Whether the fix of [r] goes on the CI's side ([kinds]). An LLM answer never
    counts: the model blames the CI too easily, and answers stored before
    apply_llm stopped saying "the CI environment" still say it. *)
let ci_side ?(kinds = ci_side_kinds) r =
  List.mem (Where.where_kind r) kinds && r.rule <> Some "llm"

let not_this_pr r =
  let shared =
    Suggestion.list r.also_failing_in <> []
    && (not (List.mem (Where.where_kind r) [ "pr"; "rerun" ]))
    && not (Where.tied_to_pr r)
  in
  (* A bound the PR adds on purpose: only as checked since where_kind exists (a lowered upper bound of
     [depends] that some version satisfies), not from a result stored before. *)
  let unchecked = r.rule = Some "bounded-out" && r.where_kind = None in
  Suggestion.nonempty r.preexisting
  && not (r.preexisting_unverified = Some true)
  || shared
  || r.Suggestion.experimental = Some true
  || (ci_side r && not unchecked)
  || List.mem (Suggestion.str r.rule) preexisting_rules

let str_list (v : Json.t) = List.map str (Json.to_list v)

(** The labels of [not_ready_labels] that still apply. *)
let blocking_labels pr =
  let labels = str_list (pr |. "labels")
  and stale = str_list (pr |. "stale_labels") in
  List.filter
    (fun l -> List.mem l labels && not (List.mem l stale))
    not_ready_labels

let seems_ready pr =
  let res = pr |. "result" in
  if
    truthy (pr |. "pending")
    || res = `Null || ci_running pr
    || truthy (pr |. "draft")
  then false
  else if
    blocking_labels pr <> []
    || truthy (pr |. "ci_unfinished")
    || truthy (res |. "ci_unfinished")
  then false
  else if truthy (res |. "unanalysed") then
    false (* failures beyond --max: nothing says they are not the PR's *)
  else
    let sugg = suggestions_of res in
    sugg <> [] && List.for_all not_this_pr sugg

let verdict pr =
  if truthy (pr |. "pending") || pr |. "result" = `Null then "error"
  else if seems_ready pr then "ready"
  else
    let sugg = suggestions_of (pr |. "result") in
    if
      List.exists
        (fun r -> List.mem (Suggestion.str r.class_) change_classes)
        sugg
    then "change"
      (* A failure that is not the PR's (it happens on master too, say) does not block it. *)
    else if List.exists (fun r -> other_package r && not (not_this_pr r)) sugg
    then "blocked"
    else if sugg = [] || truthy (pr |. "result" |. "unanalysed") then "other"
      (* Not ready only because of the PR's state (a draft, a label, CI running). *)
    else if List.for_all not_this_pr sugg then "ci"
    else
      let reruns =
        List.filter
          (fun r -> Where.where_kind r = "rerun" && not (not_this_pr r))
          sugg
      in
      if
        reruns <> []
        && List.for_all
             (fun r -> ci_side ~kinds:[ "ci"; "rerun" ] r || not_this_pr r)
             sugg
      then "rerun"
      else "other"

let is_merge pr = List.mem merge_label (str_list (pr |. "labels"))

let label_badges labels stale =
  let badge lb =
    if List.mem lb stale then
      Printf.sprintf
        " <span class=\"lbl stale\" title=\"The author pushed or commented \
         after this label was added: it may be out of date\">%s (author \
         replied since)</span>"
        (e_s lb)
    else
      let cls =
        if lb = merge_label then " merge"
        else if lb = no_merge_label then " nomerge"
        else ""
      in
      Printf.sprintf " <span class=\"lbl%s\" title=\"GitHub label\">%s</span>"
        cls (e_s lb)
  in
  String.concat ""
    (List.map badge (Py.sort_by (fun lb -> lb <> merge_label) labels))

(* --- Near-duplicate suggestions ------------------------------------------- *)

let conf_rank c =
  match c with "high" -> 0 | "medium" -> 1 | "low" -> 2 | _ -> 1

let conf_word = Render.conf_word

(** Suggestions with the same key say the same thing about different packages or
    jobs. LLM answers and ranked candidates are never merged. *)
let merge_key r =
  if r.rule = Some "llm" || Suggestion.list r.candidates <> [] then None
  else
    (* Not the title: it follows from the rule (a merged pattern's title also counts its packages). *)
    Some (r.class_, r.rule, r.fix_in, Suggestion.get r.Suggestion.new_)

let combine rs =
  match rs with
  | [ r ] -> r
  | [] -> assert false
  | r0 :: _ ->
      let uniq xs = Py.dedup (List.filter (fun x -> x <> "") xs) in
      let m = Suggestion.copy r0 in
      m.merged <- Some (List.length rs);
      let packages =
        uniq (List.map (fun r -> Suggestion.str_field r.Suggestion.failing) rs)
      in
      m.packages <- Some packages;
      m.Suggestion.failing <-
        (match packages with
        | [] -> Null
        | _ -> Set (String.concat ", " packages));
      m.Suggestion.jobs <-
        Some
          (uniq
             (List.concat_map
                (fun r ->
                  match r.Suggestion.jobs with
                  | Some (_ :: _ as l) -> l
                  | _ -> [ Suggestion.str r.Suggestion.variant ])
                rs));
      m.variant <- None;
      m.url <- None;
      (* the jobs are listed instead of the first one's *)
      m.errors <- Some (uniq (List.map (fun r -> Suggestion.str r.error) rs));
      m.notes <- Some (uniq (List.map (fun r -> Suggestion.str r.note) rs));
      m.error <- None;
      m.note <- None;
      let also =
        List.sort_uniq compare
          (List.concat_map (fun r -> Suggestion.list r.also_failing_in) rs)
      in
      m.also_failing_in <- (match also with [] -> None | l -> Some l);
      m.Suggestion.experimental <-
        Some (List.for_all (fun r -> r.Suggestion.experimental = Some true) rs);
      m.preexisting <-
        (if List.for_all (fun r -> Suggestion.nonempty r.preexisting) rs then
           r0.preexisting
         else None);
      let conf r = Suggestion.get r.confidence in
      let key c =
        conf_rank
          (List.hd (String.split_on_char ' ' (Option.value ~default:"" c)))
      in
      let best =
        List.fold_left
          (fun acc r -> if key (conf r) > key acc then conf r else acc)
          (conf r0) rs
      in
      m.confidence <- Suggestion.field_of_option best;
      m

let merge_similar rs =
  let order = ref [] and by = Hashtbl.create 16 in
  List.iter
    (fun r ->
      match merge_key r with
      | None -> order := ref [ r ] :: !order
      | Some k -> (
          match Hashtbl.find_opt by k with
          | Some g -> g := r :: !g
          | None ->
              let g = ref [ r ] in
              Hashtbl.add by k g;
              order := g :: !order))
    rs;
  List.rev_map (fun g -> combine (List.rev !g)) !order

(** Package names, versions of one package counted together. *)
let short_list ?(n = 3) names =
  let by =
    Py.group_by
      (fun x ->
        if String.contains x ' ' then x
        else List.hd (String.split_on_char '.' x))
      names
  in
  let items =
    List.map
      (fun (k, vs) ->
        match vs with
        | [ v ] -> v
        | _ -> Printf.sprintf "%s (%d versions)" k (List.length vs))
      by
  in
  String.concat ", " (Py.take n items)
  ^
  if List.length items > n then
    Printf.sprintf " and %d more" (List.length items - n)
  else ""

(* Python's y.rstrip(".… "). *)
let rec strip_dots y =
  if String.ends_with ~suffix:"." y || String.ends_with ~suffix:" " y then
    strip_dots (String.sub y 0 (String.length y - 1))
  else if String.ends_with ~suffix:"\xe2\x80\xa6" y then
    strip_dots (String.sub y 0 (String.length y - 3))
  else y

(** The packages of a suggestion, short: a merged one lists them, others may
    have a comma-separated [failing], cut with "...". *)
let packages_text ?(n = 2) r =
  let names = ref [] and cut = ref false in
  let xs =
    match r.packages with
    | Some (_ :: _ as l) -> l
    | _ -> [ Suggestion.str_field r.Suggestion.failing ]
  in
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          let y = Py.strip y in
          let y =
            if
              String.ends_with ~suffix:"..." y
              || String.ends_with ~suffix:"\xe2\x80\xa6" y
            then (
              cut := true;
              strip_dots y)
            else y
          in
          if y <> "" && not (List.mem y !names) then names := !names @ [ y ])
        (Py.split_on ", " x))
    xs;
  if !names = [] then ""
  else
    let s = short_list ~n !names in
    if !cut && not (Py.contains s " more") then s ^ " and more" else s

let css =
  {css|
:root{--bg:#fbfbfa;--fg:#1d1f21;--mut:#5f6368;--card:#fff;--bd:#dadce0;--code:#f3f3f1;--acc:#0b57d0;
--add:#116329;--addbg:#dafbe1;--del:#82071e;--delbg:#ffebe9;--err:#b3261e;
--ok:#116329;--chg:#b45309;--blk:#6639ba;--ci:#5f6368;--rerun:#0b7285;--look:#b3261e;--na:#80868b}
@media (prefers-color-scheme:dark){:root{--bg:#121416;--fg:#e3e3e3;--mut:#9aa0a6;--card:#1c1f23;--bd:#3c4043;
--code:#25292e;--acc:#8ab4f8;--add:#7ee2a8;--addbg:#12361f;--del:#ffa198;--delbg:#3d1416;--err:#f28b82;
--ok:#7ee2a8;--chg:#f2b56b;--blk:#d2a8ff;--ci:#9aa0a6;--rerun:#66d9e8;--look:#f28b82;--na:#6f757b}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.45 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}
main{max-width:1000px;margin:0 auto;padding:16px}
a{color:var(--acc)}
header{display:flex;flex-wrap:wrap;align-items:baseline;gap:2px 16px}
h1{font-size:1.35rem;margin:.2em 0;flex:1 1 auto}
header nav{font-size:.85rem;color:var(--mut)}
.lead{margin:.2em 0 1em;max-width:75ch;color:var(--mut);font-size:.92rem}
h2{font-size:1.1rem;margin:1.4em 0 .3em}h3{font-size:1rem;margin:1.2em 0 .4em}
h4{font-size:.92rem;margin:14px 0 4px}
.sub{font-weight:400;font-size:.85rem;color:var(--mut);margin:0 0 .5em}
.meta{color:var(--mut);font-size:.85rem;overflow-wrap:anywhere}
ul.ready{list-style:none;margin:0;padding:0;border:1px solid var(--bd);border-radius:8px;background:var(--card)}
ul.ready li{display:flex;flex-wrap:wrap;gap:2px 10px;padding:6px 12px;border-top:1px solid var(--bd);align-items:baseline}
ul.ready li:first-child{border-top:0}
ul.ready .t{flex:1 1 12em;min-width:0;overflow-wrap:anywhere}
ul.ready .r{font-size:.85rem;color:var(--mut);overflow-wrap:anywhere}
.num{font-weight:600;white-space:nowrap}
.filters{display:flex;flex-wrap:wrap;gap:6px 10px;margin:6px 0 8px;align-items:center;background:var(--bg);padding:6px 0}
@media (min-width:700px){.filters{position:sticky;top:0;z-index:1}details.pr{scroll-margin-top:96px}}
.seg{display:flex;flex-wrap:wrap;border:1px solid var(--bd);border-radius:8px;overflow:hidden;background:var(--card)}
.seg button{font:inherit;font-size:.85rem;padding:5px 10px;border:0;border-right:1px solid var(--bd);background:var(--card);
color:var(--fg);cursor:pointer;white-space:nowrap}
.seg button:last-child{border-right:0}
.seg button[aria-pressed=true]{background:var(--fg);color:var(--bg)}
.seg i{display:inline-block;width:8px;height:8px;border-radius:50%;margin-right:5px;background:var(--v)}
.seg b{font-weight:400;opacity:.75;margin-left:3px}
.filters label{font-size:.85rem;color:var(--mut);white-space:nowrap}
.filters input[type=search]{font:inherit;font-size:.9rem;padding:5px 10px;border-radius:8px;border:1px solid var(--bd);
background:var(--card);color:var(--fg);flex:1 1 200px;min-width:0}
button:focus-visible,summary:focus-visible,input:focus-visible,a:focus-visible{outline:2px solid var(--acc);outline-offset:1px}
details.pr{background:var(--card);border:1px solid var(--bd);border-left:4px solid var(--v);border-radius:6px;margin:4px 0}
details.pr>summary{cursor:pointer;padding:6px 12px 6px 8px;list-style:none;display:grid;
grid-template-columns:1em auto 1fr auto;gap:0 8px;align-items:baseline}
details.pr>summary::-webkit-details-marker{display:none}
details.pr>summary::before{content:"\25B8";color:var(--mut);font-size:.8rem}
details.pr[open]>summary::before{content:"\25BE"}
.ttl{min-width:0;overflow-wrap:anywhere}
.who{font-size:.82rem;color:var(--mut);white-space:nowrap}
.l2{grid-column:3/5;font-size:.85rem;color:var(--mut);overflow-wrap:anywhere}
.v{font-weight:600;color:var(--v)}
.l2 code{font-size:.8rem;background:var(--code);padding:0 4px;border-radius:4px;color:var(--fg)}
.l2 .run{color:var(--acc)}
.l2 a.ci{white-space:nowrap;margin-left:4px}
.lbl{font-size:.75rem;color:var(--mut);border:1px solid var(--bd);border-radius:4px;padding:0 4px;white-space:nowrap}
.lbl.merge{color:var(--acc);border-color:var(--acc)}.lbl.nomerge{color:var(--err);border-color:var(--err)}
.lbl.stale{border-style:dashed}
.v-ready{--v:var(--ok)}.v-change{--v:var(--chg)}.v-blocked{--v:var(--blk)}.v-ci{--v:var(--ci)}.v-rerun{--v:var(--rerun)}
.v-other{--v:var(--look)}.v-error{--v:var(--na)}.v-all{--v:transparent}
details.pr.v-error{border-left-style:dashed}
@media (max-width:600px){details.pr>summary{grid-template-columns:1em auto 1fr}
.who{grid-column:3;grid-row:3}.l2{grid-column:3}}
.body{padding:4px 12px 12px;border-top:1px solid var(--bd)}
.bar{display:flex;flex-wrap:wrap;gap:6px 14px;align-items:center;font-size:.85rem;color:var(--mut);margin:8px 0}
.bar button{font:inherit;font-size:.85rem;padding:3px 10px;border-radius:6px;border:1px solid var(--bd);
background:var(--card);color:var(--fg);cursor:pointer}
details.mdv>summary{cursor:pointer;color:var(--acc)}
details.mdv[open]{order:9;flex-basis:100%}
details.mdv textarea{width:100%;margin-top:4px;font:.8rem/1.4 ui-monospace,SFMono-Regular,Menlo,monospace;
background:var(--code);color:var(--fg);border:1px solid var(--bd);border-radius:6px;padding:6px 8px}
h4.g-change{color:var(--chg)}h4.g-else{color:var(--blk)}
.sugg{border-left:3px solid var(--chg);padding:2px 0 2px 10px;margin:6px 0 12px}
.sh{overflow-wrap:anywhere}
.x{color:var(--mut);font-size:.85rem}
.pkg{overflow-wrap:anywhere}
details.it{margin:3px 0;font-size:.9rem;overflow-wrap:anywhere}
details.it>summary,details.others>summary,details.hint>summary{cursor:pointer}
details.it>div{padding:2px 0 6px 18px}
details.others{margin:10px 0 0;font-size:.9rem}
details.others>summary{color:var(--mut)}
details.others>details.it{margin-left:18px}
pre{background:var(--code);border-radius:6px;padding:6px 8px;margin:4px 0;overflow-x:auto;font-size:.8rem;
white-space:pre-wrap;overflow-wrap:anywhere}
pre.err{max-height:16em;overflow-y:auto}
.diff span{display:block}.add{color:var(--add);background:var(--addbg)}.del{color:var(--del);background:var(--delbg)}
.alt,.cm{color:var(--mut)}
.note{font-size:.88rem;margin:4px 0;overflow-wrap:anywhere}
details.hint>summary{color:var(--mut);font-size:.85rem;margin:8px 0}
ul.jobs{margin:4px 0;padding-left:20px;font-size:.85rem}
dl.help dt{font-weight:600;margin-top:.6em}dl.help dd{margin:0 0 0 1.2em;font-size:.9rem;max-width:75ch}
dl.help dt .v{margin-right:2px}
details.jobs{color:var(--mut);font-size:.85rem;overflow-wrap:anywhere}
details.jobs>summary{cursor:pointer}details.jobs ul{margin:2px 0 4px;padding-left:20px}
ul.ev{margin:2px 0 6px;padding-left:1.4em;font-size:.85rem}.yes{color:var(--add)}.no{color:var(--del)}.unk{color:var(--mut)}
.tw{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:.88rem}
th,td{border-bottom:1px solid var(--bd);padding:5px 8px;text-align:left;vertical-align:top}
td:last-child{overflow-wrap:anywhere}.nw{white-space:nowrap}
details.more>summary{cursor:pointer;color:var(--acc);margin:6px 0 6px 12px}
details.more>ul.ready{margin-top:4px}
.hidden{display:none}
|css}

let js =
  {js|
(function(){
  // Job links carry only their variant: the href is the PR's data-ci + variant.
  [].forEach.call(document.querySelectorAll('a[data-job]'),function(a){
    var p=a.closest('[data-ci]'); if(!p)return;
    a.href=p.getAttribute('data-ci')+a.getAttribute('data-job');   // variants are checked: [A-Za-z0-9._+~,-]
    a.rel='noopener noreferrer';
  });
  var cur='all', q='', merge=false;
  var prs=[].slice.call(document.querySelectorAll('details.pr'));
  var btns=[].slice.call(document.querySelectorAll('.seg button[data-f]'));
  var mb=document.getElementById('merge'), shown=document.getElementById('shown');
  function apply(){
    var n=0;
    prs.forEach(function(d){
      var ok=(cur==='all'||d.getAttribute('data-v')===cur)&&(!merge||d.hasAttribute('data-merge'))&&
             (!q||d.getAttribute('data-search').indexOf(q)>=0);
      d.classList.toggle('hidden',!ok); if(ok)n++;
    });
    btns.forEach(function(b){b.setAttribute('aria-pressed',b.getAttribute('data-f')===cur?'true':'false');});
    if(shown)shown.textContent=(n===prs.length?'':n+' shown');
  }
  btns.forEach(function(b){b.addEventListener('click',function(){cur=b.getAttribute('data-f');apply();});});
  var qi=document.getElementById('q');
  if(qi)qi.addEventListener('input',function(ev){q=ev.target.value.toLowerCase().trim();apply();});
  if(mb)mb.addEventListener('change',function(){merge=mb.checked;apply();});
  apply();
  function openHash(){
    var d=location.hash&&document.getElementById(location.hash.slice(1));
    if(d&&d.tagName==='DETAILS'){d.classList.remove('hidden');d.open=true;}
  }
  window.addEventListener('hashchange',openHash); openHash();
  [].forEach.call(document.querySelectorAll('button.copy'),function(b){
    var label=b.textContent;
    function done(ok){
      b.textContent=ok?'Copied':'Select the text below and copy it';
      setTimeout(function(){b.textContent=label;},2000);
    }
    b.addEventListener('click',function(){
      var t=document.getElementById(b.getAttribute('data-for'));
      function fallback(){
        var d=t.closest('details'); if(d)d.open=true;
        t.focus(); t.select();
        var ok=false; try{ok=document.execCommand('copy');}catch(e){}
        done(ok);
      }
      if(navigator.clipboard&&window.isSecureContext){
        navigator.clipboard.writeText(t.value).then(function(){done(true);},fallback);
      } else fallback();
    });
  });
})();
|js}

(** The other packages where a fix goes, in order. Another package the analysis
    cannot name (the dependency declaring some depexts) is left out: the
    headline then names the finding instead. *)
let blocked_by sugg =
  List.fold_left
    (fun out r ->
      match Where.where_pkg r with
      | Some name when name <> "" && not (List.mem name out) -> out @ [ name ]
      | _ -> out)
    [] sugg

(** The title of a suggestion, from its rule (stored titles may be older
    wordings). *)
let title_of = Titles.title_of

(* A suggested change written as an opam line: a quoted package, a field, a variable. *)
let opam_line = {|\s*("[\w+.@-]+"|[a-z][\w-]*:|extra-source\b|%\{)|}

(** The failed jobs of a PR in words ("1 analysis failed"), network errors,
    cancelled and skipped jobs left out; None if there are none. *)
let failed_sorted pr =
  let failed = Json.to_assoc (pr |. "jobs" |. "failed") in
  List.stable_sort
    (fun (_, a) (_, b) -> compare (Json.to_int b) (Json.to_int a))
    failed

let unexplained_jobs pr =
  match
    String.concat ", "
      (List.filter_map
         (fun (c, n) ->
           if List.mem c noise then None
           else Some (Printf.sprintf "%s %s" (str n) (job_category c)))
         (failed_sorted pr))
  with
  | "" -> None
  | s -> Some s

let failed_count pr =
  let failed = Json.to_assoc (pr |. "jobs" |. "failed") in
  ( Py.sum
      (List.filter_map
         (fun (k, v) -> if List.mem k noise then None else Some (Json.to_int v))
         failed),
    Py.sum (List.map (fun (_, v) -> Json.to_int v) failed) )

let headline ?v ?(as_html = true) pr =
  let v = match v with Some v -> v | None -> verdict pr in
  let t s = if as_html then e_s s else s in
  let code s =
    if as_html then "<code>" ^ e_s s ^ "</code>" else "`" ^ s ^ "`"
  in
  if v = "error" then
    t
      (if truthy (pr |. "pending") then
         "not analysed yet; the next page update will cover it"
       else "the analysis failed")
  else
    let sugg = suggestions_of (pr |. "result") in
    if sugg = [] then
      t
        (match unexplained_jobs pr with
        | Some jobs ->
            Printf.sprintf "no explanation for the failed jobs (%s)" jobs
        | None -> "no build or test failure to analyse")
    else begin
      let g = groups pr in
      let with_pkgs r =
        let pkgs = packages_text r in
        title_of r ^ if pkgs <> "" then " (" ^ pkgs ^ ")" else ""
      in
      let parts =
        if v = "change" then begin
          let items =
            merge_similar (group g "fix") @ merge_similar (group g "workaround")
          in
          let r = List.hd items in
          let pkgs = packages_text r in
          let s =
            if
              Suggestion.nonempty_field r.Suggestion.new_
              && Rx.match_ opam_line (Suggestion.str_field r.Suggestion.new_)
                 <> None
            then
              code
                (shorten
                   (List.hd
                      (Py.split_on "   "
                         (Suggestion.str_field r.Suggestion.new_)))
                   48)
              ^ if pkgs <> "" then t (" for " ^ pkgs) else ""
            else t (title_of r ^ if pkgs <> "" then " (" ^ pkgs ^ ")" else "")
          in
          let s =
            if Suggestion.str r.class_ = "workaround" then s ^ t " (workaround)"
            else s
          in
          let n = List.length items in
          let s =
            if n > 1 then
              s
              ^ t
                  (Printf.sprintf " and %d more change%s" (n - 1)
                     (if n > 2 then "s" else ""))
            else s
          in
          let others = blocked_by sugg in
          [ s ]
          @
          if others <> [] then
            [ t ("also blocked by " ^ short_list ~n:2 others) ]
          else []
        end
        else if v = "blocked" && blocked_by sugg <> [] then
          let others = blocked_by sugg in
          [
            t
              ((if List.length others = 1 then "needs a fix in "
                else "needs fixes in ")
              ^ short_list others);
          ]
        else if v = "blocked" then
          (* The fix goes in a dependency the analysis cannot name (depexts declared
             by a dependency, say): the first such finding. *)
          let pool =
            match List.filter other_package sugg with [] -> sugg | l -> l
          in
          [ t (with_pkgs (List.hd (merge_similar pool))) ]
        else if v = "rerun" then
          let r =
            List.find
              (fun r -> Where.where_kind r = "rerun" && not (not_this_pr r))
              sugg
          in
          [
            t
              (title_of r
              ^
              if Suggestion.nonempty r.error then
                " (" ^ shorten (Suggestion.str r.error) 60 ^ ")"
              else "");
          ]
        else
          let pool =
            if v = "other" then
              List.filter (fun r -> Where.where_kind r = "pr") sugg
            else sugg
          in
          let pool =
            match List.filter (fun r -> r.rule <> Some "llm") pool with
            | [] -> ( match pool with [] -> sugg | p -> p)
            | l -> l
          in
          [ t (with_pkgs (List.hd (merge_similar pool))) ]
      in
      String.concat " \xc2\xb7 " parts
    end

let comment_markdown pr =
  let res = match pr |. "result" with `Null -> `Assoc [] | r -> r in
  let results =
    List.filter
      (fun r ->
        Suggestion.nonempty r.class_
        && not
             (r.rule = Some "llm"
             && String.starts_with ~prefix:"low"
                  (Suggestion.str_field r.confidence)))
      (suggestions_of res)
  in
  let total =
    match pr |. "jobs" |. "total" with v when truthy v -> num v | _ -> 0
  in
  let to_int v = if truthy v then num v else 0 in
  let md =
    Render.render_comment_counts
      ~unfinished:(to_int (pr |. "ci_unfinished"))
      ~njobs:total
      ~unanalysed:(to_int (res |. "unanalysed"))
      (sget pr "sha") results
  in
  if seems_ready pr then begin
    (* After the paragraph on where the results come from. *)
    let lines = String.split_on_char '\n' md in
    let i =
      match
        List.find_index
          (String.starts_with ~prefix:"From the [CI results]")
          lines
      with
      | Some k -> k
      | None -> 1
    in
    let extra =
      [
        "";
        "**None of the failures is this PR's fault**: each one also fails \
         without this "
        ^ "PR, comes from the CI, or is on an experimental platform (details \
           below).";
      ]
    in
    String.concat "\n"
      (Py.take (i + 1) lines @ extra @ Py.drop_list (i + 1) lines)
  end
  else md

(** A button copying the Markdown comment, and the Markdown itself, folded. *)
let copy_block pr =
  let n = num (pr |. "number") in
  match comment_markdown pr with
  | exception ex ->
      (* a malformed entry must not break the page *)
      Printf.eprintf "#%d: no Markdown comment (%s)\n%!" n
        (Printexc.to_string ex);
      ""
  | md ->
      Printf.sprintf
        "<button type=\"button\" class=\"copy\" data-for=\"md-%d\">Copy as a \
         PR comment</button><details class=\"mdv\"><summary>Show the \
         Markdown</summary><textarea id=\"md-%d\" readonly rows=\"14\" \
         spellcheck=\"false\" aria-label=\"PR comment, \
         Markdown\">%s</textarea></details>"
        n n (e_s md)

let safe_variant = "[A-Za-z0-9._+~,-]{1,200}"
let safe_sha = "[0-9a-f]{7,40}"

let job_link sha variant =
  match sha with
  | Some sha
    when sha <> ""
         && Rx.fullmatch safe_sha sha <> None
         && Rx.fullmatch safe_variant variant <> None ->
      Printf.sprintf "<a data-job=\"%s\">%s</a>" (e_s variant)
        (e_s (Ci.describe_job variant))
  | _ -> e_s (Ci.describe_job variant)

(** Where the fix goes, unless it is this PR (implied, or not meaningful for a
    failure without a change), in words (from [where_kind], as stored [where]
    values may be older wordings). *)
let where_of r =
  match Titles.where_text r with
  | None | Some "" | Some "this PR" -> None
  | w -> w

(** The confidence, only when it is not high. *)
let conf_note r =
  let c = conf_word r in
  if c <> "" && c <> "high" then Some (c ^ " confidence") else None

let release_date = {|release date of (\S+) \((\d{4}-\d\d-\d\d)\)$|}

(** The details of a suggestion: job, error, change, candidates, notes and jobs.
*)
let suggestion_body ?sha ?(full = true) ?year r =
  let rule = Suggestion.str r.rule in
  let out = ref [] in
  let add s = out := s :: !out in
  let jobs = Suggestion.list r.Suggestion.jobs in
  let lists_jobs = rule = "rerun" || rule = "unrecognised" in
  let meta = ref [] in
  if
    Suggestion.nonempty r.Suggestion.variant
    && Suggestion.nonempty r.Suggestion.url
    && not lists_jobs
  then
    meta :=
      !meta
      @ [
          "Job: "
          ^ link
              (Json.opt_string r.Suggestion.url)
              (match r.job with
              | Some j when j <> "" -> j
              | _ -> Ci.describe_job (Suggestion.str r.Suggestion.variant));
        ];
  (match where_of r with
  | Some w when full ->
      let label = Titles.fix_label w in
      meta :=
        !meta
        @ [
            e_s
              (String.capitalize_ascii (String.sub label 0 1)
              ^ String.sub label 1 (String.length label - 1));
          ]
  | _ -> ());
  if !meta <> [] then
    add
      (Printf.sprintf "<div class=\"meta\">%s</div>"
         (String.concat " \xc2\xb7 " !meta));
  let errors =
    match r.errors with
    | Some (_ :: _ as l) -> l
    | _ ->
        if Suggestion.nonempty r.error then [ Suggestion.str r.error ] else []
  in
  List.iter
    (fun err -> add (Printf.sprintf "<pre class=\"err\">%s</pre>" (e_s err)))
    (Py.take 2 errors);
  let ne = List.length errors in
  if ne > 2 then
    add
      (Printf.sprintf
         "<div class=\"meta\">and %d other error message%s (see the jobs)</div>"
         (ne - 2)
         (plural (ne - 2)));
  if Suggestion.nonempty_field r.Suggestion.new_ then begin
    let diff =
      (if Suggestion.nonempty_field r.old then
         [
           Printf.sprintf "<span class=\"del\">- %s</span>"
             (e_s (Suggestion.str_field r.old));
         ]
       else [])
      @ [
          Printf.sprintf "<span class=\"add\">+ %s</span>"
            (e_s (Suggestion.str_field r.Suggestion.new_));
        ]
      @
      if Suggestion.nonempty r.alternative then
        [
          Printf.sprintf "<span class=\"alt\">or %s</span>"
            (e_s (Suggestion.str r.alternative));
        ]
      else []
    in
    add ("<pre class=\"diff\">" ^ String.concat "\n" diff ^ "</pre>")
  end
  else if Suggestion.nonempty r.alternative then
    (* a change described in the note, and an alternative *)
    add
      (Printf.sprintf
         "<pre class=\"diff\"><span class=\"alt\">or %s</span></pre>"
         (e_s (Suggestion.str r.alternative)));
  if Suggestion.list r.candidates <> [] then begin
    let what =
      match Rx.match_ release_date (Suggestion.str r.compared_with) with
      | Some m ->
          Printf.sprintf
            "Dependencies newer than %s's release (%s) that may have changed \
             the output:"
            (Rx.group m 1)
            (day ?year (Some (Rx.group m 2)))
      | None ->
          Printf.sprintf
            "Dependencies that may have caused the failure, compared with %s:"
            (Render.compared_text ?year:(Option.bind year int_of_string_opt) r)
    in
    add (Printf.sprintf "<div class=\"meta\">%s</div>" (e_s what));
    List.iter
      (fun (c : Suggestion.Candidate.t) ->
        let g = Option.value ~default:"None" in
        let what =
          if c.first_after <> None then
            Printf.sprintf "CI used %s; first release after it: %s"
              (g c.failing) (g c.first_after)
          else
            Printf.sprintf "passes with %s, fails with %s" (g c.passing)
              (g c.failing)
        in
        let diff =
          [
            Printf.sprintf "<span class=\"cm\"># %s: %s</span>"
              (e_s (Suggestion.str c.dep))
              (e_s what);
          ]
          @ (match Suggestion.get c.old with
            | Some old when old <> "" ->
                [ Printf.sprintf "<span class=\"del\">- %s</span>" (e_s old) ]
            | _ -> [])
          @
          match Suggestion.get c.new_ with
          | Some new_ when new_ <> "" ->
              [ Printf.sprintf "<span class=\"add\">+ %s</span>" (e_s new_) ]
          | _ -> []
        in
        add ("<pre class=\"diff\">" ^ String.concat "\n" diff ^ "</pre>");
        match c.changelog with
        | Some log when not (Suggestion.changelog_is_empty log) ->
            add
              (Printf.sprintf
                 "<details class=\"jobs\"><summary>Changelog of %s: %s (%s, \
                  written by its authors)</summary><pre \
                  class=\"err\">%s%s</pre></details>"
                 (e_s (Suggestion.str c.dep))
                 (e_s (String.concat ", " (Suggestion.list log.versions)))
                 (e_s
                    (match Suggestion.get log.file with
                    | Some f when f <> "" -> f
                    | _ -> "?"))
                 (e_s (Suggestion.str log.text))
                 (if log.cut = Some true then "\n\xe2\x80\xa6" else ""))
        | _ -> ())
      (Suggestion.list r.candidates)
  end;
  if Suggestion.list r.also_newer <> [] then
    add
      (Printf.sprintf
         "<div class=\"meta\">Also released after it (not ranked): %s</div>"
         (e_s (String.concat ", " (Suggestion.list r.also_newer))));
  (match r.evidence with
  | Some (_ :: _ as ev) ->
      let items =
        List.map
          (fun (k, v) ->
            let word, cls =
              match v with
              | Some true -> ("yes", "yes")
              | Some false -> ("no", "no")
              | None -> ("?", "unk")
            in
            Printf.sprintf "<li>%s: <span class=\"%s\">%s</span></li>" (e_s k)
              cls word)
          ev
      in
      let src =
        if Suggestion.nonempty r.source then
          Printf.sprintf " (looked up in %s)" (e_s (Suggestion.str r.source))
        else ""
      in
      add
        (Printf.sprintf
           "<div class=\"meta\">Is it defined in each version?%s</div><ul \
            class=\"ev\">%s</ul>"
           src (String.concat "" items))
  | _ ->
      if Suggestion.nonempty r.source then
        add
          (Printf.sprintf "<div class=\"meta\">Looked up in: %s</div>"
             (e_s (Suggestion.str r.source))));
  List.iter
    (fun t ->
      add (Printf.sprintf "<div class=\"meta\">Tried: %s</div>" (e_s t)))
    (Suggestion.list r.tried);
  let notes =
    match r.notes with
    | Some (_ :: _ as l) -> l
    | _ -> if Suggestion.nonempty r.note then [ Suggestion.str r.note ] else []
  in
  List.iter
    (fun note -> add (Printf.sprintf "<p class=\"note\">%s</p>" (e_s note)))
    (Py.take 1 notes);
  let nn = List.length notes in
  if nn > 1 then
    add
      (Printf.sprintf
         "<details class=\"jobs\"><summary>%d similar note%s for the other \
          packages</summary>"
         (nn - 1)
         (plural (nn - 1))
      ^ String.concat ""
          (List.map
             (fun x -> Printf.sprintf "<p class=\"note\">%s</p>" (e_s x))
             (List.tl notes))
      ^ "</details>");
  if Suggestion.list r.also_failing_in <> [] then begin
    let prs = Suggestion.list r.also_failing_in in
    let k = List.length prs in
    add
      (Printf.sprintf
         "<p class=\"note shared\">Also fails in %d other open PR%s, so \
          probably not this PR's fault: "
         k
         (if k <> 1 then "s" else "")
      ^ String.concat ", "
          (List.map
             (fun n ->
               links
                 (Printf.sprintf "%s/pull/%d" repo_url n)
                 (Printf.sprintf "#%d" n))
             (Py.take 12 prs))
      ^ (if k > 12 then Printf.sprintf " and %d more" (k - 12) else "")
      ^ "</p>")
  end;
  if lists_jobs && jobs <> [] then
    add
      (Printf.sprintf "<ul class=\"jobs\">%s</ul>"
         (String.concat ""
            (List.map (fun v -> "<li>" ^ job_link sha v ^ "</li>") jobs)))
  else begin
    let variant = r.Suggestion.variant in
    let others = List.filter (fun v -> Some v <> variant) jobs in
    if others <> [] then begin
      (* A matrix suggestion's variant is a label, not one of its jobs: then list them all. *)
      let no = List.length others and nj = List.length jobs in
      let what =
        if no < nj then
          Printf.sprintf "Same failure on %d other job%s" no
            (if no <> 1 then "s" else "")
        else if nj > 1 then Printf.sprintf "All %d jobs" nj
        else "1 job"
      in
      add
        (Printf.sprintf
           "<details class=\"jobs\"><summary>%s</summary><ul>%s</ul></details>"
           what
           (String.concat ""
              (List.map (fun v -> "<li>" ^ job_link sha v ^ "</li>") others)))
    end
  end;
  String.concat "\n" (List.rev !out)

let low_llm r = r.rule = Some "llm" && conf_word r = "low"

let render_suggestion ?sha ?year r =
  let head =
    [ Printf.sprintf "<b>%s</b>" (e_s (title_of r)) ]
    @
    if Suggestion.nonempty_field r.Suggestion.failing then
      [
        Printf.sprintf "<span class=\"pkg\">%s</span>"
          (e_s (Suggestion.str_field r.Suggestion.failing));
      ]
    else []
  in
  let tags =
    (if r.class_ = Some "workaround" then
       [ "<span title=\"Skips platforms or tests\">workaround</span>" ]
     else [])
    @ (if r.Suggestion.experimental = Some true then
         [
           "<span title=\"Failures on experimental platforms do not block the \
            PR\">experimental platform</span>";
         ]
       else [])
    @
    match conf_note r with
    | Some c ->
        [
          Printf.sprintf "<span title=\"%s\">%s</span>"
            (e_s (Suggestion.str_field r.confidence))
            (e_s c);
        ]
    | None -> []
  in
  let head =
    head
    @
    if tags <> [] then
      [
        Printf.sprintf "<span class=\"x\">%s</span>"
          (String.concat " \xc2\xb7 " tags);
      ]
    else []
  in
  let cls =
    match r.class_ with Some v when v <> "" -> e_s v | _ -> "explanation"
  in
  let html_ =
    Printf.sprintf "<div class=\"sugg c-%s\"><div class=\"sh\">%s</div>" cls
      (String.concat " \xc2\xb7 " head)
    ^ suggestion_body ?sha ?year r
    ^ "</div>"
  in
  if low_llm r then
    (* Low-confidence LLM answers were never right in the evaluation: folded. *)
    Printf.sprintf
      "<details class=\"hint\"><summary>LLM diagnosis (low \
       confidence)%s</summary>%s</details>"
      (if Suggestion.nonempty_field r.Suggestion.failing then
         ": " ^ e_s (Suggestion.str_field r.Suggestion.failing)
       else "")
      html_
  else html_

let render_item ?sha ?year r =
  let title =
    if low_llm r then "LLM diagnosis (low confidence)" else title_of r
  in
  let pkgs = packages_text r in
  let head =
    [ Printf.sprintf "<b>%s</b>" (e_s title) ]
    @
    if pkgs <> "" then
      [ Printf.sprintf "<span class=\"pkg\">%s</span>" (e_s pkgs) ]
    else []
  in
  let jobs = List.length (Suggestion.list r.Suggestion.jobs) in
  let x =
    (if Option.value ~default:0 r.merged <> 0 then
       [
         Printf.sprintf "%d failures, %d job%s" (Option.get r.merged) jobs
           (if jobs <> 1 then "s" else "");
       ]
     else if jobs > 1 then [ Printf.sprintf "%d jobs" jobs ]
     else [])
    @ (match where_of r with
      | Some w when r.rule <> Some "rerun" -> [ Titles.fix_label w ]
      | _ -> []) (* "Jobs to rerun" says it *)
    @ (if
         Suggestion.nonempty r.preexisting
         && not (r.preexisting_unverified = Some true)
       then [ "pre-existing" ]
       else [])
    @ (if Suggestion.list r.also_failing_in <> [] then
         let k = List.length (Suggestion.list r.also_failing_in) in
         [
           Printf.sprintf "also in %d other PR%s" k (if k <> 1 then "s" else "");
         ]
       else [])
    @ (if r.Suggestion.experimental = Some true then [ "experimental platform" ]
       else [])
    @ match conf_note r with Some c -> [ c ] | None -> []
  in
  let head =
    head
    @
    if x <> [] then
      [
        Printf.sprintf "<span class=\"x\">%s</span>"
          (e_s (String.concat " \xc2\xb7 " x));
      ]
    else []
  in
  Printf.sprintf
    "<details class=\"it\"><summary>%s</summary><div>%s</div></details>"
    (String.concat " \xc2\xb7 " head)
    (suggestion_body ?sha ~full:false ?year r)

let ready_section ?(shown = 5) (data : Json.t) =
  let rows =
    List.filter_map
      (fun p ->
        if (not (truthy (p |. "draft"))) && blocking_labels p = [] then
          Some (p, true)
        else None)
      (Json.to_list (data |. "passing"))
    @ List.filter_map
        (fun p -> if seems_ready p then Some (p, false) else None)
        (Json.to_list (data |. "prs"))
  in
  (* Those labelled "consider for merge" first, then the most recently updated. *)
  let rows = Py.sort_by ~reverse:true (fun (p, _) -> sget p "updated") rows in
  let rows = Py.sort_by (fun (p, _) -> not (is_merge p)) rows in
  if rows = [] then ""
  else begin
    let year = Py.prefix (sget data "generated") 4 in
    let items =
      List.map
        (fun (p, passes) ->
          let n = num (p |. "number") in
          let ci =
            if passes then "CI passes"
            else
              Printf.sprintf "fails, <a href=\"#pr-%d\">not this PR's fault</a>"
                n
          in
          Printf.sprintf
            "<li>%s<span class=\"t\">%s%s</span><span class=\"r\">%s \xc2\xb7 \
             %s \xc2\xb7 %s</span></li>"
            (link ~cls:"num"
               (`String (Printf.sprintf "%s/pull/%d" repo_url n))
               (Printf.sprintf "#%d" n))
            (e (p |. "title"))
            (label_badges
               (str_list (p |. "labels"))
               (str_list (p |. "stale_labels")))
            (e
               (match p |. "author" with
               | v when truthy v -> v
               | _ -> `String "?"))
            ci
            (e_s (day_v ~year (p |. "updated"))))
        rows
    in
    let k = List.length items in
    let out =
      Printf.sprintf
        "<section id=\"ready\"><h2>Ready as far as CI goes <span \
         class=\"sub\">(%d)</span></h2>"
        k
      ^ "<p class=\"sub\">CI passes, or none of the failures is the PR's \
         fault. Reviews and discussions are not checked "
      ^ "(<a href=\"#h-ready\">more</a>).</p><ul class=\"ready\">"
      ^ String.concat "" (Py.take shown items)
      ^ "</ul>"
    in
    let out =
      if k > shown then
        out
        ^ Printf.sprintf
            "<details class=\"more\"><summary>%d more</summary><ul \
             class=\"ready\">%s</ul></details>"
            (k - shown)
            (String.concat "" (Py.drop_list shown items))
      else out
    in
    out ^ "</section>"
  end

(** The requests that failed during the analysis shown (ocaml.org's MCP server,
    HTTP, git; a git host that failed earlier the same day counts too): the
    analysis is incomplete. 0 without a result. *)
let failed_lookups pr =
  if truthy (pr |. "pending") || pr |. "result" = `Null then 0
  else
    Py.sum
      (List.map
         (fun k -> match pr |. k with v when truthy v -> num v | _ -> 0)
         [ "mcp_errors"; "net_errors"; "git_unreachable" ])

let time16 v = e_s (Py.replace (Py.prefix v 16) "T" " ")

let render_pr ?year ?max_failures (pr : Json.t) =
  let v = verdict pr in
  let g = groups pr in
  let n = num (pr |. "number") in
  let real, _ = failed_count pr in
  let failed = failed_sorted pr in
  let title = sget pr "title" in
  let pkgs =
    String.concat " "
      (Py.dedup
         (List.concat_map
            (fun (_, rs) ->
              List.map (fun r -> Suggestion.str_field r.Suggestion.failing) rs)
            g))
  in
  let search =
    String.concat " "
      (Py.split
         (lower (Printf.sprintf "%d %s %s %s" n title (sget pr "author") pkgs)))
  in
  let line2 =
    ref
      [
        Printf.sprintf "<b class=\"v\" title=\"%s\">%s</b>"
          (e_s (verdict_help v))
          (e_s (verdict_label v));
        headline ~v pr;
      ]
  in
  if ci_running pr then begin
    let left =
      if not (has "ci_state_now" pr) then pr |. "ci_unfinished" else `Null
    in
    (* unknown: a rebuild runs *)
    line2 :=
      !line2
      @ [
          Printf.sprintf
            "<span class=\"run\" title=\"These are the failures so far; the \
             page is updated when CI finishes\">CI running%s</span>"
            (if truthy left then
               Printf.sprintf ", %s job%s left" (e left) (plural (num left))
             else "");
        ]
  end;
  let jobs_title =
    String.concat ", "
      (List.filter_map
         (fun (c, k) ->
           if List.mem c noise then None
           else Some (Printf.sprintf "%s %s" (str k) (job_category c)))
         failed)
  in
  let skipped =
    match List.assoc_opt "not-available" failed with
    | Some k -> Json.to_int k
    | None -> 0
  in
  if real = 0 && skipped <> 0 then
    line2 :=
      !line2
      @ [
          Printf.sprintf
            "<span title=\"Not installable on these platforms: the jobs did \
             not run\">%d skipped job%s</span>"
            skipped
            (if skipped <> 1 then "s" else "");
        ]
  else
    line2 :=
      !line2
      @ [
          Printf.sprintf
            "<span title=\"Failed build and test jobs (%s); network errors, \
             cancelled and skipped jobs are not counted\">%d failed \
             job%s</span>"
            (e_s (if jobs_title = "" then "none" else jobs_title))
            real
            (if real <> 1 then "s" else "");
        ];
  let head =
    Printf.sprintf
      "<summary>%s<span class=\"ttl\">%s%s%s</span><span class=\"who\">%s \
       \xc2\xb7 %s</span><span class=\"l2\">%s %s</span></summary>"
      (link ~cls:"num"
         (`String (Printf.sprintf "%s/pull/%d" repo_url n))
         (Printf.sprintf "#%d" n))
      (e_s title)
      (if truthy (pr |. "draft") then " <i>(draft)</i>" else "")
      (label_badges
         (str_list (pr |. "labels"))
         (str_list (pr |. "stale_labels")))
      (e (match pr |. "author" with v when truthy v -> v | _ -> `String "?"))
      (e_s (day_v ?year (pr |. "updated")))
      (String.concat " \xc2\xb7 " !line2)
      (link ~cls:"ci" (pr |. "ci_url") "CI \xe2\x86\x97")
  in
  let sha =
    match pr |. "sha" with
    | `String s -> Some s
    | `Null -> None
    | v -> Some (str v)
  in
  let bar = ref [ link (pr |. "ci_url") "CI results" ] in
  if (not (truthy (pr |. "pending"))) && pr |. "result" <> `Null then
    bar := !bar @ [ copy_block pr ];
  bar :=
    !bar
    @ [
        Printf.sprintf "<span>commit %s \xc2\xb7 analysed %s UTC</span>"
          (e_s (Py.prefix (Option.value ~default:"" sha) 12))
          (time16 (match sget pr "analysed_at" with "" -> "never" | s -> s));
      ];
  let lookups = failed_lookups pr in
  if lookups > 0 then
    bar :=
      !bar
      @ [
          Printf.sprintf
            "<span class=\"no\" title=\"ocaml.org, a git host or another \
             server did not answer during the analysis: some versions, tags, \
             docs or logs were not read, so a suggestion may be missing. The \
             analysis is tried again up to three times a day; a git host that \
             did not answer is asked again a day later.\">analysis incomplete \
             (%d failed lookup%s)</span>"
            lookups (plural lookups);
        ];
  if truthy (pr |. "last_error") && pr |. "result" <> `Null then
    (* A newer analysis of the same commit failed: this result is the previous one. *)
    bar :=
      !bar
      @ [
          Printf.sprintf
            "<span title=\"%s\">a newer analysis failed %s UTC; this result \
             may be out of date</span>"
            (e (pr |. "last_error"))
            (time16 (sget pr "last_error_at"));
        ];
  let body =
    ref
      [
        "<div class=\"body\">";
        Printf.sprintf "<div class=\"bar\">%s</div>" (String.concat "" !bar);
      ]
  in
  let add s = body := !body @ [ s ] in
  let since = pr |. "since_last_push" in
  if truthy since && (truthy (since |. "fixed") || truthy (since |. "new")) then begin
    let items xs =
      String.concat ", " (List.map e (Py.take 8 xs))
      ^
      if List.length xs > 8 then
        Printf.sprintf " and %d more" (List.length xs - 8)
      else ""
    in
    let parts =
      (if truthy (since |. "fixed") then
         let f = Json.to_list (since |. "fixed") in
         [
           Printf.sprintf "<span class=\"yes\">%d gone</span> (%s)"
             (List.length f) (items f);
         ]
       else [])
      @ (if truthy (since |. "new") then
           let f = Json.to_list (since |. "new") in
           [
             Printf.sprintf "<span class=\"no\">%d new</span> (%s)"
               (List.length f) (items f);
           ]
         else [])
      @ [
          Printf.sprintf "%s unchanged"
            (e (if has "still" since then since |. "still" else `Int 0));
        ]
    in
    add
      (Printf.sprintf "<p class=\"note since\">Since the previous push (%s): "
         (e_s (Py.prefix (sget since "previous_sha") 12))
      ^ String.concat "; " parts ^ ".</p>")
  end;
  let stale = pr |. "result" |. "health_stale" in
  if truthy stale then
    add
      (Printf.sprintf
         "<p class=\"note\">opam-health-check's latest results are from %s, \
          more than a week ago, so failures were not compared with master: \
          some may be marked as this PR's although they also fail without \
          it.</p>"
         (e_s (day_v stale)));
  if truthy (pr |. "pending") then
    add
      "<p class=\"note\">Not analysed yet; the next page update will cover \
       it.</p>"
  else if pr |. "result" = `Null then
    add
      (Printf.sprintf
         "<p class=\"note\">The analysis failed (%s attempts): \
          <code>%s</code></p>"
         (e (pr |. "attempts"))
         (e (pr |. "error")))
  else begin
    let left = pr |. "result" |. "unanalysed" in
    if truthy left then begin
      let per =
        match max_failures with
        | Some m when truthy m -> " at most " ^ e m
        | _ -> " a few"
      in
      add
        (Printf.sprintf
           "<p class=\"note\">%s more failure%s not analysed: each page update \
            analyses%s per PR. See the CI results.</p>"
           (e left)
           (if left <> `Int 1 then "s were" else " was")
           per)
    end;
    if not (truthy (pr |. "result" |. "suggestions")) then
      add
        (match unexplained_jobs pr with
        | Some jobs ->
            Printf.sprintf
              "<p class=\"note\">No explanation for the failed jobs (%s): see \
               the CI results.</p>"
              (e_s jobs)
        | None -> "<p class=\"note\">No build or test failure to analyse.</p>");
    let change =
      merge_similar (group g "fix") @ merge_similar (group g "workaround")
    in
    if change <> [] then begin
      add
        (Printf.sprintf "<h4 class=\"g-change\">Change this PR (%d)</h4>"
           (List.length change));
      List.iter (fun r -> add (render_suggestion ?sha ?year r)) change
    end;
    let elsewhere = merge_similar (group g "elsewhere") in
    if elsewhere <> [] then begin
      add
        (Printf.sprintf "<h4 class=\"g-else\">Fix goes elsewhere (%d)</h4>"
           (List.length elsewhere));
      List.iter (fun r -> add (render_item ?sha ?year r)) elsewhere
    end;
    let other = merge_similar (group g "explanation") in
    if other <> [] then begin
      let kinds = String.concat ", " (Py.dedup (List.map title_of other)) in
      let k = List.length other in
      add
        (Printf.sprintf
           "<details class=\"others\"%s><summary>%d other failure%s, no change \
            suggested <span class=\"x\">(%s)</span></summary>"
           (if v = "other" then " open" else "")
           k
           (if k <> 1 then "s" else "")
           (e_s (shorten kinds 90))
        ^ String.concat "" (List.map (render_item ?sha ?year) other)
        ^ "</details>")
    end
  end;
  add "</div>";
  let sha = Option.value ~default:"" sha in
  let ci =
    if Rx.fullmatch safe_sha sha <> None then
      Printf.sprintf " data-ci=\"%s/variant/\"" (e_s (Net.base ^ sha))
    else ""
  in
  Printf.sprintf
    "<details class=\"pr v-%s\" id=\"pr-%d\" data-v=\"%s\"%s \
     data-search=\"%s\"%s>"
    v n v
    (if is_merge pr then " data-merge" else "")
    (e_s search) ci
  ^ head ^ String.concat "\n" !body ^ "</details>"

(** A table whose rows after the first [shown] are folded in a <details>. *)
let table ?(shown = 25) head rows =
  let th =
    String.concat "" (List.map (fun h -> "<th>" ^ e_s h ^ "</th>") head)
  in
  let tab rs =
    Printf.sprintf "<div class=\"tw\"><table><thead><tr>%s</tr></thead><tbody>"
      th
    ^ String.concat "" rs ^ "</tbody></table></div>"
  in
  let out = tab (Py.take shown rows) in
  if List.length rows > shown then
    out
    ^ Printf.sprintf
        "<details class=\"more\"><summary>%d more</summary>%s</details>"
        (List.length rows - shown)
        (tab (Py.drop_list shown rows))
  else out

let chronic ?(min_prs = 2) prs =
  (* name -> PR -> version -> jobs, in first-seen order *)
  let revdeps : (string * (int * (string * Json.t) list ref) list ref) list ref
      =
    ref []
  in
  let blamed : (string * int list ref) list ref = ref [] in
  let assoc_ref l k mk =
    match List.assoc_opt k !l with
    | Some v -> v
    | None ->
        let v = mk () in
        l := !l @ [ (k, v) ];
        v
  in
  List.iter
    (fun pr ->
      let n = num (pr |. "number") in
      List.iter
        (fun (rd, jobs) ->
          let name, _, version = Py.partition rd "." in
          let by_pr = assoc_ref revdeps name (fun () -> ref []) in
          let vs = assoc_ref by_pr n (fun () -> ref []) in
          if List.mem_assoc version !vs then
            vs :=
              List.map
                (fun (k, x) -> if k = version then (k, jobs) else (k, x))
                !vs
          else vs := !vs @ [ (version, jobs) ])
        (Json.to_assoc (pr |. "jobs" |. "failing_revdeps"));
      List.iter
        (fun r ->
          match Where.where_pkg r with
          | Some p when p <> "" ->
              let s = assoc_ref blamed p (fun () -> ref []) in
              if not (List.mem n !s) then s := n :: !s
          | _ -> ())
        (suggestions_of (pr |. "result")))
    prs;
  let rows =
    List.sort
      (fun (a, da) (b, db) ->
        compare (-List.length !da, a) (-List.length !db, b))
      (List.filter (fun (_, d) -> List.length !d >= min_prs) !revdeps)
  in
  let brows =
    List.sort
      (fun (a, sa) (b, sb) ->
        compare (-List.length !sa, a) (-List.length !sb, b))
      (List.filter (fun (_, s) -> List.length !s >= min_prs) !blamed)
  in
  let out =
    ref [ "<section id=\"chronic\"><h2>Failures shared by several PRs</h2>" ]
  in
  let add s = out := !out @ [ s ] in
  add
    (Printf.sprintf
       "<h3>Reverse dependencies failing on %d or more PRs (%d)</h3>" min_prs
       (List.length rows));
  if rows <> [] then begin
    let trs =
      List.map
        (fun (name, d) ->
          let versions =
            List.sort_uniq compare
              (List.concat_map (fun (_, vs) -> List.map fst !vs) !d)
          in
          let category j =
            match j |. "category" with
            | `String c when c <> "" -> job_category c
            | v when truthy v -> str v
            | _ -> "job"
          in
          let cells =
            String.concat " "
              (List.map
                 (fun (n, vs) ->
                   let jobs = snd (List.hd !vs) in
                   let j0 = List.hd (Json.to_list jobs) in
                   Printf.sprintf "<span class=\"nw\">%s (%s)</span>"
                     (links
                        (Printf.sprintf "%s/pull/%d" repo_url n)
                        (Printf.sprintf "#%d" n))
                     (link (j0 |. "url") (category j0)))
                 (List.sort (fun (a, _) (b, _) -> compare a b) !d))
          in
          Printf.sprintf
            "<tr><td><b>%s</b><div \
             class=\"meta\">%s</div></td><td>%d</td><td>%s</td></tr>"
            (e_s name)
            (e_s (String.concat ", " versions))
            (List.length !d) cells)
        rows
    in
    add
      (table
         [
           "Reverse dependency (failing versions)";
           "PRs";
           "One failing job per PR";
         ]
         trs)
  end
  else add "<p class=\"note\">None among the analysed PRs.</p>";
  add
    (Printf.sprintf "<h3>Packages to fix that block %d or more PRs (%d)</h3>"
       min_prs (List.length brows));
  if brows <> [] then
    add
      (table
         [ "Package"; "PRs"; "Which PRs" ]
         (List.map
            (fun (p, s) ->
              Printf.sprintf "<tr><td><b>%s</b></td><td>%d</td><td>" (e_s p)
                (List.length !s)
              ^ String.concat ", "
                  (List.map
                     (fun n ->
                       links
                         (Printf.sprintf "%s/pull/%d" repo_url n)
                         (Printf.sprintf "#%d" n))
                     (List.sort compare !s))
              ^ "</td></tr>")
            brows))
  else add "<p class=\"note\">None among the analysed PRs.</p>";
  add
    ("<p class=\"meta\">The first table counts the failed build and test jobs \
      of reverse dependencies "
   ^ "(network errors, cancelled, skipped and experimental jobs are left out). \
      The second lists the "
   ^ "packages that suggestions for several PRs point to.</p></section>");
  String.concat "\n" !out

let help_section (data : Json.t) =
  let run = data |. "run" and llm = data |. "llm" in
  let limit =
    if truthy (data |. "limit") then
      Printf.sprintf " The list was limited to %s PRs." (e (data |. "limit"))
    else ""
  in
  let extra = function
    | "ready" ->
        " These PRs are listed under <b id=\"h-ready\">\"Ready as far as CI \
         goes\"</b>, "
        ^ "with the PRs whose CI passes. Drafts and PRs labelled \"do not \
           merge\" or \"needs "
        ^ "reporter action\" are left out, unless the author pushed or \
           commented after the "
        ^ "label was added. Only CI is checked: not the reviews, nor whether a \
           maintainer " ^ "asked for changes."
    | "change" ->
        " Other failures may also need a fix elsewhere (\"also blocked by\")."
    | "blocked" ->
        " The fix goes in a dependency or a reverse dependency of the PR's \
         packages."
    | "rerun" ->
        " Opam-repository maintainers can rerun them with \"Rebuild failed\" \
         on the CI "
        ^ "page. These PRs are not listed as ready: CI did not test those jobs."
    | "error" -> " A PR not analysed yet is covered by the next page update."
    | _ -> ""
  in
  let verdicts_html =
    String.concat "\n"
      (List.map
         (fun (k, label) ->
           Printf.sprintf
             "<dt class=\"v-%s\"><span class=\"v\">%s</span></dt><dd>%s." k
             (e_s label)
             (e_s (verdict_help k))
           ^ extra k ^ "</dd>")
         verdicts)
  in
  let get_or v k d = if has k v then v |. k else d in
  Printf.sprintf
    {|<section id="help"><h2>How to read this page</h2>
<p>Each failing PR gets one verdict, the first of these that applies. The row says what the problem is (the first
suggested change, the other packages to fix, or the first failure) and how many build and test jobs failed
(network errors, cancelled and skipped jobs are not counted). "CI running" means these are the failures so far.</p>
<dl class="help">
%s
</dl>
<dl class="help">
<dt>Change this PR, Fix goes elsewhere</dt><dd>In a PR's details, the suggested changes are shown in full;
the failures whose fix goes in another package, upstream or the CI are folded, one line each, with where the fix
goes. Similar failures of several packages are merged into one line. The confidence of a suggestion is shown
only when it is not high.</dd>
<dt>Labels</dt><dd>The PR's labels on GitHub, set by the opam-repository maintainers. "consider for merge"
has a filter. A label the author may have answered (they pushed or commented after it was added) says "author
replied since", since maintainers often leave it in place.</dd>
<dt>Copy as a PR comment</dt><dd>Copies a summary of the analysis, as Markdown, to paste in a comment on
the PR. Check it before posting: low-confidence LLM diagnoses are left out, other suggestions are not
checked.</dd>
<dt>Reverse dependency</dt><dd>A package that depends on a package of the PR. CI rebuilds them to check that
the PR does not break them.</dd>
<dt>day10 builder</dt><dd>opam-repo-ci builds packages with two builders: the main builder, and the newer
day10 builder (the jobs named <code>day10,…</code>). When a job fails only on day10 while the main
builder passes on the same system, the problem is in the CI setup, not in the package.</dd>
<dt>Pre-existing</dt><dd>The package fails without this PR too: it also failed in the CI of the previous
release (the same job, with the same reason), the PR only adds an upper bound to that package, or
opam-health-check (which builds every package on master, without tests) sees it failing too. "Also fails in
other open PRs" says the same from the other PRs on this page: PRs of other packages, where the reverse
dependency fails the same way.</dd>
<dt>LLM diagnosis (unchecked)</dt><dd>For failures the tool cannot explain, an LLM (Claude) reads the log and
suggests a cause. These diagnoses are not checked: in a test on 25 of them, 15 were right, 7 plausible and 3
wrong. Changes to version bounds or depexts are shown as text to check, and low-confidence diagnoses are
folded, as "LLM diagnosis (low confidence)".</dd>
<dt>Experimental platform</dt><dd>FreeBSD, macOS and some other jobs are experimental: their failures do not
block a PR.</dd>
</dl>
<p class="meta">This update: %s PRs analysed, %s unchanged since
the last one (a PR is analysed again when its head commit or its CI status changes). Up to
%s different failures are analysed per PR.%s
%s%s</p>
<p class="meta">To post a suggestion as a line comment on a PR, review it first, then run
<code>opam-ci-triage review --pr NUMBER --post</code>
(from <a href="https://github.com/Sudha247/hackathon-opam-repo-triage" rel="noopener noreferrer">this
repository</a>).</p></section>|}
    verdicts_html
    (e (get_or run "analysed" (`Int 0)))
    (e (get_or run "unchanged" (`Int 0)))
    (e (data |. "max_failures_per_pr"))
    limit
    (if truthy (llm |. "enabled") then
       Printf.sprintf
         "LLM diagnoses are enabled: %s new LLM calls in this update."
         (e (get_or llm "calls" (`Int 0)))
     else "LLM diagnoses are not enabled.")
    (if truthy (llm |. "off") then " They were turned off during this update."
     else "")

(* Why the LLM was turned off during an update (see triage_open_prs), shown under the lead. *)
let llm_off =
  [
    ( "credentials",
      "Anthropic refused the API key (expired or invalid), so failures the \
       rules cannot " ^ "explain have no LLM diagnosis in this update." );
    ( "outage",
      "Anthropic's API failed several times in a row, so some failures the \
       rules cannot explain "
      ^ "have no LLM diagnosis in this update; the next one tries again." );
  ]

let llm_off_note (data : Json.t) =
  match data |. "llm" |. "off" with
  | `String off when List.mem_assoc off llm_off ->
      Printf.sprintf "<p class=\"note llm-off\">%s</p>"
        (e_s (List.assoc off llm_off))
  | _ -> ""

let render (data : Json.t) =
  let prs = Json.to_list (data |. "prs") in
  let verdict_of = Hashtbl.create 64 in
  List.iter
    (fun p -> Hashtbl.replace verdict_of (num (p |. "number")) (verdict p))
    prs;
  let count k =
    Hashtbl.fold (fun _ v acc -> if v = k then acc + 1 else acc) verdict_of 0
  in
  let gen = sget data "generated" in
  let year = Py.prefix gen 4 in
  let merge = List.length (List.filter is_merge prs) in
  let failing = data |. "failing_prs" and running = data |. "running_prs" in
  let scope =
    if failing = `Null then ""
    else
      Printf.sprintf "%s of the %s open %s PRs have failing opam-repo-ci jobs"
        (ei (List.length prs))
        (e (data |. "open_prs"))
        (links repo_url "ocaml/opam-repository")
      ^ (if truthy running then
           Printf.sprintf " (for %s of them, CI is still running)" (e running)
         else "")
      ^ (if truthy (data |. "limit") then
           Printf.sprintf " (limited to %s)" (e (data |. "limit"))
         else "")
      ^ ". The failures are read from the CI logs and matched against known \
         causes. Nothing is rebuilt, "
      ^ "so check a suggestion before applying it."
  in
  let seg =
    ("<button type=\"button\" class=\"v-all\" data-f=\"all\" \
      aria-pressed=\"true\">All<b>"
    ^ string_of_int (List.length prs)
    ^ "</b></button>")
    :: List.filter_map
         (fun (k, label) ->
           if count k = 0 then None
           else
             Some
               (Printf.sprintf
                  "<button type=\"button\" class=\"v-%s\" data-f=\"%s\" \
                   aria-pressed=\"false\" \
                   title=\"%s\"><i></i>%s<b>%d</b></button>"
                  k k
                  (e_s (verdict_help k))
                  (e_s label) (count k)))
         verdicts
  in
  let parts =
    [
      "<!DOCTYPE html>";
      "<html lang=\"en\"><head><meta charset=\"utf-8\">";
      "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">";
      "<meta name=\"color-scheme\" content=\"light dark\">";
      "<title>opam-repository CI triage</title>";
      "<link rel=\"alternate\" type=\"application/atom+xml\" \
       title=\"opam-repository CI triage\" href=\"feed.xml\">";
      "<style>" ^ css ^ "</style></head><body><main>";
      "<header><h1>opam-repository CI triage</h1>"
      ^ Printf.sprintf
          "<nav>Updated %s %s UTC \xc2\xb7 <a href=\"feed.xml\">Atom feed</a> \
           \xc2\xb7 "
          (e_s (day ~year (Some gen)))
          (e_s (Py.sub_chars gen 11 16))
      ^ "<a href=\"#help\">How to read this page</a></nav></header>";
      (if scope <> "" then Printf.sprintf "<p class=\"lead\">%s</p>" scope
       else "");
      llm_off_note data;
      ready_section data;
      Printf.sprintf
        "<section id=\"failing\"><h2>Failing PRs <span \
         class=\"sub\">(%d)</span></h2>"
        (List.length prs);
      "<nav class=\"filters\" aria-label=\"Filters\"><div class=\"seg\" \
       role=\"group\" aria-label=\"Verdict\">" ^ String.concat "" seg ^ "</div>"
      ^ (if merge > 0 then
           Printf.sprintf
             "<label><input type=\"checkbox\" id=\"merge\"> consider for merge \
              (%d)</label>"
             merge
         else "")
      ^ "<input id=\"q\" type=\"search\" placeholder=\"Search number, title, \
         author, package\" aria-label=\"Search\">"
      ^ "<span id=\"shown\" class=\"meta\" aria-live=\"polite\"></span></nav>";
      "<div id=\"prs\">";
    ]
    @ List.map
        (render_pr ~year ~max_failures:(data |. "max_failures_per_pr"))
        prs
    @ (if prs = [] then
         [ "<p class=\"note\">No open PR has failing opam-repo-ci jobs.</p>" ]
       else [])
    @ [
        "</div></section>";
        chronic (List.filter (fun p -> not (truthy (p |. "pending"))) prs);
        help_section data;
        "<script>" ^ js ^ "</script></main></body></html>";
      ]
  in
  String.concat "\n" parts ^ "\n"

let render_feed (data : Json.t) =
  let x (v : Json.t) = e v in
  let gen = sget data "generated" in
  let out =
    ref
      [
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
        "<feed xmlns=\"http://www.w3.org/2005/Atom\">";
        "<title>opam-repository CI triage</title>";
        Printf.sprintf "<id>urn:opam-repo-ci-triage:%s</id>"
          (x (data |. "repository"));
        Printf.sprintf "<updated>%s</updated>" (e_s gen);
        "<link rel=\"alternate\" href=\"index.html\"/>";
        "<author><name>opam-repo-ci triage</name></author>";
      ]
  in
  List.iter
    (fun pr ->
      if not (truthy (pr |. "pending") || pr |. "result" = `Null) then begin
        let n = num (pr |. "number") in
        let v = verdict pr in
        let g = groups pr in
        let lines =
          ref
            [
              Printf.sprintf "%s: %s" (verdict_label v)
                (headline ~v ~as_html:false pr);
            ]
        in
        if ci_running pr then begin
          let left =
            if not (has "ci_state_now" pr) then pr |. "ci_unfinished" else `Null
          in
          lines :=
            !lines
            @ [
                Printf.sprintf "CI still running (%s jobs left)."
                  (if truthy left then str left else "?");
              ]
        end;
        let since = pr |. "since_last_push" in
        if truthy since && (truthy (since |. "fixed") || truthy (since |. "new"))
        then
          lines :=
            !lines
            @ [
                Printf.sprintf
                  "Since the previous push: %d gone, %d new, %s unchanged."
                  (List.length (Json.to_list (since |. "fixed")))
                  (List.length (Json.to_list (since |. "new")))
                  (str (if has "still" since then since |. "still" else `Int 0));
              ];
        List.iter
          (fun k ->
            List.iter
              (fun r ->
                lines :=
                  !lines
                  @ [
                      (Printf.sprintf "%s: %s" (title_of r)
                         (match Suggestion.get r.Suggestion.failing with
                         | Some f when f <> "" -> f
                         | _ -> (
                             match r.job with
                             | Some j when j <> "" -> j
                             | _ -> ""))
                      ^
                      if Suggestion.nonempty_field r.Suggestion.new_ then
                        Printf.sprintf " (%s)"
                          (List.hd
                             (Py.split_on "   "
                                (Suggestion.str_field r.Suggestion.new_)))
                      else "");
                    ])
              (Py.take 5 (merge_similar (group g k))))
          [ "fix"; "workaround"; "elsewhere"; "explanation" ];
        let updated =
          match pr |. "analysed_at" with v when truthy v -> str v | _ -> gen
        in
        out :=
          !out
          @ [
              "<entry>";
              Printf.sprintf "<title>#%d %s</title>" n (x (pr |. "title"));
              Printf.sprintf "<id>%s/pull/%d#%s</id>" (e_s repo_url) n
                (x (pr |. "sha"));
              Printf.sprintf "<link rel=\"alternate\" href=\"%s/pull/%d\"/>"
                (e_s repo_url) n;
              Printf.sprintf "<link rel=\"related\" href=\"index.html#pr-%d\"/>"
                n;
              Printf.sprintf "<updated>%s</updated>" (e_s updated);
              Printf.sprintf "<summary>%s</summary>"
                (e_s (String.concat "\n" !lines));
              "</entry>";
            ]
      end)
    (Json.to_list (data |. "prs"));
  out := !out @ [ "</feed>" ];
  String.concat "\n" !out ^ "\n"

let run ~data_path ~out_path () =
  let data = Json.parse (Py.read_file data_path) in
  let page = render data in
  let dir = Filename.dirname (Py.abspath out_path) in
  Py.makedirs dir;
  let write path s =
    let oc = open_out_bin path in
    output_string oc s;
    close_out oc
  in
  write out_path page;
  write (Filename.concat dir "feed.xml") (render_feed data);
  Printf.printf "wrote %s (%d bytes, %d PRs)\n" out_path (String.length page)
    (List.length (Json.to_list (data |. "prs")))

let main () =
  let open Cmdliner in
  let data_path =
    Arg.(
      value
      & opt string "site/data.json"
      & info [ "data" ] ~docv:"FILE" ~doc:"the page data")
  in
  let out_path =
    Arg.(
      value
      & opt string "site/index.html"
      & info [ "out" ] ~docv:"FILE"
          ~doc:"the page; feed.xml is written beside it")
  in
  let term =
    Term.(
      const (fun data_path out_path -> run ~data_path ~out_path ())
      $ data_path $ out_path)
  in
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "render-page"
             ~doc:
               "Render the data of triage-open-prs into a single static page \
                (and its feed).")
          term))
