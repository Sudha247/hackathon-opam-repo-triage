(* Where the fix of a suggestion goes, as data.

   Each suggestion has [fix_in], the place in words (shown, and stored by the
   page), and [where_kind] (a {!Where_kind.t}), from which its class, the
   PR's verdict and the page's grouping follow. [where_pkg] names the other
   package when [where_kind] is "other-package" and the package is known.

   Rules set the three together with [fix_where] or [pr_or_other].
   Suggestions stored before [where_kind] existed, and LLM answers (whose
   place comes as [fix_in]), get it from [where_kind] / [where_pkg] below:
   the only place that reads the words of [fix_in]. *)

open Suggestion.T

let fix_where ?pkg (kind : Where_kind.t) fix_in : Suggestion.set list =
  [
    `Fix_in fix_in;
    `Where_kind kind;
    `Where_pkg (if kind = Other_package then pkg else None);
  ]

let pr_or_other pkg in_pr =
  if in_pr then fix_where Pr "this PR"
  else fix_where ~pkg Other_package (pkg ^ " (other package)")

(* The [fix_in] values written by the rules before [where_kind] existed. *)
let legacy_kinds : (string * Where_kind.t) list =
  [
    ("this PR", Pr);
    ("unknown", Unknown);
    ("CI (rerun)", Rerun);
    ("the CI environment (day10 builder)", Ci);
    ("the CI environment", Ci);
    ("opam (the CI's opam version)", Ci);
    ("upstream (the package's source)", Upstream);
    ("nothing (this PR's bound)", Nothing);
    ("nothing (an experimental platform does not block the merge)", Nothing);
    ("the dependency declaring these depexts", Other_package);
    ("the reverse dependencies (other packages)", Other_package);
    ("the PR, a shared dependency, or the CI", Unknown);
  ]

let other_suffix = " (other package)"
let upstream_suffix = " (upstream archive)"

(* (where_kind, where_pkg) of a suggestion without [where_kind], from its
   [fix_in] (and [failing_dep], the shared dependency of a pattern over
   several packages). *)
let legacy (r : Suggestion.t) : Where_kind.t * string option =
  let f = Suggestion.str r.fix_in in
  if f = "the PR, a shared dependency, or the CI" && r.failing_dep <> None then
    (Other_package, r.failing_dep)
  else
    match List.assoc_opt f legacy_kinds with
    | Some k -> (k, None)
    | None ->
        if String.ends_with ~suffix:other_suffix f then
          ( Other_package,
            Some (String.sub f 0 (String.length f - String.length other_suffix))
          )
        else if String.ends_with ~suffix:upstream_suffix f then (Upstream, None)
        else (Unknown, None)

let where_kind r =
  match r.where_kind with Some k -> k | None -> fst (legacy r)

(* Rules that tie a failure to the package the PR changes: its API changed
   (library-api), a bound on it is needed (previous-release-bound,
   lower-bound), or opam-health-check sees the failing package build on
   master (health-check-new). *)
let pr_evidence_rules : Rule.t list =
  [ Library_api; Previous_release_bound; Lower_bound; Health_check_new ]

let tied_to_pr r =
  where_kind r = Pr
  || (match r.rule with
    | Some rule -> List.mem rule pr_evidence_rules
    | None -> false)
  || Suggestion.is_set r.Suggestion.new_

let where_pkg r =
  match r.where_kind with
  | Some Other_package -> Suggestion.get r.where_pkg
  | Some _ -> None
  | None -> snd (legacy r)
