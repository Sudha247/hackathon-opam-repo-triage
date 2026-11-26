(* Where the fix of a suggestion goes, as data.

   Each suggestion has [fix_in], the place in words (shown, and stored by the
   page), and [where_kind], one of [where_kinds], from which its class, the
   PR's verdict and the page's grouping follow. [where_pkg] names the other
   package when [where_kind] is "other-package" and the package is known.

   Rules set the three together with [fix_where] or [pr_or_other].
   Suggestions stored before [where_kind] existed, and LLM answers (whose
   place comes as [fix_in]), get it from [where_kind] / [where_pkg] below:
   the only place that reads the words of [fix_in]. *)

open Suggestion.T

let where_kinds =
  [ "pr"; "other-package"; "upstream"; "ci"; "rerun"; "none"; "unknown" ]

let fix_where ?pkg kind fix_in : Suggestion.set list =
  assert (List.mem kind where_kinds);
  [
    `Fix_in fix_in;
    `Where_kind kind;
    `Where_pkg (if kind = "other-package" then pkg else None);
  ]

let pr_or_other pkg in_pr =
  if in_pr then fix_where "pr" "this PR"
  else fix_where ~pkg "other-package" (pkg ^ " (other package)")

(* The [fix_in] values written by the rules before [where_kind] existed. *)
let legacy_kinds =
  [
    ("this PR", "pr");
    ("unknown", "unknown");
    ("CI (rerun)", "rerun");
    ("the CI environment (day10 builder)", "ci");
    ("the CI environment", "ci");
    ("opam (the CI's opam version)", "ci");
    ("upstream (the package's source)", "upstream");
    ("nothing (this PR's bound)", "none");
    ("the dependency declaring these depexts", "other-package");
    ("the reverse dependencies (other packages)", "other-package");
    ("the PR, a shared dependency, or the CI", "unknown");
  ]

let other_suffix = " (other package)"
let upstream_suffix = " (upstream archive)"

(* (where_kind, where_pkg) of a suggestion without [where_kind], from its
   [fix_in] (and [failing_dep], the shared dependency of a pattern over
   several packages). *)
let legacy (r : Suggestion.t) =
  let f = Suggestion.str r.fix_in in
  if
    f = "the PR, a shared dependency, or the CI"
    && Suggestion.nonempty r.failing_dep
  then ("other-package", r.failing_dep)
  else
    match List.assoc_opt f legacy_kinds with
    | Some k -> (k, None)
    | None ->
        if String.ends_with ~suffix:other_suffix f then
          ( "other-package",
            Some (String.sub f 0 (String.length f - String.length other_suffix))
          )
        else if String.ends_with ~suffix:upstream_suffix f then
          ("upstream", None)
        else ("unknown", None)

let valid_kind r =
  match r.where_kind with
  | Some k when List.mem k where_kinds -> Some k
  | _ -> None

let where_kind r =
  match valid_kind r with Some k -> k | None -> fst (legacy r)

(* Rules that tie a failure to the package the PR changes: its API changed
   (library-api), a bound on it is needed (previous-release-bound,
   lower-bound), or opam-health-check sees the failing package build on
   master (health-check-new). *)
let pr_evidence_rules =
  [ "library-api"; "previous-release-bound"; "lower-bound"; "health-check-new" ]

let tied_to_pr r =
  where_kind r = "pr"
  || List.mem (Suggestion.str r.rule) pr_evidence_rules
  || Suggestion.nonempty_field r.Suggestion.new_

let where_pkg r =
  match valid_kind r with
  | Some "other-package" -> Suggestion.get r.where_pkg
  | Some _ -> None
  | None -> snd (legacy r)
