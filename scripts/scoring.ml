(* What evaluate-closed-prs compares: the dependencies and fields that a
   suggestion changes, and those the author changed. *)

open Triage
module S = Set.Make (String)

let version = 4
let quoted_name_re = Rx.v {|"([\w+-]+)"|}
let field_name_re = Rx.v {|([a-z][\w-]*):|}
let sha256_sha512_md5_re = Rx.v "(sha256|sha512|md5)="

let line_tokens line =
  let t = String.trim line in
  match Rx.match_ quoted_name_re t with
  | Some m -> S.singleton ("dep:" ^ Rx.group m 1)
  | None -> (
      match Rx.match_ field_name_re t with
      | Some m -> S.singleton ("field:" ^ Rx.group m 1)
      | None ->
          let toks =
            if Util.contains t "@runtest" || Util.contains t "with-test" then
              S.singleton "field:runtest"
            else S.empty
          in
          if Rx.test sha256_sha512_md5_re t then S.add "field:checksum" toks
          else toks)

let package_field_re = Rx.v {|"?([\w+-]+)"?[: ]|}

let is_advice_only (s : Suggestion.t) =
  s.advice <> None
  && (not (Suggestion.is_set s.new_))
  && Suggestion.list s.candidates = []
  && s.alternative = None

(* The phrases of the rules' advice that name the field to change. *)
let advice_fields =
  [
    (* conf-no-depext: "Add depexts for cygwin to conf-x", "Add depexts to
       conf-x"; platform-matrix: "Add the missing depext for alpine to x, or
       exclude alpine there (...)" (its other way is not counted) *)
    ((fun a -> String.starts_with ~prefix:"Add depexts" a), "field:depexts");
    ( (fun a -> String.starts_with ~prefix:"Add the missing depext" a),
      "field:depexts" );
    (* test-needs-other-package: "Run only x's tests: give the test command
       its directories, ..., or remove the test command." *)
    ((fun a -> Util.contains a "the test command"), "field:runtest");
  ]

(* The fields an advice may name in backquotes: "exclude Windows there
   (`x-ci-accept-failures` or `available`)" (platform-matrix). *)
let opam_fields =
  [
    "available";
    "x-ci-accept-failures";
    "depexts";
    "depends";
    "depopts";
    "conflicts";
    "build";
    "install";
    "run-test";
    "patches";
    "extra-source";
  ]

let backquoted_re = Rx.v {|`([a-z][\w-]*)`|}

let advice_tokens (s : Suggestion.t) =
  match s.advice with
  | Some a when is_advice_only s ->
      let named =
        List.filter_map
          (fun m ->
            let f = Rx.group m 1 in
            if List.mem f opam_fields then Some ("field:" ^ f) else None)
          (Rx.finditer backquoted_re a)
      in
      List.fold_left
        (fun acc (test, tok) -> if test a then S.add tok acc else acc)
        (S.of_list named) advice_fields
  | _ -> S.empty

let suggestion_tokens (s : Suggestion.t) =
  let toks = ref (advice_tokens s) in
  let add t = toks := S.add t !toks in
  (match s.dep with Some d when d <> "" -> add ("dep:" ^ d) | _ -> ());
  List.iter
    (fun (c : Suggestion.Candidate.t) -> add ("dep:" ^ Suggestion.str c.dep))
    (Suggestion.list s.candidates);
  let alternative = Suggestion.str s.alternative in
  let alt = Rx.match_ package_field_re alternative in
  if Util.contains alternative "@runtest" then
    add "field:runtest" (* disable the tests: server-reason-react, #30879 *);
  (match alt with
  | Some m ->
      add
        ((if
            String.ends_with ~suffix:":"
              (List.hd (Util.split_on ~maxsplit:1 " " alternative))
          then "field:"
          else "dep:")
        ^ Rx.group m 1)
  | None -> ());
  let new_ = Suggestion.str_field s.new_ in
  (match Rx.match_ quoted_name_re new_ with
  | Some m -> add ("dep:" ^ Rx.group m 1)
  | None -> ());
  (match Rx.match_ field_name_re new_ with
  | Some m -> add ("field:" ^ Rx.group m 1)
  | None -> ());
  if Util.contains new_ "@runtest" then add "field:runtest";
  if String.starts_with ~prefix:"checksum" new_ then add "field:checksum";
  if String.starts_with ~prefix:"extra-source" new_ then (
    add "field:extra-source";
    add "field:extra-files");
  if Util.contains new_ "%{_:" then (
    add "field:build";
    add "field:install";
    add "field:remove");
  !toks

let has_change (s : Suggestion.t) =
  Suggestion.is_set s.new_
  || Suggestion.list s.candidates <> []
  || s.alternative <> None
  || not (S.is_empty (advice_tokens s))
