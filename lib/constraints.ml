(* Version constraints of opam formulas. *)

(* A constraint on the package's own version (>= "1.0"), not a comparison of
   a variable (os = "linux", ocaml:version < "5.0"): the operator comes first
   in the formula or after one of the characters {&|( . That character is
   consumed (ocaml-re has no lookbehind), which makes no difference, as a
   match never ends with one. *)
let version_term = Rx.v {|(?:^|[{&|(])\s*(<=|>=|!=|<|>|=)\s*"([^"]+)"|}

let version_terms s =
  List.map
    (function [ op; v ] -> (op, v) | _ -> assert false)
    (Rx.findall_groups version_term s)

let unsatisfiable terms =
  List.exists
    (fun (lo_op, lo) ->
      List.mem lo_op [ ">="; ">"; "=" ]
      && List.exists
           (fun (up_op, up) ->
             List.mem up_op [ "<="; "<"; "=" ]
             && (lo_op, lo) <> (up_op, up)
             &&
             let c = Ci.opam_vcmp lo up in
             c > 0 || (c = 0 && (lo_op = ">" || up_op = "<")))
           terms)
    terms

let allows version terms =
  List.for_all
    (fun (op, v) ->
      let c = Ci.opam_vcmp version v in
      match op with
      | "<" -> c < 0
      | "<=" -> c <= 0
      | ">" -> c > 0
      | ">=" -> c >= 0
      | "=" -> c = 0
      | "!=" -> c <> 0
      | _ -> false)
    terms
