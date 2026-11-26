(* Where the fix of a suggestion goes. See where_kind.mli. *)

type t = Pr | Other_package | Upstream | Ci | Rerun | Nothing | Unknown

let all = [ Pr; Other_package; Upstream; Ci; Rerun; Nothing; Unknown ]

let to_string = function
  | Pr -> "pr"
  | Other_package -> "other-package"
  | Upstream -> "upstream"
  | Ci -> "ci"
  | Rerun -> "rerun"
  | Nothing -> "none"
  | Unknown -> "unknown"

let of_string s = List.find_opt (fun k -> to_string k = s) all
