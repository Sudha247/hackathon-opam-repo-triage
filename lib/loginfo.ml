type info = {
  installed : (string * string) list;
  action : string option;
  failing : (string * string) option;
  ocaml : string option;
  error_lines : string list;
  test_diff : bool;
  base : string option;
}

let log_info text =
  let installed = Py.Odict.create () in
  let lines = Py.splitlines text in
  List.iter
    (fun line ->
      match Rx.search {|-> installed ([\w+-]+)\.(\S+)|} line with
      | Some m -> Py.Odict.set installed (Rx.group m 1) (Rx.group m 2)
      | None -> ())
    lines;
  List.iter
    (fun m ->
      ignore (Py.Odict.setdefault installed (Rx.group m 1) (Rx.group m 2)))
      (* day10 dependency graph *)
    (Rx.finditer {|"([\w+-]+)\.([^"\s]+)"|}
       (String.concat "\n"
          (List.filter
             (fun l -> Py.contains l "->" && String.contains l '"')
             lines)));
  let action, failing =
    match
      Rx.search
        {|#=== ERROR while (compiling|installing|removing) ([\w+-]+)\.(\S+?) =|}
        text
    with
    | Some m -> (Some (Rx.group m 1), Some (Rx.group m 2, Rx.group m 3))
    | None -> (None, None)
  in
  let ocaml =
    match Rx.search {|ocaml(?:-base)?-compiler\.(\d+\.\d+\.\d+)|} text with
    | Some m -> Some (Rx.group m 1)
    | None -> None
  in
  let blk =
    Option.value ~default:""
      (List.assoc_opt "error_block" (Ci.excerpt ~max_output_lines:200 text))
  in
  let error_lines =
    List.map (fun l -> Rx.sub ~count:1 "^# ?" "" l) (Py.splitlines blk)
  in
  let action, failing, error_lines, blk =
    if blk <> "" then (action, failing, error_lines, blk)
    else
      (* Older day10 logs have no "#=== ERROR while" block: the build output
         (lines starting with "- ") comes before opam's "[ERROR] The
         compilation of pkg failed at ..." line. *)
      match
        Rx.search
          {|(?m)^\[ERROR\] The (compilation|installation) of ([\w+-]+)\.(\S+?) failed at|}
          text
      with
      | Some m ->
          let action =
            Some
              (if Rx.group m 1 = "compilation" then "compiling"
               else "installing")
          in
          let failing =
            match failing with
            | Some f -> Some f
            | None -> Some (Rx.group m 2, Rx.group m 3)
          in
          let before = Py.splitlines (String.sub text 0 (Rx.start m)) in
          let out =
            List.map
              (fun l ->
                if String.starts_with ~prefix:"- " l then Py.drop l 2 else l)
              (Py.lslice ~start:(-80) before)
          in
          let cut =
            List.fold_left
              (fun acc (i, l) ->
                if Rx.test_match {|\[(NOTE|ERROR)\]|Processing:|} l then i
                else acc)
              (-1) (Py.enumerate out)
          in
          let error_lines = Py.drop_list (cut + 1) out in
          (action, failing, error_lines, String.concat "\n" error_lines)
      | None -> (action, failing, error_lines, blk)
  in
  {
    installed = Py.Odict.to_list installed;
    action;
    failing;
    ocaml;
    error_lines;
    test_diff =
      Py.contains blk ".corrected" || Py.contains blk "git --no-pager diff";
    base = List.assoc_opt "base" (Ci.excerpt text);
  }

let dep_names opam_text field =
  match
    Rx.search
      (Printf.sprintf {|(?m)^%s:\s*\[([^\]]*)\]|} field)
      (Option.value ~default:"" opam_text)
  with
  | Some m ->
      Rx.findall {|"([\w+-]+)"|} (Rx.sub {|\{[^}]*\}|} "" (Rx.group m 1))
  | None -> []

let depends_block ?(close = "\n]") opam_text =
  List.hd
    (Py.split_on ~maxsplit:1 close
       (Py.last (Py.split_on ~maxsplit:1 "depends:" opam_text)))

let direct_deps opam_text =
  Rx.findall ~flags:[ Rx.M ] {|^\s*"([\w+-]+)"|}
    (depends_block (Option.value ~default:"" opam_text))
