type info = {
  installed : (string * string) list;
  action : string option;
  failing : (string * string) option;
  ocaml : string option;
  error_lines : string list;
  test_diff : bool;
  base : string option;
}

let installed_re = Rx.v {|-> installed ([\w+-]+)\.(\S+)|}
let quoted_package_re = Rx.v {|"([\w+-]+)\.([^"\s]+)"|}

let error_while_compiling_re =
  Rx.v {|#=== ERROR while (compiling|installing|removing) ([\w+-]+)\.(\S+?) =|}

let ocaml_base_compiler_re = Rx.v {|ocaml(?:-base)?-compiler\.(\d+\.\d+\.\d+)|}
let comment_mark_re = Rx.v "^# ?"

let error_compilation_installation_re =
  Rx.v ~flags:[ Rx.M ]
    {|^\[ERROR\] The (compilation|installation) of ([\w+-]+)\.(\S+?) failed at|}

let note_error_processing_re = Rx.v {|\[(NOTE|ERROR)\]|Processing:|}

let log_info text =
  let installed = Odict.create () in
  let lines = Util.splitlines text in
  List.iter
    (fun line ->
      match Rx.search installed_re line with
      | Some m -> Odict.set installed (Rx.group m 1) (Rx.group m 2)
      | None -> ())
    lines;
  List.iter
    (fun m -> ignore (Odict.setdefault installed (Rx.group m 1) (Rx.group m 2)))
      (* day10 dependency graph *)
    (Rx.finditer quoted_package_re
       (String.concat "\n"
          (List.filter
             (fun l -> Util.contains l "->" && String.contains l '"')
             lines)));
  let action, failing =
    match Rx.search error_while_compiling_re text with
    | Some m -> (Some (Rx.group m 1), Some (Rx.group m 2, Rx.group m 3))
    | None -> (None, None)
  in
  let ocaml =
    match Rx.search ocaml_base_compiler_re text with
    | Some m -> Some (Rx.group m 1)
    | None -> None
  in
  let blk =
    Option.value ~default:""
      (List.assoc_opt "error_block" (Ci.excerpt ~max_output_lines:200 text))
  in
  let error_lines =
    List.map
      (fun l -> Rx.sub ~count:1 comment_mark_re "" l)
      (Util.splitlines blk)
  in
  let action, failing, error_lines, blk =
    if blk <> "" then (action, failing, error_lines, blk)
    else
      (* Older day10 logs have no "#=== ERROR while" block: the build output
         (lines starting with "- ") comes before opam's "[ERROR] The
         compilation of pkg failed at ..." line. *)
      match Rx.search error_compilation_installation_re text with
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
          let before = Util.splitlines (String.sub text 0 (Rx.start m)) in
          let out =
            List.map
              (fun l ->
                if String.starts_with ~prefix:"- " l then Util.drop_chars l 2
                else l)
              (Util.sublist ~start:(-80) before)
          in
          (* the last note, error or "Processing:" line *)
          let cut =
            List.fold_left max (-1)
              (List.mapi
                 (fun i l ->
                   if Rx.test_match note_error_processing_re l then i else -1)
                 out)
          in
          let error_lines = Util.drop (cut + 1) out in
          (action, failing, error_lines, String.concat "\n" error_lines)
      | None -> (action, failing, error_lines, blk)
  in
  {
    installed = Odict.to_list installed;
    action;
    failing;
    ocaml;
    error_lines;
    test_diff =
      Util.contains blk ".corrected" || Util.contains blk "git --no-pager diff";
    base = List.assoc_opt "base" (Ci.excerpt text);
  }

let quoted_name_re = Rx.v {|"([\w+-]+)"|}
let filter_re = Rx.v {|\{[^}]*\}|}

let dep_names opam_text field =
  match
    Rx.search
      (Rx.of_re (Re.seq [ Re.bol; Re.str field; Rx.re {|:\s*\[([^\]]*)\]|} ]))
      (Option.value ~default:"" opam_text)
  with
  | Some m -> Rx.findall quoted_name_re (Rx.sub filter_re "" (Rx.group m 1))
  | None -> []

let depends_block ?(close = "\n]") opam_text =
  List.hd
    (Util.split_on ~maxsplit:1 close
       (Util.last (Util.split_on ~maxsplit:1 "depends:" opam_text)))

let dependency_line_re = Rx.v ~flags:[ Rx.M ] {|^\s*"([\w+-]+)"|}

let direct_deps opam_text =
  Rx.findall dependency_line_re
    (depends_block (Option.value ~default:"" opam_text))
