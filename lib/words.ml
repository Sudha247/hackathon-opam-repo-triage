(* Small helpers for the text people read: counts with plurals, dates. *)

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

let counted ?many n word =
  if n = 1 then Printf.sprintf "%d %s" n word
  else
    let many =
      match many with
      | Some m -> m
      | None ->
          if Rx.test "[^aeiou]y$" word then
            String.sub word 0 (String.length word - 1) ^ "ies"
          else word ^ "s"
    in
    Printf.sprintf "%d %s" n many

let current_year () = (Unix.localtime (Unix.time ())).Unix.tm_year + 1900

let short_date ?year iso =
  let iso = Option.value ~default:"" iso in
  match Rx.match_ {|(\d{4})-(\d\d)-(\d\d)|} iso with
  | Some m
    when let mo = int_of_string (Rx.group m 2) in
         1 <= mo && mo <= 12 ->
      let year =
        string_of_int
          (match year with Some y when y <> 0 -> y | _ -> current_year ())
      in
      let out =
        Printf.sprintf "%d %s"
          (int_of_string (Rx.group m 3))
          months.(int_of_string (Rx.group m 2) - 1)
      in
      if Rx.group m 1 = year then out else out ^ " " ^ Rx.group m 1
  | _ -> iso
