(* Dates: "YYYY-MM-DD" days and ISO 8601 date-times. *)

let today () =
  let tz_offset_s =
    Option.value ~default:0 (Ptime_clock.current_tz_offset_s ())
  in
  let y, m, d = Ptime.to_date ~tz_offset_s (Ptime_clock.now ()) in
  Printf.sprintf "%04d-%02d-%02d" y m d

let days_of_date s =
  try
    Scanf.sscanf s "%4d-%2d-%2d%!" (fun y m d ->
        match Ptime.of_date (y, m, d) with
        | Some t -> Some (fst (Ptime.Span.to_d_ps (Ptime.to_span t)))
        | None -> None)
  with _ -> None

let date_of_days n =
  match Ptime.of_span (Ptime.Span.of_int_s (n * 86400)) with
  | Some t ->
      let y, m, d = Ptime.to_date t in
      Printf.sprintf "%04d-%02d-%02d" y m d
  | None -> "1970-01-01"

let parse_iso s =
  let s = String.trim s in
  let s = if String.length s = 10 then s ^ "T00:00:00Z" else s in
  let s =
    if
      String.ends_with ~suffix:"Z" s
      || String.length s > 19
         && (String.contains (String.sub s 19 (String.length s - 19)) '+'
            || String.contains (String.sub s 19 (String.length s - 19)) '-')
    then s
    else s ^ "Z"
  in
  match Ptime.of_rfc3339 ~strict:false s with
  | Ok (t, _, _) -> Some (Ptime.to_float_s t)
  | Error _ -> None
