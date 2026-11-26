(* HTTP requests, through libcurl (ezcurl). *)

type request = {
  meth : string;
  url : string;
  headers : (string * string) list;
  body : string option;
  timeout : float;
}

type response = {
  status : int;
  resp_headers : (string * string) list;
  data : string;
}

exception
  Http_error of {
    code : int;
    url : string;
    headers : (string * string) list;
    body : string;
  }

exception Net_error of string

let reason code =
  match code with
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 408 -> "Request Timeout"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 422 -> "Unprocessable Entity"
  | 429 -> "Too Many Requests"
  | 500 -> "Internal Server Error"
  | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable"
  | 504 -> "Gateway Timeout"
  | 529 -> "Site Overloaded"
  | _ -> ""

(* A header that marks an error answered from the cache, not by the server. *)
let cached_marker = "x-opam-ci-cached"

let error_text = function
  | Http_error { code; headers; _ } ->
      (* A 404 known from the cache says so (see Net.fetch). *)
      Printf.sprintf "HTTP Error %d: %s%s" code (reason code)
        (if List.mem_assoc cached_marker headers then " (cached)" else "")
  | Net_error m -> m
  | e -> Printexc.to_string e

let () =
  Printexc.register_printer (function
    | Http_error _ as e -> Some (error_text e)
    | Net_error m -> Some m
    | _ -> None)

let header resp name =
  let name = String.lowercase_ascii name in
  List.find_map
    (fun (k, v) -> if String.lowercase_ascii k = name then Some v else None)
    resp

(* The message of a failed transfer, as the curl command prints it
   ("(28) Operation timed out after ..."), in the tool's words. *)
let curl_failure code msg =
  if code = 28 then Printf.sprintf "timed out (%s)" msg
  else if msg = "" then Printf.sprintf "curl failed with status %d" code
  else msg

(* The headers the curl command sends by default, which libcurl does not. *)
let default_headers =
  lazy
    (let version =
       match Rx.search {|libcurl/(\S+)|} (Curl.version ()) with
       | Some m -> Rx.group m 1
       | None -> ""
     in
     [ ("User-Agent", "curl/" ^ version); ("Accept", "*/*") ])

let real_request (r : request) =
  let errors = ref "" in
  let seconds t = max 1 (int_of_float (Float.round t)) in
  let set_opts c =
    Curl.set_errorbuffer c errors;
    Curl.set_timeout c (seconds r.timeout);
    Curl.set_connecttimeout c (seconds (min r.timeout 60.))
  in
  let config =
    Ezcurl.Config.(default |> follow_location true |> max_redirects 10)
  in
  let headers =
    r.headers
    @ List.filter
        (fun (k, _) -> header r.headers k = None)
        (Lazy.force default_headers)
  in
  let meth =
    match r.meth with
    | "GET" -> Ezcurl.GET
    | "POST" -> Ezcurl.POST []
    | m -> Ezcurl.HTTP_CUSTOM m
  in
  let result =
    Ezcurl.with_client ~set_opts (fun client ->
        Ezcurl.http ~client ~config
          ?content:(Option.map (fun b -> `String b) r.body)
          ~headers ~url:r.url ~meth ())
  in
  match result with
  | Ok resp ->
      { status = resp.code; resp_headers = resp.headers; data = resp.body }
  | Error (code, strerror) ->
      let code = Curl.int_of_curlCode code in
      let detail = if !errors <> "" then !errors else strerror in
      raise
        (Net_error (curl_failure code (Printf.sprintf "(%d) %s" code detail)))

let hook = ref real_request

let request ?(meth = "GET") ?(headers = []) ?body ?(timeout = 60.) url =
  let meth = if body <> None && meth = "GET" then "POST" else meth in
  !hook { meth; url; headers; body; timeout }

let urlopen ?meth ?headers ?body ?timeout url =
  let r = request ?meth ?headers ?body ?timeout url in
  if r.status >= 400 || r.status = 0 then
    raise
      (Http_error
         { code = r.status; url; headers = r.resp_headers; body = r.data });
  r.data

let quote ?(safe = "/") s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if Util.is_alnum c || String.contains "_.-~" c || String.contains safe c
      then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let unquote = Uri.pct_decode

let urljoin base href =
  Uri.to_string (Uri.resolve "" (Uri.of_string base) (Uri.of_string href))
