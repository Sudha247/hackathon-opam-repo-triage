(** HTTP requests, through libcurl (ezcurl). *)

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
(** An HTTP error status ([code] 0: no status line). *)

exception Net_error of string
(** The server could not be reached, or did not answer in time. *)

val request :
  ?meth:string ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?timeout:float ->
  string ->
  response
(** Send a request and return the response, whatever its status. A request with
    a body is a POST unless [meth] says otherwise. *)

val urlopen :
  ?meth:string ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?timeout:float ->
  string ->
  string
(** The body of the response; [Http_error] for a status of 400 or more. *)

val hook : (request -> response) ref
(** The transport [request] uses (libcurl): tests replace it. *)

val header : (string * 'a) list -> string -> 'a option
(** The header [name] (case-insensitive) of a header list. *)

val cached_marker : string
(** A response header that marks an error answered from the cache, not by the
    server. *)

val error_text : exn -> string
(** The message of an exception, ["HTTP Error 404: Not Found"] for an
    [Http_error] (with " (cached)" when it comes from the cache). *)

(** {1 URLs} *)

val quote : ?safe:string -> string -> string
(** Percent-encodes every byte but ASCII letters, digits, ["_.-~"] and [safe]
    (default ["/"]). *)

val unquote : string -> string
(** Decodes the percent-encoded bytes. *)

val urljoin : string -> string -> string
(** The URL of [href] relative to [base] (RFC 3986). *)
