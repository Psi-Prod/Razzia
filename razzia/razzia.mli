(* {1 Gemtext} *)

module Gemtext = Gemtext

(** {1 Request} *)

type request

type request_err =
  [ `AboveMaxSize
  | `BeginWithBOM
  | `EmptyURL
  | `MalformedUTF8
  | `MissingHost
  | `MissingScheme
  | `UserInfoNotAllowed ]

val make_request :
  ?default_scheme:string ->
  ?query:string ->
  string ->
  (request, request_err) result

val host : request -> string
val port : request -> int
val target : request -> Uri.t
val query : request -> string option
val pp_request : Format.formatter -> request -> unit
val pp_request_err : Format.formatter -> request_err -> unit

(** {1 Response} *)

type 'stream response = 'stream Response.t =
  | Input of { sensitive : bool; prompt : string }
  | Sucess of 'stream body
  | Redirect of [ `Temp | `Perm ] * string
  | TempFailure of
      [ `Msg | `ServerUnavailable | `CGIError | `ProxyError | `SlowDown ]
      * string
  | PermFailure of
      [ `Msg | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ] * string
  | ClientCertReq of [ `Msg | `CertNotAuth | `CertNotValid ] * string

and 'stream body = 'stream Response.body = {
  encoding : string option;
  mime : mime;
  body : 'stream;
}

and mime = Mime.t = Gemtext of { lang : string option } | MimeType of string

type response_err = [ `InvalidCode | `Malformed | `TooLong ]

val status_code : 'a response -> int
val pp_response : Format.formatter -> 'a response -> unit
val pp_response_err : Format.formatter -> response_err -> unit

(** {1 Error} *)

type err =
  [ `Header of response_err
  | `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `NetErr ]

val pp_err : Format.formatter -> err -> unit

(** {1 NET} *)

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type NET = sig
  module IO : IO

  type stack
  type stream

  val single_read : stream -> string
  val get : stack -> request -> (stream response, err) result IO.t
end

module Private : sig
  type header = Header.t

  val is_success : header -> bool
  val make_response : header:header -> body:'a -> 'a response

  module type CHANNEL = sig
    module IO : IO

    type src

    val next : src -> char option option IO.t
  end

  module type S = sig
    module IO : IO

    type src

    val parse :
      src -> ((header, response_err) result, [ `Eof | `NetErr ]) result IO.t
  end

  module MakeParser (Chan : CHANNEL) :
    S with module IO := Chan.IO and type src := Chan.src
end
