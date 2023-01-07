type request

type request_err =
  [ `AboveMaxSize
  | `BeginWithBOM
  | `EmptyURL
  | `MalformedUTF8
  | `MissingHost
  | `MissingScheme
  | `UserInfoNotAllowed ]

val make_request : ?smart_scheme:bool -> string -> (request, request_err) result
val host : request -> string
val port : request -> int
val pp_request : Format.formatter -> request -> unit
val pp_request_err : Format.formatter -> request_err -> unit

type header
type header_err = [ `InvalidCode | `Malformed | `TooLong ]

val parse_header : string -> (header, header_err) result
val pp_header : Format.formatter -> header -> unit
val pp_header_err : Format.formatter -> header_err -> unit

type fetch_err =
  [ `Header of header_err
  | `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `NetErr ]

val pp_fetch_err : Format.formatter -> fetch_err -> unit

type body = string

module type IO = sig
  type 'a t
end

module type NET = sig
  module IO : IO

  type stack

  val get : stack -> request -> (header * body, fetch_err) result IO.t
end
