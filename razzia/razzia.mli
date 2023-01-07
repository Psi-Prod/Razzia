type request
type header
type body = string

type request_err =
  [ `AboveMaxSize
  | `BeginWithBOM
  | `EmptyURL
  | `MalformedUTF8
  | `MissingHost
  | `MissingScheme
  | `UserInfoNotAllowed ]

val make_request : ?smart_scheme:bool -> string -> (request, request_err) result

type header_err = [ `InvalidCode | `Malformed | `TooLong ]

type fetch_err =
  [ `Header of header_err
  | `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `NetErr ]

val pp_request_err : Format.formatter -> request_err -> unit
val pp_header_err : Format.formatter -> header_err -> unit
val pp_fetch_err : Format.formatter -> fetch_err -> unit
val pp_header : Format.formatter -> header -> unit

module type NET = sig
  module IO : Types.IO

  type stack

  val get : stack -> request -> (header * body, fetch_err) result IO.t
end

module Mirage : sig
  module Make : functor
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    -> NET with type stack = Stack.t
end
