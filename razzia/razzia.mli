type request
type header

val make_request : Uri.t -> request

type header_err = [ `InvalidCode | `Malformed | `TooLong ]

type fetch_err =
  [ `Header of header_err
  | `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `NetErr ]

val pp_header_err : Format.formatter -> header_err -> unit
val pp_fetch_err : Format.formatter -> fetch_err -> unit
val pp_header : Format.formatter -> header -> unit

module type IO = sig
  type stack

  val get : stack -> request -> (header * string, fetch_err) Lwt_result.t
end

module Mirage : sig
  module Make : functor
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    -> IO with type stack = Stack.t
end
