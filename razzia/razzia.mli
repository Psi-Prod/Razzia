type request

val make_request : Uri.t -> request

type fetch_err =
  [ `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `TCP
  | `TLS
  | `TLSWrite ]

module type IO = sig
  type stack

  val fetch : request -> stack -> (string, fetch_err) Lwt_result.t
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
