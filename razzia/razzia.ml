open Types

type request = Request.t

let make_request = Request.make

type parse_err = Header.parse_err

type fetch_err =
  [ `Header of parse_err
  | `Host of
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

module Mirage = struct
  module Make
      (Random : Mirage_random.S)
      (Time : Mirage_time.S)
      (Mclock : Mirage_clock.MCLOCK)
      (Pclock : Mirage_clock.PCLOCK)
      (Stack : Tcpip.Stack.V4V6) : IO with type stack = Stack.t = struct
    type stack = Stack.t

    include Client_impl.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  end
end
