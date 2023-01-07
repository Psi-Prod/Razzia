type request = Request.t
type header = Header.t
type body = string
type request_err = Request.err

let make_request = Request.make

type header_err = Header.parse_err

let pp_request_err = Request.pp_err
let pp_header_err = Header.pp_err

type fetch_err =
  [ `Header of header_err
  | `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `NetErr ]

let pp_fetch_err fmt = function
  | `Header h -> Format.fprintf fmt "Header:@ %a" Header.pp_err h
  | `Host (`BadDomainName dn) -> Format.fprintf fmt "Host:@ BadDomainName %S" dn
  | `Host (`InvalidHostname h) ->
      Format.fprintf fmt "Host:@ InvalidHostname %S" h
  | `Host (`UnknownHost h) -> Format.fprintf fmt "Host:@ UnknownHost %S" h
  | `NetErr -> Format.fprintf fmt "NetErr"

let pp_header = Header.pp

module type NET = sig
  module IO : Types.IO

  type stack

  val get : stack -> request -> (header * body, fetch_err) result IO.t
end

module Mirage = struct
  module Make
      (Random : Mirage_random.S)
      (Time : Mirage_time.S)
      (Mclock : Mirage_clock.MCLOCK)
      (Pclock : Mirage_clock.PCLOCK)
      (Stack : Tcpip.Stack.V4V6) :
    NET with module IO = Lwt and type stack = Stack.t = struct
    module IO = Lwt

    type stack = Stack.t

    include Client_impl.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  end
end
