type request = Request.t
type request_err = Request.err

let make_request = Request.make
let host = Request.host
let port = Request.port
let pp_request = Request.pp
let pp_request_err = Request.pp_err

type header = Header.t
type header_err = Header.parse_err

let parse_header = Header.parse
let pp_header_err = Header.pp_err

type body = string

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
