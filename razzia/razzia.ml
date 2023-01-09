module Gemtext = Gemtext

type request = Request.t
type request_err = Request.err

let make_request = Request.make
let target = Request.target
let host = Request.host
let port = Request.port
let pp_request = Request.pp
let pp_request_err = Request.pp_err

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

and 'stream body = 'stream Response.body =
  | Gemtext of {
      encoding : string option;
      lang : string option;
      body : 'stream;
    }
  | Other of { encoding : string option; mime : string; body : 'stream }

type response_err = Response.err

let make_response = Response.make
let status_code = Response.status_code
let pp_response = Response.pp
let pp_response_err = Response.pp_err

type err =
  [ `Header of response_err
  | `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `NetErr ]

let pp_err fmt = function
  | `Header h -> Format.fprintf fmt "Header:@ %a" Response.pp_err h
  | `Host (`BadDomainName dn) -> Format.fprintf fmt "Host:@ BadDomainName %S" dn
  | `Host (`InvalidHostname h) ->
      Format.fprintf fmt "Host:@ InvalidHostname %S" h
  | `Host (`UnknownHost h) -> Format.fprintf fmt "Host:@ UnknownHost %S" h
  | `NetErr -> Format.fprintf fmt "NetErr"

module type IO = sig
  type 'a t
end

module type NET = sig
  module IO : IO

  type stack
  type stream

  val get : stack -> request -> (stream response, err) result IO.t
end
