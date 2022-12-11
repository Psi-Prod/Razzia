open Types

type fetch_err =
  [ `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `TCP
  | `TLS
  | `TLSWrite ]

let err_host v = Lwt.return_error (`Host v)
let err_tls = Lwt.return_error `TLS
let err_tls_write = Lwt.return_error `TLSWrite
let err_tcp = Lwt.return_error `TCP

module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6) =
struct
  module TLS = Tls_mirage.Make (Stack.TCP)
  module DNS = Dns_client_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  module DNS_Client = Dns_client.Make (DNS.Transport)
  open Lwt.Infix

  let ( let^ ) = Lwt_result.bind

  let write flow req =
    Request.to_string req |> Cstruct.of_string |> TLS.write flow >>= function
    | Ok () -> Lwt.return_ok ()
    | Error _ -> err_tls_write

  let read flow =
    TLS.read flow >>= function Ok v -> Lwt.return_ok v | Error _ -> err_tls

  let resolve dns host =
    match Ipaddr.of_string host with
    | Ok ip -> Lwt.return_ok ip
    | Error (`Msg _) -> (
        match Domain_name.of_string host with
        | Ok dn -> (
            match Domain_name.host dn with
            | Ok h -> (
                DNS.gethostbyname6 dns h >>= function
                | Ok addr -> Lwt.return_ok (Ipaddr.V6 addr)
                | Error (`Msg msg) -> err_host (`UnknownHost msg))
            | Error (`Msg msg) -> err_host (`InvalidHostname msg))
        | Error (`Msg msg) -> err_host (`BadDomainName msg))

  let fetch req stack : (string, fetch_err) Lwt_result.t =
    let dns = DNS_Client.create stack ~timeout:(Duration.of_sec 5) in
    resolve dns (Request.host req) >>= function
    | Error _ as err -> Lwt.return err
    | Ok addr -> (
        Stack.TCP.create_connection (Stack.tcp stack) (addr, Request.port req)
        >>= function
        | Ok flow -> (
            TLS.client_of_flow
              (Tls.Config.client
                 ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
                 ())
              flow
            >>= function
            | Ok flow -> (
                let^ () = write flow req in
                TLS.read flow >>= function
                | Ok (`Data buf) -> Cstruct.to_string buf |> Lwt.return_ok
                | Ok `Eof -> failwith "eof"
                | Error _ -> err_tls)
            | Error _ -> err_tls_write)
        | Error _ -> err_tcp)
end
