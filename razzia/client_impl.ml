open Types

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
  module Channel = Mirage_channel.Make (TLS)
  open Lwt.Infix

  let write_request flow req =
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

  let ( let* ) = Lwt_result.Syntax.( let* )
  let ( let+ ) = Lwt_result.Syntax.( let+ )

  let read_header chan =
    let buf = Buffer.create 1024 in
    let rec parse len cr =
      if len < 1024 then
        Channel.read_char chan >>= function
        | Ok (`Data '\n') when cr -> Buffer.contents buf |> Lwt.return_ok
        | Ok (`Data '\r') -> parse (len + 1) true
        | Ok (`Data c) ->
            Buffer.add_char buf c;
            parse (len + 1) false
        | Ok `Eof -> Lwt.return_error `MalformedHeader
        | Error _ -> err_tls
      else Lwt.return_error `MalformedHeader
    in
    parse 0 false >>= function
    | Ok h -> Lwt.return_ok h
    | Error err -> Lwt.return_error err

  let fetch req stack =
    let dns = DNS.create stack in
    let* addr = resolve dns (Request.host req) in
    Stack.TCP.create_connection (Stack.tcp stack) (addr, Request.port req)
    >>= function
    | Ok flow -> (
        TLS.client_of_flow
          (Tls.Config.client ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None) ())
          flow
        >>= function
        | Ok flow -> (
            write_request flow req >>= function
            | Ok () -> (
                let chan = Channel.create flow in
                read_header chan >>= function
                | Ok header -> Lwt.return_ok header
                | Error (#Header.parse_err as err) ->
                    Lwt.return_error (`Header err)
                | Error _ -> err_tls)
            | Error _ -> err_tls_write)
        | Error _ -> err_tls_write)
    | Error _ -> err_tcp
end
