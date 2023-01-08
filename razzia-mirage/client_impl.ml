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

  let ( >>= ) = Lwt.( >>= )
  let net_err = Lwt.return_error `NetErr

  let write_request flow req =
    Format.asprintf "%a" Razzia.pp_request req
    |> Cstruct.of_string |> TLS.write flow

  let read flow =
    TLS.read flow >>= function Ok v -> Lwt.return_ok v | Error _ -> net_err

  let resolve dns host =
    match Ipaddr.of_string host with
    | Ok ip -> Lwt.return_ok ip
    | Error (`Msg _) -> (
        match Domain_name.of_string host with
        | Ok dn -> (
            match Domain_name.host dn with
            | Ok h -> (
                DNS.gethostbyname dns h >>= function
                | Ok addr -> Lwt.return_ok (Ipaddr.V4 addr)
                | Error (`Msg msg) ->
                    `Host (`UnknownHost msg) |> Lwt.return_error)
            | Error (`Msg msg) ->
                `Host (`InvalidHostname msg) |> Lwt.return_error)
        | Error (`Msg msg) -> `Host (`BadDomainName msg) |> Lwt.return_error)

  let read_header chan =
    let buf = Buffer.create (1024 + 3) in
    let rec parse len cr =
      if len < 1024 then
        Channel.read_char chan >>= function
        | Ok (`Data '\n') when cr -> Buffer.contents buf |> Lwt.return_ok
        | Ok (`Data '\r') -> parse (len + 1) true
        | Ok (`Data c) ->
            Buffer.add_char buf c;
            parse (len + 1) false
        | Ok `Eof -> Lwt.return_error `Malformed
        | Error _ -> net_err
      else Lwt.return_error `Malformed
    in
    parse 0 false >>= function
    | Ok _ as ok -> Lwt.return ok
    | Error `Malformed -> Lwt.return_error (`Header `Malformed)
    | Error _ -> net_err

  let read_body chan =
    Channel.read_some chan >>= function
    | Ok (`Data cstruct) -> Cstruct.to_string cstruct |> Lwt.return_ok
    | Ok `Eof -> Lwt.return_error `PrematuredEof
    | Error _ -> net_err

  let ( let* ) = Lwt_result.bind

  let get stack req =
    let dns = DNS.create stack in
    let* addr = resolve dns (Razzia.host req) in
    Stack.TCP.create_connection (Stack.tcp stack) (addr, Razzia.port req)
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
                | Ok header -> (
                    read_body chan >>= function
                    | Ok body ->
                        Razzia.of_raw ~header ~body
                        |> Result.fold ~ok:Lwt.return_ok ~error:(fun e ->
                               Lwt.return_error (`Header e))
                    | Error (`NetErr | `PrematuredEof) -> net_err)
                | Error err -> Lwt.return_error err)
            | Error _ -> net_err)
        | Error _ -> net_err)
    | Error _ -> net_err
end
