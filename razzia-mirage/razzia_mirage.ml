module type S = sig
  type stack

  include
    Razzia.NET
      with module IO := Lwt
       and type stack := stack
       and type stream = string
end

let src = Logs.Src.create "razzia"

module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Dns : Dns_client_mirage.S with type Transport.stack = Stack.t) :
  S with type stack := Stack.t = struct
  module IO = Lwt
  module TLS = Tls_mirage.Make (Stack.TCP)
  module Channel = Mirage_channel.Make (TLS)

  type stream = string

  let ( let* ) = Lwt_result.bind

  let ( let^ ) r f =
    Lwt.bind r (function Error _ -> Lwt.return_error `NetErr | Ok x -> f x)

  let write_request flow req =
    Format.asprintf "%a" Razzia.pp_request req
    |> Cstruct.of_string |> TLS.write flow

  let gethostbyname dns h =
    let open Lwt.Infix in
    Dns.gethostbyname dns h >>= function
    | Ok addr -> Lwt.return_ok (Ipaddr.V4 addr)
    | Error _ -> (
        Dns.gethostbyname6 dns h >|= function
        | Ok addr -> Ok (Ipaddr.V6 addr)
        | Error (`Msg msg) -> Error (`Host (`UnknownHost msg)))

  let resolve dns host =
    match Ipaddr.of_string host with
    | Ok ip -> Lwt.return_ok ip
    | Error (`Msg _) -> (
        match Domain_name.of_string host with
        | Ok dn -> (
            match Domain_name.host dn with
            | Ok h -> gethostbyname dns h
            | Error (`Msg msg) ->
                Lwt.return_error (`Host (`InvalidHostname msg)))
        | Error (`Msg msg) -> Lwt.return_error (`Host (`BadDomainName msg)))

  module HeaderParser = Razzia.Private.MakeParser (struct
    module IO = Lwt

    type src = Channel.t

    let next chan =
      let open Lwt.Infix in
      Channel.read_char chan >|= function
      | Ok `Eof -> Some None
      | Ok (`Data c) -> Some (Some c)
      | Error _ -> None
  end)

  let read_body chan =
    let buf = Buffer.create 16384 in
    let rec loop () =
      let^ chunk = Channel.read_some ~len:16384 chan in
      match chunk with
      | `Eof -> Buffer.contents buf |> Lwt.return_ok
      | `Data cstruct ->
          Cstruct.to_bytes cstruct |> Buffer.add_bytes buf;
          loop ()
    in
    loop ()

  let single_read s = s

  module TlsClientCfg = Razzia.Private.TlsCfg (Pclock)

  let get stack req =
    let dns = Dns.create stack in
    let host = Razzia.host req in
    let* addr = Domain_name.to_string host |> resolve dns in
    let^ conn =
      Stack.TCP.create_connection (Stack.tcp stack) (addr, Razzia.port req)
    in
    let^ flow =
      TLS.client_of_flow ~host (TlsClientCfg.make req (ref None)) conn
    in
    let^ () = write_request flow req in
    let chan = Channel.create flow in
    let^ header = HeaderParser.parse chan in
    match header with
    | Ok header when Razzia.Private.is_success header ->
        let^ body = read_body chan in
        Razzia.Private.make_response ~header ~body
        |> Result.map_error (fun e -> `Header e)
        |> Lwt.return
    | Ok header ->
        Razzia.Private.make_response ~header ~body:""
        |> Result.map_error (fun e -> `Header e)
        |> Lwt.return
    | Error err -> Lwt.return_error (`Header err)
end
