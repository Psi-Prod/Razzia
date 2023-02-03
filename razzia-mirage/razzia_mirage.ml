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
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6) : S with type stack := Stack.t = struct
  module IO = Lwt
  module TLS = Tls_mirage.Make (Stack.TCP)
  module DNS = Dns_client_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)
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
    DNS.gethostbyname6 dns h >>= function
    | Ok addr -> Lwt.return_ok (Ipaddr.V6 addr)
    | Error _ -> (
        DNS.gethostbyname dns h >>= function
        | Ok addr -> Lwt.return_ok (Ipaddr.V4 addr)
        | Error (`Msg msg) -> `Host (`UnknownHost msg) |> Lwt.return_error)

  let resolve dns host =
    match Ipaddr.of_string host with
    | Ok ip -> Lwt.return_ok ip
    | Error (`Msg _) -> (
        match Domain_name.of_string host with
        | Ok dn -> (
            match Domain_name.host dn with
            | Ok h -> gethostbyname dns h
            | Error (`Msg msg) ->
                `Host (`InvalidHostname msg) |> Lwt.return_error)
        | Error (`Msg msg) -> `Host (`BadDomainName msg) |> Lwt.return_error)

  let read_header chan =
    let buf = Buffer.create (1024 + 3) in
    let rec parse len cr =
      if len < 1024 then
        let^ data = Channel.read_char chan in
        match data with
        | `Data '\n' when cr -> Buffer.contents buf |> Lwt.return_ok
        | `Data '\r' -> parse (len + 1) true
        | `Data c ->
            Buffer.add_char buf c;
            parse (len + 1) false
        | `Eof -> Lwt.return_error `Malformed
      else Lwt.return_error `Malformed
    in
    Lwt_result.map_error
      (function `Malformed -> `Header `Malformed | _ -> `NetErr)
      (parse 0 false)

  let read_body chan =
    let^ body = Channel.read_some chan in
    match body with
    | `Data cstruct -> Cstruct.to_string cstruct |> Lwt.return_ok
    | `Eof -> Lwt.return_error `PrematuredEof

  let single_read s = s

  let get stack req =
    let dns = DNS.create stack in
    let* addr = resolve dns (Razzia.host req) in
    let^ conn =
      Stack.TCP.create_connection (Stack.tcp stack) (addr, Razzia.port req)
    in
    let^ flow =
      TLS.client_of_flow
        (Tls.Config.client ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None) ())
        conn
    in
    let^ () = write_request flow req in
    let chan = Channel.create flow in
    let* header = read_header chan in
    Log.warn (fun l -> l "Header: %S" header);
    Log.warn (fun l -> l "read body");
    let^ body = read_body chan in
    Log.warn (fun l -> l "body readed");
    match Razzia.make_response ~header ~body with
    | Ok resp -> Lwt.return_ok resp
    | Error err -> Lwt.return_error (`Header err)
end
