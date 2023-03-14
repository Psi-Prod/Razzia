module Direct = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
end

open Eio

let ( let+ ) x f =
  match x with
  | Error (`Msg msg) -> Error (`Host (`InvalidHostname msg))
  | Ok x -> f x

module HeaderParser = Razzia.Private.MakeParser (struct
  module IO = Direct

  type src = Buf_read.t

  let next buf =
    match Buf_read.any_char buf with
    | exception End_of_file -> Some None
    | c -> Some (Some c)
end)

module TlsClientCfg = Razzia.Private.TlsCfg (Pclock)

let connect ~net (service, host) req =
  Net.with_tcp_connect net ~service ~host (fun flow ->
      let+ dn = Domain_name.of_string host in
      let+ host = Domain_name.host dn in
      let client =
        Tls_eio.client_of_flow (TlsClientCfg.make req (ref None)) ~host flow
      in
      Flow.copy_string (Format.asprintf "%a" Razzia.pp_request req) client;
      let buf = Buf_read.of_flow client ~max_size:Sys.max_string_length in
      try
        match HeaderParser.parse buf with
        | Ok (Ok header) when Razzia.Private.is_success header ->
            let body = Buf_read.take_all buf in
            Razzia.Private.make_response ~header ~body
            |> Result.map_error (fun e -> `Header e)
        | Ok (Ok header) ->
            Razzia.Private.make_response ~header ~body:""
            |> Result.map_error (fun e -> `Header e)
        | Ok (Error err) -> `Header err |> Result.error
        | Error _ -> Error `NetErr
      with
      | Failure _ -> Error (`Header `Malformed)
      | End_of_file -> Error `NetErr)

let get net req =
  let host = Razzia.host req in
  try connect ~net (Razzia.port req |> Int.to_string, host) req
  with Exn.Io (Net.E (Connection_failure No_matching_addresses), _) ->
    Error (`Host (`UnknownHost host))

let single_read s = s
