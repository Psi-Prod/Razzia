module Direct = struct
  type 'a t = 'a
end

open Eio

let tls_config =
  Tls.Config.client ~authenticator:(fun ?ip:_ ~host:_ _certs -> Ok None) ()

let ( let+ ) x f =
  match x with
  | Error (`Msg msg) -> Error (`Host (`InvalidHostname msg))
  | Ok x -> f x

let header =
  let crlf = Buf_read.string "\r\n" in
  Buf_read.(Syntax.(take_while (fun c -> not (Char.equal c '\r')) <* crlf))

let get net req =
  let service = Razzia.port req |> Int.to_string in
  let host = Razzia.host req in
  Net.with_tcp_connect net ~service ~host (fun flow ->
      let+ dn = Domain_name.of_string host in
      let+ host = Domain_name.host dn in
      let client = Tls_eio.client_of_flow tls_config ~host flow in
      Flow.copy_string (Format.asprintf "%a" Razzia.pp_request req) client;
      let header_raw, body =
        (Buf_read.pair header Buf_read.take_all)
          (Buf_read.of_flow client ~max_size:Sys.max_string_length)
      in
      match Razzia.parse_header header_raw with
      | Ok header -> Ok (header, body)
      | Error err -> Error (`Header err))
