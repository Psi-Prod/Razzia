(* let tls_config =
  let null ?ip:_ ~host:_ _certs = Ok None in
  Tls.Config.client ~authenticator:null ()
(* TODO: TOFU *)

let rec get t url =
  if Uri.scheme url <> Some "gemini" then
    Fmt.failwith "Not a gemini URL: %a" Uri.pp url;
  let port = Uri.port url |> Option.value ~default:1965 in
  match Uri.host url with
  | None -> Fmt.failwith "Missing host in URL %a" Uri.pp url
  | Some host -> (
      let response =
        Eio.Net.with_tcp_connect t.net ~service:(string_of_int port) ~host
        @@ fun conn ->
        let host =
          Domain_name.of_string_exn host |> Domain_name.host |> Result.to_option
        in
        let conn = Tls_eio.client_of_flow tls_config ?host conn in
        Eio.Flow.copy_string (Uri.to_string url ^ "\r\n") conn;
        Eio.Buf_read.parse_exn response conn ~max_size:(1024 * 1024)
      in
      match response with
      | `OK (mime_type, data) ->
          Log.debug (fun f -> f "OK: mime-type=%S" mime_type);
          OK { url; mime_type; data }
      | `Redirect uri ->
          Log.debug (fun f -> f "Redirect to %a" Uri.pp uri);
          get t uri) *)
