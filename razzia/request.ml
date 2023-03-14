type t = {
  host : string;
  port : int;
  uri : Uri.t;
  client_cert : Tls.Config.own_cert;
  trusted : cert list;
}

and cert = string * int * string * float

type err =
  [ `AboveMaxSize
  | `BeginWithBOM
  | `EmptyURL
  | `MalformedUTF8
  | `MissingHost
  | `MissingScheme
  | `UserInfoNotAllowed ]

let ( let* ) x f = match x with Error _ as err -> err | Ok x -> f x

let check_length url =
  let length = Bytes.of_string url |> Bytes.length in
  if length = 0 then Error `EmptyURL
  else if length > 1024 then Error `AboveMaxSize
  else Ok ()

let check_utf8_encoding url =
  if String.is_valid_utf_8 url then Ok () else Error `MalformedUTF8

let check_bom url =
  if
    String.get_utf_8_uchar url 0
    |> Uchar.utf_decode_uchar |> Uchar.equal Uchar.bom
  then Error `BeginWithBOM
  else Ok ()

let chek_scheme uri =
  match Uri.scheme uri with None -> Error `MissingScheme | Some _ -> Ok uri

let check_userinfo uri =
  match Uri.userinfo uri with
  | None -> Ok ()
  | Some _ -> Error `UserInfoNotAllowed

let check_host uri =
  match Uri.host uri with None -> Error `MissingHost | Some h -> Ok h

let make ?(trusted = []) ?(client_cert = `None) ?(default_scheme = "gemini") uri
    =
  let url = Uri.to_string uri in
  let* () = check_length url in
  let* () = check_utf8_encoding url in
  let* () = check_bom url in
  let uri = Uri.canonicalize uri in
  let uri =
    match Uri.scheme uri with
    | None ->
        Format.asprintf "%s://%a" default_scheme Uri.pp uri |> Uri.of_string
    | Some _ -> uri
  in
  let* uri = chek_scheme uri in
  let* () = check_userinfo uri in
  let* host = check_host uri in
  let port = Uri.port uri |> Option.value ~default:1965 in
  Ok { host; port; uri; client_cert; trusted }

let host t = t.host
let port t = t.port
let target t = t.uri
let query t = match Uri.query t.uri with [] -> None | (q, _) :: _ -> Some q
let pp fmt { uri; _ } = Format.fprintf fmt "%a\r\n" Uri.pp uri

let pp_err fmt = function
  | `AboveMaxSize -> Format.fprintf fmt "AboveMaxSize"
  | `BeginWithBOM -> Format.fprintf fmt "BeginWithBOM"
  | `EmptyURL -> Format.fprintf fmt "EmptyURL"
  | `MalformedUTF8 -> Format.fprintf fmt "MalformedUTF8"
  | `MissingHost -> Format.fprintf fmt "MissingHost"
  | `MissingScheme -> Format.fprintf fmt "MissingScheme"
  | `UserInfoNotAllowed -> Format.fprintf fmt "UserInfoNotAllowed"

module type TLS_CFG = sig
  val make : t -> cert option ref -> Tls.Config.client
end

module TlsCfg (P : Mirage_clock.PCLOCK) : TLS_CFG = struct
  let find_by_host req =
    List.find_opt (fun (h, _, _, _) -> String.equal req.host h) req.trusted

  let make req trustable =
    let tofu = find_by_host req in
    let authenticator ?ip:_ ~host certs =
      let cert = List.hd certs in
      let srv_fingerprint =
        X509.Certificate.fingerprint `SHA256 cert |> Cstruct.to_string
      in
      let exp_date =
        X509.Certificate.validity cert |> snd |> Ptime.to_float_s
      in
      let new_entry =
        Option.value
          ~default:(req.host, req.port, srv_fingerprint, exp_date)
          tofu
      in
      let fingerprint =
        Option.fold tofu ~none:srv_fingerprint ~some:(fun (_, _, f, _) -> f)
        |> Cstruct.of_string
      in
      let v =
        X509.Validation.trust_cert_fingerprint ~host
          ~time:(fun () -> P.now_d_ps () |> Ptime.unsafe_of_d_ps |> Option.some)
          ~hash:`SHA256 ~fingerprint certs
      in
      (match (v, tofu) with
      | Ok _, None -> trustable := Some new_entry
      | _ -> ());
      v
    in
    Tls.Config.client ~authenticator ~certificates:req.client_cert ()
end
