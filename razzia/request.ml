type t = { host : string; port : int; uri : Uri.t }

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

let chek_scheme smart uri =
  match Uri.scheme uri with
  | None when smart -> Uri.with_scheme uri (Some "gemini://") |> Result.ok
  | None -> Error `MissingScheme
  | Some _ -> Ok uri

let check_userinfo uri =
  match Uri.userinfo uri with
  | None -> Ok ()
  | Some _ -> Error `UserInfoNotAllowed

let check_host uri =
  match Uri.host uri with None -> Error `MissingHost | Some h -> Ok h

let make ?(smart_scheme = false) url =
  let* () = check_length url in
  let* () = check_utf8_encoding url in
  let* () = check_bom url in
  let uri = Uri.of_string url |> Uri.canonicalize in
  let* uri = chek_scheme smart_scheme uri in
  let* () = check_userinfo uri in
  let* host = check_host uri in
  let port = Uri.port uri |> Option.value ~default:1965 in
  Ok { host; port; uri }

let host t = t.host
let port t = t.port
let pp fmt { uri; _ } = Format.fprintf fmt "%a\r\n" Uri.pp uri

let pp_err fmt = function
  | `AboveMaxSize -> Format.fprintf fmt "AboveMaxSize"
  | `BeginWithBOM -> Format.fprintf fmt "BeginWithBOM"
  | `EmptyURL -> Format.fprintf fmt "EmptyURL"
  | `MalformedUTF8 -> Format.fprintf fmt "MalformedUTF8"
  | `MissingHost -> Format.fprintf fmt "MissingHost"
  | `MissingScheme -> Format.fprintf fmt "MissingScheme"
  | `UserInfoNotAllowed -> Format.fprintf fmt "UserInfoNotAllowed"
