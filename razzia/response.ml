type 'stream t =
  | Input of { sensitive : bool; prompt : string }
  | Sucess of 'stream body
  | Redirect of [ `Temp | `Perm ] * string
  | TempFailure of
      [ `Msg | `ServerUnavailable | `CGIError | `ProxyError | `SlowDown ]
      * string
  | PermFailure of
      [ `Msg | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ] * string
  | ClientCertReq of [ `Msg | `CertNotAuth | `CertNotValid ] * string

and 'stream body =
  | Gemtext of {
      encoding : string option;
      lang : string option;
      body : 'stream;
    }
  | Other of { encoding : string option; mime : string; body : 'stream }

type err = [ `InvalidCode | `Malformed | `TooLong ]

let of_int meta body = function
  | 10 -> Some (Input { sensitive = false; prompt = meta })
  | 11 -> Some (Input { sensitive = true; prompt = meta })
  | 20 ->
      let body =
        match Mime.of_string meta with
        | encoding, Gemini { lang } -> Gemtext { encoding; lang; body }
        | encoding, MimeType mime -> Other { encoding; mime; body }
      in
      Some (Sucess body)
  | 30 -> Some (Redirect (`Temp, meta))
  | 31 -> Some (Redirect (`Perm, meta))
  | 40 -> Some (TempFailure (`Msg, meta))
  | 41 -> Some (TempFailure (`ServerUnavailable, meta))
  | 42 -> Some (TempFailure (`CGIError, meta))
  | 43 -> Some (TempFailure (`ProxyError, meta))
  | 44 -> Some (TempFailure (`SlowDown, meta))
  | 50 -> Some (PermFailure (`Msg, meta))
  | 51 -> Some (PermFailure (`NotFound, meta))
  | 52 -> Some (PermFailure (`Gone, meta))
  | 53 -> Some (PermFailure (`ProxyRequestRefused, meta))
  | 59 -> Some (PermFailure (`BadRequest, meta))
  | 60 -> Some (ClientCertReq (`Msg, meta))
  | 61 -> Some (ClientCertReq (`CertNotAuth, meta))
  | 62 -> Some (ClientCertReq (`CertNotValid, meta))
  | _ -> None

let status_code = function
  | Input { sensitive = false; _ } -> 10
  | Input { sensitive = true; _ } -> 11
  | Sucess _ -> 20
  | Redirect (`Temp, _) -> 30
  | Redirect (`Perm, _) -> 31
  | TempFailure (`Msg, _) -> 40
  | TempFailure (`ServerUnavailable, _) -> 41
  | TempFailure (`CGIError, _) -> 42
  | TempFailure (`ProxyError, _) -> 43
  | TempFailure (`SlowDown, _) -> 44
  | PermFailure (`Msg, _) -> 50
  | PermFailure (`NotFound, _) -> 51
  | PermFailure (`Gone, _) -> 52
  | PermFailure (`ProxyRequestRefused, _) -> 53
  | PermFailure (`BadRequest, _) -> 59
  | ClientCertReq (`Msg, _) -> 60
  | ClientCertReq (`CertNotAuth, _) -> 61
  | ClientCertReq (`CertNotValid, _) -> 62

let make ~header ~body =
  match Header.parse header with
  | Ok { status; meta } -> (
      match of_int meta body status with
      | None -> Error `InvalidCode
      | Some resp -> Ok resp)
  | Error _ as err -> err

let pp_redirect fmt = function
  | `Temp -> Format.fprintf fmt "`Temp"
  | `Perm -> Format.fprintf fmt "`Perm"

let pp_temp_failure fmt = function
  | `Msg -> Format.fprintf fmt "`Msg"
  | `ServerUnavailable -> Format.fprintf fmt "`ServerUnavailable"
  | `CGIError -> Format.fprintf fmt "`CGIError"
  | `ProxyError -> Format.fprintf fmt "`ProxyError"
  | `SlowDown -> Format.fprintf fmt "`SlowDown"

let pp_perm_failure fmt = function
  | `Msg -> Format.fprintf fmt "`Msg"
  | `NotFound -> Format.fprintf fmt "`NotFound"
  | `Gone -> Format.fprintf fmt "`Gone"
  | `ProxyRequestRefused -> Format.fprintf fmt "`ProxyRequestRefused"
  | `BadRequest -> Format.fprintf fmt "`BadRequest"

let pp_client_cert fmt = function
  | `Msg -> Format.fprintf fmt "`Msg"
  | `CertNotAuth -> Format.fprintf fmt "`CertNotAuth"
  | `CertNotValid -> Format.fprintf fmt "`CertNotValid"

let pp_body fmt = function
  | Gemtext { encoding; lang; _ } ->
      Format.fprintf fmt "Gemtext { encoding = %S; lang = %S; body = ... }"
        (Option.value ~default:"None" encoding)
        (Option.value ~default:"None" lang)
  | Other { encoding; mime; _ } ->
      Format.fprintf fmt "Gemtext { encoding = %S; mime = %S; body = ... }"
        (Option.value ~default:"None" encoding)
        mime

let pp fmt = function
  | Input { sensitive; prompt } ->
      Format.fprintf fmt "Input { sensitive = %B; prompt = %S }" sensitive
        prompt
  | Sucess body -> Format.fprintf fmt "Sucess (%a)" pp_body body
  | Redirect (r, msg) ->
      Format.fprintf fmt "Redirect (%a, %S)" pp_redirect r msg
  | TempFailure (f, msg) ->
      Format.fprintf fmt "TempFailure (%a, %S)" pp_temp_failure f msg
  | PermFailure (f, msg) ->
      Format.fprintf fmt "PermFailure (%a, %S)" pp_perm_failure f msg
  | ClientCertReq (c, msg) ->
      Format.fprintf fmt "ClientCertReq (%a, %S)" pp_client_cert c msg

let pp_err fmt = function
  | `InvalidCode -> Format.fprintf fmt "InvalidCode"
  | (`Malformed | `TooLong) as err -> Header.pp_err fmt err
