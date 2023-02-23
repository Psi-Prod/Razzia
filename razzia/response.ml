type 'stream t =
  | Input of { sensitive : bool; prompt : string }
  | Success of 'stream body
  | Redirect of [ `Temp | `Perm ] * string
  | TempFailure of
      [ `Msg | `ServerUnavailable | `CGIError | `ProxyError | `SlowDown ]
      * string
  | PermFailure of
      [ `Msg | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ] * string
  | ClientCertReq of [ `Msg | `CertNotAuth | `CertNotValid ] * string

and 'stream body = { encoding : string option; mime : Mime.t; body : 'stream }

type err = [ `InvalidCode of int | `Malformed | `TooLong ]

let of_int meta body = function
  | 10 -> Input { sensitive = false; prompt = meta }
  | 11 -> Input { sensitive = true; prompt = meta }
  | 20 ->
      let ({ encoding; mime } : Mime.t') = Mime.of_string meta in
      Success { encoding; mime; body }
  | 30 -> Redirect (`Temp, meta)
  | 31 -> Redirect (`Perm, meta)
  | 40 -> TempFailure (`Msg, meta)
  | 41 -> TempFailure (`ServerUnavailable, meta)
  | 42 -> TempFailure (`CGIError, meta)
  | 43 -> TempFailure (`ProxyError, meta)
  | 44 -> TempFailure (`SlowDown, meta)
  | 50 -> PermFailure (`Msg, meta)
  | 51 -> PermFailure (`NotFound, meta)
  | 52 -> PermFailure (`Gone, meta)
  | 53 -> PermFailure (`ProxyRequestRefused, meta)
  | 59 -> PermFailure (`BadRequest, meta)
  | 60 -> ClientCertReq (`Msg, meta)
  | 61 -> ClientCertReq (`CertNotAuth, meta)
  | 62 -> ClientCertReq (`CertNotValid, meta)
  | _ -> assert false

let status_code = function
  | Input { sensitive = false; _ } -> 10
  | Input { sensitive = true; _ } -> 11
  | Success _ -> 20
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

let make ~header:(status, meta) ~body =
  if String.length meta > 1024 then Error `TooLong
  else Ok (of_int meta body status)

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

let pp fmt = function
  | Input { sensitive; prompt } ->
      Format.fprintf fmt "Input@ {@ sensitive@ =@ %B;@ prompt@ =@ %S@ }"
        sensitive prompt
  | Success { encoding; mime; _ } ->
      Format.fprintf fmt
        "Success@ {@ encoding = %a;@ mime@ =@ %a;@ body@ =@ ...@ }"
        (Format.pp_print_option
           ~none:(fun fmt () -> Format.pp_print_string fmt "None")
           Format.pp_print_string)
        encoding Mime.pp mime
  | Redirect (r, msg) ->
      Format.fprintf fmt "Redirect@ (%a,@ %S)" pp_redirect r msg
  | TempFailure (f, msg) ->
      Format.fprintf fmt "TempFailure@ (%a,@ %S)" pp_temp_failure f msg
  | PermFailure (f, msg) ->
      Format.fprintf fmt "PermFailure@ (%a,@ %S)" pp_perm_failure f msg
  | ClientCertReq (c, msg) ->
      Format.fprintf fmt "ClientCertReq@ (%a,@ %S)" pp_client_cert c msg

let pp_err = Header.pp_err
