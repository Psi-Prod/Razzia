type temp_failure =
  [ `Msg | `ServerUnavailable | `CGIError | `ProxyError | `SlowDown ] * string

type perm_failure =
  [ `Msg | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ] * string

type client_cert = [ `Msg | `CertNotAuth | `CertNotValid ] * string

type err =
  [ `TempFailure of temp_failure
  | `PermFailure of perm_failure
  | `ClientCertRequired of client_cert ]

type success =
  [ `Input of string * [ `Sensitive of bool ]
  | `Success of string (* TODO mime *)
  | `Redirect of Uri.t * [ `Temp | `Perm ] ]

type t = [ success | err ]

let to_int status =
  match status with
  | `Input (_, `Sensitive true) -> 10
  | `Input (_, `Sensitive false) -> 11
  | `Success _ -> 20
  | `Redirect (_, `Temp) -> 30
  | `Redirect (_, `Perm) -> 31
  | `TempFailure (`Msg, _) -> 40
  | `TempFailure (`ServerUnavailable, _) -> 41
  | `TempFailure (`CGIError, _) -> 42
  | `TempFailure (`ProxyError, _) -> 43
  | `TempFailure (`SlowDown, _) -> 44
  | `PermFailure (`Msg, _) -> 50
  | `PermFailure (`NotFound, _) -> 51
  | `PermFailure (`Gone, _) -> 52
  | `PermFailure (`ProxyRequestRefused, _) -> 53
  | `PermFailure (`BadRequest, _) -> 59
  | `ClientCertRequired (`Msg, _) -> 60
  | `ClientCertRequired (`CertNotAuth, _) -> 61
  | `ClientCertRequired (`CertNotValid, _) -> 62

let from_int meta = function
  | 10 -> Some (`Input (meta, `Sensitive true))
  | 11 -> Some (`Input (meta, `Sensitive false))
  | 20 -> Some (`Success meta)
  | 30 -> Some (`Redirect (Uri.of_string meta, `Temp))
  | 31 -> Some (`Redirect (Uri.of_string meta, `Perm))
  | 40 -> Some (`TempFailure (`Msg, meta))
  | 41 -> Some (`TempFailure (`ServerUnavailable, meta))
  | 42 -> Some (`TempFailure (`CGIError, meta))
  | 43 -> Some (`TempFailure (`ProxyError, meta))
  | 44 -> Some (`TempFailure (`SlowDown, meta))
  | 50 -> Some (`PermFailure (`Msg, meta))
  | 51 -> Some (`PermFailure (`NotFound, meta))
  | 52 -> Some (`PermFailure (`Gone, meta))
  | 53 -> Some (`PermFailure (`ProxyRequestRefused, meta))
  | 59 -> Some (`PermFailure (`BadRequest, meta))
  | 60 -> Some (`ClientCertRequired (`Msg, meta))
  | 61 -> Some (`ClientCertRequired (`CertNotAuth, meta))
  | 62 -> Some (`ClientCertRequired (`CertNotValid, meta))
  | _ -> None

let pp fmt status = Format.fprintf fmt "%i" (to_int status)
