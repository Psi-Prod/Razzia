module Request = struct
  type t = Uri.t

  let make u = u (* TODO *)
  let host t = Uri.host t |> Option.get
  let port t = Uri.port t |> Option.value ~default:1965
  let to_string uri = Uri.to_string uri ^ "\r\n"
end

module Status = struct
  type t =
    | Input of { sensitive : bool }
    | Success
    | RedirectTemp
    | RedirectPerm
    | TempFailure
    | ServerUnavailable
    | CGIError
    | ProxyError
    | SlowDown
    | PermFailure
    | NotFound
    | Gone
    | ProxyRequestRefused
    | BadRequest
    | ClientCertRequired
    | CertNotAuth
    | CertNotValid

  let to_int status =
    match status with
    | Input { sensitive = true } -> 10
    | Input { sensitive = false } -> 11
    | Success -> 20
    | RedirectTemp -> 30
    | RedirectPerm -> 31
    | TempFailure -> 40
    | ServerUnavailable -> 41
    | CGIError -> 42
    | ProxyError -> 43
    | SlowDown -> 44
    | PermFailure -> 50
    | NotFound -> 51
    | Gone -> 52
    | ProxyRequestRefused -> 53
    | BadRequest -> 59
    | ClientCertRequired -> 60
    | CertNotAuth -> 61
    | CertNotValid -> 62

  let from_int = function
    | 10 -> Some (Input { sensitive = true })
    | 11 -> Some (Input { sensitive = false })
    | 20 -> Some Success
    | 30 -> Some RedirectTemp
    | 31 -> Some RedirectPerm
    | 40 -> Some TempFailure
    | 41 -> Some ServerUnavailable
    | 42 -> Some CGIError
    | 43 -> Some ProxyError
    | 44 -> Some SlowDown
    | 50 -> Some PermFailure
    | 51 -> Some NotFound
    | 52 -> Some Gone
    | 53 -> Some ProxyRequestRefused
    | 59 -> Some BadRequest
    | 60 -> Some ClientCertRequired
    | 61 -> Some CertNotAuth
    | 62 -> Some CertNotValid
    | _ -> None

  let category status =
    match status with
    | Input _ -> `Input
    | Success -> `Success
    | RedirectPerm | RedirectTemp -> `Redirect
    | TempFailure | ServerUnavailable | CGIError | ProxyError | SlowDown ->
        `TempFailure
    | PermFailure | NotFound | Gone | ProxyRequestRefused | BadRequest ->
        `PermFailure
    | ClientCertRequired | CertNotAuth | CertNotValid -> `ClientCertRequired

  let is_input = function Input _ -> true | _ -> false
  let is_success = function Success -> true | _ -> false
  let is_redirect = function RedirectTemp | RedirectPerm -> true | _ -> false

  let is_temp_failure = function
    | TempFailure | ServerUnavailable | CGIError | ProxyError | SlowDown -> true
    | _ -> false

  let is_perm_failure = function
    | PermFailure | NotFound | Gone | ProxyRequestRefused | BadRequest -> true
    | _ -> false

  let is_client_cert_req = function
    | PermFailure | NotFound | Gone | ProxyRequestRefused | BadRequest -> true
    | _ -> false

  let is_failure s =
    is_temp_failure s || is_perm_failure s || is_client_cert_req s
end

module Header = struct
  type t = { status : Status.t; meta : string }

  let re =
    Re.compile
      Re.(seq [ group (seq [ digit; digit ]); char ' '; group (rep any) ])

  let parse head =
    match Re.exec_opt re head with
    | None -> Error `MalformedHeader
    | Some grp -> (
        let meta = Re.Group.get grp 2 in
        if Bytes.of_string meta |> Bytes.length > 1024 then Error `TooLongHeader
        else
          let code = Re.Group.get grp 1 |> int_of_string |> Status.from_int in
          match code with
          | None -> Error (`InvalidCode code)
          | Some status -> Ok { status; meta })
end
