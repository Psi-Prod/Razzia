type t = { status : Status.t; meta : string }
type parse_err = [ `InvalidCode | `Malformed | `TooLong ]

let re =
  Re.compile
    Re.(seq [ group (seq [ digit; digit ]); char ' '; group (rep any) ])

let parse head : (t, parse_err) result =
  match Re.exec_opt re head with
  | None -> Error `Malformed
  | Some grp -> (
      let meta = Re.Group.get grp 2 in
      if Bytes.of_string meta |> Bytes.length > 1024 then Error `TooLong
      else
        match Re.Group.get grp 1 |> int_of_string |> Status.from_int meta with
        | None -> Error `InvalidCode
        | Some status -> Ok { status; meta })

let pp fmt { status; meta } = Format.fprintf fmt "%a %s" Status.pp status meta

let pp_err fmt =
  let fmt = Format.fprintf fmt in
  function
  | `InvalidCode -> fmt "InvalidCode"
  | `Malformed -> fmt "Malformed"
  | `TooLong -> fmt "TooLong"
