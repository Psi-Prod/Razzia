type t = { status : int; meta : string }
type parse_err = [ `Malformed | `TooLong ]

let re =
  Re.compile Re.(seq [ group (seq [ digit; digit ]); space; group (rep any) ])

let parse head =
  match Re.exec_opt re head with
  | None -> Error `Malformed
  | Some grp ->
      let meta = Re.Group.get grp 2 in
      if Bytes.of_string meta |> Bytes.length > 1024 then Error `TooLong
      else
        let status = Re.Group.get grp 1 |> int_of_string in
        Ok { status; meta }

let pp fmt { status; meta } =
  Format.fprintf fmt "{ status = %i; meta = %S }" status meta

let pp_err fmt = function
  | `Malformed -> Format.fprintf fmt "Malformed"
  | `TooLong -> Format.fprintf fmt "TooLong"
