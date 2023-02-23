type t = int * string
type parse_err = [ `InvalidCode of int | `Malformed | `TooLong ]

let is_success (s, _) = Int.equal 20 s

let is_valid s =
  List.mem s
    [ 10; 11; 20; 30; 31; 40; 41; 42; 43; 44; 50; 51; 52; 53; 59; 60; 61; 62 ]

let pp fmt (status, meta) =
  Format.fprintf fmt "{ status = %i; meta = %S }" status meta

let pp_err fmt = function
  | `InvalidCode c -> Format.fprintf fmt "InvalidCode %i" c
  | `Malformed -> Format.fprintf fmt "Malformed"
  | `TooLong -> Format.fprintf fmt "TooLong"

module type CHANNEL = sig
  module IO : Common.IO

  type src

  val next : src -> char option option IO.t
end

module type S = sig
  module IO : Common.IO

  type src

  val parse : src -> ((t, parse_err) result, [ `Eof | `NetErr ]) result IO.t
end

module Make (Chan : CHANNEL) :
  S with module IO := Chan.IO and type src := Chan.src = struct
  let ( let* ) = Chan.IO.bind

  open Chan.IO

  let ok ok = Ok ok |> return
  let err err = Error err |> return
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_space = function ' ' | '\t' -> true | _ -> false

  let parse src =
    let buf = Buffer.create 1024 in
    let rec loop len cr status =
      if len < 1024 then (
        let* char_or_eof = Chan.next src in
        match char_or_eof with
        | None -> err `NetErr
        | Some None -> err `Eof
        | Some (Some '\n') when cr -> Ok (status, Buffer.contents buf) |> ok
        | Some (Some '\r') -> loop (len + 1) true status
        | Some (Some c) when (len = 0 || len = 1) && not (is_digit c) ->
            Error `Malformed |> ok
        | Some (Some c) when len = 0 || len = 1 ->
            loop (len + 1) false (status ^ String.make 1 c)
        | Some (Some c) when len = 2 && is_space c ->
            loop (len + 1) false status
        | Some (Some _) when len = 2 -> Error `Malformed |> ok
        | Some (Some c) ->
            Buffer.add_char buf c;
            loop (len + 1) false status)
      else Error `TooLong |> ok
    in
    let* result = loop 0 false "" in
    match result with
    | Ok (Ok (s, meta)) ->
        let code = int_of_string s in
        ok
        @@ if is_valid code then Ok (code, meta) else Error (`InvalidCode code)
    | Ok (Error _ as err) -> ok err
    | Error e -> err e
end
