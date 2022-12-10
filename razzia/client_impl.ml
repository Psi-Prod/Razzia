module type S = sig
  type stack

  val fetch : Request.t -> stack -> string Lwt.t
end

module Make (Stack : Tcpip.Stack.V4V6) : S with type stack := Stack.t = struct
  module TLS = Tls_mirage.Make (Stack.TCP)
  open Lwt.Infix

  let write flow req =
    Request.to_string req |> Cstruct.of_string |> TLS.write flow >>= function
    | Ok () -> Lwt.return_unit
    | Error _ -> failwith "writing"

  let fetch req stack =
    Stack.TCP.create_connection (Stack.tcp stack) (req.Request.host, req.port)
    >>= function
    | Ok flow -> (
        TLS.client_of_flow
          (Tls.Config.client ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None) ())
          flow
        >>= function
        | Ok flow -> (
            write flow req >>= fun () ->
            TLS.read flow >>= function
            | Ok (`Data buffer) -> Cstruct.to_string buffer |> Lwt.return
            | Ok `Eof -> failwith "eof"
            | Error _ -> failwith "reading")
        | Error _err -> failwith "OCaml => chier")
    | Error err -> failwith (Format.asprintf "%a" Stack.TCP.pp_error err)
end
