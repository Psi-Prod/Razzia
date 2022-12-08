module type S = sig
  type stack

  val fetch : Request.request -> stack -> string Lwt.t
end

module Make (Stack : Tcpip.Stack.V4V6) : S with type stack := Stack.t = struct
  module TLS = Tls_mirage.Make (Stack.TCP)
  open Lwt.Infix

  let fetch { Request.host; port; _ } stack =
    Stack.TCP.create_connection (Stack.tcp stack) (host, port) >>= function
    | Ok flow -> (
        TLS.client_of_flow
          (Tls.Config.client ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None) ())
          flow
        >>= function
        | Ok _ -> failwith "mfsdjkfglkmsj"
        | Error _ -> failwith "OCaml => chier")
    | Error _ -> failwith "fuck"
end
