module type S = sig
  type stack

  val fetch : Request.t -> stack -> (string * string) Lwt.t
end

module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6) : S with type stack := Stack.t = struct
  module TLS = Tls_mirage.Make (Stack.TCP)
  module DNS = Dns_client_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  open Lwt.Infix
  (* open Lwt.Syntax *)

  let write flow req =
    Request.to_string req |> Cstruct.of_string |> TLS.write flow >>= function
    | Ok () -> Lwt.return_unit
    | Error _ -> failwith "writing"

  module D = Dns_client.Make (DNS.Transport)

  let fetch req stack =
    (* let transport = D.create stack ~timeout:(Duration.of_sec 5) in
       let raw = Domain_name.of_string_exn "gemini.circumlunar.space" in
       let host = Domain_name.host_exn raw in
       let* host_ip = DNS.gethostbyname transport host in *)

    (* Stack.TCP.create_connection (Stack.tcp stack) (host_ip, req.port) *)
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
            | Ok (`Data buf) -> (
                let header = Cstruct.to_string buf in
                TLS.read flow >>= function
                | Ok (`Data buf) -> Lwt.return (header, Cstruct.to_string buf)
                | Ok `Eof -> failwith "eof"
                | Error _ -> failwith "err")
            | Ok `Eof -> failwith "eof"
            | Error _ -> failwith "reading")
        | Error _err -> failwith "OCaml => chier")
    | Error err -> failwith (Format.asprintf "%a" Stack.TCP.pp_error err)
end
