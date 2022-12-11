open Lwt.Syntax

let ( >>= ) = Lwt.( >>= )

let resolve host service =
  let open Lwt_unix in
  let* tcp = getprotobyname "tcp" in
  getaddrinfo host service [ AI_PROTOCOL tcp.p_proto ] >>= function
  | [] ->
      let msg = Printf.sprintf "no address for %s:%s" host service in
      Lwt.fail (Invalid_argument msg)
  | ai :: _ -> (
      match ai.ai_addr with
      | ADDR_UNIX _ -> assert false
      | ADDR_INET (addr, _) -> Ipaddr_unix.of_inet_addr addr |> Lwt.return)

let request_of_url url =
  let port = Uri.port url |> Option.value ~default:1965 in
  let* host = resolve (Uri.host url |> Option.get) (Int.to_string port) in
  Razzia.make_request ~host ~port ~url |> Lwt.return

let () =
  Printexc.record_backtrace true;
  Printexc.print_backtrace stdout

let () =
  Mirage_crypto_rng_lwt.initialize ();
  Lwt_main.run
  @@ let* req =
       request_of_url
         (Uri.make ~host:"gemini.circumlunar.space"
            ~path:"/docs/specification.gmi" ())
     in
     let* header, body = Razzia_unix.fetch req in
     let* () = Lwt_io.(printl header) in
     Lwt_io.(printl body)
