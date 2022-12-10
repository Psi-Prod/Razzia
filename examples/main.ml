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
  Lwt_main.run
  @@ let* req = request_of_url (Uri.make ~host:"gemini.circumlunar.space" ()) in
     let* resp = Razzia_unix.fetch req in
     Lwt_io.(printl resp)
