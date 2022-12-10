include Razzia.Mirage.Make (Tcpip_stack_socket.V4V6)
module Stack = Tcpip_stack_socket.V4V6
open Lwt.Syntax

let stack ~v4 ~v6 =
  let* tcp =
    Stack.TCP.connect
      ~ipv4_only:(match v6 with None -> true | _ -> false)
      ~ipv6_only:false v4 v6
  in
  let* udp =
    Stack.UDP.connect
      ~ipv4_only:(match v6 with None -> true | _ -> false)
      ~ipv6_only:false v4 v6
  in
  Stack.connect udp tcp

let fetch ?v4 ?v6 req =
  let* stack =
    let v4 =
      match v4 with
      | Some ip -> Ipaddr.V4.Prefix.of_string_exn ip
      | None -> Ipaddr.V4.Prefix.loopback
    in
    stack ~v4 ~v6:(Option.map Ipaddr.V6.Prefix.of_string_exn v6)
  in
  fetch req stack
