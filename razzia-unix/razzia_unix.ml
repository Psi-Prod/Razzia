include
  Razzia_mirage.Make (Mirage_crypto_rng) (Time) (Mclock) (Pclock)
    (Tcpip_stack_socket.V4V6)

module Stack = Tcpip_stack_socket.V4V6
open Lwt.Syntax

type absurd = |

let get ?(v4 = Ipaddr.V4.Prefix.loopback) ?v6 req =
  Mirage_crypto_rng_unix.initialize ();
  let* tcp = Stack.TCP.connect ~ipv4_only:false ~ipv6_only:false v4 v6 in
  let* udp = Stack.UDP.connect ~ipv4_only:false ~ipv6_only:false v4 v6 in
  let* stack = Stack.connect udp tcp in
  get stack req
