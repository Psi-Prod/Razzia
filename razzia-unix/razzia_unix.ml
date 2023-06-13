module Stack = Tcpip_stack_socket.V4V6

module Dns =
  Dns_client_mirage.Make (Mirage_crypto_rng) (Time) (Mclock) (Pclock) (Stack)

include Razzia_mirage.Make (Pclock) (Stack) (Dns.Transport)
open Lwt.Syntax

let get ?(v4 = Ipaddr.V4.Prefix.global) ?v6 req =
  let* tcp = Stack.TCP.connect ~ipv4_only:false ~ipv6_only:false v4 v6 in
  let* udp = Stack.UDP.connect ~ipv4_only:false ~ipv6_only:false v4 v6 in
  let* stack = Stack.connect udp tcp in
  get stack req
