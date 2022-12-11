type request

val make_request : host:Ipaddr.t -> port:int -> url:Uri.t -> request

module type IO = sig
  type stack

  val fetch : request -> stack -> (string * string) Lwt.t
end

module Mirage : sig
  module Make : functor
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    -> IO with type stack = Stack.t
end
