module type S = sig
  type stack

  include
    Razzia.NET
      with module IO := Lwt
       and type stack := stack
       and type stream = string
end

module Make : functor
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (Dns : Dns_client_mirage.S with type Transport.stack = Stack.t)
  -> S with type stack := Stack.t
