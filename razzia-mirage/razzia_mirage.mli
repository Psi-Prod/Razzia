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
  (Dns : Dns_client.S with type stack = Stack.t and type 'a io = 'a Lwt.t)
  -> S with type stack := Stack.t
