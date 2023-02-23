module type S = sig
  type stack

  include
    Razzia.NET
      with module IO := Lwt
       and type stack := stack
       and type stream = string
end

module Make : functor
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  -> S with type stack := Stack.t
