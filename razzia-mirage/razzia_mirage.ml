module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6) :
  Razzia.NET with module IO = Lwt and type stack = Stack.t = struct
  module IO = Lwt

  type stack = Stack.t

  include Client_impl.Make (Random) (Time) (Mclock) (Pclock) (Stack)
end
