type request = Request.t

let make_request = Request.make

module type IO = sig
  include Client_impl.S
end

module Mirage = struct
  module Make
      (Random : Mirage_random.S)
      (Time : Mirage_time.S)
      (Mclock : Mirage_clock.MCLOCK)
      (Pclock : Mirage_clock.PCLOCK)
      (Stack : Tcpip.Stack.V4V6) : IO with type stack = Stack.t = struct
    type stack = Stack.t

    include Client_impl.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  end
end
