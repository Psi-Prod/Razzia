type request = Request.t
let make_request = Request.make

module type IO = sig
  include Client_impl.S
end

module Mirage = struct
  module Make (Stack : Tcpip.Stack.V4V6) : IO with type stack = Stack.t = struct
    type stack = Stack.t

    include Client_impl.Make (Stack)
  end
end
