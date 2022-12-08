module type IO = sig
  type stack

  val fetch : Request.request -> stack -> string Lwt.t
end

module Mirage : sig
  module Make : functor (Stack : Tcpip.Stack.V4V6) ->
    IO with type stack = Stack.t
end
