module Direct : sig
  type 'a t = 'a
end

include Razzia.NET with module IO := Direct and type stack := Eio.Net.t
