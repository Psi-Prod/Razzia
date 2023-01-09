module Direct : sig
  type 'a t = 'a
end

type stream = string

include
  Razzia.NET
    with module IO := Direct
     and type stack := Eio.Net.t
     and type stream := stream
