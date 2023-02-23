module Direct : sig
  type 'a t = 'a

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

include
  Razzia.NET
    with module IO := Direct
     and type stack := Eio.Net.t
     and type stream := string
