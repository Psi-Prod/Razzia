module type IO = sig
  type 'a t

  val return : 'a -> 'a t
end
