include Razzia.IO

val fetch :
  ?v4:string -> ?v6:string -> Razzia.request -> (string * string) Lwt.t
