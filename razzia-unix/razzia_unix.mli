include Razzia.NET with module IO = Lwt

val get :
  ?v4:Ipaddr.V4.Prefix.t ->
  ?v6:Ipaddr.V6.Prefix.t ->
  Razzia.request ->
  (Razzia.response, Razzia.err) result Lwt.t
