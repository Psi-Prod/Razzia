include Razzia_mirage.S with type stack := unit

val get :
  ?v4:Ipaddr.V4.Prefix.t ->
  ?v6:Ipaddr.V6.Prefix.t ->
  Razzia.request ->
  (stream Razzia.response, Razzia.err) result Lwt.t
