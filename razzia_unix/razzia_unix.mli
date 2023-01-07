include Razzia.NET

val get :
  ?v4:Ipaddr.V4.Prefix.t ->
  ?v6:Ipaddr.V6.Prefix.t ->
  Razzia.request ->
  (Razzia.header * string, Razzia.fetch_err) Lwt_result.t
