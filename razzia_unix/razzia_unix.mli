include Razzia.IO

val get :
  ?v4:string ->
  ?v6:string ->
  Razzia.request ->
  (Razzia.header * string, Razzia.fetch_err) Lwt_result.t
