include Razzia.IO

val fetch :
  ?v4:string ->
  ?v6:string ->
  Razzia.request ->
  (string, Razzia.fetch_err) Lwt_result.t
