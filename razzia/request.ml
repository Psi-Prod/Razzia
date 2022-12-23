type t = Uri.t

let make u = u (* TODO *)
let host t = Uri.host t |> Option.get
let port t = Uri.port t |> Option.value ~default:1965
let to_string uri = Uri.to_string uri
