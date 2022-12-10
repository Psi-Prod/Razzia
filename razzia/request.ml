type t = { host : Ipaddr.t; port : int; url : Uri.t }

let make ~host ~port ~url = { host; port; url }
let to_string { url; _ } = Uri.to_string url ^ "\r\n"
