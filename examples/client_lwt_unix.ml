open Lwt.Syntax

let ( >>= ) = Lwt.( >>= )

let main () =
  let* () =
    match Razzia.make_request (Uri.of_string "gemini://heyplzlookat.me/") with
    | Ok req -> (
        Razzia_unix.get ~v6:Ipaddr.V6.Prefix.global_unicast_001 req >>= function
        | Ok (Success { mime = Gemtext _; body; _ }) -> Lwt_fmt.printf "%s" body
        | Ok (Success { body; _ }) -> Lwt_fmt.printf "%s" body
        | Ok resp -> Lwt_fmt.printf "%a" Razzia.pp_response resp
        | Error err -> Lwt_fmt.printf "Fetch error: %a" Razzia.pp_err err)
    | Error err -> Lwt_fmt.printf "Request error: %a" Razzia.pp_request_err err
  in
  Lwt_fmt.flush Lwt_fmt.stdout

let () =
  Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna);
  main () |> Lwt_main.run
