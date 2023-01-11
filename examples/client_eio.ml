let main net =
  (match Razzia.make_request "heyplzlookat.me/" with
  | Ok req -> (
      match Razzia_eio.get net req with
      | Ok (Sucess (Gemtext { body; _ } | Other { body; _ })) ->
          Razzia_eio.single_read body |> print_string
      | Ok resp -> Format.printf "%a" Razzia.pp_response resp
      | Error err -> Format.printf "Fetch error: %a" Razzia.pp_err err)
  | Error err -> Format.printf "Request error: %a" Razzia.pp_request_err err);
  print_newline ();
  flush stdout

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main env#net
