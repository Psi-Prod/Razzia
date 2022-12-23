open Lwt.Syntax

let ( >>= ) = Lwt.( >>= )

let main () =
  let req =
    Uri.of_string "gemini://gemini.circumlunar.space/"
    |> Razzia.make_request
  in
  let* () =
    Razzia_unix.get req >>= function
    | Ok (header, body) -> Lwt_fmt.printf "%a\n%s" Razzia.pp_header header body
    | Error err -> Lwt_fmt.printf "%a\n" Razzia.pp_fetch_err err
  in
  Lwt_fmt.flush Lwt_fmt.stdout

let () =
  Mirage_crypto_rng_lwt.initialize ();
  main () |> Lwt_main.run
