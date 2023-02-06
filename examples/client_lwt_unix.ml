open Lwt.Syntax

let ( >>= ) = Lwt.( >>= )

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let main () =
  let* () =
    match Razzia.make_request "heyplzlookat.me" with
    | Ok req -> (
        Razzia_unix.get req >>= function
        | Ok (Success { encoding = _; mime = Gemtext _; body }) ->
            Lwt_io.print body
        | Ok resp -> Lwt_fmt.printf "%a" Razzia.pp_response resp
        | Error err -> Lwt_fmt.printf "Fetch error: %a" Razzia.pp_err err)
    | Error err -> Lwt_fmt.printf "Request error: %a" Razzia.pp_request_err err
  in
  Lwt_fmt.flush Lwt_fmt.stdout

let () =
  Mirage_crypto_rng_lwt.initialize ();
  main () |> Lwt_main.run
