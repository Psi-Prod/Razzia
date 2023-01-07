open Lwt.Syntax

let ( >>= ) = Lwt.( >>= )

let main () =
  let* () =
    match Razzia.make_request "gemini://gemini.circumlunar.space/" with
    | Ok req -> (
        Razzia_unix.get req >>= function
        | Ok (header, body) ->
            Lwt_fmt.printf "%a%s" Razzia.pp_header header body
        | Error err -> Lwt_fmt.printf "Fetch error: %a" Razzia.pp_fetch_err err)
    | Error err -> Lwt_fmt.printf "Request error: %a" Razzia.pp_request_err err
  in
  let* () = Lwt_io.print "\n" in
  Lwt_fmt.flush Lwt_fmt.stdout

let () =
  Mirage_crypto_rng_lwt.initialize ();
  main () |> Lwt_main.run
