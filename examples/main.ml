let ( >>= ) = Lwt.( >>= )

let () =
  Mirage_crypto_rng_lwt.initialize ();
  Lwt_main.run
  @@
  let req =
    Uri.of_string "gemini://gemini.circumlunar.space/docs/specification.gmi"
    |> Razzia.make_request
  in
  Razzia_unix.fetch req >>= function
  | Ok header -> Lwt_io.(printl header)
  | Error _ -> Lwt_io.printl "Error"
