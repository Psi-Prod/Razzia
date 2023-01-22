type t = { encoding : string option; mime : mime }
and mime = Gemtext of { lang : string option } | MimeType of string

let re =
  Re.(
    compile
      (seq
         [
           group (rep1 (compl [ char ' '; char ';' ]));
           opt (seq [ space; char ';'; str "charset="; group (rep1 any) ]);
           opt (seq [ space; char ';'; str "lang="; group (rep1 any) ]);
         ]))

let default = { encoding = Some "utf-8"; mime = Gemtext { lang = None } }

let of_string = function
  | "" -> default
  | s -> (
      match Re.exec_opt re s with
      | None -> default
      | Some grp -> (
          match Re.Group.get grp 1 with
          | "text/gemini" ->
              {
                encoding = Re.Group.get_opt grp 2;
                mime = Gemtext { lang = Re.Group.get_opt grp 3 };
              }
          | mime -> { encoding = Re.Group.get_opt grp 2; mime = MimeType mime })
      )

let pp_mime fmt =
  let open Format in
  function
  | Gemtext { lang } ->
      fprintf fmt "Gemtext { lang = %a }"
        (pp_print_option
           ~none:(fun fmt () -> Format.fprintf fmt "None")
           pp_print_string)
        lang
  | MimeType mime -> fprintf fmt "MimeType %S" mime

let pp fmt { encoding; mime } =
  let open Format in
  fprintf fmt "{ encoding = %a; mime = %a }"
    (pp_print_option
       ~none:(fun fmt () -> Format.fprintf fmt "None")
       pp_print_string)
    encoding pp_mime mime
