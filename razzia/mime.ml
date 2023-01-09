type t = string option * t'
and t' = Gemini of { lang : string option } | MimeType of string

let re =
  Re.(
    compile
      (seq
         [
           group (rep1 (compl [ char ' ' ]));
           opt (seq [ space; char ';'; str "charset="; group (rep1 any) ]);
           opt (seq [ space; char ';'; str "lang="; group (rep1 any) ]);
         ]))

let default = (Some "utf-8", Gemini { lang = None })

let of_string = function
  | "" -> default
  | s -> (
      match Re.exec_opt re s with
      | None -> default
      | Some grp -> (
          match Re.Group.get grp 1 with
          | "text/gemini" ->
              (Re.Group.get_opt grp 2, Gemini { lang = Re.Group.get_opt grp 3 })
          | mime -> (Re.Group.get_opt grp 2, MimeType mime)))
