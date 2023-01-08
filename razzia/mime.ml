type t = string

type t' =
  | Gemini of { encoding : or_utf8 option; lang : string option }
  | Text of { encoding : or_utf8 option }
  | Other of string * or_utf8 option

and or_utf8 = [ `UTF8 | `Other of string ]

let re =
  Re.(
    compile
      (seq
         [
           group (rep1 (compl [ char ' ' ]));
           opt (seq [ space; char ';'; str "charset="; group (rep1 any) ]);
           opt (seq [ space; char ';'; str "lang="; group (rep1 any) ]);
         ]))

let encoding = function "UTF-8" | "utf-8" -> `UTF8 | e -> `Other e

(* let from_string = function
   | "" -> Gemini { encoding = Some `UTF8; lang = None }
   | s -> (
       match Re.exec_opt re s with
       | None -> Text { encoding = None }
       | Some grp when String.starts_with ~prefix:"text/" s ->
           Text { encoding = Re.Group.get_opt grp 2 |> Option.map encoding }
       | Some grp ->
           Other
             (Re.Group.get grp 1, Re.Group.get_opt grp 2 |> Option.map encoding)) *)
let from_string s = s