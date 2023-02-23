(* {1 Gemtext} *)

module Gemtext = Gemtext
(** Implementation of the Gemini own native response format. *)

(** {1 Request} *)

type request

type request_err =
  [ `AboveMaxSize  (** URL is longer than 1024 characters. *)
  | `BeginWithBOM  (** URL begins with a [U+FEFF] byte order mark. *)
  | `EmptyURL  (** URL is empty. *)
  | `MalformedUTF8  (** URL contains malformed UTF-8. *)
  | `MissingHost  (** Host component of URL is not present. *)
  | `MissingScheme  (** Scheme component of URL is not present. *)
  | `UserInfoNotAllowed  (** User info component of URL is provided. *) ]

val make_request :
  ?default_scheme:string -> Uri.t -> (request, request_err) result
(** Creates a {type:request} from an URL. [query] is [query] component used *)

(** Assuming URL is "heyplzlookat.me:80/index.html?foobar". *)

val host : request -> string
(** [host req] is ["heyplzlookat.me"]. *)

val port : request -> int
(** [port req] is [80]. *)

val target : request -> Uri.t
(** [target req] is ["/index.html"]. *)

val query : request -> string option
(** [query req] is ["foobar"]. *)

val pp_request : Format.formatter -> request -> unit
val pp_request_err : Format.formatter -> request_err -> unit

(** {1 Response} *)

type 'stream response = 'stream Response.t =
  | Input of { sensitive : bool; prompt : string }
  | Success of 'stream body
  | Redirect of [ `Temp | `Perm ] * string
  | TempFailure of
      [ `Msg | `ServerUnavailable | `CGIError | `ProxyError | `SlowDown ]
      * string
  | PermFailure of
      [ `Msg | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ] * string
  | ClientCertReq of [ `Msg | `CertNotAuth | `CertNotValid ] * string

and 'stream body = 'stream Response.body = {
  encoding : string option;  (** Charset of the document. *)
  mime : mime;  (** MIME type of the body. *)
  body : 'stream;  (** Content of the body. *)
}

and mime = Mime.t =
  | Gemtext of { lang : string option }
      (** According {{:https://gemini.circumlunar.space/docs/specification.gmi}
    specification}:
    {[The value of "lang" denotes the natural language or language(s) in which the textual content of a "text/gemini" document is written.
    ]}
*)
  | MimeType of string

type response_err =
  [ `InvalidCode of int  (** Status code is unspecified. *)
  | `Malformed  (** Response header is unparsable. *)
  | `TooLong  (** Response header is longer than 1024 characters. *) ]

val status_code : 'a response -> int
(** Retrieve the status code of response header. *)

val pp_response : Format.formatter -> 'a response -> unit
val pp_response_err : Format.formatter -> response_err -> unit

(** {1 Error} *)

type err =
  [ `Header of response_err
  | `Host of
    [ `BadDomainName of string
    | `InvalidHostname of string
    | `UnknownHost of string ]
  | `NetErr ]

val pp_err : Format.formatter -> err -> unit

(** {1 NET} *)

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type NET = sig
  module IO : IO

  type stack
  type stream

  val single_read : stream -> string
  val get : stack -> request -> (stream response, err) result IO.t
end

module Private : sig
  (** You can ignore it. *)

  type header = Header.t

  val is_success : header -> bool

  val make_response :
    header:header -> body:'a -> ('a response, response_err) result

  module type CHANNEL = sig
    module IO : IO

    type src

    val next : src -> char option option IO.t
  end

  module type S = sig
    module IO : IO

    type src

    val parse :
      src -> ((header, response_err) result, [ `Eof | `NetErr ]) result IO.t
  end

  module MakeParser (Chan : CHANNEL) :
    S with module IO := Chan.IO and type src := Chan.src
end
