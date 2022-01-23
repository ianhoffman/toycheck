(** The various tokens which make up our toylang. *)
type token

(** Get the token as a string. Useful for debugging. *)
val show_token : token -> string

(** Product a list of tokens from an input string. *)
val scan : string -> token list
