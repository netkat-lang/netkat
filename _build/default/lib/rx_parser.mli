
(* The type of tokens. *)

type token = 
  | X
  | STAR
  | RPAR
  | QMARK
  | PLUS
  | NUM of (int)
  | NEG
  | LPAR
  | EOF
  | E
  | DOT
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val rx_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Intrx.t)
