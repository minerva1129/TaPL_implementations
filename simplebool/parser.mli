
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TRUE
  | THEN
  | RPAREN
  | LPAREN
  | IF
  | FALSE
  | EOF
  | ELSE
  | DOT
  | COLON
  | BOOL
  | BACKSLASH
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.term)
