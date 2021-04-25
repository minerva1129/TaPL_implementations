type token =
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | ZERO
  | SUCC
  | PRED
  | ISZERO
  | LPAREN
  | RPAREN
  | EOF

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.term
