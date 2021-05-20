{
open Parser
}

rule lex = parse
  | [' ' '\t' '\n'] { lex lexbuf }
  | '\\' { BACKSLASH }
  | '.' { DOT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | '0' { ZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | "iszero" { ISZERO }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ['A'-'Z' 'a'-'z']+ { VAR (Lexing.lexeme lexbuf) }
  | eof {EOF}

{

}
