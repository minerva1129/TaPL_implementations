{
open Parser
}

rule lex = parse
  | [' ' '\t' '\n'] { lex lexbuf }
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
  | eof {EOF}

{

}
