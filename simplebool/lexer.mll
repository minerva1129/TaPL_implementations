{
open Parser
}

rule lex = parse
  | [' ' '\t' '\n'] { lex lexbuf }
  | '\\' { BACKSLASH }
  | ['a'-'z'] ['A'-'Z'] * { VAR (Lexing.lexeme lexbuf) }
  | ':' { COLON }
  | "Bool" { BOOL }
  | "->" { ARROW }
  | '.' { DOT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof {EOF}

{

}
