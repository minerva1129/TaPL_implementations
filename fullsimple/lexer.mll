{
open Parser
}

rule lex = parse
  | [' ' '\t' '\n'] { lex lexbuf }
  | '\\' { BACKSLASH }
  | ['a'-'z'] ['A'-'Z'] * { VAR (Lexing.lexeme lexbuf) }
  | ':' { COLON }
  | "Unit" { TYUNIT }
  | "->" { ARROW }
  | '.' { DOT }
  | "unit" { TMUNIT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }

{

}
