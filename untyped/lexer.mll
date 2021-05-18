{
open Parser
}

rule lex = parse
  | [' ' '\t' '\n'] { lex lexbuf }
  | '\\' { BACKSLASH }
  | '.' { DOT }
  | ['A'-'Z' 'a'-'z']+ { VAR (Lexing.lexeme lexbuf) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof {EOF}

{

}
