let buf = Lexing.from_channel stdin;;
let t = Parser.parse Lexer.lex buf;;



