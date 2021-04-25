let buf = Lexing.from_channel stdin;;
let t = Evaluator.eval (Parser.parse Lexer.lex buf);;

if Syntax.isval t
then if Syntax.isnumericval t
  then print_string "numeric"
  else print_string "boolean"
else print_string "stuck"
