let buf = Lexing.from_channel stdin;;

let indexed_t = buf
  |> Parser.parse Lexer.lex
  |> Syntax.removenames []
  |> Evaluator.bigeval

let rec print_term = function
  | Syntax.TmVar(x) -> x
  | Syntax.TmAbs(x, t) -> "(λ" ^ x ^ ". " ^ (print_term t) ^ ")"
  | Syntax.TmApp(t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"

let _ = indexed_t
  |> Syntax.restorenames []
  |> print_term
  |> print_string
