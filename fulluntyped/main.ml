let buf = Lexing.from_channel stdin;;

let indexed_t = buf
  |> Parser.parse Lexer.lex
  |> Syntax.removenames []
  |> Evaluator.eval

let rec print_term = function
  | Syntax.TmVar(x) -> x
  | Syntax.TmAbs(x, t) -> "(λ" ^ x ^ ". " ^ (print_term t) ^ ")"
  | Syntax.TmApp(t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Syntax.TmTrue -> "true"
  | Syntax.TmFalse -> "false"
  | Syntax.TmIf(t1, t2, t3) -> "if " ^ (print_term t1) ^ " then " ^ (print_term t2) ^ " else " ^ (print_term t3)
  | Syntax.TmZero -> "0"
  | Syntax.TmSucc(t) -> "succ " ^ (print_term t)
  | Syntax.TmPred(t) -> "prev " ^ (print_term t)
  | Syntax.TmIsZero(t) -> "iszero " ^ (print_term t)

let _ = indexed_t
  |> Syntax.restorenames []
  |> print_term
  |> print_string
