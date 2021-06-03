let buf = Lexing.from_channel stdin;;
let t = Evaluator.eval (Parser.parse Lexer.lex buf);;
let tyT = Typechecker.typeof_opt t;;

let rec string_of_term = function
  | Syntax.TmTrue -> "true"
  | Syntax.TmFalse -> "false"
  | Syntax.TmIf(t1, t2, t3) -> "if " ^ (string_of_term t1) ^ " then " ^ (string_of_term t2) ^ " else " ^ (string_of_term t3)
  | Syntax.TmZero -> "0"
  | Syntax.TmSucc(t) -> "succ " ^ (string_of_term t)
  | Syntax.TmPred(t) -> "prev " ^ (string_of_term t)
  | Syntax.TmIsZero(t) -> "iszero " ^ (string_of_term t)

let rec string_of_ty = function
  | Some(Syntax.TyBool) -> "Bool"
  | Some(Syntax.TyNat) -> "Nat"
  | None -> "Untyped"

let _ = print_string @@ (string_of_term t) ^ ": " ^ (string_of_ty tyT)
