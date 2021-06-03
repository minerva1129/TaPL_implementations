let buf = Lexing.from_channel stdin

let internal_t = buf
  |> Parser.parse Lexer.lex
  |> Syntax.removenames []
  |> Evaluator.eval

let tyT = Typechecker.typeof_opt [] internal_t

let rec string_of_ty = function
  | Syntax.TyBool -> "Bool"
  | Syntax.TyArrow(tyT1, tyT2) -> "(" ^ (string_of_ty tyT1) ^ " -> " ^ (string_of_ty tyT2) ^ ")"

let rec string_of_ty_opt = function
  | Some(tyT) -> string_of_ty tyT
  | None -> "Untyped"

let rec string_of_term = function
  | Syntax.TmVar(x) -> x
  | Syntax.TmAbs(x, tyT, t) -> "(λ" ^ x ^ ": " ^ (string_of_ty tyT) ^ ". " ^ (string_of_term t) ^ ")"
  | Syntax.TmApp(t1, t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"
  | Syntax.TmTrue -> "true"
  | Syntax.TmFalse -> "false"
  | Syntax.TmIf(t1, t2, t3) -> "if " ^ (string_of_term t1) ^ " then " ^ (string_of_term t2) ^ " else " ^ (string_of_term t3)

let _ = print_string @@ (internal_t |> Syntax.restorenames [] |> string_of_term) ^ ": " ^ (string_of_ty_opt tyT)
