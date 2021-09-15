exception ParsingError

let input = Opal.LazyStream.of_channel stdin

let t = match Opal.parse Parser.term input with
  | Some result -> result
  | None -> raise ParsingError

let internal_t = t
  |> Syntax.removenames []
  |> Evaluator.eval

let tyT = Typechecker.typeof_opt [] internal_t

let rec string_of_ty = function
  | Syntax.TyUnit -> "Unit"
  | Syntax.TyArrow(tyT1, tyT2) -> "(" ^ (string_of_ty tyT1) ^ " -> " ^ (string_of_ty tyT2) ^ ")"

let rec string_of_ty_opt = function
  | Some(tyT) -> string_of_ty tyT
  | None -> "Untyped"

let rec string_of_term = function
  | Syntax.TmVar(x) -> x
  | Syntax.TmAbs(x, tyT, t) -> "(λ" ^ x ^ ": " ^ (string_of_ty tyT) ^ ". " ^ (string_of_term t) ^ ")"
  | Syntax.TmApp(t1, t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"
  | Syntax.TmUnit -> "unit"
  | Syntax.TmSeq(t1, t2) -> "(" ^ (string_of_term t1) ^ ")" ^ "; " ^ "(" ^ (string_of_term t2) ^ ")"
  | Syntax.TmWildcard(tyT, t) -> "(λ_: " ^ (string_of_ty tyT) ^ ". " ^ (string_of_term t) ^ ")"
  | Syntax.TmAscribe(t, tyT) -> "(" ^ (string_of_term t) ^ ") as (" ^ (string_of_ty tyT) ^ ")"

let _ = print_string @@ (internal_t |> Syntax.restorenames [] |> string_of_term) ^ ": " ^ (string_of_ty_opt tyT)
