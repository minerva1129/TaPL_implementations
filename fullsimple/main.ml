exception ParsingError

let input = Opal.LazyStream.of_channel stdin

let et = match Opal.parse Parser.term input with
  | Some result -> result
  | None -> raise ParsingError

let it = Syntax.removenames [] et

let rec string_of_ty = function
  | Syntax.TyUnit -> "Unit"
  | Syntax.TyArrow(tyT1, tyT2) -> "(" ^ (string_of_ty tyT1) ^ " -> " ^ (string_of_ty tyT2) ^ ")"

let string_of_ty_opt tyT_opt = Option.value (Option.map string_of_ty tyT_opt) ~default: "Untyped"

let rec string_of_term = function
  | Syntax.ETmVar(x) -> x
  | Syntax.ETmAbs(x, tyT, t) -> "(λ" ^ x ^ ": " ^ (string_of_ty tyT) ^ ". " ^ (string_of_term t) ^ ")"
  | Syntax.ETmApp(t1, t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"
  | Syntax.ETmUnit -> "unit"
  | Syntax.ETmSeq(t1, t2) -> "(" ^ (string_of_term t1) ^ ")" ^ "; " ^ "(" ^ (string_of_term t2) ^ ")"
  | Syntax.ETmWildcard(tyT, t) -> "(λ_: " ^ (string_of_ty tyT) ^ ". " ^ (string_of_term t) ^ ")"
  | Syntax.ETmAscribe(t, tyT) -> "(" ^ (string_of_term t) ^ ") as (" ^ (string_of_ty tyT) ^ ")"

let print_term_and_ty it = print_endline @@ (it |> Syntax.restorenames [] |> string_of_term) ^ ": " ^ (it |> Typechecker.typeof_opt [] |> string_of_ty_opt)
let _ = Evaluator.tap_and_eval print_term_and_ty (fun () -> print_endline "Evaluation halted") it
