open Syntax;;

let input = Opal.LazyStream.of_channel stdin
let et = Lexer_and_parser.lex_and_parse input
let it = Compiler.remove_names [] [] et

let rec string_of_ty = function
  | ETyUnit -> "Unit"
  | ETyVar(tyX) -> tyX
  | ETyArrow(tyT1, tyT2) -> "(" ^ (string_of_ty tyT1) ^ " -> " ^ (string_of_ty tyT2) ^ ")"
  | ETyAll(tyX, tyT) -> "(∀" ^ tyX ^ ". " ^ (string_of_ty tyT) ^ ")"

let string_of_ty_opt tyT_opt = Option.value (Option.map string_of_ty tyT_opt) ~default: "Untyped"

let rec string_of_term = function
  | ETmUnit -> "unit"
  | ETmVar(x) -> x
  | ETmAbs(x, tyT, t) -> "(λ" ^ x ^ ": " ^ (string_of_ty tyT) ^ ". " ^ (string_of_term t) ^ ")"
  | ETmApp(t1, t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"
  | ETmTAbs(tyX, t) -> "(Λ" ^ tyX ^ ". " ^ (string_of_term t) ^ ")"
  | ETmTApp(t, tyT) -> "(" ^ (string_of_term t) ^ " [" ^ (string_of_ty tyT) ^ "])"

let print_term_and_ty it = print_endline @@ (it |> Compiler.restore_names [] [] |> string_of_term) ^ ": " ^ (it |> Typechecker.typeof_opt [] |> Option.map (Compiler.restore_type_names []) |> string_of_ty_opt)
let _ = Evaluator.tap_and_eval print_term_and_ty (fun () -> print_endline "Evaluation halted") it
