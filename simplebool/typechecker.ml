open Syntax
exception NoTypeRuleApplies

let rec index_of context x =
  match context with
    | [] -> None
    | h::t -> if h = x
      then Some 0
      else Option.map succ (index_of t x)

let rec typeof context = function
  | ITmVar(k) -> (
    match List.nth_opt context k with
      | None -> raise Syntax.NoSuchVariable
      | Some(tyT) -> tyT
  )
  | ITmAbs(tyT, t) -> TyArrow(tyT, typeof (tyT::context) t)
  | ITmApp(t1, t2) -> (
    match typeof context t1 with
      | TyArrow(tyT1, tyT2) when typeof context t2 = tyT1 -> tyT2
      | _ -> raise NoTypeRuleApplies
  )
  | ITmTrue -> TyBool
  | ITmFalse -> TyBool
  | ITmIf(t1, t2, t3) when typeof context t1 = TyBool -> (
      let tyT2 = typeof context t2 in
      if tyT2 = typeof context t3
      then tyT2
      else raise NoTypeRuleApplies
  )
  | _ -> raise NoTypeRuleApplies

let rec typeof_opt context t =
  try Some (typeof context t)
  with NoTypeRuleApplies -> None
