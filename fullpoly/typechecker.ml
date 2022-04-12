open Utils
open Syntax
exception NoTypeRuleApplies

let rec typeof context = function
  | ITmUnit -> ITyUnit
  | ITmVar(k) -> (
    match List.nth_opt context k with
    | None -> raise NoSuchVariable
    | Some(tyT) -> tyT
    )
  | ITmAbs(tyT, t) -> ITyArrow(tyT, typeof (tyT::context) t)
  | ITmApp(t1, t2) -> (
    match typeof context t1 with
    | ITyArrow(tyT1, tyT2) when typeof context t2 = tyT1 -> tyT2
    | _ -> raise NoTypeRuleApplies
    )
  | ITmTAbs(t) -> ITyAll(typeof (List.map (shift_ty 1 0) context) t)
  | ITmTApp(t, tyT) -> (
    match typeof context t with
    | ITyAll(tyT1) -> shift_ty (-1) 0 (substitute_ty 0 (shift_ty 1 0 tyT) tyT1)
    | _ -> raise NoTypeRuleApplies
    )

let typeof_opt context t =
  try Some (typeof context t)
  with NoTypeRuleApplies -> None
