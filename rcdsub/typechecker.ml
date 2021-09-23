open Utils
open Syntax
exception NoTypeRuleApplies

let rec subtype tyS tyT =
  tyS = tyT ||
  match (tyS, tyT) with
    | (_, TyTop) -> true
    | (TyArrow(tyS1, tyS2), TyArrow(tyT1, tyT2)) -> (subtype tyT1 tyS1) && (subtype tyS2 tyT2)
    | (TyRecord(fS), TyRecord(fT)) ->
        let st (li, tyTi) = (
          match List.assoc_opt li fS with
            | Some(tySi) -> subtype tySi tyTi
            | None -> false
        ) in
        List.for_all st fT
    | (_, _) -> false

let rec typeof context = function
  | ITmRecord(ls) -> TyRecord(List.map (fun (k, t) -> (k, typeof context t)) ls)
  | ITmProj(t, k) -> (
    match typeof context t with
      | TyRecord(ls) -> (
        match List.assoc_opt k ls with
          | Some(tyT) -> tyT
          | None -> raise NoSuchField
      )
      | _ -> raise NoTypeRuleApplies
  )
  | ITmVar(k) -> (
    match List.nth_opt context k with
      | None -> raise NoSuchVariable
      | Some(tyT) -> tyT
  )
  | ITmAbs(tyT, t) -> TyArrow(tyT, typeof (tyT::context) t)
  | ITmApp(t1, t2) ->
    let tyT1 = typeof context t1 in
    let tyT2 = typeof context t2 in
    (match tyT1 with
        | TyArrow(tyT11, tyT12) -> if subtype tyT2 tyT11 then tyT12 else raise NoTypeRuleApplies
        | _ -> raise NoTypeRuleApplies
    )
  | ITmUnit -> TyUnit

let typeof_opt context t =
  try Some (typeof context t)
  with NoTypeRuleApplies -> None
