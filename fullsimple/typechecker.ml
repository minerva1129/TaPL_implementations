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
  | ITmLet(t1, t2) -> let tyT1 = typeof context t1 in typeof (tyT1::context) t2
  | ITmUnit -> TyUnit
  | ITmRecord(ls) -> TyRecord(List.map (fun (k, t) -> (k, typeof context t)) ls)
  | ITmProj(t, k) -> (
    match typeof context t with
      | TyRecord(ls) -> (
        match List.assoc_opt k ls with
          | Some(tyT) -> tyT
          | None -> raise Syntax.NoSuchField
      )
      | _ -> raise NoTypeRuleApplies
  )

let typeof_opt context t =
  try Some (typeof context t)
  with NoTypeRuleApplies -> None
