open Syntax
exception NoTypeRuleApplies

let rec typeof = function
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmIf(t1, t2, t3) when typeof t1 = TyBool ->
      let tyT2 = typeof t2 in
      if tyT2 = typeof t3
      then tyT2
      else raise NoTypeRuleApplies
  | TmZero -> TyNat
  | TmSucc(t1) when typeof t1 = TyNat -> TyNat
  | TmPred(t1) when typeof t1 = TyNat -> TyNat
  | TmIsZero(t1) when typeof t1 = TyNat -> TyBool
  | _ -> raise NoTypeRuleApplies

let rec typeof_opt t =
  try Some (typeof t)
  with NoTypeRuleApplies -> None
