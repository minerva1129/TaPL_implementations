open Syntax
exception NoRuleApplies

let rec eval1 = function
  | TmIf(TmTrue, t2, t3) -> t2
  | TmIf(TmFalse, t2, t3) -> t3
  | TmIf(t1, t2, t3) -> let t1' = eval1 t1 in TmIf(t1', t2, t3)
  | TmSucc(t1) -> let t1' = eval1 t1 in TmSucc(t1')
  | TmPred(TmZero) -> TmZero
  | TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1
  | TmPred(t1) -> let t1' = eval1 t1 in TmPred(t1')
  | TmIsZero(TmZero) -> TmTrue
  | TmIsZero(TmSucc(nv1)) when (isnumericval nv1) -> TmFalse
  | TmIsZero(t1) -> let t1' = eval1 t1 in TmIsZero(t1')
  | _ -> raise NoRuleApplies

let rec eval t =
  let t'opt =
    try Some (eval1 t)
    with NoRuleApplies -> None in
  match t'opt with
    | Some t' -> eval t'
    | None -> t

let rec bigeval t =
  if isval t
  then t
  else match t with
    | TmIf(t1, t2, t3) when TmTrue = bigeval t1 && isval (bigeval t2) -> bigeval t2
    | TmIf(t1, t2, t3) when TmFalse = bigeval t1 && isval (bigeval t3) -> bigeval t3
    | TmSucc(t1) when isnumericval (bigeval t1) -> TmSucc(bigeval t1)
    | TmPred(t1) -> (
      let t1' = bigeval t1 in
      match t1' with
        | TmZero -> TmZero
        | TmSucc(nv1) when isnumericval nv1 -> nv1
        | _ -> raise NoRuleApplies
    )
    | TmIsZero(t1) -> (
      let t1' = bigeval t1 in
      match t1' with
        | TmZero -> TmTrue
        | TmSucc(nv1) when isnumericval nv1 -> TmFalse
        | _ -> raise NoRuleApplies
    )
    | _ -> raise NoRuleApplies
