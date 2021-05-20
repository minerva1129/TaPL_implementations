open Syntax
exception NoRuleApplies

let shift d =
  let rec walk c = function
    | ITmVar(k) -> if k < c
      then ITmVar(k)
      else ITmVar(k + d)
    | ITmAbs(t) -> ITmAbs(walk (c + 1) t)
    | ITmApp(t1, t2) -> ITmApp(walk c t1, walk c t2)
    | ITmTrue -> ITmTrue
    | ITmFalse -> ITmFalse
    | ITmIf(t1, t2, t3) -> ITmIf(walk c t1, walk c t2, walk c t3)
    | ITmZero -> ITmZero
    | ITmSucc(t) -> ITmSucc(walk c t)
    | ITmPred(t) -> ITmPred(walk c t)
    | ITmIsZero(t) -> ITmIsZero(walk c t)
  in walk 0

let rec substitute j s = function
  | ITmVar(k) -> if k = j
    then s
    else ITmVar(k)
  | ITmAbs(t) -> ITmAbs(substitute (j + 1) (shift 1 s) t)
  | ITmApp(t1, t2) -> ITmApp(substitute j s t1, substitute j s t2)
  | ITmTrue -> ITmTrue
  | ITmFalse -> ITmFalse
  | ITmIf(t1, t2, t3) -> ITmIf(substitute j s t1, substitute j s t2, substitute j s t3)
  | ITmZero -> ITmZero
  | ITmSucc(t) -> ITmSucc(substitute j s t)
  | ITmPred(t) -> ITmPred(substitute j s t)
  | ITmIsZero(t) -> ITmIsZero(substitute j s t)

let rec eval1 = function
  | ITmApp(ITmAbs(t), v) when (isval v) -> shift (-1) (substitute 0 (shift 1 v) t)
  | ITmApp(v1, t2) when (isval v1) -> let t2' = eval1 t2 in ITmApp(v1, t2')
  | ITmApp(t1, t2) -> let t1' = eval1 t1 in ITmApp(t1', t2)
  | ITmIf(ITmTrue, t2, t3) -> t2
  | ITmIf(ITmFalse, t2, t3) -> t3
  | ITmIf(t1, t2, t3) -> let t1' = eval1 t1 in ITmIf(t1', t2, t3)
  | ITmSucc(t) -> let t' = eval1 t in ITmSucc(t')
  | ITmPred(ITmZero) -> ITmZero
  | ITmPred(ITmSucc(nv)) when (isnumericval nv) -> nv
  | ITmPred(t) -> let t' = eval1 t in ITmPred(t')
  | ITmIsZero(ITmZero) -> ITmTrue
  | ITmIsZero(ITmSucc(nv)) when (isnumericval nv) -> ITmFalse
  | ITmIsZero(t) -> let t' = eval1 t in ITmIsZero(t')
  | _ -> raise NoRuleApplies

let rec eval t =
  let t'opt =
    try Some(eval1 t)
    with NoRuleApplies -> None in
  match t'opt with
    | Some t' -> (fun x -> x) (eval t')
    | None -> t

