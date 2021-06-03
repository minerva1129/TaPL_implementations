open Syntax
exception NoEvaluationRuleApplies

let shift d =
  let rec walk c = function
    | ITmVar(k) -> if k < c
      then ITmVar(k)
      else ITmVar(k + d)
    | ITmAbs(tyT, t) -> ITmAbs(tyT, walk (c + 1) t)
    | ITmApp(t1, t2) -> ITmApp(walk c t1, walk c t2)
    | ITmTrue -> ITmTrue
    | ITmFalse -> ITmFalse
    | ITmIf(t1, t2, t3) -> ITmIf(walk c t1, walk c t2, walk c t3)
  in walk 0

let rec substitute j s = function
  | ITmVar(k) -> if k = j
    then s
    else ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, substitute (j + 1) (shift 1 s) t)
  | ITmApp(t1, t2) -> ITmApp(substitute j s t1, substitute j s t2)
  | ITmTrue -> ITmTrue
  | ITmFalse -> ITmFalse
  | ITmIf(t1, t2, t3) -> ITmIf(substitute j s t1, substitute j s t2, substitute j s t3)

let rec eval1 = function
  | ITmApp(ITmAbs(_, t), v) when (isval v) -> shift (-1) (substitute 0 (shift 1 v) t)
  | ITmApp(v1, t2) when (isval v1) -> let t2' = eval1 t2 in ITmApp(v1, t2')
  | ITmApp(t1, t2) -> let t1' = eval1 t1 in ITmApp(t1', t2)
  | ITmIf(ITmTrue, t2, t3) -> t2
  | ITmIf(ITmFalse, t2, t3) -> t3
  | ITmIf(t1, t2, t3) -> let t1' = eval1 t1 in ITmIf(t1', t2, t3)
  | _ -> raise NoEvaluationRuleApplies

let rec eval t =
  let t'opt =
    try Some(eval1 t)
    with NoEvaluationRuleApplies -> None in
  match t'opt with
    | Some t' -> eval t'
    | None -> t

