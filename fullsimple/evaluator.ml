open Syntax
exception NoEvaluationRuleApplies

let rec substitute j s = function
  | ITmVar(k) -> if k = j
    then s
    else ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, substitute (j + 1) (Syntax.shift 1 s) t)
  | ITmApp(t1, t2) -> ITmApp(substitute j s t1, substitute j s t2)
  | ITmUnit -> ITmUnit
  | ITmLet(t1, t2) -> ITmLet(substitute j s t1, substitute (j + 1) (Syntax.shift 1 s) t2)

let rec eval1 = function
  | ITmApp(ITmAbs(_, t), v) when (isval v) -> Syntax.shift (-1) (substitute 0 (Syntax.shift 1 v) t)
  | ITmApp(v1, t2) when (isval v1) -> let t2' = eval1 t2 in ITmApp(v1, t2')
  | ITmApp(t1, t2) -> let t1' = eval1 t1 in ITmApp(t1', t2)
  | ITmLet(v1, t2) when (isval v1) -> Syntax.shift (-1) (substitute 0 (Syntax.shift 1 v1) t2)
  | ITmLet(t1, t2) -> let t1' = eval1 t1 in ITmLet(t1', t2)
  | _ -> raise NoEvaluationRuleApplies

let rec eval t =
  let t'opt =
    try Some(eval1 t)
    with NoEvaluationRuleApplies -> None in
  match t'opt with
    | Some t' -> eval t'
    | None -> t

let rec tap_and_eval f g t =
  let _ = f t in
  let t'opt =
    try Some(eval1 t)
    with NoEvaluationRuleApplies -> None in
  match t'opt with
    | Some t' -> tap_and_eval f g t'
    | None -> g ()
