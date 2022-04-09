open Syntax
exception NoEvaluationRuleApplies

let rec substitute j s = function
  | ITmVar(k) -> if k = j
    then s
    else ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, substitute (j + 1) (shift 1 s) t)
  | ITmLet(t1, t2) -> ITmLet(substitute j s t1, substitute (j + 1) (shift 1 s) t2)
  | t -> traverse_iterm (substitute j s) t

let rec eval_key_term eval_func = function
  | [] -> raise NoEvaluationRuleApplies
  | (k, t)::rest -> if (isval t)
    then (k, t)::(eval_key_term eval_func rest)
    else (k, eval_func t)::rest

let rec eval1 = function
  | ITmApp(ITmAbs(_, t), v) when (isval v) -> shift (-1) (substitute 0 (shift 1 v) t)
  | ITmApp(v1, t2) when (isval v1) -> let t2' = eval1 t2 in ITmApp(v1, t2')
  | ITmApp(t1, t2) -> let t1' = eval1 t1 in ITmApp(t1', t2)
  | ITmLet(v1, t2) when (isval v1) -> shift (-1) (substitute 0 (shift 1 v1) t2)
  | ITmLet(t1, t2) -> let t1' = eval1 t1 in ITmLet(t1', t2)
  | ITmProj(ITmRecord(ls) as v, k) when (isval v) -> (
    match List.assoc_opt k ls with
      | Some(t) -> t
      | None -> raise NoSuchField
  )
  | ITmProj(t, k) -> let t' = eval1 t in ITmProj(t', k)
  | ITmRecord(ls) -> ITmRecord(eval_key_term eval1 ls)
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
