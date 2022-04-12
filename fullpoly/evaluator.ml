open Syntax
exception NoEvaluationRuleApplies

let rec eval1 = function
  | ITmApp(ITmAbs(_, t), v) when (isval v) -> shift (-1) 0 (substitute 0 (shift 1 0 v) t)
  | ITmApp(v1, t2) when (isval v1) -> let t2' = eval1 t2 in ITmApp(v1, t2')
  | ITmApp(t1, t2) -> let t1' = eval1 t1 in ITmApp(t1', t2)
  | ITmTApp(ITmTAbs(t), tyT) -> shift_ty_term (-1) 0 (substitute_ty_term 0 (shift_ty 1 0 tyT) t)
  | ITmTApp(t, tyT) -> let t' = eval1 t in ITmTApp(t', tyT)
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
