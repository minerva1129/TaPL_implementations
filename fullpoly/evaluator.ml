open Syntax
exception NoEvaluationRuleApplies

let rec substitute_ty j tyS = function
  | ITyVar(k) -> if k = j then tyS else ITyVar(k)
  | ITyAll(tyT) -> ITyAll(substitute_ty (j + 1) (shift_ty 1 0 tyS) tyT)
  | tyT -> traverse_ty (substitute_ty j tyS) tyT

let rec substitute j s = function
  | ITmVar(k) -> if k = j then s else ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, substitute (j + 1) (shift 1 0 s) t)
  | t -> traverse (substitute j s) t

let rec substitute_ty_term j tyS = function
  | ITmAbs(tyT, t) -> ITmAbs(substitute_ty j tyS tyT, substitute_ty_term j tyS t)
  | ITmTAbs(t) -> ITmTAbs(substitute_ty_term (j + 1) (shift_ty 1 0 tyS) t)
  | ITmTApp(t, tyT) -> ITmTApp(substitute_ty_term j tyS t, substitute_ty j tyS tyT)
  | t -> traverse (substitute_ty_term j tyS) t

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
