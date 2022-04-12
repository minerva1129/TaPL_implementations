exception NoSuchVariable

type etype =
  | ETyUnit
  | ETyVar of string
  | ETyArrow of etype * etype
  | ETyAll of string * etype

type eterm =
  | ETmUnit
  | ETmVar of string
  | ETmAbs of string * etype * eterm
  | ETmApp of eterm * eterm
  | ETmTAbs of string * eterm
  | ETmTApp of eterm * etype

(* de Bruijn indexed *)
type itype =
  | ITyUnit
  | ITyVar of int
  | ITyArrow of itype * itype
  | ITyAll of itype

type iterm =
  | ITmUnit
  | ITmVar of int
  | ITmAbs of itype * iterm
  | ITmApp of iterm * iterm
  | ITmTAbs of iterm
  | ITmTApp of iterm * itype

let traverse_ty f = function
  | ITyUnit -> ITyUnit
  | ITyVar(k) -> ITyVar(k)
  | ITyArrow(tyT1, tyT2) -> ITyArrow(f tyT1, f tyT2)
  | ITyAll(tyT) -> ITyAll(f tyT)

let traverse f g = function
  | ITmUnit -> ITmUnit
  | ITmVar(k) -> ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(g tyT, f t)
  | ITmApp(t1, t2) -> ITmApp(f t1, f t2)
  | ITmTAbs(t) -> ITmTAbs(f t)
  | ITmTApp(t, tyT) -> ITmTApp(f t, g tyT) 

let rec shift_ty d c = function
  | ITyVar(k) -> if k < c then ITyVar(k) else ITyVar(k + d)
  | ITyAll(tyT) -> ITyAll(shift_ty d (c + 1) tyT)
  | tyT -> traverse_ty (shift_ty d c) tyT

let rec shift d c = function
  | ITmVar(k) -> if k < c then ITmVar(k) else ITmVar(k + d)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, shift d (c + 1) t)
  | t -> traverse (shift d c) Fun.id t

let rec shift_ty_term d c = function
  | ITmTAbs(t) -> ITmTAbs(shift_ty_term d (c + 1) t)
  | t -> traverse (shift_ty_term d c) (shift_ty d c) t

let rec substitute_ty j tyS = function
  | ITyVar(k) -> if k = j then tyS else ITyVar(k)
  | ITyAll(tyT) -> ITyAll(substitute_ty (j + 1) (shift_ty 1 0 tyS) tyT)
  | tyT -> traverse_ty (substitute_ty j tyS) tyT

let rec substitute j s = function
  | ITmVar(k) -> if k = j then s else ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, substitute (j + 1) (shift 1 0 s) t)
  | t -> traverse (substitute j s) Fun.id t

let rec substitute_ty_term j tyS = function
  | ITmTAbs(t) -> ITmTAbs(substitute_ty_term (j + 1) (shift_ty 1 0 tyS) t)
  | t -> traverse (substitute_ty_term j tyS) (substitute_ty j tyS) t

let rec isval = function
  | ITmUnit -> true
  | ITmAbs(_, _) -> true
  | ITmTAbs(_) -> true
  | _ -> false
