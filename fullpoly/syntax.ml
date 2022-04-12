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

let traverse f = function
  | ITmUnit -> ITmUnit
  | ITmVar(k) -> ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, f t)
  | ITmApp(t1, t2) -> ITmApp(f t1, f t2)
  | ITmTAbs(t) -> ITmTAbs(f t)
  | ITmTApp(t, tyT) -> ITmTApp(f t, tyT) 

let rec shift_ty d c = function
  | ITyVar(k) -> if k < c then ITyVar(k) else ITyVar(k + d)
  | ITyAll(tyT) -> ITyAll(shift_ty d (c + 1) tyT)
  | tyT -> traverse_ty (shift_ty d c) tyT

let rec shift d c = function
  | ITmVar(k) -> if k < c then ITmVar(k) else ITmVar(k + d)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, shift d (c + 1) t)
  | t -> traverse (shift d c) t

let rec shift_ty_term d c = function
  | ITmAbs(tyT, t) -> ITmAbs(shift_ty d c tyT, shift_ty_term d c t)
  | ITmTAbs(t) -> ITmTAbs(shift_ty_term d (c + 1) t)
  | ITmTApp(t, tyT) -> ITmTApp(shift_ty_term d c t, shift_ty d c tyT)
  | t -> traverse (shift_ty_term d c) t

let rec isval = function
  | ITmAbs(_, _) -> true
  | ITmUnit -> true
  | _ -> false
