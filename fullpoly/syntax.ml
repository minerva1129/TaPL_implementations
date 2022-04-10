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

let traverse_iterm f = function
  | ITmUnit -> ITmUnit
  | ITmVar(k) -> ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, f t)
  | ITmApp(t1, t2) -> ITmApp(f t1, f t2)
  | ITmTAbs(t) -> ITmTAbs(f t)
  | ITmTApp(t, tyT) -> ITmTApp(f t, tyT) 

let shift d =
  let rec walk c = function
    | ITmVar(k) -> if k < c
      then ITmVar(k)
      else ITmVar(k + d)
    | ITmAbs(tyT, t) -> ITmAbs(tyT, walk (c + 1) t)
    | t -> traverse_iterm (walk c) t
  in walk 0

let rec isval = function
  | ITmAbs(_, _) -> true
  | ITmUnit -> true
  | _ -> false
