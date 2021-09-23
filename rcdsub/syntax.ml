exception NoSuchVariable
exception NoSuchField

type ty =
  | TyUnit
  | TyArrow of ty * ty
  | TyRecord of (string * ty) list
  | TyTop

type eterm =
  | ETmVar of string
  | ETmAbs of string * ty * eterm
  | ETmApp of eterm * eterm
  | ETmUnit
  | ETmRecord of (string * eterm) list
  | ETmProj of eterm * string

(* de Bruijn indexed *)
type iterm =
  | ITmVar of int
  | ITmAbs of ty * iterm
  | ITmApp of iterm * iterm
  | ITmUnit
  | ITmRecord of (string * iterm) list
  | ITmProj of iterm * string

let traverse_iterm f = function
  | ITmVar(k) -> ITmVar(k)
  | ITmAbs(tyT, t) -> ITmAbs(tyT, f t)
  | ITmApp(t1, t2) -> ITmApp(f t1, f t2)
  | ITmUnit -> ITmUnit
  | ITmRecord(ls) -> ITmRecord(List.map (fun (k, t) -> (k, f t)) ls)
  | ITmProj(t, k) -> ITmProj(f t, k)

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
  | ITmRecord(ls) -> List.for_all (fun (k, t) -> isval t) ls
  | _ -> false
