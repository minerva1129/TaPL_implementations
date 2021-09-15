exception NoSuchVariable

type ty =
  | TyUnit
  | TyArrow of ty * ty

type term =
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmUnit
  | TmSeq of term * term
  | TmWildcard of ty * term

(* de Bruijn indexed *)
type internal_term =
  | ITmVar of int
  | ITmAbs of ty * internal_term
  | ITmApp of internal_term * internal_term
  | ITmUnit

let shift d =
  let rec walk c = function
    | ITmVar(k) -> if k < c
      then ITmVar(k)
      else ITmVar(k + d)
    | ITmAbs(tyT, t) -> ITmAbs(tyT, walk (c + 1) t)
    | ITmApp(t1, t2) -> ITmApp(walk c t1, walk c t2)
    | ITmUnit -> ITmUnit
  in walk 0

let rec index_of context x =
  match context with
    | [] -> None
    | h::t -> if h = x
      then Some 0
      else Option.map succ (index_of t x)

let rec removenames context = function
  | TmVar(x) -> (
    match (index_of context x) with
      | None -> raise NoSuchVariable
      | Some(k) -> ITmVar(k)
  )
  | TmAbs(x, tyT, t) -> ITmAbs(tyT, removenames (x::context) t)
  | TmApp(t1, t2) -> ITmApp(removenames context t1, removenames context t2)
  | TmUnit -> ITmUnit
  | TmSeq(t1, t2) -> ITmApp(ITmAbs(TyUnit, shift 1 (removenames context t2)), removenames context t1)
  | TmWildcard(tyT, t) -> ITmAbs(tyT, shift 1 (removenames context t))

let isval = function
  | ITmAbs(_, _) -> true
  | ITmUnit -> true
  | _ -> false

let pickfreshname context =
  let rec pf commas =
    let xc = "x" ^ (String.make commas '\'') in
    let yc = "y" ^ (String.make commas '\'') in
    let zc = "z" ^ (String.make commas '\'') in
    if not (List.mem xc context)
    then xc
    else if not (List.mem yc context)
      then yc
      else if not (List.mem zc context)
        then zc
        else pf (commas + 1)
  in pf 0

let rec restorenames context = function
  | ITmVar(k) -> (
    match List.nth_opt context k with
      | None -> raise NoSuchVariable
      | Some(x) -> TmVar(x)
  )
  | ITmAbs(tyT, t) -> let x = pickfreshname context in TmAbs(x, tyT, restorenames (x::context) t)
  | ITmApp(t1, t2) -> TmApp(restorenames context t1, restorenames context t2)
  | ITmUnit -> TmUnit
