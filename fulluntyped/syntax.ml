exception NoSuchVariable

type term =
  | TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

(* de Bruijn indexed *)
type indexed_term =
  | ITmVar of int
  | ITmAbs of indexed_term
  | ITmApp of indexed_term * indexed_term
  | ITmTrue
  | ITmFalse
  | ITmIf of indexed_term * indexed_term * indexed_term
  | ITmZero
  | ITmSucc of indexed_term
  | ITmPred of indexed_term
  | ITmIsZero of indexed_term

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
  | TmAbs(x, t) -> ITmAbs(removenames (x::context) t)
  | TmApp(t1, t2) -> ITmApp(removenames context t1, removenames context t2)
  | TmTrue -> ITmTrue
  | TmFalse -> ITmFalse
  | TmIf(t1, t2, t3) -> ITmIf(removenames context t1, removenames context t2, removenames context t3)
  | TmZero -> ITmZero
  | TmSucc(t) -> ITmSucc(removenames context t)
  | TmPred(t) -> ITmPred(removenames context t)
  | TmIsZero(t) -> ITmIsZero(removenames context t)

let rec isnumericval = function
  | ITmZero -> true
  | ITmSucc(t) -> isnumericval t
  | _ -> false

let isval = function
  | ITmAbs(_) -> true
  | ITmTrue -> true
  | ITmFalse -> true
  | t -> isnumericval t

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
  | ITmAbs(t) -> let x = pickfreshname context in TmAbs(x, restorenames (x::context) t)
  | ITmApp(t1, t2) -> TmApp(restorenames context t1, restorenames context t2)
  | ITmTrue -> TmTrue
  | ITmFalse -> TmFalse
  | ITmIf(t1, t2, t3) -> TmIf(restorenames context t1, restorenames context t2, restorenames context t3)
  | ITmZero -> TmZero
  | ITmSucc(t) -> TmSucc(restorenames context t)
  | ITmPred(t) -> TmPred(restorenames context t)
  | ITmIsZero(t) -> TmIsZero(restorenames context t)
