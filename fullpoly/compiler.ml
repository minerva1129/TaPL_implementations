open Utils
open Syntax

let rec removenames context = function
  | ETmVar(x) -> (
    match (index_of context x) with
      | None -> raise NoSuchVariable
      | Some(k) -> ITmVar(k)
  )
  | ETmAbs(x, tyT, t) -> ITmAbs(tyT, removenames (x::context) t)
  | ETmApp(t1, t2) -> ITmApp(removenames context t1, removenames context t2)
  | ETmUnit -> ITmUnit
  | ETmSeq(t1, t2) -> ITmApp(ITmAbs(TyUnit, shift 1 (removenames context t2)), removenames context t1)
  | ETmWildcard(tyT, t) -> ITmAbs(tyT, shift 1 (removenames context t))
  | ETmAscribe(t, tyT) -> ITmApp(ITmAbs(tyT, ITmVar(0)), removenames context t)
  | ETmLet(x, t1, t2) -> ITmLet(removenames context t1, removenames (x::context) t2)
  | ETmRecord(ls) -> ITmRecord(List.map (fun (k, t) -> (k, removenames context t)) ls)
  | ETmProj(t, k) -> ITmProj(removenames context t, k)

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
      | Some(x) -> ETmVar(x)
  )
  | ITmAbs(tyT, t) -> let x = pickfreshname context in ETmAbs(x, tyT, restorenames (x::context) t)
  | ITmApp(t1, t2) -> ETmApp(restorenames context t1, restorenames context t2)
  | ITmUnit -> ETmUnit
  | ITmLet(t1, t2) -> let x = pickfreshname context in ETmLet(x, restorenames context t1, restorenames (x::context) t2)
  | ITmRecord(ls) -> ETmRecord(List.map (fun (k, t) -> (k, restorenames context t)) ls)
  | ITmProj(t, k) -> ETmProj(restorenames context t, k)
