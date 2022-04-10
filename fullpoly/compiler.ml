open Utils
open Syntax

let rec remove_type_names context = function
  | ETyUnit -> ITyUnit
  | ETyVar(tyX) -> (
    match (index_of context tyX) with
    | None -> raise NoSuchVariable
    | Some(k) -> ITyVar(k)
    )
  | ETyArrow(tyT1, tyT2) -> ITyArrow(remove_type_names context tyT1, remove_type_names context tyT2)
  | ETyAll(tyX, tyT) -> ITyAll(remove_type_names (tyX::context) tyT)

let rec remove_names context tycontext = function
  | ETmUnit -> ITmUnit
  | ETmVar(x) -> (
    match (index_of context x) with
    | None -> raise NoSuchVariable
    | Some(k) -> ITmVar(k)
    )
  | ETmAbs(x, tyT, t) -> ITmAbs(remove_type_names tycontext tyT, remove_names (x::context) tycontext t)
  | ETmApp(t1, t2) -> ITmApp(remove_names context tycontext t1, remove_names context tycontext t2)
  | ETmTAbs(tyX, t) -> ITmTAbs(remove_names context (tyX::tycontext) t)
  | ETmTApp(t, tyT) -> ITmTApp(remove_names context tycontext t, remove_type_names tycontext tyT)

let pick_fresh_name context =
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

let pick_fresh_type_name context =
  let rec pf commas =
    let xc = "X" ^ (String.make commas '\'') in
    let yc = "Y" ^ (String.make commas '\'') in
    let zc = "Z" ^ (String.make commas '\'') in
    if not (List.mem xc context)
    then xc
    else if not (List.mem yc context)
      then yc
      else if not (List.mem zc context)
        then zc
        else pf (commas + 1)
  in pf 0

let rec restore_type_names context = function
  | ITyUnit -> ETyUnit
  | ITyVar(k) -> (
    match (List.nth_opt context k) with
    | None -> raise NoSuchVariable
    | Some(tyX) -> ETyVar(tyX)
    )
  | ITyArrow(tyT1, tyT2) -> ETyArrow(restore_type_names context tyT1, restore_type_names context tyT2)
  | ITyAll(tyT) -> let tyX = pick_fresh_type_name context in ETyAll(tyX, restore_type_names (tyX::context) tyT)

let rec restore_names context tycontext = function
  | ITmUnit -> ETmUnit
  | ITmVar(k) -> (
    match (List.nth_opt context k) with
    | None -> raise NoSuchVariable
    | Some(x) -> ETmVar(x)
    )
  | ITmAbs(tyT, t) -> let x = pick_fresh_name context in ETmAbs(x, restore_type_names tycontext tyT, restore_names (x::context) tycontext t)
  | ITmApp(t1, t2) -> ETmApp(restore_names context tycontext t1, restore_names context tycontext t2)
  | ITmTAbs(t) -> let tyX = pick_fresh_type_name tycontext in ETmTAbs(tyX, restore_names context (tyX::tycontext) t)
  | ITmTApp(t, tyT) -> ETmTApp(restore_names context tycontext t, restore_type_names tycontext tyT)
