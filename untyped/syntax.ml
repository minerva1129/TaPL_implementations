exception NoSuchVariable

type term =
  | TmVar of string
  | TmAbs of string * term
  | TmApp of term * term

(* de Bruijn indexed *)
type indexed_term =
  | ITmVar of int
  | ITmAbs of indexed_term
  | ITmApp of indexed_term * indexed_term

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

let isval = function
  | ITmAbs(_) -> true
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
  | ITmAbs(t) -> let x = pickfreshname context in TmAbs(x, restorenames (x::context) t)
  | ITmApp(t1, t2) -> TmApp(restorenames context t1, restorenames context t2)
