type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec isnumericval = function
  | TmZero -> true
  | TmSucc(t1) -> isnumericval t1
  | _ -> false

let isval = function
  | TmTrue -> true
  | TmFalse -> true
  | t -> isnumericval t
