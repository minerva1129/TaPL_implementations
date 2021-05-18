type term =
  | TmVar of string
  | TmAbs of term
  | TmApp of term * term

