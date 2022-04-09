open Opal
exception LexingError
exception ParsingError

type token_type =
  | TkArrow
  | TkBackslash
  | TkColon
  | TkDot
  | TkForall
  | TkUpUnit
  | TkLowUnit
  | TkLParen
  | TkRParen
  | TkLBracket
  | TkRBracket
  | TkUpIdentifier of string
  | TkLowIdentifier of string

let lex_one_token =
      (token "->" >> return TkArrow)
  <|> (token "\\" >> return TkBackslash)
  <|> (token ":" >> return TkColon)
  <|> (token "." >> return TkDot)
  <|> (token "forall" >> return TkForall)
  <|> (token "Unit" >> return TkUpUnit)
  <|> (token "unit" >> return TkLowUnit)
  <|> (token "(" >> return TkLParen)
  <|> (token ")" >> return TkRParen)
  <|> (token "[" >> return TkLBracket)
  <|> (token "]" >> return TkRBracket)
  <|> ((lexeme (upper <~> many (alpha_num <|> exactly '_' <|> exactly '\''))) => implode => (fun x -> TkUpIdentifier x))
  <|> ((lexeme (lower <~> many (alpha_num <|> exactly '_' <|> exactly '\''))) => implode => (fun x -> TkLowIdentifier x))

let lex = (many lex_one_token << spaces) >>= eof

let parens x = between (exactly TkLParen) (exactly TkRParen) x
let (<*>) xf x = xf >>= fun f -> x >>= f % return
let (let*) = (>>=)
let foldl x y =
  let rec loop a = ((y <*> return a) >>= loop) <|> return a in
  x >>= loop 

let parse_Unit = (exactly TkUpUnit) >> return Syntax.ETyUnit
let parse_upper_identifier = any >>= function
  | TkUpIdentifier(x) -> return x
  | _ -> mzero
let parse_type_variable = parse_upper_identifier => fun x -> Syntax.ETyVar x
let parse_arrow = (exactly TkArrow) >> return (fun l r -> Syntax.ETyArrow(l, r))
let parse_all =
  let* _ = (exactly TkForall) in
  let* x = parse_upper_identifier in
  let* _ = (exactly TkDot) in
  return (fun t -> Syntax.ETyAll(x, t))

let rec atomic_ty input = (parse_Unit <|> parse_type_variable <|> parens ty) input
and arrow_ty input = chainr1 atomic_ty parse_arrow input
and ty input = ((parse_all <*> ty) <|> arrow_ty) input

let parse_unit = (exactly TkLowUnit) >> return Syntax.ETmUnit
let parse_lower_identifier = any >>= function
  | TkLowIdentifier(x) -> return x
  | _ -> mzero
let parse_variable = parse_lower_identifier => fun x -> Syntax.ETmVar x
let parse_abstruction =
  let* _ = (exactly TkBackslash) in
  let* x = parse_lower_identifier in
  let* _ = (exactly TkColon) in
  let* tyT = ty in
  let* _ = (exactly TkDot) in
  return (fun t -> Syntax.ETmAbs(x, tyT, t))
let parse_application r = return (fun t -> Syntax.ETmApp(t, r))
let parse_type_abstruction =
  let* _ = (exactly TkBackslash) in
  let* x = parse_upper_identifier in
  let* _ = (exactly TkDot) in
  return (fun t -> Syntax.ETmTAbs(x, t))
let parse_type_application =
  let* _ = (exactly TkLBracket) in
  let* tyT = ty in
  let* _ = (exactly TkRBracket) in
  return (fun t -> Syntax.ETmTApp(t, tyT))

let rec atomic_term input = (parse_unit <|> parse_variable <|> parens term) input
and application_term input = (foldl atomic_term ((atomic_term >>= parse_application) <|> parse_type_application)) input
and term input = (((parse_abstruction <|> parse_type_abstruction) <*> term) <|> application_term) input

let lex_and_parse input = match parse lex input with
  | Some(tokens) -> (
    match parse (term >>= eof) (LazyStream.of_stream (Stream.of_list tokens)) with
      | Some(et) -> et
      | None -> raise ParsingError
  )
  | None -> raise LexingError
