open Opal
exception LexingError
exception ParsingError

type token_type =
  | Token_backslash
  | Token_colon
  | Token_Unit
  | Token_arrow
  | Token_dot
  | Token_unit
  | Token_lparen
  | Token_rparen
  | Token_semicolon
  | Token_underscore
  | Token_as
  | Token_let
  | Token_equal
  | Token_in
  | Token_lbrace
  | Token_rbrace
  | Token_comma
  | Token_identifier of string

let parse_token =
      (token "\\" >> return Token_backslash)
  <|> (token ":" >> return Token_colon)
  <|> (token "Unit" >> return Token_Unit)
  <|> (token "->" >> return Token_arrow)
  <|> (token "." >> return Token_dot)
  <|> (token "unit" >> return Token_unit)
  <|> (token "(" >> return Token_lparen)
  <|> (token ")" >> return Token_rparen)
  <|> (token ";" >> return Token_semicolon)
  <|> (token "_" >> return Token_underscore)
  <|> (token "as" >> return Token_as)
  <|> (token "let" >> return Token_let)
  <|> (token "=" >> return Token_equal)
  <|> (token "in" >> return Token_in)
  <|> (token "{" >> return Token_lbrace)
  <|> (token "}" >> return Token_rbrace)
  <|> (token "," >> return Token_comma)
  <|> ((lexeme ((lower <|> exactly '_') <~> many (alpha_num <|> exactly '_'))) => implode => (fun x -> Token_identifier x))

let lex = (many parse_token << spaces) >>= eof

let parens x = between (exactly Token_lparen) (exactly Token_rparen) x
let (<*>) xf x = xf >>= fun f -> x >>= f % return
let (let*) = (>>=)

let token_identifier = any >>= function
  | Token_identifier(x) -> return x
  | _ -> mzero

let parse_key_elem elem_parser =
  let* x = token_identifier in
  let* _ = (exactly Token_equal) in
  let* e = elem_parser in
  return (x, e)

let parse_Unit = (exactly Token_Unit) >> return Syntax.TyUnit
let parse_arrow = (exactly Token_arrow) >> return (fun l r -> Syntax.TyArrow(l, r))

let parse_record_ty ty_parser =
  let* _ = (exactly Token_lbrace) in
  let* ls = sep_by (parse_key_elem ty_parser) (exactly Token_comma) in
  let* _ = (exactly Token_rbrace) in
  return (Syntax.TyRecord(ls))

let rec atomic_ty input = (parse_record_ty ty <|> parse_Unit <|> parens ty) input
and ty input = chainr1 atomic_ty parse_arrow input

let parse_identifier = token_identifier => fun x -> Syntax.ETmVar x
let parse_unit = (exactly Token_unit) >> return Syntax.ETmUnit

let parse_lambda term_parser =
  let* _ = (exactly Token_backslash) in
  let* x = token_identifier in
  let* _ = (exactly Token_colon) in
  let* tyT = ty in
  let* _ = (exactly Token_dot) in
  let* t = term_parser in
  return (Syntax.ETmAbs(x, tyT, t))

let parse_application = return (fun l r -> Syntax.ETmApp(l, r))
let parse_sequence = (exactly Token_semicolon) >> return (fun l r -> Syntax.ETmSeq(l, r))

let parse_wildcard term_parser =
  let* _ = (exactly Token_backslash) in
  let* _ = (exactly Token_underscore) in
  let* _ = (exactly Token_colon) in
  let* tyT = ty in
  let* _ = (exactly Token_dot) in
  let* t = term_parser in
  return (Syntax.ETmWildcard(tyT, t))

let parse_ascription term_parser =
  let* t = term_parser in
  let* _ = (exactly Token_as) in
  let* tyT = ty in
  return (Syntax.ETmAscribe(t, tyT))

let parse_let term1_parser term2_parser =
  let* _ = (exactly Token_let) in
  let* x = token_identifier in
  let* _ = (exactly Token_equal) in
  let* t1 = term1_parser in
  let* _ = (exactly Token_in) in
  let* t2 = term2_parser in
  return (Syntax.ETmLet(x, t1, t2))

let parse_record term_parser =
  let* _ = (exactly Token_lbrace) in
  let* ls = sep_by (parse_key_elem term_parser) (exactly Token_comma) in
  let* _ = (exactly Token_rbrace) in
  return (Syntax.ETmRecord(ls))

let parse_proj t =
  let* _ = (exactly Token_dot) in
  let* k = token_identifier in
  return (Syntax.ETmProj(t, k))

let parse_proj_chain term_parser =
  let rec loop t = (parse_proj t >>= loop) <|> return t in
  term_parser >>= loop

let rec atomic_term input = (parse_record term <|> parse_unit <|> parse_identifier <|> parens term) input
and projection_term input = parse_proj_chain atomic_term input
and application_term input = (parse_ascription projection_term <|> chainl1 projection_term parse_application) input
and sequence_term input = (chainr1 application_term parse_sequence) input
and term input = ((parse_wildcard term) <|> (parse_lambda term) <|> (parse_let atomic_term term) <|> sequence_term) input

let lex_and_parse input = match parse lex input with
  | Some(tokens) -> (
    match parse (term >>= eof) (LazyStream.of_stream (Stream.of_list tokens)) with
      | Some(et) -> et
      | None -> raise ParsingError
  )
  | None -> raise LexingError
