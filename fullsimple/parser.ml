open Opal

let token_backslash = token "\\";;
let token_colon = token ":";;
let token_Unit = token "Unit";;
let token_arrow = token "->";;
let token_dot = token ".";;
let token_unit = token "unit";;
let token_lparen = token "(";;
let token_rparen = token ")";;
let token_semicolon = token ";";;
let token_underscore = token "_";;
let token_as = token "as";;
let token_let = token "let";;
let token_equal = token "=";;
let token_in = token "in";;
let token_identifier = (lexeme (lower <~> many (alpha_num <|> exactly '_'))) => implode;;

let braces x = between token_lparen token_rparen x;;
let (<*>) xf x = xf >>= fun f -> x >>= f % return;;
let (let*) = (>>=);;

let parse_Unit = token_Unit >> return Syntax.TyUnit;;
let parse_arrow = token_arrow >> return (fun l r -> Syntax.TyArrow(l, r));;

let rec atomic_ty input = (parse_Unit <|> braces ty) input
and ty input = chainr1 atomic_ty parse_arrow input;;

let parse_identifier = token_identifier => fun r -> Syntax.ETmVar r;;
let parse_unit = token_unit >> return Syntax.ETmUnit;;
let parse_lambda term_parser =
  let* _ = token_backslash in
  let* x = token_identifier in
  let* _ = token_colon in
  let* tyT = ty in
  let* _ = token_dot in
  let* t = term_parser in
  return (Syntax.ETmAbs(x, tyT, t));;
let parse_application = space >> return (fun l r -> Syntax.ETmApp(l, r));;
let parse_sequence = token_semicolon >> return (fun l r -> Syntax.ETmSeq(l, r));;
let parse_wildcard term_parser =
  let* _ = token_backslash in
  let* _ = token_underscore in
  let* _ = token_colon in
  let* tyT = ty in
  let* _ = token_dot in
  let* t = term_parser in
  return (Syntax.ETmWildcard(tyT, t));;
let parse_ascription term_parser =
  let* t = term_parser in
  let* _ = token_as in
  let* tyT = ty in
  return (Syntax.ETmAscribe(t, tyT));;
let parse_let term1_parser term2_parser =
  let* _ = token_let in
  let* x = token_identifier in
  let* _ = token_equal in
  let* t1 = term1_parser in
  let* _ = token_in in
  let* t2 = term2_parser in
  return (Syntax.ETmLet(x, t1, t2));;

let rec atomic_term input = (parse_unit <|> parse_identifier <|> braces term) input
and ascription_term input = (parse_ascription atomic_term <|> atomic_term) input
and application_term input = (chainl1 ascription_term parse_application) input
and sequence_term input = (chainr1 application_term parse_sequence) input
and term input = ((parse_wildcard term) <|> (parse_lambda term) <|> (parse_let atomic_term term) <|> application_term) input;;
