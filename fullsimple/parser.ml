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
let token_identifier = (lexeme (lower <~> many (alpha_num <|> exactly '_'))) => implode;;

let braces x = between token_lparen token_rparen x;;
let (<*>) xf x = xf >>= fun f -> x >>= f % return;;
let (let*) = (>>=);;

let parse_Unit = token_Unit >> return Syntax.TyUnit;;
let parse_arrow = token_arrow >> return (fun l r -> Syntax.TyArrow(l, r));;

let rec atomic_ty input = (parse_Unit <|> braces ty) input
and ty input = chainr1 atomic_ty parse_arrow input;;

let parse_identifier = token_identifier => fun r -> Syntax.TmVar r;;
let parse_unit = token_unit >> return Syntax.TmUnit;;
let parse_lambda term_parser =
  let* _ = token_backslash in
  let* x = token_identifier in
  let* _ = token_colon in
  let* tyT = ty in
  let* _ = token_dot in
  let* t = term_parser in
  return (Syntax.TmAbs(x, tyT, t));;
let parse_application = space >> return (fun l r -> Syntax.TmApp(l, r));;
let parse_sequence = token_semicolon >> return (fun l r -> Syntax.TmSeq(l, r));;
let parse_wildcard term_parser =
  let* _ = token_backslash in
  let* _ = token_underscore in
  let* _ = token_colon in
  let* tyT = ty in
  let* _ = token_dot in
  let* t = term_parser in
  return (Syntax.TmWildcard(tyT, t));;
let parse_ascription term_parser =
  let* t = term_parser in
  let* _ = token_as in
  let* tyT = ty in
  return (Syntax.TmAscribe(t, tyT));;

let rec atomic_term input = (parse_unit <|> parse_identifier <|> braces term) input
and ascription_term input = (parse_ascription atomic_term <|> atomic_term) input
and application_term input = (chainl1 ascription_term parse_application) input
and sequence_term input = (chainr1 application_term parse_sequence) input
and term input = (application_term <|> (parse_wildcard term) <|> (parse_lambda term)) input;;
