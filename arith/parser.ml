type token =
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | ZERO
  | SUCC
  | PRED
  | ISZERO
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Syntax
# 20 "parser.ml"
let yytransl_const = [|
  257 (* TRUE *);
  258 (* FALSE *);
  259 (* IF *);
  260 (* THEN *);
  261 (* ELSE *);
  262 (* ZERO *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LPAREN *);
  267 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\006\000\001\000\002\000\002\000\002\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\000\000\005\000\000\000\000\000\
\000\000\000\000\010\000\000\000\000\000\006\000\007\000\008\000\
\000\000\001\000\000\000\009\000\000\000\000\000\004\000"

let yydgoto = "\002\000\
\011\000\012\000"

let yysindex = "\005\000\
\017\255\000\000\000\000\000\000\017\255\000\000\017\255\017\255\
\017\255\017\255\000\000\001\000\003\255\000\000\000\000\000\000\
\253\254\000\000\017\255\000\000\004\255\017\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\251\255"

let yytablesize = 27
let yytable = "\013\000\
\018\000\014\000\015\000\016\000\017\000\001\000\019\000\020\000\
\022\000\000\000\000\000\000\000\000\000\021\000\000\000\000\000\
\023\000\003\000\004\000\005\000\000\000\000\000\006\000\007\000\
\008\000\009\000\010\000"

let yycheck = "\005\000\
\000\000\007\000\008\000\009\000\010\000\001\000\004\001\011\001\
\005\001\255\255\255\255\255\255\255\255\019\000\255\255\255\255\
\022\000\001\001\002\001\003\001\255\255\255\255\006\001\007\001\
\008\001\009\001\010\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ZERO\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 13 "parser.mly"
             ( _1 )
# 106 "parser.ml"
               : Syntax.term))
; (fun __caml_parser_env ->
    Obj.repr(
# 16 "parser.mly"
         ( TmTrue )
# 112 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 17 "parser.mly"
          ( TmFalse )
# 118 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 18 "parser.mly"
                                ( TmIf(_2, _4, _6) )
# 127 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 19 "parser.mly"
         ( TmZero )
# 133 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 20 "parser.mly"
              ( TmSucc(_2) )
# 140 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 21 "parser.mly"
              ( TmPred(_2) )
# 147 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 22 "parser.mly"
                ( TmIsZero(_2) )
# 154 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 23 "parser.mly"
                       ( _2 )
# 161 "parser.ml"
               : 'term))
(* Entry parse *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.term)
;;
