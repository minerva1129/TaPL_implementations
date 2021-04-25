%{
open Syntax
%}

%token TRUE FALSE IF THEN ELSE ZERO SUCC PRED ISZERO LPAREN RPAREN EOF

%start parse
%type <Syntax.term> parse

%%

parse:
  | term EOF { $1 }

term:
  | TRUE { TmTrue }
  | FALSE { TmFalse }
  | IF term THEN term ELSE term { TmIf($2, $4, $6) }
  | ZERO { TmZero }
  | SUCC term { TmSucc($2) }
  | PRED term { TmPred($2) }
  | ISZERO term { TmIsZero($2) }
  | LPAREN term RPAREN { $2 }

%%
