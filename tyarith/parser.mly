%{
open Syntax
%}

%token TRUE FALSE IF THEN ELSE ZERO SUCC PRED ISZERO LPAREN RPAREN EOF

%start <Syntax.term> parse

%%

parse:
  | term EOF { $1 }

term:
  | TRUE { TmTrue }
  | FALSE { TmFalse }
  | IF t1 = term THEN t2 = term ELSE t3 = term { TmIf(t1, t2, t3) }
  | ZERO { TmZero }
  | SUCC t1 = term { TmSucc(t1) }
  | PRED t1 = term { TmPred(t1) }
  | ISZERO t1 = term { TmIsZero(t1) }
  | LPAREN t1 = term RPAREN { t1 }

%%
