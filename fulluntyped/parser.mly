%{
open Syntax
%}

%token BACKSLASH DOT TRUE FALSE IF THEN ELSE ZERO SUCC PRED ISZERO LPAREN RPAREN EOF
%token <string> VAR

%start <Syntax.term> parse

%%

parse:
  | term EOF { $1 }

term:
  | arithterm { $1 }
  | BACKSLASH x = VAR DOT t = term { TmAbs(x, t) }

arithterm:
  | appterm { $1 }
  | IF t1 = arithterm THEN t2 = arithterm ELSE t3 = arithterm { TmIf(t1, t2, t3) }
  | SUCC t = arithterm { TmSucc(t) }
  | PRED t = arithterm { TmPred(t) }
  | ISZERO t = arithterm { TmIsZero(t) }

appterm:
  | aterm { $1 }
  | t1 = appterm t2 = aterm { TmApp(t1, t2) }

aterm:
  | VAR { TmVar($1) }
  | TRUE { TmTrue }
  | FALSE { TmFalse }
  | ZERO { TmZero }
  | LPAREN t = term RPAREN { t }

%%
