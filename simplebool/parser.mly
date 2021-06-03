%{
open Syntax
%}

%token BACKSLASH COLON BOOL ARROW DOT TRUE FALSE IF THEN ELSE LPAREN RPAREN EOF
%token <string> VAR

%start <Syntax.term> parse

%%

parse:
  | term EOF { $1 }

term:
  | boolterm { $1 }
  | BACKSLASH x = VAR COLON tyT = ty DOT t = term { TmAbs(x, tyT, t) }

boolterm:
  | appterm { $1 }
  | IF t1 = boolterm THEN t2 = boolterm ELSE t3 = boolterm { TmIf(t1, t2, t3) }

appterm:
  | aterm { $1 }
  | t1 = appterm t2 = aterm { TmApp(t1, t2) }

aterm:
  | VAR { TmVar($1) }
  | TRUE { TmTrue }
  | FALSE { TmFalse }
  | LPAREN t = term RPAREN { t }

ty:
  | aty { $1 }
  | tyT1 = aty ARROW tyT2 = ty { TyArrow(tyT1, tyT2) }

aty:
  | BOOL { TyBool }
  | LPAREN tyT = ty RPAREN { tyT }

%%
