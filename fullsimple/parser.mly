%{
open Syntax
%}

%token BACKSLASH COLON TYUNIT ARROW DOT TMUNIT LPAREN RPAREN EOF
%token <string> VAR

%start <Syntax.term> parse

%%

parse:
  | term EOF { $1 }

term:
  | appterm { $1 }
  | BACKSLASH x = VAR COLON tyT = ty DOT t = term { TmAbs(x, tyT, t) }

appterm:
  | aterm { $1 }
  | t1 = appterm t2 = aterm { TmApp(t1, t2) }

aterm:
  | VAR { TmVar($1) }
  | TMUNIT { TmUnit }
  | LPAREN t = term RPAREN { t }

ty:
  | aty { $1 }
  | tyT1 = aty ARROW tyT2 = ty { TyArrow(tyT1, tyT2) }

aty:
  | TYUNIT { TyUnit }
  | LPAREN tyT = ty RPAREN { tyT }

%%
