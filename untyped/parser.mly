%{
open Syntax
%}

%token BACKSLASH DOT LPAREN RPAREN EOF
%token <string> VAR

%start <Syntax.term> parse

%%

parse:
  | term EOF { $1 }

term:
  | appterm { $1 }
  | BACKSLASH x = VAR DOT t = term { TmAbs(t) }

appterm:
  | aterm { $1 }
  | t1 = appterm t2 = aterm { TmApp(t1, t2) }

aterm:
  | VAR { TmVar($1) }
  | LPAREN t = term RPAREN { t }

%%
