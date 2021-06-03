build:
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -c syntax.ml parser.mli parser.ml lexer.ml evaluator.ml typechecker.ml main.ml
	ocamlc -o f syntax.cmo lexer.cmo parser.cmo evaluator.cmo typechecker.cmo main.cmo

clean:
	rm -rf lexer.ml parser.ml parser.mli *.cmo *.cmi f
