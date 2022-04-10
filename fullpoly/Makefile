build:
	ocamlfind ocamlc -o f -linkpkg -package opal utils.ml syntax.ml lexer_and_parser.ml compiler.ml evaluator.ml typechecker.ml main.ml

clean:
	rm -rf *.cmi *.cmo f
	cp ~/.opam/default/lib/opal/opal.cmi .
