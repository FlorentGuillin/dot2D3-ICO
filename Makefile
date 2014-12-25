all: 
	ocamllex lexer.mll
	ocamlyacc -v parser.mly 
	ocamlc -c syntax.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c dot2D3.ml
	ocamlc -o dot2D3 lexer.cmo 	parser.cmo dot2D3.cmo

clean:
	rm -rf dot2D3 *.cmo *.cmi parser.mli *~ lexer.ml parser.ml parser.output
