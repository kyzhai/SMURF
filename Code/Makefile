all: ast parser scanner test

test: test.cmo
	ocamlc -o test parser.cmo scanner.cmo ast.cmo test.cmo

test.cmo: test.ml
	ocamlc -c test.ml

scanner.ml: scanner.mll
	ocamllex scanner.mll

ast: ast.ml
	ocamlc -c ast.ml

parser.ml parser.mli:  parser.mly
	ocamlyacc -v parser.mly

parser: parser.mli parser.ml ast
	ocamlc -c parser.mli parser.ml

scanner: scanner.ml
	ocamlc -c scanner.ml

clean:
	rm *.cmi *.cmo parser.ml scanner.ml *.output parser.mli test
