all: ast parser scanner 

scanner.ml: scanner.mll
	ocamllex scanner.mll

ast: ast.mli
	ocamlc -c ast.mli

parser.ml parser.mli:  parser.mly
	ocamlyacc parser.ml

parser: parser.mli parser.ml ast
	ocamlc -c parser.mli parser.ml

scanner: scanner.ml
	ocamlc -c scanner.ml

clean:
	rm *.cmi *.ml parser.mli
