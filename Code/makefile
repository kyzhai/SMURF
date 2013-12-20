SOURCES = scanner.mll \
          parser.mly \
          sast.ml \
          ast.ml \
          semanalyze.ml \
          parser.ml \
          scanner.ml \
          parser_test.ml \
          semantic_test.ml \
          interpreter.ml \
          util.ml \
          toplevel.ml \
          values.ml \
          output.ml

OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) parser_test.native semantic_test.native toplevel.native

clean:
	$(OCAMLBUILD) -clean
	rm -f a.csv a.midi

