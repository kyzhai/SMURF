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
	$(OCAMLBUILD) parser_test.byte semantic_test.byte toplevel.byte

clean:
	$(OCAMLBUILD) -clean
	rm -f parser_testout.*

