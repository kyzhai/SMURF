SOURCES = scanner.mll \
          parser.mly \
          sast.ml \
          ast.ml \
          semanalyze.ml \
          parser.ml \
          scanner.ml \
          parser-test.ml \
          semantic-test.ml \
          interpreter.ml \
          util.ml \
          toplevel.ml \
          values.ml \
          output.ml

OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) parser-test.byte semantic-test.byte toplevel.byte

clean:
	$(OCAMLBUILD) -clean
	rm -f parser-testout.*

