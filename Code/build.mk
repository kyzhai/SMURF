TARGET=toplevel

SOURCES = scanner.mll \
          parser.mly \
          sast.ml \
          ast.ml \
          semanalyze.ml \
          parser.ml \
          scanner.ml \
          test.ml \
          smurf.ml \
          toplevel.ml \
          util.ml \
          interpreter.ml \
          message.ml


OCAMLBUILD=ocamlbuild

byte: 
	make -f Makefile clean
	$(OCAMLBUILD) $(TARGET).byte

native:
	make -f Makefile clean
	$(OCAMLBUILD) $(TARGET).native


clean:
	make -f Makefile clean
	$(OCAMLBUILD) -clean
