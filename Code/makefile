SOURCES = scanner.mll \
          parser.mly \
          sast.ml \
          ast.ml \
          semanalyze.ml \
          parser.ml \
          scanner.ml \
          test.ml \
          smurf.ml \
          interpreter.ml \
          util.ml \
          toplevel.ml \
          values.ml \
          output.ml

OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) test.byte smurf.byte toplevel.byte

midi:
	java -jar CSV2MIDI.jar testout.csv testout.midi

clean:
	$(OCAMLBUILD) -clean
	rm -f testout.*
    

