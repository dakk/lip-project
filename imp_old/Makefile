MAIN=main.cmo

.PHONY : clean run test
.SILENT: clean run test

$(MAIN): main.ml imp-syntax.ml imp-prettyp.ml imp-env.ml imp-store.ml imp-parser.ml imp-interpreter.ml
	ocamlc -c -I +camlp4 -pp camlp4of.opt main.ml

run: $(MAIN)
	camlp4 main.cmo

test: $(MAIN)
	camlp4 main.cmo < test
clean:
	'rm' *.cmo *.cmi