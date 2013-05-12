ASM = asmi.cmo

.PHONY : clean run test
.SILENT: clean run test

$(ASM): asmi.ml asm-syntax.ml asm-prettyp.ml asm-parser.ml set.ml
	ocamlc -c -I +camlp4 -pp camlp4of.opt asmi.ml

fact: $(IMP) $(ASM)
	camlp4 asmi.cmo < fact.asm

fib: $(IMP) $(ASM)
	camlp4 asmi.cmo < fib.asm

clean:
	'rm' *.cmo *.cmi