# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

grace$(EXE): Types.cmo Symbol.cmo PrintAst.cmo Ast.cmo SemAst.cmo Lexer.cmo Parser.cmo Main.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly
	menhir Parser.mly

.PHONY: clean distclean

-include .depend

depend: Types.ml Types.mli Symbol.ml Symbol.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli Ast.ml Ast.mli Lexer.ml Lexer.mli Parser.ml Parser.mli Main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) Lexer.ml Parser.ml Parser.mli Parser.output Lexer.cmx Lexer Lexer.o Parser.automaton Parser.conflicts *.cmo *.cmi *~

distclean: clean
	$(RM) grace$(EXE) .depend

# To format the ocaml code, first install the ocamlformat tool with "opam install ocamlformat"
format: Ast.ml Lexer.mli Main.ml Symbol.ml Symbol.mli Types.ml Types.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli
	ocamlformat -i $^

update: clean format depend grace$(EXE)
	make clean

test:
	echo "Testing correct programs:" ; \
	echo "arrays.grc:" ; ./grace < examples/arrays.grc ; echo "" ; \
	echo "arraysum.grc" ; ./grace < examples/arraysum.grc ; echo "" ; \
	echo "bsort.grc" ; ./grace < examples/bsort.grc ; echo "" ; \
	echo "checkpairs.grc" ; ./grace < examples/checkpairs.grc ; echo "" ; \
	echo "evenodd.grc" ; ./grace < examples/evenodd.grc ; echo "" ; \
	echo "hanoi.grc" ; ./grace < examples/hanoi.grc ; echo "" ; \
	echo "helloworld.grc" ; ./grace < examples/helloworld.grc ; echo "" ; \
	echo "mergesort.grc" ; ./grace < examples/mergesort.grc ; echo "" ; \
	echo "primes.grc" ; ./grace < examples/primes.grc ; echo "" ; \
	echo "shortcircuit.grc" ; ./grace < examples/shortcircuit.grc ; echo "" ; \
	echo "sierpinski.grc" ; ./grace < examples/sierpinski.grc ; echo "" ; \
	echo "Testing erroneous programs:" ; \
	echo "errorcode.grc:" ; ./grace < examples/erroneous/errorcode.grc ; echo "" ; \
	echo "issorted.grc:" ; ./grace < examples/erroneous/issorted.grc ; echo "" ; \
	echo "overloaded.grc:" ; ./grace < examples/erroneous/overloaded.grc ; echo "" ; \
	echo "paramtypehavoc.grc:" ; ./grace < examples/erroneous/paramtypehavoc.grc ; echo "" ; \
	echo "redefine.grc:" ; ./grace < examples/erroneous/redefine.grc ; echo "" ; \
	echo "returntypehavoc.grc:" ; ./grace < examples/erroneous/returntypehavoc.grc ; echo "" ; \
	echo "varorfunc.grc:" ; ./grace < examples/erroneous/varorfunc.grc ; echo "" ; \
