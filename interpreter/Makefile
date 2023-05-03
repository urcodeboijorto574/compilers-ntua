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

grace$(EXE): Lexer.cmo Parser.cmo Main.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly
	menhir Parser.mly

.PHONY: clean distclean

-include .depend

depend: Lexer.ml Lexer.mli Parser.ml Parser.mli Main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) Lexer.ml Parser.ml Parser.mli Parser.output Lexer.cmx grace Lexer Lexer.o Parser.automaton Parser.conflicts .depend *.cmo *.cmi *~ 

distclean: clean
	$(RM) grace$(EXE) .depend
