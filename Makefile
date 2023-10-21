# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep
OCAMLFIND=ocamlfind

#GenAst.cmo: GenAst.ml 
#	$(OCAMLC) -I /home/jimv/.opam/4.14.0/lib/llvm/ $(OCAMLC_FLAGS) -c GenAst.ml

#GenAst.cmi: GenAst.mli
#	$(OCAMLC) $(OCAMLC_FLAGS) -c GenAst.mli

#%.cmo: %.ml %.mli
#	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

#%.cmi: %.mli
#	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

#%.cmo %.cmi: %.ml
#	$(OCAMLC) $(OCAMLC_FLAGS) -c $<
%.cmo: %.ml %.mli
	@if [ "$(word 1, $^)" = "GenAst.ml" ] || [ "$(word 1, $^)" = "GenAst.mli" ] || [ "$(word 1, $^)" = "Main.ml" ]; then \
		$(OCAMLC) -I /home/jimv/.opam/4.14.0/lib/llvm/ $(OCAMLC_FLAGS) -c $<; \
	else \
		$(OCAMLC) $(OCAMLC_FLAGS) -c $<; \
	fi

%.cmi: %.mli
	@if [ "$(word 1, $^)" = "GenAst.ml" ] || [ "$(word 1, $^)" = "GenAst.mli" ] || [ "$(word 1, $^)" = "Main.ml" ]; then \
		$(OCAMLC) -I /home/jimv/.opam/4.14.0/lib/llvm/ $(OCAMLC_FLAGS) -c $<; \
	else \
		$(OCAMLC) $(OCAMLC_FLAGS) -c $<; \
	fi

%.cmo %.cmi: %.ml
	@if [ "$(word 1, $^)" = "GenAst.ml" ] || [ "$(word 1, $^)" = "GenAst.mli" ] || [ "$(word 1, $^)" = "Main.ml" ]; then \
		$(OCAMLC) -I /home/jimv/.opam/4.14.0/lib/llvm/ $(OCAMLC_FLAGS) -c $<; \
	else \
		$(OCAMLC) $(OCAMLC_FLAGS) -c $<; \
	fi

grace$(EXE): Types.cmo Symbol.cmo PrintAst.cmo Ast.cmo SemAst.cmo Lexer.cmo Parser.cmo GenAst.cmo Main.cmo
	$(OCAMLFIND) $(OCAMLC) $(OCAMLC_FLAGS) -package llvm -package unix -linkpkg -o $@ $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly
	menhir Parser.mly

.PHONY: clean distclean

-include .depend

depend: Types.ml Types.mli Symbol.ml Symbol.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli Ast.ml Ast.mli Lexer.ml Lexer.mli Parser.ml Parser.mli GenAst.ml GenAst.mli Main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) Lexer.ml Parser.ml Parser.mli Parser.output Lexer.cmx Lexer Lexer.o Parser.automaton Parser.conflicts *.cmo *.cmi *~

distclean: clean
	$(RM) grace$(EXE) .depend

# To format the ocaml code, first install the ocamlformat tool with "opam install ocamlformat"
format: Ast.ml Lexer.mli Main.ml Symbol.ml Symbol.mli Types.ml Types.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli GenAst.ml GenAst.mli
	ocamlformat -i $^

update: clean format depend grace$(EXE)
	make clean
