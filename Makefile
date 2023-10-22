# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLOPT_FLAGS=-g
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLFIND=ocamlfind

%.cmx: %.ml %.mli
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) -package llvm -package unix -c $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) -package llvm -package unix -c $<
	
%.cmx %.cmi: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) -package llvm -package unix -c $<

grace$(EXE): Types.cmx Symbol.cmx PrintAst.cmx Ast.cmx SemAst.cmx Lexer.cmx Parser.cmx GenAst.cmx Main.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) -package llvm -package unix -linkpkg -o $@ $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly
	menhir Parser.mly

.PHONY: clean distclean

-include .depend

depend: Types.ml Types.mli Symbol.ml Symbol.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli Ast.ml Ast.mli Lexer.ml Lexer.mli Parser.ml Parser.mli GenAst.ml GenAst.mli Main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) Lexer.ml Parser.ml Parser.mli Parser.output Lexer.cmx Lexer Lexer.o Parser.automaton Parser.conflicts grace *.cmx *.cmi *~ *.o a.ll

distclean: clean
	$(RM) grace$(EXE) .depend

# To format the OCaml code, first install the ocamlformat tool with "opam install ocamlformat"
format: Ast.ml Lexer.mli Main.ml Symbol.ml Symbol.mli Types.ml Types.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli GenAst.ml GenAst.mli
	ocamlformat -i $^

update: clean format depend grace$(EXE)
	make clean

	