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
PACKAGES=-package llvm -package llvm.analysis -package llvm.target \
		-package llvm.scalar_opts -package llvm.ipo \
		-package llvm.vectorize -package llvm.all_backends \
	   	-package unix


%.cmx: %.ml %.mli
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) $(PACKAGES) -c $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) $(PACKAGES) -c $<

%.cmx %.cmi: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) $(PACKAGES) -c $<

grace$(EXE): Types.cmx Error.cmx Symbol.cmx PrintAst.cmx Ast.cmx SemAst.cmx Lexer.cmx Parser.cmx GenAst.cmx Main.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) $(PACKAGES) -linkpkg -o $@ $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly
	menhir Parser.mly

.PHONY: clean distclean

-include .depend

depend: Types.ml Types.mli Error.ml Error.mli Symbol.ml Symbol.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli Ast.ml Ast.mli Lexer.ml Lexer.mli ParserMessages.ml ParserMessages.mli Parser.ml Parser.mli GenAst.ml GenAst.mli Main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) Lexer.ml Parser.ml Parser.mli Parser.output Lexer Lexer.o Parser.automaton Parser.conflicts *.cmx *.cmi *~ *.o a.ll a.s a.out lib/lib.o

distclean: clean
	$(RM) grace$(EXE) .depend

# To format the OCaml code, first install the ocamlformat tool with "opam install ocamlformat"
format: Ast.ml* Error.ml* GenAst.ml* Lexer.mli Main.ml ParserMessages.ml* PrintAst.ml* SemAst.ml* Symbol.ml* Types.ml*
	ocamlformat -i $^
