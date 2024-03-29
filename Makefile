# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLOPT_FLAGS=-g -O3
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLFIND=ocamlfind
PACKAGES=-package llvm -package llvm.analysis -package llvm.target \
		-package llvm.scalar_opts -package llvm.ipo \
		-package llvm.vectorize -package llvm.all_backends \
		-package menhirLib -package unix


%.cmx: %.ml %.mli
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) $(PACKAGES) -c $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) $(PACKAGES) -c $<

%.cmx %.cmi: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) $(PACKAGES) -c $<

grace$(EXE): Types.cmx Error.cmx Symbol.cmx PrintAst.cmx Ast.cmx SemAst.cmx Lexer.cmx ParserMessages.cmx Parser.cmx UnitActionsParser.cmx GenAst.cmx Main.cmx lib/lib.a
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPT_FLAGS) $(PACKAGES) -linkpkg -o $@ $^

lib/lib.a: lib/lib.o
	ar rcs lib/lib.a lib/lib.o

lib/lib.o: lib/lib.c
	gcc -c lib/lib.c -o lib/lib.o

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

ParserMessages.ml: ParserMessages.messages Parser.mly
	menhir --compile-errors $^ > $@

ParserMessages.mli: ParserMessages.ml
	ocamlc -i $< > $@

UnitActionsParser.mly: Parser.mly
	menhir --only-preprocess-u $< > $@

UnitActionsParser.ml UnitActionsParser.mli: Parser.ml UnitActionsParser.mly
	menhir --table --external-tokens Parser UnitActionsParser.mly

Parser.ml Parser.mli: Parser.mly
	menhir Parser.mly

.PHONY: clean distclean

-include .depend

depend: Types.ml Types.mli Error.ml Error.mli Symbol.ml Symbol.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli Ast.ml Ast.mli Lexer.ml Lexer.mli ParserMessages.ml ParserMessages.mli Parser.ml Parser.mli UnitActionsParser.ml UnitActionsParser.mli GenAst.ml GenAst.mli Main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) Lexer.ml Parser.ml Parser.mli Parser.output Lexer Lexer.o Parser.automaton Parser.conflicts ParserMessages.ml* UnitActionsParser.ml* *.cmx *.cmi *~ *.o a.ll a.s a.out lib/lib.o

distclean: clean
	$(RM) grace$(EXE) .depend

# To format the OCaml code, first install the ocamlformat tool with "opam install ocamlformat.0.26.1"
format: Ast.ml* Error.ml* GenAst.ml* Lexer.mli Main.ml PrintAst.ml* SemAst.ml* Symbol.ml* Types.ml*
	ocamlformat -i $^
