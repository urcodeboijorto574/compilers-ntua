# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep

GenAst.cmo: GenAst.ml
	$(OCAMLC) -I /home/jimv/.opam/4.14.0/lib/llvm/ -c GenAst.ml

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

grace$(EXE): Types.cmo Symbol.cmo PrintAst.cmo Ast.cmo SemAst.cmo Lexer.cmo Parser.cmo Main.cmo GenAst.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly
	menhir Parser.mly

.PHONY: clean distclean

-include .depend

depend: Types.ml Types.mli Symbol.ml Symbol.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli Ast.ml Ast.mli Lexer.ml Lexer.mli Parser.ml Parser.mli Main.ml GenAst.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) Lexer.ml Parser.ml Parser.mli Parser.output Lexer.cmx Lexer Lexer.o Parser.automaton Parser.conflicts *.cmo *.cmi *~

distclean: clean
	$(RM) grace$(EXE) .depend

# To format the ocaml code, first install the ocamlformat tool with "opam install ocamlformat"
format: Ast.ml Lexer.mli Main.ml Symbol.ml Symbol.mli Types.ml Types.mli PrintAst.ml PrintAst.mli SemAst.ml SemAst.mli GenAst.ml
	ocamlformat -i $^

update: clean format depend grace$(EXE)
	make clean
