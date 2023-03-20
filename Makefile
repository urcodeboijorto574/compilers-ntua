.PHONY: all

all: lexer

lexer: lexer.ml
	ocamlopt -o lexer lexer.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

test1:
	./lexer < examples/helloworld.grc

clean: rm -f lexer lexer.cmi lexer.cmx lexer.o
