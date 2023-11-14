open Llvm
open GenAst

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let asts = Parser.program Lexer.lexer lexbuf in
    if Types.debugMode then (
      Printf.printf "Syntactic analysis:\n";
      PrintAst.print_on asts;
      Printf.printf "\n");
    Printf.printf "Successful parsing.\n";
    if Types.debugMode then (
      Printf.printf "\n";
      Printf.printf "Semantic analysis:\n");
    SemAst.sem_on asts;
    Printf.printf "\n\027[32mSemantically correct.\027[0m\n";
    GenAst.gen_on asts;

    (* command to compile GenAst.ml
       ocamlc -I /home/jimv/.opam/4.14.0/lib/llvm/ -c GenAst.ml*)

    (* creates an a.ll file which includes the llvm
       intermediate code*)
    (* Then, the following 2 commands need to be executed:
       llc -o a.s a.ll -> uses the llc compiler to create an
                           a.s assembly file from the a.ll
       clang -o a.out a.s ./lib/lib.a -> creates the executable a.out
                               linking it with the library
       ./a.out
    *)
    print_module "a.ll" GenAst.the_module
  with Parsing.Parse_error ->
    Printf.eprintf "Syntax error\n";
    exit 1
