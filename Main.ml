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
    Printf.printf "\027[32mSuccessful parsing.\027[0m\n%!";
    if Types.debugMode then (
      Printf.printf "\n";
      Printf.printf "Semantic analysis:\n");
    SemAst.sem_on asts;
    if Types.debugMode then Printf.printf "\n";
    Printf.printf "\027[32mSemantically correct.\027[0m\n%!";
    GenAst.gen_on asts;
    print_module "a.ll" GenAst.the_module;
    Printf.printf "\027[32mIR code generation completed.\027[0m\n%!"
  with Parsing.Parse_error | _ ->
    Printf.eprintf "Syntax error\n";
    exit 1
