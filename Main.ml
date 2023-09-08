let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let asts = Parser.program Lexer.lexer lexbuf in
    if Types.debugMode then (
      Printf.printf "Syntactic analysis:\n";
      PrintAst.print_on asts;
      Printf.printf "\nSuccessful parsing.\n";
      Printf.printf "\n";
      Printf.printf "Semantic analysis:\n");
    SemAst.sem_on asts;
    Printf.printf "Semantically correct.\n";
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "Syntax error\n";
    exit 1
