let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let asts = Parser.program Lexer.lexer lexbuf in
    Printf.printf "Successful parsing.\n";
    PrintAst.print_on asts;
    Printf.printf "\n";
    SemAst.sem_on asts;
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "Syntax error\n";
    exit 1
