let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    Parser.program Lexer.lexer lexbuf;
    Printf.printf("Successful parsing\n");
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error\n";
    exit 1