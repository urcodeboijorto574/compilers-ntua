open PrintAst
open SemAst

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let asts = Parser.program Lexer.lexer lexbuf in
    Printf.printf "Successful parsing.\n";
    print_on asts;
    Printf.printf "\n";
    (* sem_on asts; *)
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error\n";
    exit 1
