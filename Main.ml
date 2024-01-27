open Llvm
open Arg
open Filename
open Parser

let main =
  let has_o_flag = ref false in
  let has_f_flag = ref false in
  let has_i_flag = ref false in
  let filename = ref "" in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [-O] [-i | -f] filename" in

  let speclist =
    [
      ("-O", Set has_o_flag, "Optimizaztion flag");
      ("-f", Set has_f_flag, "Read from stdin, assembly code to stdout");
      ("-i", Set has_i_flag, "Read from stdin, intermediate code to stdout");
    ]
  in

  Arg.parse speclist (fun s -> filename := s) usage_msg;

  try
    let in_channel =
      if !has_i_flag && !has_f_flag then begin
        Printf.eprintf "%s\n" usage_msg;
        exit 1
      end
      else if !has_i_flag || !has_f_flag then
        stdin
      else
        try Stdlib.open_in !filename
        with _ -> Error.handle_error ("File '" ^ !filename ^ "' not found.")
    in
    let lexbuf = Lexing.from_channel in_channel in
    Lexing.set_filename lexbuf
      (if in_channel = stdin then "stdin" else basename !filename);
    if Types.debugMode then Printf.printf "Syntactic analysis:\n";
    let asts =
      try Parser.program Lexer.lexer lexbuf
      with Parsing.Parse_error ->
        Error.handle_error "Syntax error";
        exit 1
    in
    if Types.debugMode then (
      PrintAst.print_on asts;
      Printf.printf "\n");
    Error.handle_success "Successful parsing.";
    if Types.debugMode then (
      Printf.printf "\n";
      Printf.printf "Semantic analysis:\n");
    SemAst.sem_on asts;
    if Types.debugMode then Printf.printf "\n";
    Error.handle_success "Semantically correct.";
    GenAst.gen_on asts !has_o_flag;

    print_module "a.ll" GenAst.the_module;
    let llc_command = "llc -o a.s a.ll" in
    ignore (Sys.command llc_command);
    let build_exec_command = "clang -o a.out a.s ./lib/lib.a" in
    ignore (Sys.command build_exec_command);
    if !has_i_flag || !has_f_flag then begin
      let fileToPrint = if !has_i_flag then "a.ll" else "a.s" in
      ignore (Sys.command ("cat " ^ fileToPrint));
      let deleteCommand = "rm a.ll a.s" in
      ignore (Sys.command deleteCommand)
    end
    else begin
      let path = dirname !filename in
      let filename = chop_extension (basename !filename) in
      let ll_file = filename ^ ".imm" in
      let asm_file = filename ^ ".asm" in
      ignore (Sys.command ("mv a.ll " ^ path ^ "/" ^ ll_file));
      ignore (Sys.command ("mv a.s " ^ path ^ "/" ^ asm_file))
    end;
    Error.handle_success "IR code generation completed.";
    exit 0
  with
  | Failure _ -> exit 1
  | Assert_failure _ ->
      Error.handle_error "Internal error";
      exit 1
  | _ -> exit 1
