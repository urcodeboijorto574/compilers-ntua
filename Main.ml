open Llvm
open Arg
open Filename
open Parser
module LexerUtil = MenhirLib.LexerUtil

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

  if !has_i_flag && !has_f_flag then begin
    Printf.eprintf "%s\n" usage_msg;
    exit 1
  end;

  let isInChannelStdin = !has_i_flag || !has_f_flag in

  try
    let text, lexbuf =
      if isInChannelStdin then
        ("", Lexing.from_channel stdin)
      else
        LexerUtil.read !filename
    in
    Lexing.set_filename lexbuf
      (if isInChannelStdin then "stdin" else basename !filename);
    let asts =
      try Parser.program Lexer.lexer lexbuf
      with Parser.Error -> raise (Error.Syntax_error text)
    in
    Error.handle_success "Successful parsing.";
    SemAst.sem_on asts;
    Error.handle_success "Semantically correct.";
    if not !Error.isErrorsRaised then
      GenAst.gen_on asts !has_o_flag
    else
      failwith Error.semantic_error_msg;

    print_module "a.ll" GenAst.the_module;
    let llc_command = "llc -o a.s a.ll" in
    ignore (Sys.command llc_command);
    let build_exec_command = "clang -o a.out a.s ./lib/lib.a" in
    ignore (Sys.command build_exec_command);
    if isInChannelStdin then begin
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
  | Sys_error _ ->
      Error.handle_error_fatal "File not found"
        (Printf.sprintf "File '%s' not found." !filename)
  | Assert_failure _ ->
      (if !Error.isErrorsRaised then
         Printf.eprintf "%s.\n" Error.compilation_failed_msg
       else
         Error.(print_error_header internal_error_msg));
      exit 1
  | Error.Syntax_error _ when isInChannelStdin -> (
      try Error.(handle_error_fatal syntax_error_msg syntax_error_msg)
      with Failure _ -> exit 1)
  | Error.Syntax_error text ->
      Error.(print_error_header syntax_error_msg);
      exit 1
  | Failure msg when msg = Error.semantic_error_msg ->
      Printf.eprintf "%s.\n" Error.compilation_failed_msg;
      exit 1
  | Failure _ -> exit 1
  | e -> (
      try Error.(handle_error internal_error_msg "Unexpected error caught.")
      with Failure _ ->
        Printf.eprintf "Exception: %s\n" (Printexc.to_string e);
        exit 1)
