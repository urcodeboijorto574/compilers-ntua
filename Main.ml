open Llvm
open GenAst
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
    let input_from =
      if !has_i_flag && !has_f_flag then begin
        Printf.eprintf "%s\n" usage_msg;
        exit 1
      end
      else if !has_i_flag || !has_f_flag then
        stdin
      else
        Stdlib.open_in !filename
    in
    let lexbuf = Lexing.from_channel input_from in
    (try
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
       GenAst.gen_on asts !has_o_flag
     with
    | Parsing.Parse_error ->
        Printf.printf "Syntax error\n";
        exit 1
    | _ -> exit 1);
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
      ignore (Sys.command ("mv a.ll " ^ ll_file));
      ignore (Sys.command ("mv " ^ ll_file ^ " " ^ path ^ "/"));
      ignore (Sys.command ("mv a.s " ^ asm_file));
      ignore (Sys.command ("mv " ^ asm_file ^ " " ^ path ^ "/"))
    end;
    Printf.printf "\027[32mIR code generation completed.\027[0m\n%!";
    exit 0
  with
  | Failure _ -> exit 1
  | Parsing.Parse_error ->
      Printf.printf "Syntax error\n";
      exit 1
  | _ ->
      Printf.printf "Internal error!\n";
      exit 1
