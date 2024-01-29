open Llvm
open Arg
open Filename
open Parser

let print_lexbuf : Lexing.lexbuf -> unit = function
  | {
      refill_buff (* lexbuf -> unit *) = rb;
      lex_buffer (* bytes *) = lb;
      lex_buffer_len (* int *) = lbl;
      lex_abs_pos (* int *) = lap;
      lex_start_pos (* int *) = lsp;
      lex_curr_pos (* int *) = lcp;
      lex_last_pos (* int *) = llp;
      lex_last_action (* int *) = lla;
      lex_eof_reached (* bool *) = ler;
      lex_mem (* int array *) = lm;
      lex_start_p (* position *) = lspPos;
      lex_curr_p (* position *) = lcpPos;
    } ->
      let string_of_int_array intArray =
        let elements =
          Array.fold_left
            (fun acc integer ->
              String.concat " " [ acc; string_of_int integer ])
            "" intArray
        in
        Printf.sprintf "[|%s |]" elements
      in
      let string_of_position (pos : Lexing.position) =
        Printf.sprintf
          "{ pos_fname = %s; pos_lnum = %d; pos_bol = %d; pos_cnum = %d; }"
          pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum
      in

      Printf.printf " lexbuf = {\n";

      Printf.printf "\tlex_buffer = \n>>>>>>>>\n%s\n<<<<<<<<\n"
        (Bytes.to_string lb);
      Printf.printf "\tlex_buffer_len = %d\n" lbl;
      Printf.printf "\tlex_abs_pos = %d\n" lap;
      Printf.printf "\tlex_start_pos = %d\n" lsp;
      Printf.printf "\tlex_curr_pos = %d\n" lcp;
      Printf.printf "\tlex_last_pos = %d\n" llp;
      Printf.printf "\tlex_last_action = %d\n" lla;
      Printf.printf "\tlex_eof_reached = %b\n" ler;
      Printf.printf "\tlex_mem = %s\n" (string_of_int_array lm);
      Printf.printf "\tlex_start_p = %s\n" (string_of_position lspPos);
      Printf.printf "\tlex_curr_p = %s\n" (string_of_position lcpPos);

      Printf.printf "}\n"

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
        with _ ->
          Error.handle_error_fatal "File not found"
            ("File '" ^ !filename ^ "' not found.")
    in
    let lexbuf = Lexing.from_channel in_channel in
    Lexing.set_filename lexbuf
      (if in_channel = stdin then "stdin" else basename !filename);
    if Types.debugMode then Printf.printf "Syntactic analysis:\n";
    let asts =
      try Parser.program Lexer.lexer lexbuf
      with Parser.Error ->
        Error.(handle_error_fatal syntax_error_msg syntax_error_msg)
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
    if not !Error.isErrorsRaised then
      GenAst.gen_on asts !has_o_flag
    else
      failwith Error.semantic_error_msg;

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
  | Assert_failure _ -> (
      try Error.(handle_error internal_error_msg internal_error_msg)
      with Failure _ -> exit 1)
  | Failure msg when msg = Error.semantic_error_msg ->
      Printf.eprintf "The final executable could not be created.\n";
      exit 1
  | Failure _ -> exit 1
  | e -> (
      try Error.(handle_error internal_error_msg "Unexpected error caught")
      with Failure _ ->
        Printf.eprintf "Exception: %s\n" (Printexc.to_string e);
        exit 1)
