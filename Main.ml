open Llvm
open Arg
open Filename
open Parser
module LexerUtil = MenhirLib.LexerUtil
module ErrorReports = MenhirLib.ErrorReports
module MenhirInterpreter = UnitActionsParser.MenhirInterpreter

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
  let inChannel = if isInChannelStdin then stdin else open_in !filename in

  try
    let text =
      let rec get_text_from_in_channel acc =
        try
          let line = input_line inChannel in
          get_text_from_in_channel (acc ^ line ^ "\n")
        with End_of_file -> acc
      in
      get_text_from_in_channel ""
    in
    let lexbuf = LexerUtil.init !filename (Lexing.from_string text) in
    let asts =
      try Parser.program Lexer.lexer lexbuf
      with Parser.Error -> raise (Error.Syntax_error text)
    in
    if not !Error.isErrorsRaised then
      Error.handle_success "Successful parsing.";
    SemAst.sem_on asts;
    if not !Error.isErrorsRaised then (
      Error.handle_success "Semantically correct.";
      GenAst.gen_on asts !has_o_flag)
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
  | Error.Syntax_error text ->
      Error.(print_error_header syntax_error_msg);
      let lexbuf = LexerUtil.init !filename (Lexing.from_string text) in
      let buffer, supplier =
        MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.lexer lexbuf
        |> ErrorReports.wrap_supplier
      in
      let checkpoint =
        UnitActionsParser.Incremental.program lexbuf.lex_curr_p
      in
      let fail text buffer (checkpoint : _ MenhirInterpreter.checkpoint) =
        (* [env checkpoint] extracts a parser environment out of [checkpoint]
            checkpoint, which must be of the form [HandlingError env]. *)
        let env checkpoint : 'a MenhirInterpreter.env =
          match checkpoint with
          | MenhirInterpreter.HandlingError env -> env
          | _ -> assert false
        in
        (* Indicate where in the input file the error occurred. *)
        let location = LexerUtil.range (ErrorReports.last buffer) in
        (* Fetch an error message from the database and expand away the $i
            keywords that might appear in the message. *)
        let message =
          (* [state checkpoint] extracts the number of the current state out of
              a checkpoint [checkpoint]. *)
          let state checkpoint : int =
            match MenhirInterpreter.top (env checkpoint) with
            | Some (MenhirInterpreter.Element (s, _, _, _)) ->
                MenhirInterpreter.number s
            | None (* The parser is in its initial state, so we return 0. *) ->
                0
          in
          (* [get text checkpoint i] extracts and shows the range of the input
             text that corresponds to the [i]-th stack cell. The top stack cell
             is numbered zero. *)
          let get text checkpoint i : string =
            (* [show text (pos1, pos2)] displays a range of the input text [text]
                delimited by the positions [pos1] and [pos2]. *)
            let show text positions : string =
              ErrorReports.extract text positions
              |> ErrorReports.sanitize |> ErrorReports.compress
              |> ErrorReports.shorten 20 (* max width 43 *)
            in
            match MenhirInterpreter.get i (env checkpoint) with
            | Some (MenhirInterpreter.Element (_, _, pos1, pos2)) ->
                show text (pos1, pos2)
            | None ->
                (* The index is out of range. This should not happen if [$i]
                    keywords are correctly inside the syntax error message
                    database. The integer [i] should always be a valid offset
                    into the known suffix of the stack. Keep in mind that the
                    numbering goes from right to left. *)
                "???"
          in
          ParserMessages.message (state checkpoint)
          |> ErrorReports.expand (get text checkpoint)
        in
        Printf.eprintf "%s%s%!" location message;
        exit 1
      in
      MenhirInterpreter.loop_handle
        (fun _ -> assert false)
        (fail text buffer) supplier checkpoint
  | Failure msg when msg = Error.semantic_error_msg ->
      Printf.eprintf "%s.\n" Error.compilation_failed_msg;
      exit 1
  | Failure _ -> exit 1
  | e -> (
      try Error.(handle_error internal_error_msg "Unexpected error caught.")
      with Failure _ ->
        Printf.eprintf "Exception: %s\n" (Printexc.to_string e);
        exit 1)
