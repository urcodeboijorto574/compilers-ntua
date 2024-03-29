open Llvm
open Arg
open Filename
open Parser
open Printf
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
    eprintf "%s\n" usage_msg;
    exit 1
  end;

  let isInChannelStdin = !has_i_flag || !has_f_flag in
  let inChannel =
    if isInChannelStdin then
      stdin
    else
      try open_in !filename
      with Sys_error _ -> raise (Error.File_not_found !filename)
  in

  try
    let text =
      let rec get_text_from_in_channel acc =
        try
          let line = input_line inChannel in
          get_text_from_in_channel (acc ^ line ^ "\n")
        with End_of_file ->
          close_in inChannel;
          acc
      in
      get_text_from_in_channel ""
    in
    let lexbuf = LexerUtil.init !filename (Lexing.from_string text) in
    let asts =
      try Parser.program Lexer.lexer lexbuf
      with Parser.Error -> raise (Error.Syntax_error text)
    in
    if !Error.errorsRaisedCounter = 0 then
      Error.handle_success "Successful parsing.";
    SemAst.sem_on asts;
    if !Error.errorsRaisedCounter <> 0 then failwith Error.semantic_error_msg;
    Error.handle_success "Semantically correct.";
    GenAst.gen_on asts !has_o_flag;

    print_module "a.ll" GenAst.the_module;
    let llc_command = "llc -o a.s a.ll" in
    ignore (Sys.command llc_command);
    let build_exec_command = "clang -o a.out a.s ./lib/lib.a" in
    let clangExitCode = Sys.command build_exec_command in
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
    if clangExitCode <> 0 then raise (Error.File_not_found "./lib/lib.a");
    if !Error.warningsRaisedCounter <> 0 then Error.error_report ();
    exit 0
  with
  | Error.File_not_found filename ->
      Error.handle_error "File not found"
        (sprintf "File \"%s\" not found.%s" filename
           (if filename = "./lib/lib.a" then
              "\n\
               Make sure the standard library is built before using the \
               compiler.\n\
               The final executable could not be created."
            else
              ""))
  | Assert_failure _ ->
      Error.(print_error_header internal_error_msg);
      if Error.(!errorsRaisedCounter <> 0 || !warningsRaisedCounter <> 0) then
        Error.error_report ();
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
        eprintf "%s%s%!" location message;
        exit 1
      in
      MenhirInterpreter.loop_handle
        (fun _ -> assert false)
        (fail text buffer) supplier checkpoint
  | Failure msg when msg = Error.semantic_error_msg ->
      Error.error_report ();
      exit 1
  | Failure msg ->
      Error.(print_error_header (sprintf "%s: %s\n" internal_error_msg msg));
      if !Error.errorsRaisedCounter <> 0 || !Error.warningsRaisedCounter <> 0
      then
        Error.error_report ();
      exit 1
  | e ->
      Error.(print_error_header internal_error_msg);
      eprintf "Unexpected error caught.\nException: %s\n%!"
        (Printexc.to_string e);
      exit 1
