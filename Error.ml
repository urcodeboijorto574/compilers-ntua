open Types

let isErrorsRaised = ref false

exception Shared_name_func_var
exception Overloaded_functions
exception Redefined_function
exception Expected_type_not_returned
exception Non_matching_parameter_types
exception Unexpected_number_of_parameters
exception Type_error
exception Passing_error

let internal_error_msg = "Internal error"
let lexing_error_msg = "Lexing error"
let syntax_error_msg = "Syntax error"
let semantic_error_msg = "Semantic error"
let type_error_msg = "Type error"

let handle_error finalMsg infoMsg =
  if not !isErrorsRaised then isErrorsRaised := true;
  Printf.eprintf "\027[31mError\027[0m: %s:\n%s\n" finalMsg infoMsg

let handle_error_fatal finalMsg infoMsg =
  handle_error finalMsg infoMsg;
  failwith finalMsg

let handle_warning = Printf.eprintf "\027[35mWarning\027[0m: %s\n"
let handle_success = Printf.printf "\027[32m%s\027[0m\n"

let print_position_info (lexbuf : Lexing.lexbuf) =
  let open Lexing in
  let position_info : string =
    Printf.sprintf "File \"%s\", line %d, characters %d-%d: "
      lexbuf.lex_curr_p.pos_fname lexbuf.lex_curr_p.pos_lnum lexbuf.lex_last_pos
      lexbuf.lex_curr_p.pos_cnum
  in
  Printf.eprintf "%s\n" position_info
