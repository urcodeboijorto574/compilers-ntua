open Types

let isErrorsRaised = ref false

exception File_not_found of string
exception Shared_name_func_var
exception Type_error of Types.t_type * Types.t_type
exception Syntax_error of string

let compilation_failed_msg = "Compilation failed"
let internal_error_msg = "Internal error"
let lexing_error_msg = "Lexing error"
let syntax_error_msg = "Syntax error"
let semantic_error_msg = "Semantic error"
let type_error_msg = "Type error"
let print_error_header msg = Printf.eprintf "\027[31mError\027[0m: %s\n%!" msg

let handle_error finalMsg infoMsg =
  if not !isErrorsRaised then isErrorsRaised := true;
  print_error_header finalMsg;
  Printf.eprintf "%s\n%!" infoMsg

let handle_error_fatal finalMsg infoMsg =
  handle_error finalMsg infoMsg;
  failwith finalMsg

let handle_warning = Printf.eprintf "\027[35mWarning\027[0m: %s\n%!"
let handle_success = Printf.printf "\027[32m%s\027[0m\n%!"

let handle_type_error expT foundT infoMsg =
  if not !isErrorsRaised then isErrorsRaised := true;
  print_error_header type_error_msg;
  Printf.eprintf "%s\n%s\n%!"
    (Printf.sprintf "Expected type: %s, Encountered type: %s."
       (Types.string_of_t_type expT)
       (Types.string_of_t_type foundT))
    infoMsg
