open Types
open Printf

let errorsRaisedCounter = ref 0
let warningsRaisedCounter = ref 0

exception File_not_found of string
exception Shared_name_func_var
exception Type_error of Types.t_type * Types.t_type
exception Syntax_error of string

let compilation_failed_msg = "Compilation process ended"
let internal_error_msg = "Internal error"
let lexing_error_msg = "Lexical error"
let syntax_error_msg = "Syntax error"
let semantic_error_msg = "Semantic error"
let type_error_msg = "Type error"
let print_error_header msg = eprintf "\027[31mError\027[0m: %s\n%!" msg

let error_report () =
  eprintf "%s with %d \027[31merrors\027[0m and %d \027[35mwarnings\027[0m.\n%!"
    compilation_failed_msg !errorsRaisedCounter !warningsRaisedCounter

let handle_error finalMsg infoMsg =
  incr errorsRaisedCounter;
  print_error_header finalMsg;
  eprintf "%s\n%!" infoMsg

let handle_error_fatal finalMsg infoMsg =
  handle_error finalMsg infoMsg;
  failwith finalMsg

let handle_warning msg =
  incr warningsRaisedCounter;
  eprintf "\027[35mWarning\027[0m: %s\n%!" msg

let handle_success msg = printf "\027[32m%s\027[0m\n%!" msg

let handle_type_error expT foundT infoMsg =
  incr errorsRaisedCounter;
  print_error_header type_error_msg;
  eprintf "%s\n%s\n%!"
    (sprintf "Expected type: %s, Encountered type: %s."
       (Types.string_of_t_type expT)
       (Types.string_of_t_type foundT))
    infoMsg
