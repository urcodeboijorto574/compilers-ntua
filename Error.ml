open Types

exception Shared_name_func_var
exception Overloaded_functions
exception Redefined_function
exception Expected_type_not_returned
exception Non_matching_parameter_types
exception Unexpected_number_of_parameters
exception Type_error
exception Passing_error

let type_error_msg = "Type error"
let handle_error msg = Printf.eprintf "\027[31mError\027[0m: %s\n" msg
let handle_warning msg = Printf.printf "\027[35mWarning\027[0m: %s\n" msg
