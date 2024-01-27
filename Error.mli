open Types

exception Shared_name_func_var
exception Overloaded_functions
exception Redefined_function
exception Expected_type_not_returned
exception Non_matching_parameter_types
exception Unexpected_number_of_parameters
exception Type_error
exception Passing_error

val type_error_msg : string

(** [handle_error msg] prints the [msg] to the standard error output. *)
val handle_error : string -> unit

(** [handle_warning msg] prints the [msg] to the standard output. *)
val handle_warning : string -> unit

(** [handle_success msg] prints the [msg] to the standard output in green. *)
val handle_success : string -> unit
