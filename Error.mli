open Types

exception Shared_name_func_var
exception Overloaded_functions
exception Redefined_function
exception Expected_type_not_returned
exception Non_matching_parameter_types
exception Unexpected_number_of_parameters
exception Type_error
exception Passing_error

val internal_error_msg : string
val lexing_error_msg : string
val syntax_error_msg : string
val type_error_msg : string

(** [handle_error finalMsg infoMsg] prints the [infoMsg] to the standard error
    output.
    @raise (Failure finalMsg) (* always *) *)
val handle_error : string -> string -> 'a

(** [handle_warning msg] prints the [msg] to the standard output. *)
val handle_warning : string -> unit

(** [handle_success msg] prints the [msg] to the standard output in green. *)
val handle_success : string -> unit

(** [print_position_info lexbuf] *)
val print_position_info : Lexing.lexbuf -> unit
