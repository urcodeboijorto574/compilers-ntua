open Types

val isErrorsRaised : bool ref

exception Shared_name_func_var
exception Unexpected_number_of_parameters
exception Type_error
exception Passing_error
exception Syntax_error of string

val compilation_failed_msg : string
val internal_error_msg : string
val lexing_error_msg : string
val syntax_error_msg : string
val semantic_error_msg : string
val type_error_msg : string

(** [print_error_header msg] prints "Error: " in red and then the [msg] given. *)
val print_error_header : string -> unit

(** [handle_error finalMsg infoMsg] prints the [infoMsg] to the standard error
    output. *)
val handle_error : string -> string -> unit

(** [handle_error_fatal finalMsg infoMsg] prints the [infoMsg] to the standard
    error output.
    @raise (Failure finalMsg) (* always *) *)
val handle_error_fatal : string -> string -> 'a

(** [handle_warning msg] prints the [msg] to the standard output. *)
val handle_warning : string -> unit

(** [handle_success msg] prints the [msg] to the standard output in green. *)
val handle_success : string -> unit
