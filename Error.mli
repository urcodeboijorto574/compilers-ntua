open Types

val errorsRaisedCounter : int ref
val warningsRaisedCounter : int ref

exception File_not_found of string
exception Shared_name_func_var
exception Type_error of Types.t_type * Types.t_type
exception Syntax_error of string

val compilation_failed_msg : string
val internal_error_msg : string
val lexing_error_msg : string
val syntax_error_msg : string
val semantic_error_msg : string
val type_error_msg : string

(** [print_error_header msg] prints "Error: " in red and then the [msg] given. *)
val print_error_header : string -> unit

(** [error_report] prints the total numbers of errors and warnings in stderr. *)
val error_report : unit -> unit

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

(** [handle_type_error expT foundT infoMsg] prints the expected type [expT] and
    the encountered type [foundT] and then the info message [infoMsg] to the
    stderr. *)
val handle_type_error : Types.t_type -> Types.t_type -> string -> unit
