type param_passing =
| BY_VALUE
| BY_REFERENCE

and scope = {
parent : scope option;
mutable symbol_entries : entry list;
}

and entry = {
id : string;
scope : scope;
mutable kind : entry_kind;
}

and entry_kind =
| ENTRY_none
| ENTRY_variable of entry_variable
| ENTRY_function of entry_function
| ENTRY_parameter of entry_parameter

and entry_variable = {
variable_type : Types.t_type;
variable_array_size : int option;
}

and entry_function = {
parameters_list : entry_parameter list;
return_type : Types.t_type;
}

and entry_parameter = {
parameter_type : Types.t_type;
mutable parameter_array_size : int option;
passing : param_passing;
}

val create_symbol_table : int -> unit
(** [create_symbol_table] takes an integer as an argument and creates
    a hashtable with that initial number of buckets. *)

val current_scope : scope ref
(** [current_scope] is a variable that stores the current scope
    during the semantic analysis of the AST. *)

val open_scope : unit -> unit
(** [open_scope] opens a new scope during the semantic analysis of the AST.
    The current scope becomes the parent scope of the new one. *)

val close_scope : unit -> unit
(** [close_scope] closes the current scope and makes the parent scope the current scope. *)

(*
   val enter_entry : string -> entry_kind -> unit
   (** [enter_entry] takes the identifier of type [string] and the [entry_kind] of the entry. *)
*)

val enter_variable : string -> Types.t_type -> int option -> unit
(** [enter_variable] *)

val enter_function :
  string ->
  (Types.t_type * int option * param_passing) list ->
  Types.t_type option ->
  unit
(** [enter_function] takes 3 arguments:
    - the 1st argument is the name of the function of type [string],
    - the 2nd argument is a list of pairs. The 2 fields of each pair are:
      · the type of the parameter
      · the type of passing of the parameter.
    - the 3rd argument is the return type of the function of type [Types.t_type option] *)

val look_up_entry : string -> entry option
(** [look_up_entry] takes the name of an identifier and returns the entry found. *)
