type passing_params =
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

and entry_parameter = {
parameter_type : Types.t_type;
passing : passing_params;
}

and entry_function = {
parameters_list : entry_parameter list;
return_type : Types.t_type;
}

(** [create_symbol_table] takes an integer as an argument and creates
    a hashtable with that initial number of buckets. *)
val create_symbol_table : int -> unit

(** [current_scope] is a variable that stores the current scope
    during the semantic analysis of the AST. *)
val current_scope : scope ref

(** [open_scope] opens a new scope during the semantic analysis of the AST.
    The current scope becomes the parent scope of the new one. *)
val open_scope : unit -> unit

(** [close_scope] closes the current scope and makes the parent scope the
    current scope. *)
val close_scope : unit -> unit
