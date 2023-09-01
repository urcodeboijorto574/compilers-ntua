type param_passing =
  | BY_VALUE
  | BY_REFERENCE

and scope = {
  name : string;
  parent : scope option;
  mutable scope_entries : entry list;
}

and entry = {
  id : string;
  scope : scope;
  mutable kind : entry_kind;
}

and entry_kind =
  | ENTRY_variable of entry_variable
  | ENTRY_function of entry_function
  | ENTRY_parameter of entry_parameter

and entry_variable = { variable_type : Types.t_type }

and entry_function = {
  parameters_list : entry_parameter list;
  return_type : Types.t_type;
}

and entry_parameter = {
  parameter_type : Types.t_type;
  passing : param_passing;
}

(** [current_scope] is a variable that stores the current scope during the
    semantic analysis of the AST. *)
val current_scope : scope ref

(** [open_scope] opens a new scope during the semantic analysis of the AST.
    The current scope becomes the parent scope of the new one. Takes one
    arguement of type [string], that is the name of the function in which the
    scope belongs. *)
val open_scope : string -> unit

(** [close_scope] closes the current scope and makes the parent scope the
    current scope. *)
val close_scope : unit -> unit

(** [create_symbol_table n] initializes the symbolTable as a Hashtbl with [n]
    number of buckets. It also initializes the [current_scope]. *)
val create_symbol_table : int -> unit

(** [enter_variable i t] takes an identifier [i] and a type [t] and creates a
    new [entry_variable] that is then entered in the symbolTable. *)
val enter_variable : string -> Types.t_type -> unit

(** [enter_parameter i t isRef] enters a parameter in the symbolTable.
    [i] is the parameter's identifier, [t] is the parameter's type and [isRef]
    [true] if the parameter is passed by reference or [false] if by value. *)
val enter_parameter : string -> Types.t_type -> bool -> unit

(** [enter_function i pL rt] enters a new function entry in the symbolTable.
    [i] is the function's identifier, [rt] is the return type of the function
    and [pL] is a list of type [(int * Types.t_type * param_passing)
    list]. Each element of the [pL] is a tuple with 3 fields.
    The first field signifies the number of parameters in each
    parameter definition, the second the type, and the third the kind of
    parameter passing. *)
val enter_function :
  string -> (int * Types.t_type * param_passing) list -> Types.t_type -> unit

(** [look_up_entry_temp id] searches in the symbolTable an entry with [id] as
    the key. TODO: must be deleted after debugging completes *)
val look_up_entry_temp : string -> entry option

(** [look_up_entry id] searches in the symbolTable an entry with [id] as the key.
    It returns the entry found. If no entry is found, [Not_found] exception is
    thrown. *)
val look_up_entry : string -> entry
