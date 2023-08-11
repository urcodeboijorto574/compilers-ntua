type param_passing =
| BY_VALUE
| BY_REFERENCE

and scope = {
parent : scope option;
mutable scope_entries : entry list;
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
variable_array_size : int list;
}

and entry_function = {
parameters_list : entry_parameter list;
return_type : Types.t_type;
}

and entry_parameter = {
parameter_type : Types.t_type;
mutable parameter_array_size : int list;
passing : param_passing;
}

(** [create_symbol_table] takes an integer as an argument and creates a
    hashtable with that initial number of buckets. *)
val create_symbol_table : int -> unit

(** [current_scope] is a variable that stores the current scope during the
    semantic analysis of the AST. *)
val current_scope : scope ref

(** [open_scope] opens a new scope during the semantic analysis of the AST.
    The current scope becomes the parent scope of the new one. *)
val open_scope : unit -> unit

(** [close_scope] closes the current scope and makes the parent scope the
    current scope. *)
val close_scope : unit -> unit

(** [enter_variable] takes the variable's name [string], its type [Types.t_type]
    and the array_size [int], if the variable is an array and inserts it as an
    entry in the SymbolTable. [unit] is returned. *)
val enter_variable : string -> Types.t_type -> int list -> unit

(** [enter_function] takes 3 arguments:
    - the 1st argument is the name of the function of type [string],
    - the 2nd argument is a list of [int * triple]. The integer
      corresponds to the number of parameters with the same passing type
      and type. The 3 fields of each triple are:
      · the type of the parameter
      · the array dimensions
      · the type of passing of the parameter.
    - the 3rd argument is the return type of the function of type
      [Types.t_type option],
    Then it inserts the function as an entry in the SymbolTable. *)
val enter_function :
  string ->
  (int * (Types.t_type * int list * param_passing)) list ->
  Types.t_type ->
  unit

(* val enter_parameter : string -> Types.t_type -> int list -> bool -> unit
   (** [enter_parameter] takes the parameter's name [string], its type
       [Types.t_type], the array_size [int], if the parameter is an array and
       whether or not is is passed by reference or by value ([true] if it's passed
       by reference) and inserts it in the SymbolTable. [unit] is returned. *) *)

(** [look_up_entry] takes the name of an identifier and returns the entry found.
    *)
val look_up_entry : string -> entry option

(* The functions below are written just for testing purposes.
   Could be deleted when the semantic analysis is done. *)

(** [scope_name] contains the function's name whose scope is currently the
    current_scope as its head. *)
val scope_name : string list ref

(** [add_scope_name] updates the scope_name with the new one. *)
val add_scope_name : string -> unit

(** [rem_scope_name] removes the lastly added scope_name. *)
val rem_scope_name : unit -> unit
