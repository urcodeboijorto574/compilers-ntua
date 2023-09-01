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
  | ENTRY_none
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

(** [create_symbol_table] takes an integer as an argument and creates a
    hashtable with that initial number of buckets. *)
val create_symbol_table : int -> unit

(** [current_scope] is a variable that stores the current scope during the
    semantic analysis of the AST. *)
val current_scope : scope ref

(** [open_scope] opens a new scope during the semantic analysis of the AST.
    The current scope becomes the parent scope of the new one. *)
val open_scope : string -> unit

(** [close_scope] closes the current scope and makes the parent scope the
    current scope. *)
val close_scope : unit -> unit

(** [enter_variable] takes the variable's name [string] and its type [Types.t_type]
    and inserts it as an entry in the SymbolTable. [unit] is returned. *)
val enter_variable : string -> Types.t_type -> unit

(** [enter_function id fparDefList rt] takes 3 arguments:
    - [id] is the name of the function of type [string].
    - [fparDefList] is a list of tuples with 3 elements each. The 1st element
      corresponds to the number of parameters in a single parameter definition.
      The 2nd element is the type of the parameters of type [Types.t_type]. The
      3rd element is the way the parameters are passed of type [Symbol.param_passing].
    - [rt] is the return type of the function of type [Types.t_type option].
    Then it inserts the function as an entry in the SymbolTable. *)
val enter_function :
  string -> (int * Types.t_type * param_passing) list -> Types.t_type -> unit

(* val enter_parameter : string -> Types.t_type -> int list -> bool -> unit
   (** [enter_parameter] takes the parameter's name [string], its type
       [Types.t_type], the array_size [int], if the parameter is an array and
       whether or not is is passed by reference or by value ([true] if it's passed
       by reference) and inserts it in the SymbolTable. [unit] is returned. *) *)

(** [look_up_entry] takes the name of an identifier and returns the entry found. *)
val look_up_entry_temp : string -> entry option
