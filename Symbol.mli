(** [lib_function_names] contains all the names of the standard library
    functions *)
val lib_function_names : string list

type param_passing =
  | BY_VALUE
  | BY_REFERENCE

and scope = {
  name : string;
  parent : scope option;
  depth : int;
}

and entry = {
  id : string;
  scope : scope;
  mutable kind : entry_kind;
  mutable is_used : bool;
}

and entry_kind =
  | ENTRY_variable of entry_variable
  | ENTRY_function of entry_function
  | ENTRY_parameter of entry_parameter

and entry_variable = {
  variable_type : Types.t_type;
  mutable is_initialized : bool;
}

and entry_parameter = {
  parameter_type : Types.t_type;
  passing : param_passing;
}

and entry_func_state =
  | DECLARED
  | DEFINED

and entry_function = {
  parameters_list : entry_parameter list;
  (* [return_type] is encapsulated in [Types.T_func]. *)
  return_type : Types.t_type;
  scope_depth : int;
  mutable state : entry_func_state;
}

(** [set_func_defined entryFunc] sets the state of [entryFunc] to [DEFINED]. *)
val set_func_defined : entry_function -> unit

(** [set_entry_isUsed e] sets the field 'is_used' of [e] to [true]. *)
val set_entry_isUsed : entry -> unit

(** [set_var_isInitialized ev] sets the field 'is_initialized' of [ev] to
    [true]. *)
val set_var_isInitialized : entry_variable -> unit

(** [initialScopeDepthValue] is the depth of the scope in which the root
    function exists. *)
val initialScopeDepthValue : int

(** [current_scope] is a variable that stores the current scope during the
    semantic analysis of the AST. *)
val current_scope : scope ref

(** [open_scope] opens a new scope during the semantic analysis of the AST. The
    current scope becomes the parent scope of the new one. Takes one arguement
    of type [string], that is the name of the function in which the scope
    belongs. *)
val open_scope : string -> unit

(** [close_scope] closes the current scope and makes the parent scope the
    current scope.
    @raise Failure if the current scope is the scope of the root function. *)
val close_scope : unit -> unit

(** [equal_scopes] checks whether the 2 scopes given as arguments are the same
    or not, [true] if they are, [false] if they are not. *)
val equal_scopes : scope -> scope -> bool

(** [different_scopes] produces the opposite result of [Symbol.equal_scopes]. *)
val different_scopes : scope -> scope -> bool

(** [create_symbol_table n] initializes the symbolTable as a Hashtbl with [n]
    number of buckets. It also initializes the [current_scope]. *)
val create_symbol_table : int -> unit

(** [enter_variable i t] takes an identifier [i] and a type [t] and creates a
    new [entry_variable] that is then entered in the symbolTable. *)
val enter_variable : string -> Types.t_type -> unit

(** [enter_parameter i t isRef] enters a parameter in the symbolTable. [i] is
    the parameter's identifier, [t] is the parameter's type and [isRef] [true]
    if the parameter is passed by reference or [false] if by value. *)
val enter_parameter : string -> Types.t_type -> bool -> unit

(** [enter_function i pL rt st] enters a new function entry in the symbolTable.
    [i] is the function's identifier ([string]), [rt] is the return type of the
    function, [pL] is a list of type [(int * Types.t_type * param_passing) list]
    that contains information about the parameters and [st] is of type
    [entry_func_state] and is [DECLARED] if the entry of the function originated
    from a declaration and [DEFINED] if it originated from a definition. Each
    element of the [pL] is a tuple with 3 fields. The first field signifies the
    number of parameters in each parameter definition, the second the type, and
    the third the kind of parameter passing. *)
val enter_function :
  string ->
  (int * Types.t_type * param_passing) list ->
  Types.t_type ->
  entry_func_state ->
  unit

(** [add_standard_library] inserts to the symbolTable all the functions of the
    standard library of Grace. *)
val add_standard_library : unit -> unit

(** [look_up_entry id] searches in the symbolTable an entry with [id] as the
    key. It returns the entry found. If no entry is found, [Not_found] exception
    is thrown. *)
val look_up_entry : string -> entry option

(** [get_unused_entries] returns a list of the unused names. *)
val get_unused_entries : unit -> string list
