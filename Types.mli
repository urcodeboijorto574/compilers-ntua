type t_type =
  | T_int
  | T_char
  | T_array of t_type * int
  | T_none
  | T_func of t_type

(** [string_of_t_type t] takes a [t : t_type] and returns a string corresponding
    to the type given. Returns [string]. *)
val string_of_t_type : t_type -> string

(** [construct_array_type arrayList dataType] takes two arguments,
    [arrayList : int list], which represents the dimensions of the array, and
    [dataType], which is the type of expressions that will be stored in the
    array. Returns [Types.t_type]. *)
val construct_array_type : int list -> t_type -> t_type

(** [equal_type] takes two arguments of type [t_type] and checks if they are the
    same. If they are the same, unit is returned, otherwise an exception is
    thrown. *)
val equal_type : t_type -> t_type -> unit

(** [t_type_of_dataType dt] takes an object [dt] of type [Ast.dataType] and
    returns the corresponding type. Returns [Types.t_type]. *)
val t_type_of_dataType : Ast.dataType -> t_type

(** [t_type_of_retType rt] takes an object [rt] of type [Ast.retType] and
    returns the corresponding type. Returns [Types.t_type]. *)
val t_type_of_retType : Ast.retType -> t_type

(** [t_type_of_fparType fpt] takes an object [fpt] of type [Ast.fparType] and
    returns the corresponding type. Returns [Types.t_type]. *)
val t_type_of_fparType : Ast.fparType -> t_type

(** [t_type_of_fparType vt] takes an object [vt] of type [Ast.varType] and
    returns the corresponding type. Returns [Types.t_type]. *)
val t_type_of_varType : Ast.varType -> t_type
