type t_type =
  | T_int
  | T_char
  | T_array of t_type * int
  | T_none
  | T_func of t_type

(* DEBUG: delete when finished *)

(** [debugMode] is [true] when debug messages will be printed out. *)
val debugMode : bool

(** [string_of_t_type t] takes a [t : t_type] and returns a string corresponding
    to the type given. Returns [string]. *)
val string_of_t_type : t_type -> string

(** [construct_array_type arrayList dataType] takes two arguments,
    [arrayList : int list], which represents the dimensions of the array, and
    [dataType], which is the type of expressions that will be stored in the
    array. Returns [Types.t_type]. *)
val construct_array_type : int list -> t_type -> t_type

(** [equal_types] takes two arguments of type [t_type] and checks if they are
    the same. If they are the same [true] is returned, otherwise [false]. *)
val equal_types : t_type -> t_type -> bool

(** [t_type_of_t_func t] takes a type [T_func ti] type and returns the
    encapsulated type [ti]. Returns [Types.t_type] *)
val t_type_of_t_func : t_type -> t_type

(* Functions that convert types defined in Ast to t_type types are defined in
   Ast. *)
