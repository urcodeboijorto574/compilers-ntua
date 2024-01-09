type t_type =
  | T_int
  | T_char
  | T_array of t_type * int
  | T_none
  | T_func of t_type

(* DEBUG: delete when finished *)

(** [debugMode] is [true] when debug messages will be printed out. *)
val debugMode : bool

val debugModeCodeGen : bool
val debugModeI10 : bool

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

(** [t_type_of_t_func t] takes a [T_func ti] type and returns the encapsulated
    type [ti]. Returns [Types.t_type]. *)
val t_type_of_t_func : t_type -> t_type

(** [t_type_of_t_array t] takes a [T_array (ti, size)] type and returns the
    encapsulated type [ti]. Returns [Types.t_type]. *)
val t_type_of_t_array : t_type -> t_type

(** [final_t_type_of_t_array t] returns the type of data that an array stores.
    The only types that can be returned are [T_int] and [T_char]. Raises
    [Invalid_argument] if the argument is not of [T_array] or [T_int] and
    [T_char]. Returns [Types.t_type]. *)
val final_t_type_of_t_array : t_type -> t_type

(* Functions that convert types defined in Ast to t_type types are defined in
   Ast. *)
