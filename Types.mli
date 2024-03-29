type t_type =
  | T_int
  | T_char
  | T_array of int * t_type
  | T_none
  | T_func of t_type

(** [string_of_t_type t] takes a [t : t_type] and returns a string corresponding
    to the type given. *)
val string_of_t_type : t_type -> string

(** [construct_array_type arrayList dataType] takes two arguments,
    [arrayList : int list], which represents the dimensions of the array, and
    [dataType], which is the type of expressions that will be stored in the
    array.
    @raise Failure if [dataType] is not [T_int] and [T_char]. *)
val construct_array_type : int list -> t_type -> t_type

(** [dimensions_list_of_t_array t] converts the type [t] to a dimensions list
    (it is the reverse operation of construct_array_type).
    @raise Failure if [t] is [T_func] or [T_none]. *)
val dimensions_list_of_t_array : t_type -> int list

(** [equal_types] takes two arguments of type [t_type] and checks if they are
    the same. If they are the same [true] is returned, otherwise [false]. *)
val equal_types : t_type -> t_type -> bool

(** [get t] is [t_in] if [t] is [T_func t_in] or [T_array (_, t_in)].
    @raise Failure if [t] is [T_int], [T_char] or [T_none]. *)
val get : t_type -> t_type

(** [join t] returns the type of data that an array stores. The only types that
    can be returned are [T_int] and [T_char].
    @raise Failure
      if the argument is neither of [T_array], [T_int] and [T_char]. *)
val join : t_type -> t_type
