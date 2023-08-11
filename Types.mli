type t_type =
  | T_int
  | T_char
  | T_array of t_type
  | T_func of t_type option

(** [equal_type] takes two arguments of type [t_type] and checks
    if they are the same. If they are the same, unit is returned,
    otherwise an exception is thrown. *)
val equal_type : t_type -> t_type -> unit
