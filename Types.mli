type t_type =
| T_int
| T_char
| T_array of t_type * int
| T_func of t_type option

val equal_type : t_type -> t_type -> unit
val check_type_int : t_type -> unit
