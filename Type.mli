type typ =
  | T_int
  | T_char
  | T_bool
  | T_array of typ * int
  | T_proc of typ

val equalType : typ -> typ -> bool
