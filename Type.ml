type typ =
  | T_int
  | T_char
  | T_bool
  | T_array of typ * int
  | T_proc of typ

let rec equalType t1 t2 =
  match (t1, t2) with
  | T_array (t1', _), T_array (t2', _) -> equalType t1' t2'
  | _ -> t1 = t2
