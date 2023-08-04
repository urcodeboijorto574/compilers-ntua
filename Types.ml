type t_type =
| T_int
| T_char
| T_array of t_type * int
| T_func of t_type option

let rec equal_type t1 t2 =
  match (t1, t2) with
  | T_array (t1', _), T_array (t2', _) -> equal_type t1' t2'
  | T_func None, T_func None -> ()
  | T_func (Some t1'), T_func (Some t2') -> equal_type t1' t2'
  | _ -> if t1 <> t2 then Printf.eprintf "Type error\n"
