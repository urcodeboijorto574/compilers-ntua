type t_type =
  | T_int
  | T_char
  | T_array of t_type
  | T_func of t_type option

let rec equal_type t1 t2 =
  match (t1, t2) with
  | T_array t1', T_array t2' -> equal_type t1' t2'
  | T_func None, T_func None -> ()
  | T_func (Some t1'), T_func (Some t2') -> equal_type t1' t2'
  | _ ->
      let rec string_of_type = function
        | T_int -> "integer"
        | T_char -> "character"
        | T_func None -> "nothing (function)"
        | T_func (Some t) ->
            String.concat " " [ string_of_type t; "(function)" ]
        | T_array t -> String.concat " " [ "array of"; string_of_type t ]
      in
      if t1 <> t2 then (
        Printf.printf "Type mismatch! ";
        Printf.printf "Type1: %s, Type2: %s\n" (string_of_type t1)
          (string_of_type t2);
        failwith "Type error")
      else
        Printf.printf "Same type! ";
      Printf.printf "Type1: %s, Type2: %s\n" (string_of_type t1)
        (string_of_type t2)
