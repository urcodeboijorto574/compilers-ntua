type t_type =
  | T_int
  | T_char
  | T_array of t_type * int
  | T_none
  | T_func of t_type

let rec string_of_type = function
  | T_int -> "integer"
  | T_char -> "character"
  | T_array (t, n) ->
      String.concat "" [ "array("; string_of_int n; ") of "; string_of_type t ]
  | T_none -> "nothing"
  | T_func t -> String.concat " " [ string_of_type t; "(function)" ]

let construct_array_type dimList endType =
  let rec construct_array_type_helper counter len dimList endType =
    if counter = len then
      endType
    else
      T_array
        ( construct_array_type_helper (counter + 1) len (List.tl dimList) endType,
          List.hd dimList )
  in
  construct_array_type_helper 0 (List.length dimList) dimList endType

let rec equal_type t1 t2 =
  match (t1, t2) with
  | T_array (t1', _), T_array (t2', _) | T_func t1', T_func t2' ->
      equal_type t1' t2'
  | _ ->
      let print_types () =
        Printf.printf "Type1: %s, Type2: %s\n" (string_of_type t1)
          (string_of_type t2)
      in
      if t1 <> t2 then (
        Printf.printf "Type mismatch! ";
        print_types ();
        failwith "Type error")
      else (
        Printf.printf "Same type! ";
        print_types ())

let t_type_of_dataType = function
  | Ast.ConstInt -> T_int
  | Ast.ConstChar -> T_char

let t_type_of_retType = function
  | Ast.RetDataType dt -> t_type_of_dataType dt
  | Ast.Nothing -> T_none
