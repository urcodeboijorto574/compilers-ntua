type t_type =
  | T_int
  | T_char
  | T_array of t_type * int
  | T_none
  | T_func of t_type

(* DEBUG *)
let debugMode = false

let rec string_of_t_type = function
  | T_int -> "integer"
  | T_char -> "character"
  | T_array (t, n) ->
      String.concat ""
        [ "array("; string_of_int n; ") of "; string_of_t_type t ]
  | T_none -> "nothing"
  | T_func t -> String.concat " " [ string_of_t_type t; "(function)" ]

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

let rec equal_types t1 t2 =
  match (t1, t2) with
  | T_array (t1', s1), T_array (t2', s2) ->
      if s1 = 0 || s2 = 0 then equal_types t1' t2'
  | _ ->
      if debugMode then
        Printf.printf "%s, %s -> " (string_of_t_type t1) (string_of_t_type t2);
      if t1 <> t2 then (
        Printf.eprintf "Error: Type mismatch!\n";
        failwith "Type error")
      else if debugMode then
        Printf.printf "Same type!\n"

let t_type_of_dataType = function
  | Ast.ConstInt -> T_int
  | Ast.ConstChar -> T_char

let t_type_of_retType = function
  | Ast.RetDataType dt -> t_type_of_dataType dt
  | Ast.Nothing -> T_none

let t_type_of_fparType : Ast.fparType -> t_type = function
  | { data_type = dt; array_dimensions = dimList } ->
      construct_array_type dimList (t_type_of_dataType dt)

let t_type_of_varType : Ast.varType -> t_type = function
  | { data_type = dt; array_dimensions = dimList } ->
      construct_array_type dimList (t_type_of_dataType dt)
