type t_type =
  | T_int
  | T_char
  | T_array of int * t_type
  | T_none
  | T_func of t_type

let rec string_of_t_type = function
  | T_int -> "integer"
  | T_char -> "character"
  | T_array (n, t) -> Printf.sprintf "array(%d) of %s" n (string_of_t_type t)
  | T_none -> "nothing"
  | T_func t -> string_of_t_type t ^ " (function)"

let construct_array_type dimList endType =
  let rec helper cnt len dl =
    if cnt = len then
      endType
    else
      T_array (List.hd dl, helper (cnt + 1) len (List.tl dl))
  in
  match endType with
  | T_int | T_char -> helper 0 (List.length dimList) dimList
  | _ -> failwith "Can't construct array of non-integers and non-characters"

let rec dimensions_list_of_t_array = function
  | T_func _ | T_none -> failwith "t_type is T_func or T_none"
  | T_int | T_char -> []
  | T_array (n, t) -> n :: dimensions_list_of_t_array t

let rec equal_types t1 t2 =
  match (t1, t2) with
  | T_array (s1, t1'), T_array (s2, t2') ->
      if s1 = -1 || s2 = -1 then true else equal_types t1' t2'
  | _ -> t1 = t2

let get = function
  | T_array (_, t) -> t
  | T_func t -> t
  | T_int | T_char | T_none -> failwith "t_type is not an encapsulated t_type"

let rec join = function
  | T_func _ | T_none -> failwith "t_type is not an array"
  | T_array (_, t) -> join t
  | t -> t
