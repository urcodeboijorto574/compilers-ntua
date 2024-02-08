open Llvm

type arithmOperator =
  | O_plus
  | O_minus
  | O_mul
  | O_div
  | O_mod

and sign =
  | O_plus
  | O_minus

and logOperator =
  | O_and
  | O_or
  | O_not

and compOperator =
  | O_equal
  | O_less
  | O_greater
  | O_less_eq
  | O_greater_eq
  | O_not_equal

and stackFrame = {
  stack_frame_type : Llvm.lltype;
  (* [access_link] is a pointer lltype that points to the stack frame of its
     parents' stack frame type *)
  access_link : Llvm.lltype option;
  (* [stack_frame_addr] is the allocated memory for the stack frame *)
  mutable stack_frame_addr : Llvm.llvalue option;
  (* [al_par_var_records] is a list of tuples with 4 fields each:
      1st field: the name of the variable
      2nd field: the position of the record in the stack frame
      3rd field: the variable is reference (only for parameters)
      4th field: the variable is an array *)
  al_par_var_records : (string * int * bool * bool) list;
  (* [stack_frame_length] is the numbers of fields that the struct has *)
  stack_frame_length : int;
}

and funcDef = {
  header : header;
  local_def_list : localDef list;
  block : stmt list;
  mutable parent_func : funcDef option;
  mutable stack_frame : stackFrame option;
}

and header = {
  id : string;
  mutable comp_id : string;
  fpar_def_list : fparDef list;
  ret_type : retType;
}

and retType =
  | RetDataType of dataType
  | Nothing

and fparDef = {
  ref : bool;
  id_list : string list;
  fpar_type : fparType;
}

and fparType = {
  data_type : dataType;
  (* [array_dimensions] has -1 as its head when the
     1st dimension of the array is not of fixed size *)
  array_dimensions : int list;
}

and dataType =
  | ConstInt
  | ConstChar

and localDef =
  | L_funcDef of funcDef
  | L_funcDecl of funcDecl
  | L_varDef of varDef

and funcDecl = {
  header : header;
  mutable func_def : funcDef option;
  mutable is_redundant : bool;
}

and varDef = {
  id_list : string list;
  var_type : varType;
}

and varType = {
  data_type : dataType;
  array_dimensions : int list;
}

and stmt =
  | S_assignment of lvalue * expr
  | S_block of stmt list
  | S_func_call of funcCall
  | S_if of cond * stmt
  | S_if_else of cond * stmt * stmt
  | S_while of cond * stmt
  | S_return of expr option
  | S_semicolon

and lvalue = {
  lv_kind : lvalue_kind;
  mutable lv_type : lvalue_type option;
}

and lvalue_kind =
  | L_id of string
  | L_string of string
  | L_comp of lvalue_kind * expr

and lvalue_type = {
  elem_type : Types.t_type;
  array_type : Types.t_type option;
}

and expr =
  | E_const_int of int
  | E_const_char of char
  | E_lvalue of lvalue
  | E_func_call of funcCall
  | E_sgn_expr of sign * expr
  | E_op_expr_expr of expr * arithmOperator * expr
  | E_expr_parenthesized of expr

and funcCall = {
  id : string;
  mutable comp_id : string;
  expr_list : expr list;
  (* [ret_type] is not encapsulated in [Types.T_func]. *)
  mutable ret_type : Types.t_type option;
}

and cond =
  | C_not_cond of logOperator * cond
  | C_cond_cond of cond * logOperator * cond
  | C_expr_expr of expr * compOperator * expr
  | C_cond_parenthesized of cond

(* Functions to construct the records above *)
let rec newFuncDef (a, b, c) =
  {
    header = a;
    local_def_list = b;
    block = c;
    parent_func = None;
    stack_frame = None;
  }

and newFuncDecl a = { header = a; func_def = None; is_redundant = false }

and newHeader (a, b, c) =
  { id = a; comp_id = a; fpar_def_list = b; ret_type = c }

and newFparDef (a, b, c) = { ref = a; id_list = b; fpar_type = c }
and newFparType (a, b) : fparType = { data_type = a; array_dimensions = b }
and newVarDef (a, b) = { id_list = a; var_type = b }
and newVarType (a, b) : varType = { data_type = a; array_dimensions = b }
and newLValue a = { lv_kind = a; lv_type = None }
and newFuncCall (a, b) = { id = a; comp_id = a; expr_list = b; ret_type = None }

(* Type conversion functions *)
let t_type_of_dataType = function
  | ConstInt -> Types.T_int
  | ConstChar -> Types.T_char

let t_type_of_retType = function
  | RetDataType dt -> Types.T_func (t_type_of_dataType dt)
  | Nothing -> Types.T_func Types.T_none

let t_type_of_fparType (fpt : fparType) : Types.t_type =
  let open Types in
  construct_array_type fpt.array_dimensions (t_type_of_dataType fpt.data_type)

let t_type_of_varType (vt : varType) : Types.t_type =
  let open Types in
  construct_array_type vt.array_dimensions (t_type_of_dataType vt.data_type)

(* Helper functions for checks *)

let rec get_const_expr_value : expr -> int option = function
  | E_const_int ci -> Some ci
  | E_sgn_expr (sign, e) -> (
      match sign with
      | O_plus -> get_const_expr_value e
      | O_minus -> Option.bind (get_const_expr_value e) (fun i -> Some (-1 * i))
      )
  | E_op_expr_expr (e1, ao, e2) -> (
      match (get_const_expr_value e1, get_const_expr_value e2) with
      | Some i1, Some i2 ->
          Some
            (match ao with
            | O_plus -> i1 + i2
            | O_minus -> i1 - i2
            | O_mul -> i1 * i2
            | O_div -> i1 / i2
            | O_mod -> i1 mod i2)
      | _ -> None)
  | E_expr_parenthesized e -> get_const_expr_value e
  | E_const_char _ | E_lvalue _ | E_func_call _ -> None

let get_const_cond_value c =
  let rec get_const_cond_value_helper = function
    | C_not_cond (lo, c) ->
        Option.bind (get_const_cond_value_helper c) (fun v -> Some (not v))
    | C_cond_cond (c1, lo, c2) -> begin
        Option.bind (get_const_cond_value_helper c1) (fun v1 ->
            if lo = O_or && v1 then
              Some true
            else if lo = O_and && not v1 then
              Some false
            else
              get_const_cond_value_helper c2)
      end
    | C_expr_expr (e1, co, e2) -> (
        match (get_const_expr_value e1, get_const_expr_value e2) with
        | Some i1, Some i2 ->
            Some
              (match co with
              | O_equal -> i1 = i2
              | O_less -> i1 < i2
              | O_greater -> i1 > i2
              | O_less_eq -> i1 <= i2
              | O_greater_eq -> i1 >= i2
              | O_not_equal -> i1 <> i2)
        | _ -> None)
    | C_cond_parenthesized c -> get_const_cond_value_helper c
  in
  Option.bind (get_const_cond_value_helper c) (fun v ->
      let string_of_v = if v then "true" else "false" in
      Error.handle_warning ("Condition is always " ^ string_of_v ^ ".");
      Some v)
