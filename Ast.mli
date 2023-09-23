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

and funcDef = {
  header : header;
  local_def_list : localDef list;
  block : block;
}

and header = {
  id : string;
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

and funcDecl = FuncDecl_Header of header

and varDef = {
  id_list : string list;
  var_type : varType;
}

and varType = {
  data_type : dataType;
  array_dimensions : int list;
}

and block = Block of stmt list

and stmt =
  | S_assignment of lvalue * expr
  | S_block of block
  | S_func_call of funcCall
  | S_if of cond * stmt
  | S_if_else of cond * stmt * stmt
  | S_while of cond * stmt
  | S_return of expr option
  | S_semicolon

and lvalue =
  | L_id of string
  | L_string of string
  | L_comp of lvalue * expr

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
  expr_list : expr list;
}

and cond =
  | C_not_cond of logOperator * cond
  | C_cond_cond of cond * logOperator * cond
  | C_expr_expr of expr * compOperator * expr
  | C_cond_parenthesized of cond

(* Functions to construct the records above *)
val newFuncDef : header * localDef list * block -> funcDef
val newHeader : string * fparDef list * retType -> header
val newFparDef : bool * string list * fparType -> fparDef
val newFparType : dataType * int list -> fparType
val newVarDef : string list * varType -> varDef
val newVarType : dataType * int list -> varType
val newFuncCall : string * expr list -> funcCall

(* Helper functions for checks *)

(** [get_const_expr_value e] checks whether an expression [e] has a constant
    integer value or not. Returns [int option]. *)
val get_const_expr_value : expr -> int option

(** [get_const_cond_value c] checks whether a condition [c] has a constant
    value. If so, a warning message gets printed in standard error output.
    Returns [bool option]. *)
val get_const_cond_value : cond -> bool option
