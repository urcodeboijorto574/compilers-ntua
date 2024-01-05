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
  (* pointer to the stack frame of parent function *)
  mutable access_link : Llvm.lltype option;
  (* the parent funcDef of the function*)
  mutable parent_func : funcDef option;
  (* stack frame of the function -- later will become a struct *)
  mutable stack_frame : Llvm.lltype option;
  mutable stack_frame_addr : Llvm.llvalue option;
  mutable var_records : (string * int * bool * bool) list;
  mutable stack_frame_length : int;
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
  expr_list : expr list;
  mutable ret_type : Types.t_type option;
  (* 'ret_type' is not an encapsulation of T_func *)
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
val newLValue : lvalue_kind -> lvalue
val newFuncCall : string * expr list -> funcCall

(* Type conversion functions *)

(** [t_type_of_dataType dt] takes an object [dt] of type [dataType] and
    returns the corresponding type. Returns [Types.t_type]. *)
val t_type_of_dataType : dataType -> Types.t_type

(** [t_type_of_retType rt] takes an object [rt] of type [retType] and
    returns the corresponding [t_type] (encapsulated in [T_func]). *)
val t_type_of_retType : retType -> Types.t_type

(** [t_type_of_fparType fpt] takes an object [fpt] of type [fparType] and
    returns the corresponding type. Returns [Types.t_type]. *)
val t_type_of_fparType : fparType -> Types.t_type

(** [t_type_of_fparType vt] takes an object [vt] of type [varType] and
    returns the corresponding type. Returns [Types.t_type]. *)
val t_type_of_varType : varType -> Types.t_type

(* Helper functions for checks *)

(** [get_const_expr_value e] checks whether an expression [e] has a constant
    integer value or not. Returns [int option]. *)
val get_const_expr_value : expr -> int option

(** [get_const_cond_value c] checks whether a condition [c] has a constant
    value. If so, a warning message gets printed in standard error output.
    Returns [bool option]. *)
val get_const_cond_value : cond -> bool option
