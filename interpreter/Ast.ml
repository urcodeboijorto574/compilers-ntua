(* type Const = int;
type Char = char;
 *)
type Operator = O_plus | O_minus | O_mul | O_div | O_mod
type BinOperator =  O_and | O_or | O_equal | O_less | O_greater | O_less_eq | O_greater_eq | O_not_equal

type FuncDef = {
  header: Header;
  local_def_list: LocalDef list;
  block: Block;
}

let rec print_func_def func_def = Prinf.printf("FuncDef("); print_header

and Header = {
  id: string;
  fpar_def_list: FparDef list;
  ret_type: RetType;
}

and FparDef = {
  ref: string;
  id_list: string list; 
  fpar_type: FparType;
}

and DataType = Const of int | Char of char;

and MyType = {
  data_type: DataType;
  array_dimension: int list;
}

and RetType = RetDataType of DataType | Nothing of string;

and FparType = {
  data_type: DataType;
  array_dimension: int list;
}

and LocalDef = 
  | L_FuncDef of FuncDef 
  | L_FuncDecl of FuncDecl
  | L_varDef of VarDef

and FuncDecl = FuncDecl_Header of Header;

and VarDef = {
  id_list: string list;
  mytype: MyType;
}

and Stmt =
  | S_assignment of Lvalue * Expr
  | S_block of Block
  | S_func_call of FuncCall
  | S_if of Cond * Stmt
  | S_if_else of Cond * Stmt * Stmt
  | S_while of Cond * Stmt
  | S_return of Expr

and FuncCall = {
  id: string;
  expr_list: Expr list;
}

and Lvalue = 
  | L_id of string 
  | L_string of string; 
  | L_comp of Lvalue * Expr;

and Expr = 
  | E_const of Const 
  | E_char of Char
  | E_lvalue of Lvalue
  | E_func_call of FuncCall
  | E_op_expr of Operator * Expr 
  | E_op_expr_expr of Expr * Operator * Expr

and Cond = 
  | C_not_cond of Oper * Cond
  | C_cond_cond of Cond * BinOperator * Cond
  | C_expr_expr of Expr * BinOperator * Expr
 
