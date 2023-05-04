type Const = int;
type Char = char;

type FuncDef = {
  header: Header;
  local_def_list: LocalDef list;
  block: Block;
}

and Header = {
  id: string;
  fpar_def_list: FparDef list;
  ret_type: RetType;
}

and FparDef = {
  ref: bool;
  id_list: string list; 
  fpar_type: FparType;
}

and DataType = Const of int | Char of char;

and MyType = {
  data_type: DataType;
  array_dimension: Const list;
}

and RetType = DataType | Nothing of string;

and FparType = {
  data_type: DataType;
  array_dimension: Const list;
}

and LocalDef = FuncDef | FuncDecl | VarDef

and FuncDecl = Header;

and VarDef = {
  id: string;
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

and Block = Stmt list

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
  | E_expr of Expr
  | E_func_call of FuncCall
  | E_op_expr of Operator * Expr 
  | E_op_expr_expr of Expr * Operator * Expr

and Cond = 
  | C_not_cond of Oper * Cond
  | C_cond_cond of Cond * BinOperator * Cond
  | C_expr_expr of Expr * BinOperator * Expr
