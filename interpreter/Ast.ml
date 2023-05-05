type operator = O_plus | O_minus | O_mul | O_div | O_mod
and binOperator =  O_and | O_or | O_equal | O_less | O_greater | O_less_eq | O_greater_eq | O_not_equal | O_not

and funcDef = {
  header: header;
  local_def_list: localDef list;
  block: stmt list;
}

and header = {
  id: string;
  fpar_def_list: fparDef list;
  ret_type: retType;
}

and fparDef = {
  ref: string;
  id_list: string list; 
  fpar_type: fparType;
}

and dataType = 
  | Const of string 
  | Char of string

and myType = {
  data_type: dataType;
  array_dimension: int list;
}

and retType = 
  | RetDataType of dataType 
  | Nothing of string

and fparType = {
  data_type2: dataType;
  array_dimension2: int list;
}

and localDef = 
  | L_FuncDef of funcDef 
  | L_FuncDecl of funcDecl
  | L_varDef of varDef

and funcDecl = FuncDecl_Header of header

and varDef = {
  id_list: string list;
  mytype: myType;
}

and stmt =
  | S_assignment of lvalue * expr
  | S_block of stmt list
  | S_func_call of funcCall
  | S_if of cond * stmt
  | S_if_else of cond * stmt * stmt
  | S_while of cond * stmt
  | S_return of expr
  | S_semicolon of string

and funcCall = {
  id: string;
  expr_list: expr list;
}

and lvalue = 
  | L_id of string 
  | L_string of string
  | L_comp of lvalue * expr

and expr = 
  | E_const of int 
  | E_char of char
  | E_lvalue of lvalue
  | E_func_call of funcCall
  | E_op_expr of operator * expr 
  | E_op_expr_expr of expr * operator * expr

and cond = 
  | C_not_cond of binOperator * cond
  | C_cond_cond of cond * binOperator * cond
  | C_expr_expr of expr * binOperator * expr
 

let newFuncDef (a, b, c) = {
  header = a;
  local_def_list = b;
  block = c
}  

let newHeader (a, b, c) = {
  id = a;
  fpar_def_list = b;
  ret_type = c;
}

let newFparDef (a, b, c) = {
  ref = a;
  id_list = b;
  fpar_type = c;
}

let newMyType (a, b) = {
  data_type = a;
  array_dimension = b
}

let newFparType (a, b) = {
  data_type2 = a;
  array_dimension2 = b
}

let newVarDef (a, b) = {
  id_list = a;
  mytype = b
}

let newFuncCall (a, b) = {
  id = a;
  expr_list = b;
}

let rec print_funcDef funcDef = 
  Printf.printf("FuncDef("); print_header funcDef.header; List.iter print_localDef funcDef.local_def_list; List.iter print_stmt funcDef.block; Printf.printf(")");

let rec print_header header = 
  Printf.printf("Header(fun("); Printf.print("%s") id; Printf.printf ("(") List.iter print_fparDef header.fpar_def_list; Printf.printf("):"); print_retType header.ret_type Printf.printf(")");

let rec print_fparDef fparDef = 
  match fparDef.ref with
  | ""  -> Printf.printf("FparDef("); List.iter print_id fparDef.id_list; Printf.printf(":") print_fparType fparDef.fpar_type; Printf.printf(")");
  
  | "ref" -> Printf.printf("FparDef(ref"); List.iter print_id fparDef.id_list; Printf.printf(":") print_fparType fparDef.fpar_type; Printf.printf(")");

let rec print_myType myType = 
  print_dataType myType.data_type; print_arrayDimension myType.array_dimension;

let rec 