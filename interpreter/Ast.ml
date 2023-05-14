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


let rec print_dataType dataType =
  match dataType with
  | "int" -> Printf.printf("(DataType(int)");
  | "char" -> Printf.printf("(DataType(char)");

  
let rec print_myType myType = 
  Printf.printf("MyType(") ;print_dataType myType.data_type; List.iter (fun x -> Printf.printf "%d" x; Printf.printf(" ")) myType.array_dimension Printf.printf(")");


let rec print_retType retType =
  let help retType =
    match retType with 
    | RetDataType dataType -> print_dataType dataType
    | Nothing nothing -> Printf.printf("nothing");
  in
    Printf.printf("(RetType("); help retType; Printf.printf(")");

  
let rec print_fparType fparType = 
  Printf.printf("FparType(") ;print_dataType fparType.data_type2; List.iter (fun x -> Printf.printf "%d" x; Printf.printf(" ")) fparType.array_dimension2 Printf.printf(")");


let rec print_localDef localDef =
  let help localDef =
    match localDef with
    | L_FuncDef funcDef -> print_funcDef funcDef;
    | L_FuncDecl funcDecl -> print_funcDecl funcDecl;
    | L_varDef varDef -> print_varDef varDef;
  in
    Printf.printf("LocalDef("); help localDef; Printf.prinf(")");

  
let rec print_funcDecl funcDecl = 
  Printf.printf("FuncDecl("); print_header funcDecl; Printf.printf(";)");

let rec print_idList idList =
  match idList with 
  | h -> Printf.printf("%s") h;
  | h :: tail -> if tail = [] then Printf.printf ("%s") h
                 else Printf.printf("%s ,") h; print_idList tail;

let rec print_varDef varDef =
  Printf.printf("VarDef(var"); print_idList varDef.id_list; Printf.printf(":"); print_myType varDef.mytype; Printf.printf(";");


let rec print_stmt stmt =
  match stmt with
  | S_assignment (l, e)   -> Printf.printf("Assignment("); print_lvalue l; Printf.printf("<-") print_expr e; Printf.printf(";)");
  | S_block stmt_list     -> Printf.printf("Block("); List.iter print_stmt stmt_list; Printf.printf(")");
  | S_func_call f         -> print_funcCall f; Printf.printf(";");
  | S_if (c, s)           -> Printf.printf("If(If("); print_cond c; Printf.printf("), Then("); print_stmt s; Printf.printf("))");
  | S_if_else (c, s1, s2) -> Printf.printf("IfElse("); print_cond c; Printf.printf("), Then("); print_stmt s1; 
                             Printf.printf("), Else"); print_stmt s2; Printf.printf("))");
  | S_while (c, s)        -> Printf.printf("While("); print_cond c; print_stmt s; Printf.printf(")");
  | S_return e            -> Printf.printf("Return("); print_expr e; Printf.printf(";)");
  | S_semicolon           -> Printf.printf("Semicolon(;)");


let rec printf_funcCall funcCall =
  Printf.printf("FuncCall("); Printf.printf("%s ") funcCall.id; print_exprList funcCall.exrp_list; Printf.printf(")");


let rec print_lvalue lvalue =
  let help lvalue =
    match lvalue with
    | L_id str      -> Printf.printf("%s ") str;
    | L_string str  -> Printf.printf("% s") str;
    | L_comp (l, e) -> print_lvalue l; Printf.printf("[") print_expr e; Printf.printf("]");
  in
    Printf.printf("Lvalue(") help lvalue; Printf.printf(")");


let rec print_expr expr =
  