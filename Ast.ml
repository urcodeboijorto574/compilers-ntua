type operator = O_plus | O_minus | O_mul | O_div | O_mod
and binOperator = O_and | O_or | O_equal | O_less | O_greater | O_less_eq | O_greater_eq | O_not_equal | O_not

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

and fparDef = {
  ref : string;
  id_list : string list;
  fpar_type : fparType;
}

and dataType =
| Const of string
| Char of string

and myType = {
  data_type : dataType;
  array_dimension : int list;
}

and retType =
| RetDataType of dataType
| Nothing of string

and fparType = {
  data_type2 : dataType;
  array_dimension2 : int list;
  has_squares : bool;
}

and localDef =
| L_FuncDef of funcDef
| L_FuncDecl of funcDecl
| L_varDef of varDef

and funcDecl = FuncDecl_Header of header

and varDef = {
  id_list : string list;
  mytype : myType;
}

and stmt =
| S_assignment of lvalue * expr
| S_block of block
| S_func_call of funcCall
| S_if of cond * stmt
| S_if_else of cond * stmt * stmt
| S_while of cond * stmt
| S_return of expr
| S_semicolon of string

and block = Block of stmt list

and funcCall = {
  id : string;
  expr_list : expr list;
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
| E_expr_parenthesized of expr

and cond =
| C_not_cond of binOperator * cond
| C_cond_cond of cond * binOperator * cond
| C_expr_expr of expr * binOperator * expr
| C_cond_parenthesized of cond


let newFuncDef (a, b, c) = {
  header = a;
  local_def_list = b;
  block = c;
}

and newHeader (a, b, c) = {
  id = a;
  fpar_def_list = b;
  ret_type = c;
}

and newFparDef (a, b, c) = {
  ref = a;
  id_list = b;
  fpar_type = c;
}

and newMyType (a, b) = {
  data_type = a;
  array_dimension = b;
}

and newFparType (a, b, c) = {
  data_type2 = a;
  array_dimension2 = b;
  has_squares = c;
}

and newVarDef (a, b) = {
  id_list = a;
  mytype = b;
}

and newFuncCall (a, b) = {
  id = a;
  expr_list = b;
}

let rec print_fparDef_list fpar_def_list =
  match fpar_def_list with
  | [] -> ()
  | h :: tail -> print_fparDef h; if tail <> [] then Printf.printf("; "); print_fparDef_list tail


and print_funcDef funcDef =
  Printf.printf("FuncDef("); print_header funcDef.header; Printf.printf(", "); List.iter print_localDef funcDef.local_def_list; Printf.printf(", "); print_block funcDef.block; Printf.printf(")");


and print_header header =
  Printf.printf("Header(fun("); Printf.printf("%s") header.id; Printf.printf ("("); print_fparDef_list header.fpar_def_list; Printf.printf("):"); print_retType header.ret_type; Printf.printf(")");


and print_fparDef fparDef =
  match fparDef.ref with
  | ""  -> Printf.printf("FparDef("); print_idList fparDef.id_list; Printf.printf(":"); print_fparType fparDef.fpar_type; Printf.printf(")")
  | "ref" -> Printf.printf("FparDef(ref"); print_idList fparDef.id_list; Printf.printf(":"); print_fparType fparDef.fpar_type; Printf.printf(")");


and print_dataType dataType =
  match dataType with
  | Const str-> Printf.printf("(DataType(int)");
  | Char str -> Printf.printf("(DataType(char)");


and print_myType myType =
  let rec help array_dimension =
    match array_dimension with
    | [] -> Printf.printf("")
    | h :: tail -> Printf.printf("["); Printf.printf("%d") h; Printf.printf("]"); help tail
  in
  Printf.printf("MyType("); print_dataType myType.data_type; help myType.array_dimension; Printf.printf(")");


and print_retType retType =
  let help retType =
    match retType with
    | RetDataType dataType -> print_dataType dataType
    | Nothing nothing -> Printf.printf("nothing")
  in
  Printf.printf("(RetType("); help retType; Printf.printf(")")



and print_fparType fparType =
  let hasSquares = if fparType.has_squares = true then Printf.printf("[]") else Printf.printf("") in 
  match fparType.array_dimension2 with
  | [] -> Printf.printf("FparType("); print_dataType fparType.data_type2; hasSquares; Printf.printf(")")
  | list -> Printf.printf("FparType("); print_dataType fparType.data_type2; hasSquares;
            List.iter (fun x -> Printf.printf "[%d]" x; ) fparType.array_dimension2;  Printf.printf(")")


and print_localDef localDef =
  let help localDef =
    match localDef with
    | L_FuncDef funcDef -> print_funcDef funcDef;
    | L_FuncDecl funcDecl -> print_funcDecl funcDecl;
    | L_varDef varDef -> print_varDef varDef;
  in
  Printf.printf("LocalDef("); help localDef; Printf.printf(")");


and print_funcDecl funcDecl =
  match funcDecl with
  | FuncDecl_Header f -> Printf.printf("FuncDecl("); print_header f; Printf.printf(";)");


and print_idList idList =
  match idList with
  | [] -> ()
  | [h] -> Printf.printf("%s") h
  | h :: tail -> Printf.printf("%s ,") h; print_idList tail;

and print_varDef varDef =
  Printf.printf("VarDef(var"); print_idList varDef.id_list; Printf.printf(":"); print_myType varDef.mytype; Printf.printf(";)");


and print_stmt stmt =
  let help stmt =
    match stmt with
    | S_assignment (l, e)   -> Printf.printf("Assignment("); print_lvalue l; Printf.printf("<-"); print_expr e; Printf.printf(";)");
    | S_block block         -> print_block block;
    | S_func_call f         -> print_funcCall f; Printf.printf(";");
    | S_if (c, s)           -> Printf.printf("If(If("); print_cond c; Printf.printf("), Then("); print_stmt s; Printf.printf("))");
    | S_if_else (c, s1, s2) -> Printf.printf("IfElse("); print_cond c; Printf.printf("), Then("); print_stmt s1; 
                              Printf.printf("), Else"); print_stmt s2; Printf.printf("))");
    | S_while (c, s)        -> Printf.printf("While("); print_cond c; print_stmt s; Printf.printf(")");
    | S_return e            -> Printf.printf("Return("); print_expr e; Printf.printf(";)");
    | S_semicolon str       -> Printf.printf("Semicolon(;)");
  in
  Printf.printf("Statement("); help stmt; Printf.printf(")");


and print_block block =
  match block with
  | Block stmt_list -> Printf.printf("Block({"); List.iter print_stmt stmt_list; Printf.printf("})");


and print_funcCall funcCall =
  Printf.printf("FuncCall("); Printf.printf("%s") funcCall.id; Printf.printf("("); print_exprList funcCall.expr_list; Printf.printf("))");

and print_exprList expr_list =
  match expr_list with
  | [] -> ()
  | h :: tail -> print_expr h; if tail <> [] then Printf.printf(", "); print_exprList tail


and print_lvalue lvalue =
  let help lvalue =
    match lvalue with
    | L_id str      -> Printf.printf("%s") str;
    | L_string str  -> Printf.printf("%s") str;
    | L_comp (l, e) -> print_lvalue l; Printf.printf("["); print_expr e; Printf.printf("]");
  in
  Printf.printf("Lvalue("); help lvalue; Printf.printf(")");


and print_expr expr =
  let help expr =
    match expr with
    | E_const x                   -> Printf.printf("Const(%d)") x;
    | E_char chr                  -> Printf.printf("Char(%c)") chr;
    | E_lvalue l                  -> print_lvalue l;
    | E_func_call f               -> print_funcCall f;
    | E_op_expr (op, e)           -> begin
                                      match op with
                                      | O_plus  ->  Printf.printf("+"); print_expr e;
                                      | O_minus -> Printf.printf("-"); print_expr e;
                                     end
    | E_op_expr_expr (e1, op, e2) -> begin
                                      match op with
                                      | O_plus  -> print_expr e1; Printf.printf("+"); print_expr e2
                                      | O_minus -> print_expr e1; Printf.printf("-"); print_expr e2
                                      | O_mul   -> print_expr e1; Printf.printf("*"); print_expr e2
                                      | O_div   -> print_expr e1; Printf.printf("div"); print_expr e2
                                      | O_mod   -> print_expr e1; Printf.printf("mod"); print_expr e2;
                                     end
    | E_expr_parenthesized e      -> Printf.printf("("); print_expr e; Printf.printf(")");
  in
  Printf.printf("Expression("); help expr; Printf.printf(")");


and print_cond cond =
  let help cond =
    match cond with
    | C_not_cond (binop, c)       -> Printf.printf("not"); print_cond c
    | C_cond_cond (c1, binop, c2) -> begin
                                      match binop with
                                      | O_and -> print_cond c1; Printf.printf("and"); print_cond c2
                                      | O_or  -> print_cond c1; Printf.printf("or"); print_cond c2
                                     end
    | C_expr_expr (e1, binop, e2) -> begin
                                      match binop with
                                      | O_equal      -> print_expr e1; Printf.printf("="); print_expr e2
                                      | O_not_equal  -> print_expr e1; Printf.printf("#"); print_expr e2
                                      | O_less       -> print_expr e1; Printf.printf("<"); print_expr e2
                                      | O_greater    -> print_expr e1; Printf.printf(">"); print_expr e2
                                      | O_less_eq    -> print_expr e1; Printf.printf("<="); print_expr e2
                                      | O_greater_eq -> print_expr e1; Printf.printf(">="); print_expr e2
                                     end
    | C_cond_parenthesized c      -> Printf.printf("("); print_cond c; Printf.printf(")");
  in
  Printf.printf("Cond("); help cond; Printf.printf(")");

and print_on asts = print_funcDef asts;
