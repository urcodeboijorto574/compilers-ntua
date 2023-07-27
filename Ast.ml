open Types

type arithmOperator =
| O_plus
| O_minus
| O_mul
| O_div
| O_mod

and sign =
| O_plus
| O_minus

and binOperator =
| O_and
| O_or
| O_equal
| O_less
| O_greater
| O_less_eq
| O_greater_eq
| O_not_equal
| O_not

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
ref : bool;
id_list : string list;
fpar_type : fparType;
}

and dataType =
| ConstInt
| ConstChar

and myType = {
data_type : dataType;
array_dimension : int list;
}

and retType =
| RetDataType of dataType
| Nothing

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
| S_assignment of lvalue * sem_expr
| S_block of block
| S_func_call of funcCall
| S_if of cond * stmt
| S_if_else of cond * stmt * stmt
| S_while of cond * stmt
| S_return of sem_expr
| S_semicolon

and block = Block of stmt list

and funcCall = {
id : string; (* was sem_id when the comment below wasn't a comment *)
expr_list : sem_expr list;
func_type : Types.t_type;
}

and lvalue =
| L_id of string
| L_string of string
| L_comp of lvalue * sem_expr

(* added for stmts semantic checks. An identifier must have a type and we don't know it's type *)
(* and sem_id = {
   id_name : string;
   id_type : Types.t_type; (* what happens with T_func? Why does it exist? *)
   } *)
and sem_expr = {
expr_kind : expr;
expr_type : Types.t_type;
}

and expr =
| E_const_int of int
| E_const_char of char
| E_lvalue of lvalue
| E_func_call of funcCall
| E_sgn_expr of sign * sem_expr
| E_op_expr_expr of sem_expr * arithmOperator * sem_expr
| E_expr_parenthesized of sem_expr

and cond =
| C_not_cond of binOperator * cond
| C_cond_cond of cond * binOperator * cond
| C_expr_expr of sem_expr * binOperator * sem_expr
| C_cond_parenthesized of cond

(* Functions to construct the records above *)
let newFuncDef (a, b, c) = { header = a; local_def_list = b; block = c }
and newHeader (a, b, c) = { id = a; fpar_def_list = b; ret_type = c }
and newFparDef (a, b, c) = { ref = a; id_list = b; fpar_type = c }
and newMyType (a, b) = { data_type = a; array_dimension = b }

and newFparType (a, b, c) =
  { data_type2 = a; array_dimension2 = b; has_squares = c }

and newVarDef (a, b) = { id_list = a; mytype = b }
and newFuncCall (a, b, c) = { id = a; expr_list = b; func_type = c }

and newAssignment (a, b) =
  let error_str = Printf.eprintf "Semantic Error: Cannot assign to string" in
  match a with
  | L_id id ->
      (* TODO: this requires symbol table "equal_type T_int ... " *)
      S_assignment (a, b)
  | L_comp (lv, _) ->
      (* TODO: this requires symbol table *)
      S_assignment (a, b)
  | _ ->
      error_str;
      S_assignment (a, b)

and newSemExpr (a, b) = { expr_kind = a; expr_type = b }

(* Functions to print the ast *)
let rec print_fparDef_list fpar_def_list =
  match fpar_def_list with
  | [] -> ()
  | h :: tail ->
      print_fparDef h;
      if tail <> [] then Printf.printf "; ";
      print_fparDef_list tail

and print_funcDef funcDef =
  Printf.printf "FuncDef(";
  print_header funcDef.header;
  Printf.printf ", ";
  if funcDef.local_def_list <> [] then (
    List.iter print_localDef funcDef.local_def_list;
    Printf.printf "")
  else
    Printf.printf "%s" "(noLocalDef)";
  Printf.printf ", ";
  print_block funcDef.block;
  Printf.printf ")"

and print_header header =
  Printf.printf "Header(fun(";
  Printf.printf "%s" header.id;
  Printf.printf "(";
  print_fparDef_list header.fpar_def_list;
  Printf.printf "):";
  print_retType header.ret_type;
  Printf.printf ")"

and print_fparDef fparDef =
  Printf.printf "FparDef(";
  if fparDef.ref then Printf.printf "ref";
  print_idList fparDef.id_list;
  Printf.printf ":";
  print_fparType fparDef.fpar_type;
  Printf.printf ")"

and print_dataType dataType =
  match dataType with
  | ConstInt -> Printf.printf "(DataType(int)"
  | ConstChar -> Printf.printf "(DataType(char)"

and print_myType myType =
  let rec help array_dimension =
    match array_dimension with
    | [] -> Printf.printf ""
    | h :: tail ->
        Printf.printf "[%d]" h;
        help tail
  in
  Printf.printf "MyType(";
  print_dataType myType.data_type;
  help myType.array_dimension;
  Printf.printf ")"

and print_retType retType =
  let help retType =
    match retType with
    | RetDataType dataType -> print_dataType dataType
    | Nothing -> Printf.printf "nothing"
  in
  Printf.printf "(RetType(";
  help retType;
  Printf.printf ")"

and print_fparType fparType =
  let hasSquares =
    if fparType.has_squares = true then Printf.printf "[]" else Printf.printf ""
  in
  Printf.printf "FparType(";
  print_dataType fparType.data_type2;
  hasSquares;
  if fparType.array_dimension2 <> [] then
    List.iter (fun x -> Printf.printf "[%d]" x) fparType.array_dimension2;
  Printf.printf ")"

and print_localDef localDef =
  let help localDef =
    match localDef with
    | L_FuncDef funcDef -> print_funcDef funcDef
    | L_FuncDecl funcDecl -> print_funcDecl funcDecl
    | L_varDef varDef -> print_varDef varDef
  in
  Printf.printf "LocalDef(";
  help localDef;
  Printf.printf ")"

and print_funcDecl funcDecl =
  match funcDecl with
  | FuncDecl_Header f ->
      Printf.printf "FuncDecl(";
      print_header f;
      Printf.printf ";)"

and print_idList idList =
  match idList with
  | [] -> ()
  | [ h ] -> Printf.printf "%s" h
  | h :: tail ->
      Printf.printf "%s ," h;
      print_idList tail

and print_varDef varDef =
  Printf.printf "VarDef(var";
  print_idList varDef.id_list;
  Printf.printf ":";
  print_myType varDef.mytype;
  Printf.printf ";)"

and print_stmt stmt =
  let help stmt =
    match stmt with
    | S_assignment (l, e) ->
        Printf.printf "Assignment(";
        print_lvalue l;
        Printf.printf "<-";
        print_semExpr e;
        Printf.printf ";)"
    | S_block block -> print_block block
    | S_func_call f ->
        print_funcCall f;
        Printf.printf ";"
    | S_if (c, s) ->
        Printf.printf "If(If(";
        print_cond c;
        Printf.printf "), Then(";
        print_stmt s;
        Printf.printf "))"
    | S_if_else (c, s1, s2) ->
        Printf.printf "IfElse(";
        print_cond c;
        Printf.printf "), Then(";
        print_stmt s1;
        Printf.printf "), Else";
        print_stmt s2;
        Printf.printf "))"
    | S_while (c, s) ->
        Printf.printf "While(";
        print_cond c;
        print_stmt s;
        Printf.printf ")"
    | S_return e ->
        Printf.printf "Return(";
        print_semExpr e;
        Printf.printf ";)"
    | S_semicolon -> Printf.printf "Semicolon(;)"
  in
  Printf.printf "Statement(";
  help stmt;
  Printf.printf ")"

and print_block block =
  match block with
  | Block stmt_list ->
      Printf.printf "Block({";
      List.iter print_stmt stmt_list;
      Printf.printf "})"

and print_funcCall funcCall =
  Printf.printf "FuncCall(";
  Printf.printf "%s" funcCall.id;
  Printf.printf "(";
  print_exprList funcCall.expr_list;
  Printf.printf "))"

and print_exprList expr_list =
  match expr_list with
  | [] -> ()
  | h :: tail ->
      print_semExpr h;
      if tail <> [] then Printf.printf ", ";
      print_exprList tail

and print_lvalue lvalue =
  let help lvalue =
    match lvalue with
    | L_id id -> Printf.printf "%s" id
    | L_string str -> Printf.printf "%s" str
    | L_comp (l, e) ->
        print_lvalue l;
        Printf.printf "[";
        print_semExpr e;
        Printf.printf "]"
  in
  Printf.printf "Lvalue(";
  help lvalue;
  Printf.printf ")"

and print_semExpr semExpr =
  let rec print_type =
    let print_type_option = function
    | Some t -> print_type t
    | None -> Printf.printf "nothing"
    in
    function
    | Types.T_int -> Printf.printf "int"
    | Types.T_char -> Printf.printf "char"
    | Types.T_array (t, i) ->
        print_type t;
        Printf.printf "[%d]" i
    | Types.T_func t ->
        Printf.printf "func(";
        print_type_option t;
        Printf.printf ")"
  in
  Printf.printf "SemExpr(";
  print_expr semExpr.expr_kind;
  Printf.printf ",";
  print_type semExpr.expr_type;
  Printf.printf ")"

and print_expr expr =
  let help expr =
    match expr with
    | E_const_int x -> Printf.printf "ConstInt(%d)" x
    | E_const_char chr -> Printf.printf "ConstChar(%c)" chr
    | E_lvalue l -> print_lvalue l
    | E_func_call f -> print_funcCall f
    | E_sgn_expr (op, s_e) -> (
        match op with
        | O_plus ->
            Printf.printf "+";
            print_semExpr s_e
        | O_minus ->
            Printf.printf "-";
            print_semExpr s_e)
    | E_op_expr_expr (s_e1, op, s_e2) ->
        print_semExpr s_e1;
        let str_from_op = function
        | O_plus -> Printf.printf "+"
        | O_minus -> Printf.printf "-"
        | O_mul -> Printf.printf "*"
        | O_div -> Printf.printf "div"
        | O_mod -> Printf.printf "mod"
        in
        str_from_op op;
        print_semExpr s_e2
    | E_expr_parenthesized s_e ->
        Printf.printf "(";
        print_semExpr s_e;
        Printf.printf ")"
  in
  Printf.printf "Expression(";
  help expr;
  Printf.printf ")"

and print_cond cond =
  let help cond =
    match cond with
    | C_not_cond (binop, c) ->
        Printf.printf "not";
        print_cond c
    | C_cond_cond (c1, binop, c2) -> (
        match binop with
        | O_and ->
            print_cond c1;
            Printf.printf "and";
            print_cond c2
        | O_or ->
            print_cond c1;
            Printf.printf "or";
            print_cond c2
        | O_equal | O_less | O_greater | O_less_eq | O_greater_eq
         |O_not_equal | O_not ->
            ())
    | C_expr_expr (e1, binop, e2) -> (
        match binop with
        | O_equal ->
            print_semExpr e1;
            Printf.printf "=";
            print_semExpr e2
        | O_not_equal ->
            print_semExpr e1;
            Printf.printf "#";
            print_semExpr e2
        | O_less ->
            print_semExpr e1;
            Printf.printf "<";
            print_semExpr e2
        | O_greater ->
            print_semExpr e1;
            Printf.printf ">";
            print_semExpr e2
        | O_less_eq ->
            print_semExpr e1;
            Printf.printf "<=";
            print_semExpr e2
        | O_greater_eq ->
            print_semExpr e1;
            Printf.printf ">=";
            print_semExpr e2
        | O_and | O_or | O_not -> ())
    | C_cond_parenthesized c ->
        Printf.printf "(";
        print_cond c;
        Printf.printf ")"
  in
  Printf.printf "Cond(";
  help cond;
  Printf.printf ")"

and print_on asts = print_funcDef asts
