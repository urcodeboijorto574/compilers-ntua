open Ast
open Printf

let rec print_fparDef_list fpar_def_list =
  match fpar_def_list with
  | [] -> ()
  | h :: tail ->
      print_fparDef h;
      if tail <> [] then printf "; ";
      print_fparDef_list tail

and print_funcDef (funcDef : Ast.funcDef) =
  printf "FuncDef(";
  print_header funcDef.header;
  printf ", ";
  if funcDef.local_def_list <> [] then
    List.iter print_localDefList funcDef.local_def_list
  else
    printf "(noLocalDef)";
  printf ", ";
  print_block funcDef.block;
  printf ")"

and print_header header =
  printf "Header( fun(";
  printf "%s" header.id;
  printf "(";
  print_fparDef_list header.fpar_def_list;
  printf "): ";
  print_retType header.ret_type;
  printf "))"

and print_retType retType =
  let help retType =
    match retType with
    | RetDataType dataType -> print_dataType dataType
    | Nothing -> printf "nothing"
  in
  printf "RetType(";
  help retType;
  printf ")"

and print_fparDef fparDef =
  printf "FparDef(";
  if fparDef.ref then printf "ref";
  print_idList fparDef.id_list;
  printf " : ";
  print_fparType fparDef.fpar_type;
  printf ")"

and print_dataType dataType =
  sprintf "DataType(%s)"
    (match dataType with ConstInt -> "int" | ConstChar -> "char")
  |> printf "%s"

and print_varType varType =
  let rec help array_dimensions =
    match array_dimensions with
    | [] -> printf ""
    | ad -> List.iter (printf "[%d]") ad
  in
  printf "VarType(";
  print_dataType varType.data_type;
  help varType.array_dimensions;
  printf ")"

and print_fparType fparType =
  printf "FparType(";
  print_dataType fparType.data_type;
  let f = function -1 -> printf "[]" | x -> printf "[%d]" x in
  List.iter f fparType.array_dimensions;
  printf ")"

and print_localDefList localDef =
  let print_localDef localDef =
    match localDef with
    | L_funcDef funcDef -> print_funcDef funcDef
    | L_funcDecl funcDecl -> print_funcDecl funcDecl
    | L_varDef varDef -> print_varDef varDef
  in
  printf "LocalDefList(";
  print_localDef localDef;
  printf ")"

and print_funcDecl funcDecl =
  printf "FuncDecl(";
  print_header funcDecl.header;
  printf ";)"

and print_idList idList =
  match idList with
  | [] -> ()
  | [ h ] -> printf "%s" h
  | h :: tail ->
      printf "%s ," h;
      print_idList tail

and print_varDef (varDef : Ast.varDef) =
  printf "VarDef( var";
  print_idList varDef.id_list;
  printf " : ";
  print_varType varDef.var_type;
  printf ";)"

and print_stmt stmt =
  let help stmt =
    match stmt with
    | S_assignment (l, e) ->
        printf "Assignment(";
        print_lvalue l;
        printf " <- ";
        print_expr e;
        printf ";)"
    | S_block block -> print_block block
    | S_func_call f ->
        print_funcCall f;
        printf ";"
    | S_if (c, s) ->
        printf "IfThen(If(";
        print_cond c;
        printf "), Then(";
        print_stmt s;
        printf "))"
    | S_if_else (c, s1, s2) ->
        printf "IfThenElse(If(";
        print_cond c;
        printf "), Then(";
        print_stmt s1;
        printf "), Else(";
        print_stmt s2;
        printf "))"
    | S_while (c, s) ->
        printf "While(";
        print_cond c;
        print_stmt s;
        printf ")"
    | S_return e -> (
        printf "Return(";
        match e with
        | None -> ()
        | Some v ->
            print_expr v;
            printf ";)")
    | S_semicolon -> printf "Semicolon(;)"
  in
  printf "Statement(";
  help stmt;
  printf ")"

and print_block block =
  printf "Block({";
  List.iter print_stmt block;
  printf "})"

and print_funcCall funcCall =
  printf "FuncCall(";
  printf "%s" funcCall.id;
  printf "(";
  print_exprList funcCall.expr_list;
  printf "))"

and print_exprList expr_list =
  match expr_list with
  | [] -> ()
  | [ e ] -> print_expr e
  | h :: tail ->
      print_expr h;
      printf ", ";
      print_exprList tail

and print_lvalue lvalue =
  let rec print_lvalue_kind = function
    | L_id id -> printf "%s" id
    | L_string str -> printf "\"%s\"" str
    | L_comp (l, e) ->
        print_lvalue_kind l;
        printf "[";
        print_expr e;
        printf "]"
  in
  printf "Lvalue(";
  print_lvalue_kind lvalue.lv_kind;
  printf ")"

and print_expr expr =
  printf "Expression(";
  (match expr with
  | E_const_int x -> printf "ConstInt(%d)" x
  | E_const_char chr -> printf "ConstChar(%c)" chr
  | E_lvalue l -> print_lvalue l
  | E_func_call f -> print_funcCall f
  | E_sgn_expr ((op : sign), e) -> (
      match op with
      | O_plus ->
          printf " + ";
          print_expr e
      | O_minus ->
          printf " - ";
          print_expr e)
  | E_op_expr_expr (e1, (op : arithmOperator), e2) ->
      print_expr e1;
      let print_op (ao : arithmOperator) =
        match ao with
        | O_plus -> printf " + "
        | O_minus -> printf " - "
        | O_mul -> printf " * "
        | O_div -> printf " div "
        | O_mod -> printf " mod "
      in
      print_op op;
      print_expr e2
  | E_expr_parenthesized e ->
      printf "(";
      print_expr e;
      printf ")");
  printf ")"

and print_cond cond =
  let help cond =
    match cond with
    | C_not_cond (logop, c) ->
        printf "not(";
        print_cond c;
        printf ")"
    | C_cond_cond (c1, logop, c2) -> (
        match logop with
        | O_and ->
            print_cond c1;
            printf " and ";
            print_cond c2
        | O_or ->
            print_cond c1;
            printf " or ";
            print_cond c2
        | O_not -> ())
    | C_expr_expr (e1, compop, e2) -> (
        match compop with
        | O_equal ->
            print_expr e1;
            printf " = ";
            print_expr e2
        | O_not_equal ->
            print_expr e1;
            printf " # ";
            print_expr e2
        | O_less ->
            print_expr e1;
            printf " < ";
            print_expr e2
        | O_greater ->
            print_expr e1;
            printf " > ";
            print_expr e2
        | O_less_eq ->
            print_expr e1;
            printf " <= ";
            print_expr e2
        | O_greater_eq ->
            print_expr e1;
            printf " >= ";
            print_expr e2)
    | C_cond_parenthesized c ->
        printf "(";
        print_cond c;
        printf ")"
  in
  printf "Cond(";
  help cond;
  printf ")"

and print_on asts = print_funcDef asts
