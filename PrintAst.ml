open Ast

let rec print_fparDef_list fpar_def_list =
  match fpar_def_list with
  | [] -> ()
  | h :: tail ->
      print_fparDef h;
      if tail <> [] then Printf.printf "; ";
      print_fparDef_list tail

and print_funcDef (funcDef : Ast.funcDef) =
  Printf.printf "FuncDef(";
  print_header funcDef.header;
  Printf.printf ", ";
  if funcDef.local_def_list <> [] then
    List.iter print_localDef funcDef.local_def_list
  else
    Printf.printf "%s" "(noLocalDef)";
  Printf.printf ", ";
  print_block funcDef.block;
  Printf.printf ")"

and print_header header =
  Printf.printf "Header( fun(";
  Printf.printf "%s" header.id;
  Printf.printf "(";
  print_fparDef_list header.fpar_def_list;
  Printf.printf "): ";
  print_retType header.ret_type;
  Printf.printf "))"

and print_retType retType =
  let help retType =
    match retType with
    | RetDataType dataType -> print_dataType dataType
    | Nothing -> Printf.printf "nothing"
  in
  Printf.printf "RetType(";
  help retType;
  Printf.printf ")"

and print_fparDef fparDef =
  Printf.printf "FparDef(";
  if fparDef.ref then Printf.printf "ref";
  print_idList fparDef.id_list;
  Printf.printf " : ";
  print_fparType fparDef.fpar_type;
  Printf.printf ")"

and print_dataType dataType =
  match dataType with
  | ConstInt -> Printf.printf "DataType(int)"
  | ConstChar -> Printf.printf "DataType(char)"

and print_varType varType =
  let rec help array_dimensions =
    match array_dimensions with
    | [] -> Printf.printf ""
    | ad -> List.iter (Printf.printf "[%d]") ad
  in
  Printf.printf "VarType(";
  print_dataType varType.data_type;
  help varType.array_dimensions;
  Printf.printf ")"

and print_fparType fparType =
  Printf.printf "FparType(";
  print_dataType fparType.data_type;
  let f = function -1 -> Printf.printf "[]" | x -> Printf.printf "[%d]" x in
  List.iter f fparType.array_dimensions;
  Printf.printf ")"

and print_localDef localDef =
  let help localDef =
    match localDef with
    | L_funcDef funcDef -> print_funcDef funcDef
    | L_funcDecl funcDecl -> print_funcDecl funcDecl
    | L_varDef varDef -> print_varDef varDef
  in
  Printf.printf "LocalDef(";
  help localDef;
  Printf.printf ")"

and print_funcDecl funcDecl =
  Printf.printf "FuncDecl(";
  print_header funcDecl.header;
  Printf.printf ";)"

and print_idList idList =
  match idList with
  | [] -> ()
  | [ h ] -> Printf.printf "%s" h
  | h :: tail ->
      Printf.printf "%s ," h;
      print_idList tail

and print_varDef (varDef : Ast.varDef) =
  Printf.printf "VarDef( var";
  print_idList varDef.id_list;
  Printf.printf " : ";
  print_varType varDef.var_type;
  Printf.printf ";)"

and print_stmt stmt =
  let help stmt =
    match stmt with
    | S_assignment (l, e) ->
        Printf.printf "Assignment(";
        print_lvalue l;
        Printf.printf " <- ";
        print_expr e;
        Printf.printf ";)"
    | S_block block -> print_block block
    | S_func_call f ->
        print_funcCall f;
        Printf.printf ";"
    | S_if (c, s) ->
        Printf.printf "IfThen(If(";
        print_cond c;
        Printf.printf "), Then(";
        print_stmt s;
        Printf.printf "))"
    | S_if_else (c, s1, s2) ->
        Printf.printf "IfThenElse(If(";
        print_cond c;
        Printf.printf "), Then(";
        print_stmt s1;
        Printf.printf "), Else(";
        print_stmt s2;
        Printf.printf "))"
    | S_while (c, s) ->
        Printf.printf "While(";
        print_cond c;
        print_stmt s;
        Printf.printf ")"
    | S_return e -> (
        Printf.printf "Return(";
        match e with
        | None -> ()
        | Some v ->
            print_expr v;
            Printf.printf ";)")
    | S_semicolon -> Printf.printf "Semicolon(;)"
  in
  Printf.printf "Statement(";
  help stmt;
  Printf.printf ")"

and print_block block =
  Printf.printf "Block({";
  List.iter print_stmt block;
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
      print_expr h;
      if tail <> [] then Printf.printf ", ";
      print_exprList tail

and print_lvalue lvalue =
  let rec print_lvalue_kind = function
    | L_id id -> Printf.printf "%s" id
    | L_string str -> Printf.printf "\"%s\"" str
    | L_comp (l, e) ->
        print_lvalue_kind l;
        Printf.printf "[";
        print_expr e;
        Printf.printf "]"
  in
  Printf.printf "Lvalue(";
  print_lvalue_kind lvalue.lv_kind;
  Printf.printf ")"

and print_expr expr =
  Printf.printf "Expression(";
  (match expr with
  | E_const_int x -> Printf.printf "ConstInt(%d)" x
  | E_const_char chr -> Printf.printf "ConstChar(%c)" chr
  | E_lvalue l -> print_lvalue l
  | E_func_call f -> print_funcCall f
  | E_sgn_expr ((op : sign), e) -> (
      match op with
      | O_plus ->
          Printf.printf " + ";
          print_expr e
      | O_minus ->
          Printf.printf " - ";
          print_expr e)
  | E_op_expr_expr (e1, (op : arithmOperator), e2) ->
      print_expr e1;
      let str_from_op (ao : arithmOperator) =
        match ao with
        | O_plus -> Printf.printf " + "
        | O_minus -> Printf.printf " - "
        | O_mul -> Printf.printf " * "
        | O_div -> Printf.printf " div "
        | O_mod -> Printf.printf " mod "
      in
      str_from_op op;
      print_expr e2
  | E_expr_parenthesized e ->
      Printf.printf "(";
      print_expr e;
      Printf.printf ")");
  Printf.printf ")"

and print_cond cond =
  let help cond =
    match cond with
    | C_not_cond (logop, c) ->
        Printf.printf "not(";
        print_cond c;
        Printf.printf ")"
    | C_cond_cond (c1, logop, c2) -> (
        match logop with
        | O_and ->
            print_cond c1;
            Printf.printf " and ";
            print_cond c2
        | O_or ->
            print_cond c1;
            Printf.printf " or ";
            print_cond c2
        | O_not -> ())
    | C_expr_expr (e1, compop, e2) -> (
        match compop with
        | O_equal ->
            print_expr e1;
            Printf.printf " = ";
            print_expr e2
        | O_not_equal ->
            print_expr e1;
            Printf.printf " # ";
            print_expr e2
        | O_less ->
            print_expr e1;
            Printf.printf " < ";
            print_expr e2
        | O_greater ->
            print_expr e1;
            Printf.printf " > ";
            print_expr e2
        | O_less_eq ->
            print_expr e1;
            Printf.printf " <= ";
            print_expr e2
        | O_greater_eq ->
            print_expr e1;
            Printf.printf " >= ";
            print_expr e2)
    | C_cond_parenthesized c ->
        Printf.printf "(";
        print_cond c;
        Printf.printf ")"
  in
  Printf.printf "Cond(";
  help cond;
  Printf.printf ")"

and print_on asts = print_funcDef asts
