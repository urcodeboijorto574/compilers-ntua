open Ast

let rec sem_funcDef = function
| { header = h; local_def_list = l; block = b } ->
    sem_header h;
    sem_localDefList l;
    sem_block b

and sem_header = function
| { id; fpar_def_list = fpdl; ret_type = rt } ->
    sem_fparDefList fpdl;
    sem_retType rt

and sem_fparDefList = function [] -> () | fpdl -> List.iter sem_fparDef fpdl

and sem_fparDef = function
| { ref; id_list; fpar_type = fpt } -> sem_fparType fpt

and sem_fparType = function
| { data_type = dt; array_dimension; has_squares } -> sem_dataType

and sem_dataType = function _ -> () (* TODO: this requires symbol table *)

and sem_retType = function
| RetDataType dt -> () (* TODO: this requires symbol table *)
| Nothing -> () (* TODO: this requires symbol table *)

and sem_localDefList = function [] -> () | ldl -> List.iter sem_localDef ldl

and sem_localDef = function
| L_FuncDef fd -> sem_funcDef fd
| L_FuncDecl fd -> sem_funcDecl fd
| L_varDef vd -> sem_varDef vd

and sem_funcDecl = function FuncDecl_Header h -> sem_header h

and sem_varDef = function
| { id_list; mytype = mt } ->
    (* TODO: this requires symbol table *)
    sem_myType

and sem_myType = function
| { data_type = dt; array_dimension } -> sem_dataType dt
(* TODO: this requires symbol table *)

and sem_dataType = function
| ConstInt -> () (* TODO: this requires symbol table *)
| ConstChar -> () (* TODO: this requires symbol table *)

and sem_block = function [] -> () | b -> List.iter sem_stmt b

and sem_stmt = function
| S_assignment (lv, e) ->
    sem_lvalue lv;
    sem_expr e
| S_block b -> sem_block b
| S_func_call fc -> List.iter sem_expression fc.expr_list
| S_if (c, s) ->
    sem_cond c;
    sem_stmt s
| S_if_else (c, s1, s2) ->
    sem_cond c;
    sem_stmt s1;
    sem_stmt s2
| S_while (c, s) ->
    sem_cond c;
    sem_stmt s
| S_return re -> (
    match re with
    | None -> () (* TODO: this requires symbol table *)
    | Some v -> sem_expr v)
| S_semicolon -> ()

and sem_lvalue = function
| L_id _ -> () (* TODO: this requires symbol table *)
| L_string _ -> () (* TODO: this requires symbol table *)
| L_comp (lv, e) ->
    sem_lvalue lv;
    sem_expr e

and sem_expr = function { expr_value = e; expr_type = t } -> sem_exprValue e

and sem_exprValue = function
| E_const_int ci -> () (* TODO: this requires symbol table *)
| E_const_char cc -> () (* TODO: this requires symbol table *)
| E_lvalue lv -> sem_lvalue lv
| E_func_call fc -> sem_funcCall fc
| E_sgn_expr (s, e) -> sem_expr e
| E_op_exrp_expr (e1, ao, e2) ->
    check_type_int e1.expr_type;
    equal_type e1.expr_type e2.expr_type

and sem_exprType = function _ -> ()
and sem_funcCall = function _ -> ()
and sem_on asts = function _ -> ()
