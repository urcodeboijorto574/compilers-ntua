open Ast
open Symbol

let rec sem_funcDef = function
(* done *)
| { header = h; local_def_list = l; block = b } ->
    sem_header h;
    sem_localDefList l;
    sem_block b

and sem_header = function
(* TODO: insert to SymbolTable the identifier and the return type
   of the function, as well as the number and types of parameters and their
   types of passing *)
| { id; fpar_def_list = fpdl; ret_type = rt } -> sem_fparDefList fpdl

and sem_fparDefList = function
(* TODO: insert to ST the number and types of the parameters, and the type
   of passing *)
| [] -> ()
| fpdl -> List.iter sem_fparDef fpdl

and sem_fparDef = function
(* TODO: insert ever element in SymbolTable *)
| { ref; id_list; fpar_type = fpt } -> ()

(* and sem_fparType = () *)
(* and sem_dataType = () *)
(* and sem_retType = () *)

and sem_localDefList = function
(* TODO: open a new scope for this particular function.
      All the variable/function declarations will be entered in this scope. *)
| [] -> ()
| ldl -> List.iter sem_localDef ldl

and sem_localDef = function
(* done *)
| L_FuncDef fd -> sem_funcDef fd
| L_FuncDecl fd -> sem_funcDecl fd
| L_varDef vd -> sem_varDef vd

and sem_funcDecl = function (* done *)
| FuncDecl_Header h -> sem_header h

and sem_varDef = function
(* TODO: insert to SymbolTable the variable's name, its type and the size
   of memory it needs, if it's an array *)
| { id_list = idl; var_type = vt } -> ()

and sem_block = function
(* TODO: if a function has a return type,
   then its block must contain the statement 'return'
   with the corresponding type returned *)
| Block [] -> ()
| Block b -> List.iter sem_stmt b

and sem_stmt = function
| S_assignment (lv, e) ->
    (* TODO: compare the types of the lv and e. lv shouldn't be a string
       literal *)
    ()
| S_block b -> sem_block b
| S_func_call fc ->
    (* TODO: find function in SymbolTable. Every param should have the same
       type and, if is passed by reference, should be an lvalue. *)
    List.iter sem_expr fc.expr_list
| S_if (c, s) ->
    (* done *)
    sem_cond c;
    sem_stmt s
| S_if_else (c, s1, s2) ->
    (* done *)
    sem_cond c;
    sem_stmt s1;
    sem_stmt s2
| S_while (c, s) ->
    (* done *)
    sem_cond c;
    sem_stmt s
| S_return re -> (
    (* TODO: the type of re should be the type that the function returns.
       Also, this statement should be in every block of a function that
       returns a non-nothing value *)
    match re with None -> () | Some v -> sem_expr v)
| S_semicolon -> (* done *) ()

and sem_lvalue = function
(* TODO: search in SymbolTable for its type *)
| L_id _ -> ()
| L_string _ -> ()
| L_comp (lv, e) ->
    sem_lvalue lv;
    (* TODO: e should be of type integer *) sem_expr e

and sem_expr = function
| E_const_int ci -> (* TODO: should return Types.T_int *) ()
| E_const_char cc -> (* TODO: should return Types.T_char *) ()
| E_lvalue lv -> (* done *) sem_lvalue lv
| E_func_call fc -> (* done *) sem_funcCall fc
| E_sgn_expr (s, e) -> (* TODO: check for the type to be integer *) sem_expr e
| E_op_expr_expr (e1, ao, e2) ->
    () (* TODO: types of e{1,2} should be integer *)
| E_expr_parenthesized e -> (* done *) sem_expr e

and sem_cond = function
| C_not_cond (bo, c) -> (* done *) sem_cond c
| C_cond_cond (c1, bo, c2) ->
    (* done *)
    sem_cond c1;
    sem_cond c2
| C_expr_expr (e1, bo, e2) ->
    ()
    (* TODO: check for the types of e1, e2 to be the same. Return type bool *)
| C_cond_parenthesized c -> (* done *) sem_cond c

and sem_funcCall = function
| _ -> (* TODO: check the number and types of arguments given *) ()

and sem_on asts =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
