open Ast
open Symbol

let rec sem_funcDef = function
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

and sem_localDefList = function
(* TODO: open a new scope for this particular function.
   All the variable/function declarations will be entered in this scope. *)
| [] -> ()
| ldl -> List.iter sem_localDef ldl

and sem_localDef = function
| L_FuncDef fd -> sem_funcDef fd
| L_FuncDecl fd -> sem_funcDecl fd
| L_varDef vd -> sem_varDef vd

and sem_funcDecl = function FuncDecl_Header h -> sem_header h

and sem_varDef = function
(* TODO: insert to SymbolTable the variable's name, its type and the size
   of memory it needs, if it's an array *)
| { id_list = idl; var_type = vt } -> ()

and sem_block = function
(* TODO: if a function has a return type,
   then its block must contain the statement 'return'
   with the corresponding type returned.
   The return type of sem_block will be Types.t_type option.
   This will help distinguish whether the block has a return statement
   with and expression or not. *)
| Block [] -> ()
| Block b -> List.iter sem_stmt b

and sem_stmt = function
| S_assignment (lv, e) -> (
    let lv_t = sem_lvalue lv in
    match lv_t with
    | Types.T_array (T_char, _) ->
        failwith "Assignment to string literal is not possible.\n"
    | _ -> Types.equal_type (sem_lvalue lv) (sem_expr e))
| S_block b -> sem_block b
| S_func_call fc -> Types.equal_type (T_func None) (sem_funcCall fc)
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
    (* TODO: the type of re should be the type that the function returns.
       Also, this statement should be in every block of a function that
       returns a non-nothing value *)
    match re with
    | None -> ()
    | Some v ->
        Types.equal_type (sem_expr v)
          Types.T_int (* placeholder, needs SymbolTable *))
| S_semicolon -> ()

and sem_lvalue = function
(* TODO: search in SymbolTable for its type *)
| L_id _ -> Types.T_int (* placeholder, needs SymbolTable *)
| L_string s -> Types.T_array (Types.T_char, String.length s)
| L_comp (lv, e) ->
    Types.equal_type Types.T_int (sem_expr e);
    let eValue =
      match e with
      | E_const_int i -> i
      | E_lvalue lval -> 1 (* placeholder, needs SymbolTable *)
      | E_func_call fc -> 1 (* placeholder, needs SymbolTable *)
      | _ -> assert false
    in
    Types.T_array (sem_lvalue lv, eValue)

and sem_expr = function
(* done *)
| E_const_int ci -> Types.T_int
| E_const_char cc -> Types.T_char
| E_lvalue lv -> sem_lvalue lv
| E_func_call fc -> sem_funcCall fc
| E_sgn_expr (s, e) ->
    Types.equal_type Types.T_int (sem_expr e);
    Types.T_int
| E_op_expr_expr (e1, ao, e2) ->
    Types.equal_type Types.T_int (sem_expr e1);
    Types.equal_type Types.T_int (sem_expr e2);
    Types.T_int
| E_expr_parenthesized e -> sem_expr e

and sem_cond = function
(* done *)
| C_not_cond (lo, c) -> sem_cond c
| C_cond_cond (c1, lo, c2) -> assert (sem_cond c1 = sem_cond c2)
| C_expr_expr (e1, co, e2) ->
    let typ1, typ2 = (sem_expr e1, sem_expr e2) in
    Types.equal_type typ1 typ2
| C_cond_parenthesized c -> sem_cond c

and sem_funcCall = function
| _ ->
    (* TODO: check the number and types of arguments given *)
    Types.T_func None (* placeholder, needs SymbolTable *)

and sem_on asts =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
