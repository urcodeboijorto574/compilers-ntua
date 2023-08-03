open Ast
open Symbol

let rec sem_funcDef = function
| { header = h; local_def_list = l; block = b } ->
    sem_header h;
    sem_localDefList l;
    sem_block b

and sem_header = function
| { id; fpar_def_list = fpdl; ret_type = rt } ->
    (* TODO: insert to SymbolTable the identifier and the return type *)
    sem_fparDefList fpdl

and sem_fparDefList = function [] -> () | fpdl -> List.iter sem_fparDef fpdl
and sem_fparDef = function { ref; id_list; fpar_type = fpt } -> ()
(* TODO: insert to SymbolTable the type of passing and for each identifier,
   its name and return type *)

(* and sem_fparType = function
   | { data_type = dt; array_dimension; fixed_size } -> sem_dataType dt *)

(* and sem_dataType = function
   | ConstInt -> () (* TODO: this requires symbol table *)
   | ConstChar -> () (* TODO: this requires symbol table *) *)

(* and sem_retType = function
   | RetDataType _ -> () (* TODO: this requires symbol table *)
   | Nothing -> () (* TODO: this requires symbol table *) *)

and sem_localDefList = function [] -> () | ldl -> List.iter sem_localDef ldl

and sem_localDef = function
| L_FuncDef fd -> sem_funcDef fd
| L_FuncDecl fd -> sem_funcDecl fd
| L_varDef vd -> sem_varDef vd

and sem_funcDecl = function FuncDecl_Header h -> sem_header h
and sem_varDef = function { id_list = idl; var_type = vt } -> ()
(* TODO: insert to SymbolTable the variable's name and its type *)
(* let helper id =
     Hashtbl.add Symbol.HT id
       ({
          id;
          scope = None (* TODO *);
          kind =
            ENTRY_variable
              {
                variable_type = vt.data_type;
                variable_array_size = vt.array_dimension;
              };
        }
         : Symbol.entry)
   in
   List.iter helper idl;
   sem_varType vt *)

(* and sem_varType = function
   | { data_type = dt; array_dimension } -> sem_dataType dt
   (* TODO: this requires symbol table *) *)

and sem_block = function Block [] -> () | Block b -> List.iter sem_stmt b

and sem_stmt = function
| S_assignment (lv, e) ->
    (* TODO: compare the type of the lvalue in the SymbolTable with the
       type of the expression on the rhs *)
    ()
| S_block b -> sem_block b
| S_func_call fc ->
    (* TODO: check if there is a function with as many params in the
       SymbolTable (Warning if the type is non-nothing and isn't used) *)
    List.iter sem_expr fc.expr_list
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
    (* TODO: check in the SymbolTable if this return value has type matching
       the one of the function it is in *)
    match re with
    | None -> () (* TODO: this requires symbol table *)
    | Some v -> sem_expr v)
| S_semicolon -> ()

and sem_lvalue = function
(* TODO: maybe check the SymbolTable and get its type *)
| L_id _ -> () (* TODO: this requires symbol table *)
| L_string _ -> () (* TODO: this requires symbol table *)
| L_comp (lv, e) ->
    sem_lvalue lv;
    sem_expr
      e (* TODO: this has to be of integer type (this represents an array) *)

and sem_expr = function
| E_const_int ci -> () (* TODO: return its type *)
| E_const_char cc -> () (* TODO: return its type *)
| E_lvalue lv -> sem_lvalue lv
| E_func_call fc -> sem_funcCall fc
| E_sgn_expr (s, e) -> sem_expr e (* TODO: check for the type to be integer *)
| E_op_expr_expr (e1, ao, e2) ->
    () (* TODO: check for the type of the expressions to be integer *)
| E_expr_parenthesized e -> sem_expr e

and sem_cond = function
| C_not_cond (bo, c) ->
    sem_cond c (* TODO: check for the condition to be of bool type *)
| C_cond_cond (c1, bo, c2) ->
    sem_cond c1;
    (* TODO: check for the conditions' types to be of bool. Return type bool *)
    sem_cond c2
| C_expr_expr (e1, bo, e2) ->
    ()
    (* TODO: check for the types of e1, e2 to be the same. Return type bool *)
| C_cond_parenthesized c -> sem_cond c

and sem_funcCall = function
| _ -> () (* TODO: check the number and types of arguments given *)

and sem_on asts = sem_funcDef asts
