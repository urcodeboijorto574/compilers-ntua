open Ast
open Symbol

let rec sem_funcDef = function
| { header = h; local_def_list = l; block = b } ->
    (* TODO: must add procedure that checks if the block has a return statement.
       That statement must return the expected type, declared in the header. *)
    sem_header h;
    sem_localDefList l;
    sem_block b

and sem_header = function
(* TODO: insert to SymbolTable the identifier and the return type
   of the function, as well as the number and types of parameters and their
   types of passing *)
| { id; fpar_def_list = fpdl; ret_type = rt } -> sem_fparDefList fpdl

and sem_fparDefList = function
(* TODO: insert to SymbolTable the number and types of the parameters, and the
   type of passing *)
| [] -> ()
| fpdl -> List.iter sem_fparDef fpdl

and sem_fparDef = function
(* TODO: insert all elements in SymbolTable *)
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
    match sem_lvalue lv with
    | Types.T_array t -> Types.equal_type t (sem_expr e)
    | t -> Types.equal_type t (sem_expr e))
| S_block b -> sem_block b
| S_func_call fc -> (
    match sem_funcCall fc with
    | Types.T_func None -> ()
    | Types.T_func (Some _) ->
        Printf.eprintf "The return value of the function %s is not used.\n"
          fc.id
    | Types.T_int | Types.T_char | Types.T_array _ -> ())
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
    | None -> ()
    | Some v ->
        Types.equal_type (sem_expr v)
          Types.T_int (* placeholder, needs SymbolTable *))
| S_semicolon -> ()

and sem_lvalue = function
| L_id id -> (
    match look_up_entry id with
    | Some e -> (
        match e.kind with
        | ENTRY_variable ev -> ev.variable_type
        | ENTRY_parameter ep -> ep.parameter_type
        | ENTRY_none | ENTRY_function _ -> assert false)
    | None -> failwith "Undefined variable.\n")
| L_string s -> Types.T_array Types.T_char
| L_comp (lv, e) ->
    (* (Its value must be at most n - 1, if n is the size of the array). *)
    Types.equal_type Types.T_int (sem_expr e);
    Types.T_array (sem_lvalue lv)

and sem_expr = function
| E_const_int ci -> Types.T_int
| E_const_char cc -> Types.T_char
| E_lvalue lv -> sem_lvalue lv
| E_func_call fc -> (
    match sem_funcCall fc with
    | Types.T_func None ->
        failwith "A function of type void is being used as an expression.\n"
    | Types.T_func (Some t) -> t
    | Types.T_int | Types.T_char | Types.T_array _ -> assert false)
| E_sgn_expr (s, e) ->
    Types.equal_type Types.T_int (sem_expr e);
    Types.T_int
| E_op_expr_expr (e1, ao, e2) ->
    Types.equal_type Types.T_int (sem_expr e1);
    Types.equal_type Types.T_int (sem_expr e2);
    Types.T_int
| E_expr_parenthesized e -> sem_expr e

and sem_cond = function
| C_not_cond (lo, c) -> sem_cond c
| C_cond_cond (c1, lo, c2) -> assert (sem_cond c1 = sem_cond c2)
| C_expr_expr (e1, co, e2) ->
    let typ1, typ2 = (sem_expr e1, sem_expr e2) in
    Types.equal_type typ1 typ2
| C_cond_parenthesized c -> sem_cond c

and sem_funcCall = function
| { id = ident; expr_list = el } ->
    let el_types = List.map sem_expr el in
    let getV = function None -> failwith "no value" | Some v -> v in
    let e = getV (look_up_entry ident) in
    let pl_types =
      match e.kind with
      | ENTRY_function ef ->
          let rec getEntryTypesList = function
          | [] -> []
          | { parameter_type = pt; _ } :: t -> pt :: getEntryTypesList t
          in
          getEntryTypesList ef.parameters_list
      | ENTRY_none | ENTRY_variable _ | ENTRY_parameter _ -> assert false
    in
    let bool_from_unit f x y =
      f x y;
      true
    in
    if List.equal (bool_from_unit Types.equal_type) el_types pl_types then
      match e.kind with
      | ENTRY_function ef -> ef.return_type
      | ENTRY_none | ENTRY_variable _ | ENTRY_parameter _ -> assert false
    else
      failwith "The arguments' types don't match.\n"

and sem_on asts =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
