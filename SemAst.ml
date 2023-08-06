open Ast
open Symbol

let rec sem_funcDef = function
| { header = h; local_def_list = l; block = b } ->
    sem_header h;
    Printf.printf "Opening new scope for '%s' function\n" h.id;
    Symbol.open_scope ();
    sem_localDefList l;
    sem_block b;
    (let retTyp =
       match h.ret_type with
       | Nothing -> None
       | RetDataType ConstInt -> Some Types.T_int
       | RetDataType ConstChar -> Some Types.T_char
     in
     let rec type_of_ret_stmt = function
     | Block [] -> None
     | Block (h :: t) -> (
         match h with
         | S_return None -> None
         | S_return (Some e) -> Some (sem_expr e)
         | S_assignment _ | S_block _ | S_func_call _ | S_if _ | S_if_else _
          |S_while _ | S_semicolon ->
             type_of_ret_stmt (Block t))
     in
     let trs = type_of_ret_stmt b in
     if trs <> retTyp then (
       let rec to_str = function
       | None -> "nothing"
       | Some Types.T_int -> "int"
       | Some Types.T_char -> "char"
       | Some (Types.T_func t) -> to_str t
       | Some (Types.T_array t) ->
           String.concat " " [ to_str (Some t); "array" ]
       in
       Printf.eprintf "Expected type %s but got in return %s\n" (to_str retTyp)
         (to_str trs);
       failwith "Return statement doesn't return the expected type"));
    Printf.printf "Closing scope for '%s' function's declarations.\n" h.id;
    Symbol.close_scope ()

and sem_header = function
| { id = ident; fpar_def_list = fpdl; ret_type = rt } -> (
    match look_up_entry ident with
    | None ->
        enter_function ident (sem_fparDefList fpdl)
          (T_func
             (match rt with
             | Nothing -> None
             | RetDataType ConstInt -> Some Types.T_int
             | RetDataType ConstChar -> Some Types.T_char))
    | Some ent ->
        if
          ent.id <> ident
          || ((match ent.kind with
              | ENTRY_function ef -> ef.return_type
              | ENTRY_none | ENTRY_variable _ | ENTRY_parameter _ ->
                  assert false)
             <>
             match rt with
             | Nothing -> Types.T_func None
             | RetDataType ConstInt -> Types.T_func (Some Types.T_int)
             | RetDataType ConstChar -> Types.T_func (Some Types.T_char))
          || ent.scope <> !current_scope
        then (
          Printf.eprintf "Function %s is defined more than once.\n" ident;
          failwith "Function's signature is defined more than once"))

and sem_fparDefList = function
| [] -> []
| h :: t -> sem_fparDef h :: sem_fparDefList t

and sem_fparDef = function
| { ref = r; id_list = il; fpar_type = fpt } ->
    let t = if fpt.data_type = ConstInt then Types.T_int else Types.T_char in
    ( List.length il,
      (t, fpt.array_dimensions, if r = true then BY_REFERENCE else BY_VALUE) )

and sem_localDefList = function [] -> () | ldl -> List.iter sem_localDef ldl

and sem_localDef = function
| L_FuncDef fd -> sem_funcDef fd
| L_FuncDecl fd -> sem_funcDecl fd
| L_varDef vd -> sem_varDef vd

and sem_funcDecl = function FuncDecl_Header h -> sem_header h

and sem_varDef = function
| { id_list = idl; var_type = vt } ->
    let vTyp =
      match vt.data_type with
      | ConstInt -> Types.T_int
      | ConstChar -> Types.T_char
    in
    let typ =
      let rec getArrayType len accum =
        match len with
        | 0 -> accum
        | len -> getArrayType (len - 1) (Types.T_array accum)
      in
      getArrayType (List.length vt.array_dimensions) vTyp
    in
    let helper i = enter_variable i typ vt.array_dimensions in
    List.iter helper idl

and sem_block = function Block [] -> () | Block b -> List.iter sem_stmt b

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
| S_return re -> () (* this check is happening in sem_funcDef *)
| S_semicolon -> ()

and sem_lvalue = function
| L_id id -> (
    match look_up_entry id with
    | Some e -> (
        match e.kind with
        | ENTRY_variable ev -> ev.variable_type
        | ENTRY_parameter ep -> ep.parameter_type
        | ENTRY_none | ENTRY_function _ -> assert false)
    | None ->
        Printf.eprintf "Undefined variable %s is being used.\n" id;
        failwith "Undefined variable")
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
        Printf.eprintf "Function %s returns nothing." fc.id;
        failwith "A function of type nothing is being used as an expression"
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
    else (
      Printf.eprintf "Arguments' types of function %s don't match" ident;
      failwith "The arguments' types don't match")

and sem_on asts =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
