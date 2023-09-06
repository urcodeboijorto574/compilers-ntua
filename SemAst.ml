open Ast
open Symbol

(** [isMainProgram] is a variable that has [true] if the main function
    of a program is currently analysed and [false] if not. *)
let isMainProgram = ref true

(** [sem_funcDef (fd : Ast.funcDef)] semantically analyses the function definition [fd].
    After semantically analysing the header, local definitions list and the block,
    it is checked if in the function's block a value of the expected type is
    returned.
    Returns [unit]. *)
let rec sem_funcDef = function
  | { header = h; local_def_list = l; block = b } ->
      sem_header true h;
      isMainProgram := false;
      sem_localDefList l;
      sem_block b;

      let expectedReturnType = Types.(T_func (t_type_of_retType h.ret_type)) in
      let typeReturnedInBlock =
        let rec type_of_block : block -> Types.t_type option = function
          | Block [] -> None
          | Block (headStmt :: tailStmt) -> (
              let rec type_of_stmt : stmt -> Types.t_type option = function
                | S_block b -> type_of_block b
                | S_return x -> (
                    match x with None -> None | Some e -> Some (sem_expr e))
                | S_if_else (c, s1, s2) -> (
                    let type_of_s1 = type_of_stmt s1 in
                    let type_of_s2 = type_of_stmt s2 in
                    if type_of_s1 = type_of_s2 then
                      type_of_s1
                    else
                      match Ast.get_const_cond_value c with
                      | Some constValue ->
                          Printf.eprintf
                            "Warning: In an if-then-else statement two \
                             different types are returned. However, the type \
                             of ";
                          if constValue then (
                            Printf.eprintf "'else' branch is never returned.\n";
                            type_of_s1)
                          else (
                            Printf.eprintf "'then' branch is never returned.\n";
                            type_of_s2)
                      | None ->
                          Printf.eprintf
                            "Error: In an if-then-else statement two different \
                             types are returned.\n";
                          failwith "Multiple types returned in if-then-else")
                | S_assignment _ | S_func_call _ | S_if _ | S_while _
                 |S_semicolon ->
                    None
              in
              match type_of_stmt headStmt with
              | Some t -> Some t
              | None -> type_of_block (Block tailStmt))
        in
        Types.T_func (type_of_block b)
      in
      if expectedReturnType <> typeReturnedInBlock then (
        Printf.eprintf
          "Error: In function '%s': Expected type %s but got %s instead.\n" h.id
          (Types.string_of_type expectedReturnType)
          (Types.string_of_type typeReturnedInBlock);
        failwith "Return statement doesn't return the expected type");
      Printf.printf "Closing scope for '%s' function's declarations.\n" h.id;
      Symbol.close_scope ()

(** [sem_header (isPartOfAFuncDef : bool) (h : Ast.header)] takes [isPartOfAFuncDef]
    ([true] when the header is part of a function definition and [false] when
    it's part of a function declaration) and the function's header [h].
    If [h] is part of a function definition, then a new scope is opened and the
    function's parameters are inserted in it.
    Returns [unit]. *)
and sem_header isPartOfAFuncDef = function
  | { id = ident; fpar_def_list = fpdl; ret_type = rt } ->
      if !isMainProgram && rt <> Nothing then (
        Printf.eprintf "Error: Main function must return 'nothing' type\n";
        failwith "Main function should return nothing")
      else if !isMainProgram && fpdl <> [] then (
        Printf.eprintf "Error: Main function shouldn't have parameters\n";
        failwith "Main function shouldn't have parameters");

      let returnTypeFromThisHeader = Types.(T_func (t_type_of_retType rt)) in
      begin
        match look_up_entry_temp ident with
        | None ->
            enter_function ident (sem_fparDefList fpdl) returnTypeFromThisHeader
        | Some entry -> begin
            let expectedReturnTypeFromSymbolTable =
              match entry.kind with
              | ENTRY_function ef -> ef.return_type
              | ENTRY_variable _ | ENTRY_parameter _ -> assert false
            in
            if entry.scope.name <> !current_scope.name then (
              Printf.eprintf
                "Error: The name '%s' is used for two functions in different \
                 scopes.\n"
                ident;
              failwith "Function overloading is not permitted")
            else if
              expectedReturnTypeFromSymbolTable <> returnTypeFromThisHeader
            then (
              Printf.eprintf
                "Error: Function %s's return type differs between declarations.\n"
                ident;
              failwith "Function's return type differs between declarations")
            else if
              (* TODO: in the section below it is checked whether the type of
                 parameters are the same between declarations. However, the
                 names of the parameters are not checked. This can be decided
                 after consideration. *)
              begin
                let paramListFromSymbolTable : Symbol.entry_parameter list =
                  match entry.kind with
                  | ENTRY_function funcEntry -> funcEntry.parameters_list
                  | ENTRY_variable _ | ENTRY_parameter _ -> assert false
                in
                let paramListFromThisHeader : (int * Types.t_type * bool) list =
                  let rec helper_function :
                      fparDef list -> (int * Types.t_type * bool) list =
                    function
                    | [] -> []
                    | { ref = r; id_list = il; fpar_type = fpt } :: tail ->
                        let wholeType =
                          Types.construct_array_type fpt.array_dimensions
                            (Types.t_type_of_dataType fpt.data_type)
                        in
                        (List.length il, wholeType, r) :: helper_function tail
                  in
                  helper_function fpdl
                in
                let lists_are_equal paramListEntry paramListHeader =
                  let compareElements x y =
                    match y with
                    | _, t, r ->
                        x.parameter_type = t
                        && x.passing = Symbol.BY_REFERENCE == r
                  in
                  List.for_all2 compareElements paramListEntry
                    paramListFromThisHeader
                in
                lists_are_equal paramListFromSymbolTable paramListFromThisHeader
              end
            then (
              Printf.eprintf
                "Error: Function %s's parameters differ between declarations.\n"
                ident;
              failwith "Function's parameters differ between declarations")
            else if not isPartOfAFuncDef then
              Printf.printf "Warning: Redeclaration of function '%s'" ident
          end
      end;

      if isPartOfAFuncDef then begin
        Printf.printf "Opening new scope for '%s' function\n" ident;
        Symbol.open_scope ident;

        let add_fparDef_to_scope : fparDef -> unit = function
          | { ref = r; id_list = idl; fpar_type = fpt } ->
              let rec add_param_names_to_scope : string list -> unit = function
                | [] -> ()
                | headId :: tailId ->
                    !current_scope.scope_entries <-
                      {
                        id = headId;
                        scope = !current_scope;
                        kind =
                          ENTRY_parameter
                            {
                              parameter_type =
                                Types.construct_array_type fpt.array_dimensions
                                  (Types.t_type_of_dataType fpt.data_type);
                              passing =
                                (if r then
                                   Symbol.BY_REFERENCE
                                 else
                                   Symbol.BY_VALUE);
                            };
                      }
                      :: !current_scope.scope_entries;
                    add_param_names_to_scope tailId
              in
              add_param_names_to_scope idl
        in
        List.iter add_fparDef_to_scope fpdl
      end

(** [sem_fparDefList (fpdl : Ast.fparDef list)] semantically analyses the
    function's parameter definitions [fpdl].
    Returns [(int * Types.t_type * Symbol.param_passing) list]. *)
and sem_fparDefList = function
  | [] -> []
  | h :: t -> sem_fparDef h :: sem_fparDefList t

(** [sem_fparDef (fpd : Ast.fparDef)] semantically analyses the function's
    parameter definition [fpd].
    Returns [int * Types.t_type * Symbol.param_passing]. *)
and sem_fparDef = function
  | { ref = r; id_list = il; fpar_type = fpt } ->
      let paramIsArray = fpt.array_dimensions <> [] in
      let passedByValue = not r in
      if paramIsArray && passedByValue then (
        Printf.eprintf
          "Error: arrrays should always be passed as parameters by reference.\n";
        failwith "Array passed as a parameter by value");
      let completeType =
        let dataType = Types.t_type_of_dataType fpt.data_type in
        Types.construct_array_type fpt.array_dimensions dataType
      in
      (List.length il, completeType, if r then BY_REFERENCE else BY_VALUE)

(** [sem_localDefList (ldl : Ast.localDef list)] semantically analyses the
    function's local definitions list [ldl].
    Returns [unit]. *)
and sem_localDefList = function [] -> () | ldl -> List.iter sem_localDef ldl

(** [sem_localDef (ld : Ast.localDef)] adds in the symbolTable the functions and
    parameters defined in the local
    definition [ld].
    Returns [unit]. *)
and sem_localDef = function
  | L_FuncDef fd -> sem_funcDef fd
  | L_FuncDecl fd -> sem_funcDecl fd
  | L_varDef vd -> sem_varDef vd

(** [sem_funcDecl (fd : Ast.funcDef)] semantically analyses the header of the
    function declaration [fd] (uses the function [sem_header]).
    Returns [unit]. *)
and sem_funcDecl = function FuncDecl_Header h -> sem_header false h

(** [sem_varDef (vd : Ast.varDef)] enters in the symbolTable every variable
    defined in the variable definition [vd].
    Returns [unit]. *)
and sem_varDef = function
  | { id_list = idl; var_type = vt } ->
      let wholeType =
        let dataType = Types.t_type_of_dataType vt.data_type in
        Types.construct_array_type vt.array_dimensions dataType
      in
      let helper i = enter_variable i wholeType in
      List.iter helper idl

(** [sem_block (bl : Ast.block)] semantically analyses every statement of the
    block [bl].
    Returns [unit]. *)
and sem_block = function Block [] -> () | Block b -> List.iter sem_stmt b

(** [sem_stmt (s : Ast.stmt)] semantically analyses the statement [s].
    Returns [unit]. *)
and sem_stmt = function
  | S_assignment (lv, e) -> (
      match sem_lvalue lv with
      | Types.T_array _ ->
          Printf.eprintf
            "Error: Assignment to an l-value of type array is not possible.\n";
          failwith "Assignment to array" (* Types.equal_type t (sem_expr e) *)
      | Types.T_func _ ->
          Printf.eprintf
            "Error: Assignment to a function call is not possible.\n";
          failwith "Assignment to function"
      | t ->
          Printf.printf
            "... checking the types of an lvalue and an expression (assignment)\n";
          Types.equal_type t (sem_expr e))
  | S_block b -> sem_block b
  | S_func_call fc -> (
      let open Types in
      match sem_funcCall fc with
      | T_func None -> ()
      | T_func (Some _) ->
          Printf.eprintf
            "Warning: The return value of the function %s is not used.\n" fc.id
      | T_int | T_char | T_array _ -> assert false)
  | S_if (c, s) ->
      sem_cond c;
      sem_stmt s;
      Ast.check_condition c
  | S_if_else (c, s1, s2) ->
      sem_cond c;
      sem_stmt s1;
      sem_stmt s2;
      Ast.check_condition c
  | S_while (c, s) ->
      sem_cond c;
      sem_stmt s;
      Ast.check_condition c
  | S_return re -> () (* this check is happening in sem_funcDef *)
  | S_semicolon -> ()

(** [sem_lvalue (lv : Ast.lvalue)] returns the type of the l-value [lv].
    Returns [Types.t_type]. *)
and sem_lvalue = function
  | L_id id -> (
      match look_up_entry_temp id with
      | Some e -> (
          Printf.printf "Entry for %s found. Information:\n" id;
          Printf.printf "\tid: %s, scope: %s" id e.scope.name;
          let entryType =
            match e.kind with
            | ENTRY_variable ev -> ev.variable_type
            | ENTRY_parameter ep -> ep.parameter_type
            | ENTRY_function ef -> ef.return_type
          in
          Printf.printf ", type: %s\n" (Types.string_of_type entryType);
          let print_type t =
            Printf.printf "<<%s>> type\n" (Types.string_of_type t)
          in
          match e.kind with
          | ENTRY_variable ev ->
              print_type ev.variable_type;
              ev.variable_type
          | ENTRY_parameter ep ->
              print_type ep.parameter_type;
              ep.parameter_type
          | ENTRY_function _ -> assert false)
      | None ->
          Printf.eprintf "Error: Undefined variable %s is being used.\n" id;
          failwith "Undefined variable")
  | L_string s -> Types.T_array (Types.T_char, 0)
  | L_comp (lv, e) -> (
      (* (Its value must be at most n - 1, if n is the size of the array). *)
      Printf.printf
        "... checking the type of the content inside the brackets (position in \
         array must be an integer)\n";
      Types.(equal_type T_int (sem_expr e));
      match sem_lvalue lv with
      | Types.T_array (t, n) ->
          (* Check for segmentation fault if the expression has a constant value. *)
          begin
            match Ast.get_const_expr_value e with
            | None -> ()
            | Some index ->
                if index < 0 || index >= n then (
                  Printf.eprintf "Error: Segmentation fault.\n";
                  failwith "Segmentation fault")
          end;
          Printf.printf "<<%s>> type\n" (Types.string_of_type t);
          t
      | _ ->
          Printf.eprintf
            "Error: Iteration cannot happen in non-array type of \
             variables/functions.\n";
          failwith "Iteration on non-array type of variable")

(** [sem_expr (e : Ast.expr)] returns the type of the expression [e].
    Returns [Types.t_type]. *)
and sem_expr = function
  | E_const_int ci -> Types.T_int
  | E_const_char cc -> Types.T_char
  | E_lvalue lv ->
      let lvalue_type = sem_lvalue lv in
      begin
        match lv with
        | L_comp _ ->
            Printf.printf "Composite l-value is of type '%s'"
              (Types.string_of_type lvalue_type)
        | _ -> ()
      end;
      lvalue_type
  | E_func_call fc -> (
      match sem_funcCall fc with
      | Types.T_func None ->
          Printf.eprintf "Error: Function %s returns nothing.\n" fc.id;
          failwith "A function of type nothing is being used as an expression"
      | Types.T_func (Some t) -> t
      | Types.T_int | Types.T_char | Types.T_array _ ->
          (* A function must always be inserted in the SymbolTable with type
             Types.T_func (t : Types.t_type option) *)
          assert false)
  | E_sgn_expr (s, e) ->
      Printf.printf "... checking a signed expression (must be int)\n";
      Types.equal_type Types.T_int (sem_expr e);
      Types.T_int
  | E_op_expr_expr (e1, ao, e2) ->
      Printf.printf
        "... checking whether the arguments of an arithmOperator are of type int\n";
      Types.equal_type Types.T_int (sem_expr e1);
      Types.equal_type Types.T_int (sem_expr e2);
      Types.T_int
  | E_expr_parenthesized e -> sem_expr e

(** [sem_cond (c : Ast.cond)] semantically analyses condition [c].
    Returns [unit]. *)
and sem_cond = function
  | C_not_cond (lo, c) -> sem_cond c
  | C_cond_cond (c1, lo, c2) ->
      sem_cond c1;
      sem_cond c2
  | C_expr_expr (e1, co, e2) ->
      Printf.printf
        "... checking whether the arguments of a compOperator are of the same \
         type\n";
      let typ1, typ2 = (sem_expr e1, sem_expr e2) in
      Types.equal_type typ1 typ2
  | C_cond_parenthesized c -> sem_cond c

(** [sem_funcCall (fc : Ast.funcCall)] returns the return type of function call
    [fc]. Additionally, it checks if the types of its arguments match the
    expected ones defined in the function's header.
    Returns [Types.t_type]. *)
and sem_funcCall = function
  | { id = ident; expr_list = el } ->
      let entr =
        match look_up_entry_temp ident with
        | Some e -> e
        | None ->
            Printf.eprintf "Error: Function %s is called, but never declared\n"
              ident;
            failwith "Undeclared function called"
      in
      (* [exprTypesList] is a list of [Types.t_type] and the nth element
         is the type of the nth argument of the function call *)
      let exprTypesList = List.map sem_expr el in
      (* [paramTypesList] is a list of [Types.t_type] and the nth element
         is the type of the nth parameter of the function *)
      let paramTypesList =
        match entr.kind with
        | ENTRY_function ef ->
            let rec getEntryTypesList = function
              | [] -> []
              | { parameter_type = pt; _ } :: t -> pt :: getEntryTypesList t
            in
            getEntryTypesList ef.parameters_list
        | ENTRY_variable _ | ENTRY_parameter _ -> assert false
      in
      (* [paramByRefList] is a list of [bool] and the nth element is [true] if
         the nth parameter of the function is passed by reference, and [false]
         otherwise. *)
      let paramByRefList =
        match entr.kind with
        | ENTRY_function ef ->
            let rec getRefList = function
              | [] -> []
              | { passing = p; _ } :: t ->
                  (p = Symbol.BY_REFERENCE) :: getRefList t
            in
            getRefList ef.parameters_list
        | ENTRY_variable _ | ENTRY_parameter _ -> assert false
      in
      (* Check if the number of arguments is the expected one *)
      if List.length exprTypesList <> List.length paramTypesList then (
        Printf.eprintf
          "Error: Function called without the expected number of parameters.\n";
        failwith "Unexpected number of parameters in function call");
      (* Check if the parameters passed by reference are l-values *)
      begin
        (* [f] checks if the nth expression of the [expressionList] is an
           l-value if the nth element of the [byRefList] is [true].
           Returns [unit] if all checks pass. *)
        let rec f expressionList byRefList =
          match (expressionList, byRefList) with
          | e :: exprTail, r :: refTail ->
              if r then (
                match e with
                | E_lvalue _ -> ()
                | E_const_int _ | E_const_char _ | E_func_call _
                 |E_sgn_expr _ | E_op_expr_expr _ | E_expr_parenthesized _ ->
                    Printf.eprintf
                      "Error: Expression passed by reference isn't an l-value.\n";
                    failwith "Parameter passed by reference must be an l-value")
              else
                f exprTail refTail
          | [], [] -> ()
          | _ ->
              failwith "Unexpected number of parameters given in function call"
        in
        f el paramByRefList
      end;
      let bool_of_unit_func f x y =
        Printf.printf
          "... checking whether the arguments of funcCall %s are indeed the \
           declared types\n"
          ident;
        Printf.printf "Got expression of type %s\n" (Types.string_of_type x);
        Printf.printf "Expected type %s\n" (Types.string_of_type y);
        f x y;
        true
      in
      if
        List.equal
          (bool_of_unit_func Types.equal_type)
          exprTypesList paramTypesList
      then
        match entr.kind with
        | ENTRY_function ef -> ef.return_type
        | ENTRY_variable _ | ENTRY_parameter _ -> assert false
      else (
        Printf.eprintf "Error: Arguments' types of function %s don't match"
          ident;
        failwith "The arguments' types don't match")

(** [sem_on (ast : Ast.funcDef)] semantically analyses the root of the ast [ast]
    (produced by the parser). It also initializes the SymbolTable.
    Returns [unit]. *)
and sem_on asts =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
