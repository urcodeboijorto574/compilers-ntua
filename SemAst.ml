open Ast
open Symbol

(* Exceptions used by sem_header and sem_funcCall *)
exception Shared_name_func_var
exception Overloaded_functions
exception Redifined_function
exception Expected_type_not_returned
exception Non_matching_parameter_types
exception Unexpected_number_of_parameters
exception Type_error
exception Passing_error

(** [funcDefAncestors] is a stack that stores all the ancestors of a funcDef in
    runtime. *)
let funcDefAncestors = Stack.create ()

(** [sem_funcDef (fd : Ast.funcDef)] semantically analyses the function
    definition [fd]. After semantically analysing the header, local definitions
    list and the block, it is checked if in the function's block a value of the
    expected type is returned. Returns [unit]. *)
let rec sem_funcDef fd =
  match fd with
  | { header = h; local_def_list = l; block = b } ->
      let isMainProgram = !current_scope.depth = 0 in
      if isMainProgram then Symbol.add_standard_library ();
      if isMainProgram then Stack.push None funcDefAncestors;
      fd.parent_func <- Stack.top funcDefAncestors;
      sem_header true h;
      Stack.push (Some fd) funcDefAncestors;
      sem_localDefList l;
      ignore (Stack.pop funcDefAncestors);

      let overloadedParVarNameOption =
        let duplicate_element lst =
          let rec helper seen = function
            | [] -> None
            | h :: t -> if List.mem h seen then Some h else helper (h :: seen) t
          in
          helper [] lst
        in
        let parNames : string list =
          let rec get_par_names = function
            | [] -> []
            | { id_list = il; ref; fpar_type } :: tail ->
                il @ get_par_names tail
          in
          let resultList = get_par_names h.fpar_def_list in
          let overloadedParNameOption = duplicate_element resultList in
          if overloadedParNameOption <> None then (
            Printf.eprintf
              "\027[31mError\027[0m: Parameter name '%s' in function '%s' is \
               used twice.\n"
              (Option.get overloadedParNameOption)
              h.id;
            failwith "Overloaded parameter name");
          resultList
        in
        let varNames : string list =
          let rec get_var_names = function
            | [] -> []
            | L_funcDef _ :: tail | L_funcDecl _ :: tail -> get_var_names tail
            | L_varDef vd :: tail -> vd.id_list @ get_var_names tail
          in
          let resultList = get_var_names l in
          let overloadedVarNameOption = duplicate_element resultList in
          if overloadedVarNameOption <> None then (
            Printf.eprintf
              "\027[31mError\027[0m: Variable '%s' is declared twice in the \
               function '%s'.\n"
              (Option.get overloadedVarNameOption)
              h.id;
            failwith "Overloaded variable name");
          resultList
        in
        let rec share_common_elem l1 l2 =
          match l1 with
          | [] -> None
          | head :: tail ->
              if List.mem head l2 then Some head else share_common_elem tail l2
        in
        share_common_elem parNames varNames
      in
      if overloadedParVarNameOption <> None then (
        Printf.eprintf
          "\027[31mError\027[0m: The name '%s' is shared between a variable \
           and a parameter in the function '%s'.\n"
          (Option.get overloadedParVarNameOption)
          h.id;
        failwith "Overloaded variable/parameter name");

      let isMainProgram = !current_scope.depth = 1 in
      if isMainProgram then begin
        let funcIdListOption = Symbol.get_undefined_functions () in
        if funcIdListOption <> None then (
          List.iter
            (fun fid ->
              Printf.eprintf
                "\027[31mError\027[0m: Function '%s' is declared, but not \
                 defined.\n"
                fid)
            (Option.get funcIdListOption);
          failwith "Undefined function")
      end;
      let expectedReturnType = Ast.t_type_of_retType h.ret_type in
      let typeReturnedInBlock =
        Types.T_func (match sem_block b with None -> T_none | Some t -> t)
      in
      if expectedReturnType <> typeReturnedInBlock then (
        Printf.eprintf
          "\027[31mError\027[0m: In function '%s': Expected type %s but got %s \
           instead.\n"
          h.id
          (Types.string_of_t_type expectedReturnType)
          (Types.string_of_t_type typeReturnedInBlock);
        failwith "Return statement doesn't return the expected type");
      if Types.debugMode then
        Printf.printf "Closing scope for '%s' function's declarations.\n" h.id;
      Symbol.close_scope () (* TODO: raise warning for unused variables. *)

(** [sem_header (isPartOfAFuncDef : bool) (h : Ast.header)] takes
    [isPartOfAFuncDef] ([true] when the header is part of a function definition
    and [false] when it's part of a function declaration) and the function's
    header [h]. If [h] is part of a function definition, then a new scope is
    opened and the function's parameters are inserted in it. Returns [unit]. *)
and sem_header isPartOfAFuncDef = function
  | { id = ident; fpar_def_list = fpdl; ret_type = rt } -> (
      let isMainProgram = !current_scope.depth = 0 in
      if isMainProgram then
        if rt <> Nothing then (
          Printf.eprintf
            "\027[31mError\027[0m: Main function must return 'nothing' type.\n";
          failwith "Main function should return nothing")
        else if fpdl <> [] then (
          Printf.eprintf
            "\027[31mError\027[0m: Main function shouldn't have parameters.\n";
          failwith "Main function shouldn't have parameters");

      let add_params_to_scope () =
        if isPartOfAFuncDef then begin
          if Types.debugMode then
            Printf.printf "Opening new scope for '%s' function\n" ident;
          Symbol.open_scope ident;
          let add_fparDef : fparDef -> unit = function
            | { ref = r; id_list = idl; fpar_type = fpt } ->
                List.iter
                  (fun id -> enter_parameter id (Ast.t_type_of_fparType fpt) r)
                  idl
          in
          List.iter add_fparDef fpdl
        end
      in
      let resultLookUpOption = look_up_entry ident in
      if resultLookUpOption = None then (
        enter_function ident (sem_fparDefList fpdl) (Ast.t_type_of_retType rt)
          Symbol.(if isPartOfAFuncDef then DEFINED else DECLARED);
        add_params_to_scope ())
      else
        try
          let functionEntry =
            match (Option.get resultLookUpOption).kind with
            | ENTRY_function ef -> ef
            | ENTRY_variable _ | ENTRY_parameter _ -> raise Shared_name_func_var
          in
          if Types.debugMode then (
            Printf.printf "Parameter list from ST:\n\t[ ";
            List.iter
              (fun ep ->
                Printf.printf "{ type('%s'), pass('%s') } "
                  (Types.string_of_t_type ep.parameter_type)
                  (if ep.passing = Symbol.BY_VALUE then
                     "byVal"
                   else
                     "byRef"))
              functionEntry.parameters_list;
            Printf.printf "]\n");
          let returnTypeFromHeader = Ast.t_type_of_retType rt in
          let paramListFromHeader : (int * Types.t_type * bool) list =
            let rec helper : fparDef list -> (int * Types.t_type * bool) list =
              function
              | [] -> []
              | { ref = r; id_list = il; fpar_type = fpt } :: tail ->
                  let paramType = Ast.t_type_of_fparType fpt in
                  (List.length il, paramType, r) :: helper tail
            in
            let resultList = helper fpdl in
            if Types.debugMode then (
              Printf.printf "Parameter list from this header:\n\t[ ";
              List.iter
                begin
                  let rec print_elem = function
                    | 0, t, r -> ()
                    | n, t, r ->
                        Printf.printf "{ type('%s'), pass('%s') } "
                          (Types.string_of_t_type t)
                          (if r then "byRef" else "byVal");
                        print_elem (n - 1, t, r)
                  in
                  print_elem
                end
                resultList;
              Printf.printf "]\n");
            resultList
          in
          let matchingNumOfParams =
            let lengthOfParamListHeader =
              let rec f accum = function
                | [] -> accum
                | (n, _, _) :: tail -> f (accum + n) tail
              in
              f 0 paramListFromHeader
            in
            List.length functionEntry.parameters_list = lengthOfParamListHeader
          in
          let matchingParamTypes =
            let lists_are_equal paramListEntry paramListHeader =
              let elems_are_equal x y =
                match y with
                | _, t, r ->
                    x.parameter_type = t || x.passing = Symbol.BY_REFERENCE = r
              in
              if List.length paramListEntry <> List.length paramListHeader then
                false
              else
                List.for_all2 elems_are_equal paramListEntry paramListHeader
            in
            lists_are_equal functionEntry.parameters_list paramListFromHeader
          in

          if Types.debugMode then
            Printf.printf
              "(Option.get resultLookUpOption).scope.depth = %d, \
               !current_scope.depth = %d\n"
              (Option.get resultLookUpOption).scope.depth !current_scope.depth;

          if
            not
              (Symbol.equal_scopes (Option.get resultLookUpOption).scope
                 !current_scope)
          then (
            enter_function ident (sem_fparDefList fpdl)
              (Ast.t_type_of_retType rt)
              Symbol.(if isPartOfAFuncDef then DEFINED else DECLARED);
            add_params_to_scope ())
          else if not matchingNumOfParams then
            raise Overloaded_functions
          else if functionEntry.return_type <> returnTypeFromHeader then
            raise Expected_type_not_returned
          else if not matchingParamTypes then
            raise Non_matching_parameter_types
          else if functionEntry.state = Symbol.DEFINED then
            raise Redifined_function
          else (
            Symbol.set_func_defined functionEntry;
            add_params_to_scope ())
        with
        | Shared_name_func_var ->
            Printf.eprintf
              "\027[31mError\027[0m: Name '%s' is shared with a function and a \
               variable.\n"
              ident;
            failwith "Function and variable share the same name"
        | Overloaded_functions ->
            Printf.eprintf
              "\027[31mError\027[0m: Function '%s' is overloaded.\n" ident;
            failwith "Function overload"
        | Redifined_function ->
            Printf.eprintf
              "\027[31mError\027[0m: Function '%s' is defined twice.\n" ident;
            failwith "Redefinition of function"
        | Expected_type_not_returned ->
            Printf.eprintf
              "\027[31mError\027[0m: Return type of function '%s' differs \
               between declarations\n"
              ident;
            failwith "Function's return type differs between declarations"
        | Non_matching_parameter_types ->
            Printf.eprintf
              "\027[31mError\027[0m: Parameter types of function '%s' differ \
               between declarations.\n"
              ident;
            failwith "Parameter types differ between declarations")

(** [sem_fparDefList (fpdl : Ast.fparDef list)] semantically analyses the
    function's parameter definitions [fpdl]. Returns
    [(int * Types.t_type * Symbol.param_passing) list]. *)
and sem_fparDefList = function
  | [] -> []
  | h :: t -> sem_fparDef h :: sem_fparDefList t

(** [sem_fparDef (fpd : Ast.fparDef)] semantically analyses the function's
    parameter definition [fpd]. Returns
    [int * Types.t_type * Symbol.param_passing]. *)
and sem_fparDef = function
  | { ref = r; id_list = il; fpar_type = fpt } ->
      if List.exists (fun n -> n = 0) fpt.array_dimensions then (
        Printf.eprintf
          "\027[31mError\027[0m: Array declared to have size a non-positive \
           number.\n";
        failwith "Array of zero size");
      let paramIsArray = fpt.array_dimensions <> [] in
      let passedByValue = not r in
      if paramIsArray && passedByValue then (
        Printf.eprintf
          "\027[31mError\027[0m: Arrays should always be passed as parameters \
           by reference.\n";
        failwith "Array passed as a parameter by value");
      ( List.length il,
        Ast.t_type_of_fparType fpt,
        if r then BY_REFERENCE else BY_VALUE )

(** [sem_localDefList (ldl : Ast.localDef list)] semantically analyses the
    function's local definitions list [ldl]. Returns [unit]. *)
and sem_localDefList = function [] -> () | ldl -> List.iter sem_localDef ldl

(** [sem_localDef (ld : Ast.localDef)] adds in the symbolTable the functions and
    parameters defined in the local definition [ld]. Returns [unit]. *)
and sem_localDef = function
  | L_funcDef fd -> sem_funcDef fd
  | L_funcDecl fd -> sem_funcDecl fd
  | L_varDef vd -> sem_varDef vd

(** [sem_funcDecl (fd : Ast.funcDef)] semantically analyses the header of the
    function declaration [fd] (uses the function [sem_header]). Returns [unit]. *)
and sem_funcDecl = function FuncDecl_Header h -> sem_header false h

(** [sem_varDef (vd : Ast.varDef)] enters in the symbolTable every variable
    defined in the variable definition [vd]. Returns [unit]. *)
and sem_varDef = function
  | { id_list = idl; var_type = vt } ->
      if List.exists (fun n -> n = 0) vt.array_dimensions then (
        Printf.eprintf
          "\027[31mError\027[0m: Array declared to have size a non-positive \
           number.\n";
        failwith "Array of zero size");
      List.iter (fun i -> enter_variable i (Ast.t_type_of_varType vt)) idl

(** [sem_block (bl : Ast.block)] semantically analyses every statement of the
    block [bl]. Returns [Types.t_type option]. *)
and sem_block = function
  | Block [] -> None
  | Block stmtList ->
      let rec get_type_of_stmt_list res warningRaised = function
        | [] -> res
        | head :: tail -> (
            match sem_stmt head with
            | None -> get_type_of_stmt_list res warningRaised tail
            | Some typ ->
                if tail <> [] && not warningRaised then
                  Printf.eprintf
                    "Warning: A section of a block is never reached.\n";
                get_type_of_stmt_list
                  (if res = None then Some typ else res)
                  true tail)
      in
      get_type_of_stmt_list None false stmtList

(** [sem_stmt (s : Ast.stmt)] semantically analyses the statement [s] and
    returns [Some t] if [s] is a return statement or [None] if not. Returns
    [Types.t_type option]. *)
and sem_stmt = function
  | S_assignment (lv, e) -> (
      (match lv.lv_kind with
      | L_comp (L_string _, _) ->
          Printf.eprintf
            "\027[31mError\027[0m: Assignment to a string literal's element is \
             not possible.\n";
          failwith "Assignment to string literal"
      | _ -> ());
      match sem_lvalue lv with
      | Types.T_array _ ->
          Printf.eprintf
            "\027[31mError\027[0m: Assignment to an l-value of type array is \
             not possible.\n";
          failwith "Assignment to array"
      | Types.T_func _ ->
          Printf.eprintf
            "\027[31mError\027[0m: Assignment to a function call is not \
             possible.\n";
          failwith "Assignment to function"
      | t ->
          if Types.debugMode then
            Printf.printf
              "... checking the types of an lvalue and an expression \
               (assignment)\n";
          let typeExpr = sem_expr e in
          if not (Types.equal_types t typeExpr) then (
            Printf.eprintf
              "\027[31mError\027[0m: The value of an expression of type %s is \
               tried to be assigned to an l-value of type %s.\n"
              (Types.string_of_t_type t)
              (Types.string_of_t_type typeExpr);
            failwith "Type error");
          None)
  | S_block b -> sem_block b
  | S_func_call fc -> (
      let open Types in
      match sem_funcCall fc with
      | T_func t ->
          if t <> T_none then
            Printf.eprintf
              "Warning: The return value of the function '%s' is not used.\n"
              fc.id;
          None
      | T_none | T_int | T_char | T_array _ -> assert false)
  | S_if (c, s) -> (
      sem_cond c;
      let constCondValue = Ast.get_const_cond_value c in
      let type_of_s = sem_stmt s in
      match constCondValue with
      | None | Some false -> None
      | Some true -> type_of_s)
  | S_if_else (c, s1, s2) -> (
      sem_cond c;
      let type_of_s1 = sem_stmt s1 in
      let type_of_s2 = sem_stmt s2 in
      if type_of_s1 = type_of_s2 then
        type_of_s1
      else
        match (type_of_s1, type_of_s2) with
        | None, type_of_s | type_of_s, None -> type_of_s
        | _ ->
            Printf.eprintf
              "\027[31mError\027[0m: In an if-then-else statement two \
               different types are returned.\n";
            failwith "Multiple types returned in if-then-else")
  | S_while (c, s) -> (
      sem_cond c;
      let constCondValue = Ast.get_const_cond_value c in
      let type_of_s = sem_stmt s in
      match constCondValue with
      | Some false -> None
      | Some true ->
          if type_of_s = None then Printf.eprintf "Warning: Infinite loop.\n";
          type_of_s
      | None -> type_of_s)
  | S_return x -> (
      match x with None -> Some T_none | Some e -> Some (sem_expr e))
  | S_semicolon -> None

(** [sem_lvalue (lval : Ast.lvalue)] returns the type of the l-value [lval].
    Returns [Types.t_type]. *)
and sem_lvalue lv =
  let resultArrayType : Types.t_type option ref = ref None in
  let rec sem_lvalue_kind = function
    | L_id id ->
        let entryFoundOption = look_up_entry id in
        if entryFoundOption = None then (
          Printf.eprintf
            "\027[31mError\027[0m: Undefined variable '%s' is being used in \
             function '%s'.\n"
            id
            Symbol.(!current_scope.name);
          failwith "Undefined variable");
        let entryFound = Option.get entryFoundOption in
        if Types.debugMode then (
          Printf.printf "Entry for '%s' found. Information:\n" id;
          Printf.printf "\tid: %s, scope: %s" id entryFound.scope.name);
        let entryType =
          match entryFound.kind with
          | ENTRY_variable ev -> ev.variable_type
          | ENTRY_parameter ep -> ep.parameter_type
          | ENTRY_function ef -> ef.return_type
        in
        if Types.debugMode then
          Printf.printf ", type: %s\n" (Types.string_of_t_type entryType);
        resultArrayType := Some entryType;
        entryType
    | L_string s ->
        let resultType = Types.T_array (Types.T_char, String.length s + 1) in
        resultArrayType := Some resultType;
        resultType
        (* Note: the last character of a string literal is not the '\0' character. *)
    | L_comp (lv, e) -> (
        if Types.debugMode then
          Printf.printf
            "... checking the type of the content inside the brackets \
             (position in array must be an integer)\n";
        let typeExpr = sem_expr e in
        if not Types.(equal_types T_int typeExpr) then (
          Printf.eprintf
            "\027[31mError\027[0m: Expected type integer, but received %s.\n\
             Index of array elements must be of integer type.\n"
            (Types.string_of_t_type typeExpr);
          failwith "Type error");
        let rec get_name_of_lv = function
          | L_id id -> id
          | L_string s -> s
          | L_comp (lvalue, _) -> get_name_of_lv lvalue
        in
        match sem_lvalue_kind lv with
        | Types.T_array (t, n) ->
            if n <> -1 then begin
              match Ast.get_const_expr_value e with
              | None -> ()
              | Some index ->
                  if index < 0 || index >= n then (
                    Printf.eprintf
                      "\027[31mError\027[0m: Attempt to access an out of \
                       bounds element of the array '%s'.\n"
                      (get_name_of_lv lv);
                    failwith "Segmentation fault")
            end;
            t
        | _ ->
            Printf.eprintf
              "\027[31mError\027[0m: Variable '%s' is either not an array or \
               it is declared as an array with less dimensions than as used.\n"
              (get_name_of_lv lv);
            failwith "Iteration on non-array type of variable")
  in
  let resultType = sem_lvalue_kind lv.lv_kind in
  if lv.lv_type = None then
    lv.lv_type <- Some { elem_type = resultType; array_type = !resultArrayType };
  resultType

(** [sem_expr (e : Ast.expr)] returns the type of the expression [e]. Returns
    [Types.t_type]. *)
and sem_expr = function
  | E_const_int ci -> Types.T_int
  | E_const_char cc -> Types.T_char
  | E_lvalue lv ->
      let lvalue_type = sem_lvalue lv in
      if Types.debugMode then begin
        match lv.lv_kind with
        | L_comp _ ->
            Printf.printf "Composite l-value is of type '%s'\n"
              (Types.string_of_t_type lvalue_type)
        | _ -> ()
      end;
      lvalue_type
  | E_func_call fc -> (
      let open Types in
      match sem_funcCall fc with
      | T_func T_none ->
          Printf.eprintf
            "\027[31mError\027[0m: Function '%s' returns nothing and can't be \
             used as an expression.\n"
            fc.id;
          failwith "A function of type nothing is being used as an expression"
      | T_func t -> t
      | T_none | T_int | T_char | T_array _ -> assert false)
  | E_sgn_expr (s, e) ->
      if Types.debugMode then
        Printf.printf "... checking a signed expression (must be int)\n";
      let typeExpr = sem_expr e in
      if not (Types.equal_types Types.T_int typeExpr) then (
        Printf.eprintf
          "\027[31mError\027[0m: Operator `-` (minus sign) is applied to a \
           non-integer type of argument.\n";
        failwith "Type error");
      Types.T_int
  | E_op_expr_expr (e1, ao, e2) ->
      if Types.debugMode then
        Printf.printf
          "... checking whether the arguments of an arithmOperator are of type \
           int\n";
      let typeExpr1, typeExpr2 = (sem_expr e1, sem_expr e2) in
      let open Types in
      if not (equal_types T_int typeExpr1) then (
        Printf.eprintf
          "\027[31mError\027[0m: Left argument of an arithmetic operator is an \
           argument of type %s.\n"
          (Types.string_of_t_type typeExpr1);
        failwith "Type error");
      if not (equal_types Types.T_int typeExpr2) then (
        Printf.eprintf
          "\027[31mError\027[0m: Right argument of an arithmetic operator is \
           an argument of type %s.\n"
          (Types.string_of_t_type typeExpr2);
        failwith "Type error");
      Types.T_int
  | E_expr_parenthesized e -> sem_expr e

(** [sem_cond (c : Ast.cond)] semantically analyses condition [c]. Returns
    [unit]. *)
and sem_cond = function
  | C_not_cond (lo, c) -> sem_cond c
  | C_cond_cond (c1, lo, c2) ->
      sem_cond c1;
      sem_cond c2
  | C_expr_expr (e1, co, e2) ->
      if Types.debugMode then
        Printf.printf
          "... checking whether the arguments of a compOperator are of the \
           same type\n";
      let typeExpr1, typeExpr2 = (sem_expr e1, sem_expr e2) in
      if not (Types.equal_types typeExpr1 typeExpr2) then (
        Printf.eprintf
          "\027[31mError\027[0m: Arguments of a logical operator have \
           different types. Only expressions of the same type can be compared \
           with a logical operator.\n";
        failwith "Type error")
  | C_cond_parenthesized c -> sem_cond c

(** [sem_funcCall (fc : Ast.funcCall)] returns the return type of function call
    [fc]. Additionally, it checks if the types of its arguments match the
    expected ones defined in the function's header. Returns [Types.t_type]. *)
and sem_funcCall fc =
  match fc with
  | { id = ident; expr_list = el; ret_type = rt } -> (
      let resultLookUpOption = look_up_entry ident in
      try
        if resultLookUpOption = None then raise Not_found;
        let functionEntry =
          match (Option.get resultLookUpOption).kind with
          | ENTRY_function ef -> ef
          | ENTRY_variable _ | ENTRY_parameter _ -> raise Shared_name_func_var
        in
        if rt = None then
          fc.ret_type <- Some (Types.t_type_of_t_func functionEntry.return_type)
        else (* This case won't ever be ran twice. *)
          Printf.printf "The impossible happened!\n";

        if List.compare_lengths el functionEntry.parameters_list <> 0 then
          raise Unexpected_number_of_parameters;

        let exprTypesListInFuncCall = List.map sem_expr el in
        let paramTypesListFromST =
          let rec get_entry_types_list = function
            | [] -> []
            | { parameter_type = pt; _ } :: tl -> pt :: get_entry_types_list tl
          in
          get_entry_types_list functionEntry.parameters_list
        in
        let typeListsAreEqual =
          List.for_all2 Types.equal_types exprTypesListInFuncCall
            paramTypesListFromST
        in
        if not typeListsAreEqual then raise Type_error;

        let exprIsLValueList =
          let rec is_lvalue_of_expr = function
            | E_lvalue _ -> true
            | E_expr_parenthesized expr -> is_lvalue_of_expr expr
            | _ -> false
          in
          List.map is_lvalue_of_expr el
        in
        let paramIsByRefListFromST =
          let get_is_ref_of_param_entry pe = pe.passing = Symbol.BY_REFERENCE in
          List.map get_is_ref_of_param_entry functionEntry.parameters_list
        in
        let byRefListsAreEqual =
          let helper eLV pBR = eLV || not pBR in
          List.equal helper exprIsLValueList paramIsByRefListFromST
        in
        if not byRefListsAreEqual then raise Passing_error;
        functionEntry.return_type
      with
      | Not_found ->
          Printf.eprintf
            "\027[31mError\027[0m: Function '%s' is called, but never declared.\n"
            ident;
          failwith "Undeclared function called"
      | Shared_name_func_var ->
          Printf.eprintf
            "\027[31mError\027[0m: Name '%s' is shared with a function and a \
             variable.\n"
            ident;
          failwith "Function and variable share the same name"
      | Unexpected_number_of_parameters ->
          let functionEntry =
            match (Option.get resultLookUpOption).kind with
            | ENTRY_function ef -> ef
            | _ -> assert false
          in
          Printf.eprintf
            "\027[31mError\027[0m: Function '%s' expected %d arguments, but \
             instead got %d.\n"
            ident
            (List.length functionEntry.parameters_list)
            (List.length el);
          failwith "Unexpected number of parameters in function call"
      | Type_error ->
          Printf.eprintf
            "\027[31mError\027[0m: Arguments' types of function '%s' don't \
             match.\n"
            ident;
          failwith "The arguments' types don't match"
      | Passing_error ->
          Printf.eprintf
            "\027[31mError\027[0m: '%s' function call: Expression that is \
             passed by reference isn't an l-value.\n"
            ident;
          failwith "r-value passed by reference")

(** [sem_on (ast : Ast.funcDef)] semantically analyses the root of the ast [ast]
    (produced by the parser). It also initializes the SymbolTable. Returns
    [unit]. *)
and sem_on asts =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
