open Ast
open Symbol

(** [funcDefAncestors] is a stack that stores all the ancestors of a funcDef in
    runtime. *)
let funcDefAncestors : funcDef option Stack.t = Stack.create ()

(** [sem_funcDef (fd : Ast.funcDef)] semantically analyses the function
    definition [fd]. The field 'parent_func' of [fd] is set. After semantically
    analysing the header, local definitions list and the block, it is checked if
    in the function's block a value of the expected type is returned. *)
let rec sem_funcDef fd : unit =
  let isMainProgram = !current_scope.depth = 0 in
  if isMainProgram then Symbol.add_standard_library ();
  if isMainProgram then Stack.push None funcDefAncestors;
  fd.parent_func <- Stack.top funcDefAncestors;
  sem_header true fd.header;
  Symbol.open_scope fd.header.id;
  let add_fparDef (fpd : fparDef) : unit =
    let typ = Ast.t_type_of_fparType fpd.fpar_type in
    List.iter (fun id -> Symbol.enter_parameter id typ fpd.ref) fpd.id_list
  in
  List.iter add_fparDef fd.header.fpar_def_list;
  Stack.push (Some fd) funcDefAncestors;
  sem_localDefList fd.local_def_list;
  ignore (Stack.pop funcDefAncestors);

  let overloadedParVarNameOption : string option =
    let duplicate_element lst =
      let rec helper seen = function
        | [] -> None
        | h :: t -> if List.mem h seen then Some h else helper (h :: seen) t
      in
      helper [] lst
    in
    let parNames : string list =
      let resultList =
        List.map
          (fun (fpd : Ast.fparDef) -> fpd.id_list)
          fd.header.fpar_def_list
        |> List.flatten
      in
      let overloadedParNameOption = duplicate_element resultList in
      if overloadedParNameOption <> None then
        Error.handle_error "Two parameters share identifier"
          (Printf.sprintf "Parameter name '%s' in function '%s' is used twice."
             (Option.get overloadedParNameOption)
             fd.header.id);
      resultList
    in
    let varNames : string list =
      let resultList =
        List.map
          (fun ld -> match ld with L_varDef vd -> vd.id_list | _ -> [])
          fd.local_def_list
        |> List.flatten
      in
      let overloadedVarNameOption = duplicate_element resultList in
      if overloadedVarNameOption <> None then
        Error.handle_error "Two variables share identifier"
          (Printf.sprintf
             "Variable '%s' is declared twice in the function '%s'."
             (Option.get overloadedVarNameOption)
             fd.header.id);
      resultList
    in
    let rec share_common_elem l1 l2 : 'a option =
      match l1 with
      | [] -> None
      | head :: tail ->
          if List.mem head l2 then Some head else share_common_elem tail l2
    in
    share_common_elem parNames varNames
  in
  if overloadedParVarNameOption <> None then
    Error.handle_error "Parameter and variable share identifier"
      (Printf.sprintf
         "The name '%s' is shared between a variable and a parameter in \
          function '%s'."
         (Option.get overloadedParVarNameOption)
         fd.header.id);

  let expectedReturnType = Ast.t_type_of_retType fd.header.ret_type in
  let typeReturnedInBlock =
    Types.T_func (match sem_block fd.block with None -> T_none | Some t -> t)
  in
  if expectedReturnType <> typeReturnedInBlock then
    Error.handle_type_error expectedReturnType typeReturnedInBlock
      (Printf.sprintf
         "Function '%s' doesn't return the expected type in its block."
         fd.header.id);

  let isMainProgram = !current_scope.depth = 1 in
  if isMainProgram then
    List.iter
      (fun id -> Error.handle_warning ("Unused name '" ^ id ^ "'."))
      (Symbol.get_unused_entries ());

  Symbol.close_scope ()

(** [sem_header (isPartOfAFuncDef : bool) (h : Ast.header)] takes
    [isPartOfAFuncDef] ([true] when the header is part of a function definition
    and [false] when it's part of a function declaration) and the function's
    header [h]. The field 'comp_id' of [h] is set. If [h] is part of a function
    definition, then a new scope is opened and the function's parameters are
    inserted in it. *)
and sem_header isPartOfAFuncDef header : unit =
  if not (List.mem header.id Symbol.lib_function_names) then begin
    let postfix : string =
      let ancestorsNames : string list =
        List.rev
          (Stack.fold
             (fun acc (fd : Ast.funcDef option) ->
               if fd <> None then
                 (Option.get fd).header.id :: acc
               else
                 acc)
             [] funcDefAncestors)
      in
      Printf.sprintf "(%d)" (Hashtbl.hash (String.concat "" ancestorsNames))
    in
    header.comp_id <- header.id ^ postfix
  end;
  let isMainProgram = !current_scope.depth = 0 in
  if isMainProgram then (
    if header.ret_type <> Nothing then
      Error.handle_type_error (T_func T_none)
        (Ast.t_type_of_retType header.ret_type)
        "Main function must return 'nothing' type.";
    if header.fpar_def_list <> [] then
      Error.handle_error "Main function shouldn't have parameters"
        "Main function shouldn't have parameters.");

  let resultLookUpOption = look_up_entry header.id in
  if
    resultLookUpOption = None
    || Symbol.different_scopes (Option.get resultLookUpOption).scope
         !current_scope
  then
    Symbol.enter_function header.id
      (sem_fparDefList header.fpar_def_list)
      (Ast.t_type_of_retType header.ret_type)
      Symbol.(if isPartOfAFuncDef then DEFINED else DECLARED)
  else
    try
      set_entry_isUsed (Option.get resultLookUpOption);
      let functionEntry =
        match (Option.get resultLookUpOption).kind with
        | ENTRY_function ef -> ef
        | ENTRY_variable _ | ENTRY_parameter _ ->
            raise Error.Shared_name_func_var
      in
      let returnTypeFromHeader : Types.t_type =
        Ast.t_type_of_retType header.ret_type
      in
      let paramListFromHeader : (int * Types.t_type * bool) list =
        List.map
          (fun (fpd : Ast.fparDef) ->
            ( List.length fpd.id_list,
              Ast.t_type_of_fparType fpd.fpar_type,
              fpd.ref ))
          header.fpar_def_list
      in
      let matchingNumOfParams : bool =
        let lengthOfParamListHeader =
          List.fold_left (fun len (n, _, _) -> len + n) 0 paramListFromHeader
        in
        List.length functionEntry.parameters_list = lengthOfParamListHeader
      in
      let matchingParamTypes : bool =
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
      if not matchingNumOfParams then
        Error.handle_error "Function overload"
          ("Function '" ^ header.id ^ "' is overloaded.");
      if functionEntry.return_type <> returnTypeFromHeader then
        Error.handle_type_error functionEntry.return_type returnTypeFromHeader
          (Printf.sprintf
             "Return type of function '%s' differs between declarations."
             header.id);
      if not matchingParamTypes then
        Error.handle_type_error (failwith "TODO") (failwith "TODO")
          (Printf.sprintf
             "Parameter types of function '%s' differ between declarations."
             header.id);
      if functionEntry.state = Symbol.DEFINED then
        Error.handle_error "Redefinition of function"
          ("Function '" ^ header.id ^ "' is defined twice.");
      Symbol.set_func_defined functionEntry
    with Error.Shared_name_func_var ->
      Error.handle_error "Function and variable share the same name"
        ("Name '" ^ header.id ^ "' is shared with a function and a variable.")

(** [sem_fparDefList (fpdl : Ast.fparDef list)] semantically analyses the
    function's parameter definitions [fpdl]. *)
and sem_fparDefList fpdl : (int * Types.t_type * Symbol.param_passing) list =
  List.map sem_fparDef fpdl

(** [sem_fparDef (fpd : Ast.fparDef)] semantically analyses the function's
    parameter definition [fpd]. *)
and sem_fparDef fpd : int * Types.t_type * Symbol.param_passing =
  if List.exists (fun n -> n = 0) fpd.fpar_type.array_dimensions then
    Error.handle_error "Array of zero size"
      "Parameter array declared to have size a non-positive number.";
  let paramIsArray = fpd.fpar_type.array_dimensions <> [] in
  let passedByValue = not fpd.ref in
  if paramIsArray && passedByValue then
    Error.handle_error "Array passed as a parameter by value"
      "Arrays should always be passed as parameters by reference.";
  ( List.length fpd.id_list,
    Ast.t_type_of_fparType fpd.fpar_type,
    if fpd.ref then BY_REFERENCE else BY_VALUE )

(** [sem_localDefList (ldl : Ast.localDef list)] semantically analyses the
    function's local definitions list [ldl]. The fields 'is_redundant' and
    'func_def of the funcDecl are set. *)
and sem_localDefList : Ast.localDef list -> unit = function
  | [] -> ()
  | L_funcDecl fdecl :: tail ->
      let correspondingFuncDef : Ast.funcDef option =
        match
          List.find_opt
            (fun ld ->
              match ld with
              | L_funcDef fdef -> fdef.header.id = fdecl.header.id
              | L_funcDecl fdecl2 ->
                  fdecl2.is_redundant <- fdecl2.header.id = fdecl.header.id;
                  false
              | _ -> false)
            tail
        with
        | Some (L_funcDef fd) -> Some fd
        | _ ->
            Error.handle_error "Function declared but never defined"
              (Printf.sprintf "Function '%s' declared but never defined."
                 fdecl.header.id);
            None
      in
      fdecl.func_def <- correspondingFuncDef;
      sem_localDef (L_funcDecl fdecl);
      sem_localDefList tail
  | ld :: tail ->
      sem_localDef ld;
      sem_localDefList tail

(** [sem_localDef (ld : Ast.localDef)] adds in the symbolTable the functions and
    parameters defined in the local definition [ld]. *)
and sem_localDef : Ast.localDef -> unit = function
  | L_funcDef fd -> sem_funcDef fd
  | L_funcDecl fd -> sem_funcDecl fd
  | L_varDef vd -> sem_varDef vd

(** [sem_funcDecl (fd : Ast.funcDecl)] semantically analyses the header of the
    function declaration [fd] (uses the function [sem_header]). *)
and sem_funcDecl fd : unit =
  if not fd.is_redundant then
    sem_header false fd.header
  else
    Error.handle_warning
      ("Function '" ^ fd.header.id ^ "' has redundant declarations.")

(** [sem_varDef (vd : Ast.varDef)] enters in the symbolTable every variable
    defined in the variable definition [vd]. *)
and sem_varDef vd : unit =
  if List.exists (Int.equal 0) vd.var_type.array_dimensions then
    Error.handle_error "Array of zero size"
      "Variable array declared to have size a non-positive number.";
  let typ = Ast.t_type_of_varType vd.var_type in
  List.iter (fun i -> Symbol.enter_variable i typ) vd.id_list

(** [sem_block (bl : Ast.stmt list)] semantically analyses every statement of
    the block [bl]. *)
and sem_block (bl : Ast.stmt list) : Types.t_type option =
  let rec get_type_of_stmt_list result warningRaised = function
    | [] -> result
    | head :: tail -> (
        match sem_stmt head with
        | None -> get_type_of_stmt_list result warningRaised tail
        | Some typ ->
            if tail <> [] && not warningRaised then
              Error.handle_warning "A section of a block is never reached.";
            get_type_of_stmt_list
              (if result = None then Some typ else result)
              true tail)
  in
  get_type_of_stmt_list None false bl

(** [sem_stmt (s : Ast.stmt)] semantically analyses the statement [s] and
    returns [Some t] if [s] is a return statement or [None] if not. *)
and sem_stmt : Ast.stmt -> Types.t_type option = function
  | S_assignment (lv, e) -> (
      (match lv.lv_kind with
      | L_comp (L_string _, _) ->
          Error.handle_error "Assignment to const type"
            "Assignment to a string literal's element is not possible."
      | _ -> ());
      match sem_lvalue lv with
      | Types.T_array (_, t) ->
          Error.handle_error "Assignment to array"
            "Assignment to an l-value of type array is not possible.";
          Some (Types.final_t_type_of_t_array t)
      | Types.T_func t ->
          Error.handle_error "Assignment to function"
            "Assignment to a function call is not possible.";
          Some t
      | t ->
          let typeExpr = sem_expr e in
          if not (Types.equal_types t typeExpr) then
            Error.handle_type_error t typeExpr
              (Printf.sprintf
                 "The value of an expression of type %s is tried to be \
                  assigned to an l-value of type %s."
                 (Types.string_of_t_type typeExpr)
                 (Types.string_of_t_type t));
          None)
  | S_block b -> sem_block b
  | S_func_call fc -> (
      let open Types in
      match sem_funcCall fc with
      | T_func t ->
          if t <> T_none then
            Error.handle_warning
              ("The return value of the function '" ^ fc.id ^ "' is not used.");
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
            Error.handle_error
              "Multiple types returned in if-then-else statement"
              "In an if-then-else statement two different types are returned.";
            None)
  | S_while (c, s) -> (
      sem_cond c;
      let constCondValue = Ast.get_const_cond_value c in
      let type_of_s = sem_stmt s in
      match constCondValue with
      | Some false -> None
      | Some true ->
          if type_of_s = None then Error.handle_warning "Infinite loop.";
          type_of_s
      | None -> type_of_s)
  | S_return x -> (
      match x with None -> Some T_none | Some e -> Some (sem_expr e))
  | S_semicolon -> None

(** [sem_lvalue (lval : Ast.lvalue)] returns the type of the l-value [lval].
    Also, the field 'lv_type' of [lv] is set. *)
and sem_lvalue lv : Types.t_type =
  let resultArrayType : Types.t_type option ref = ref None in
  let rec sem_lvalue_kind = function
    | L_id id ->
        let entryFoundOption = look_up_entry id in
        if entryFoundOption = None then
          Error.handle_error "Undefined variable"
            (Printf.sprintf "Undefined variable '%s' is being used in function."
               Symbol.(!current_scope.name));
        let entryFound = Option.get entryFoundOption in
        set_entry_isUsed entryFound;
        let entryType =
          match entryFound.kind with
          | ENTRY_variable ev -> ev.variable_type
          | ENTRY_parameter ep -> ep.parameter_type
          | ENTRY_function ef -> ef.return_type
        in
        resultArrayType := Some entryType;
        entryType
    | L_string s ->
        let resultType = Types.T_array (String.length s + 1, Types.T_char) in
        resultArrayType := Some resultType;
        resultType
        (* Note: the last character of a string literal is not the '\0' character. *)
    | L_comp (lv, e) -> (
        let typeExpr = sem_expr e in
        if not Types.(equal_types T_int typeExpr) then
          Error.handle_type_error Types.T_int typeExpr
            (Printf.sprintf "Index of arrays must be of integer type.");
        let rec get_name_of_lv = function
          | L_id id -> id
          | L_string s -> s
          | L_comp (lvalue, _) -> get_name_of_lv lvalue
        in
        match sem_lvalue_kind lv with
        | Types.T_array (n, t) ->
            if n <> -1 then begin
              match Ast.get_const_expr_value e with
              | None -> ()
              | Some index ->
                  if index < 0 || index >= n then
                    Error.handle_error "Segmentation fault"
                      ("Attempt to access an out of bounds element of the \
                        array '" ^ get_name_of_lv lv ^ "'.")
            end;
            t
        | t ->
            Error.handle_error "Iteration on non-array type of variable"
              (Printf.sprintf
                 "Variable '%s' is either not an array or it is declared as an \
                  array with less dimensions than as used."
                 (get_name_of_lv lv));
            t)
  in
  let resultType = sem_lvalue_kind lv.lv_kind in
  if lv.lv_type = None then
    lv.lv_type <- Some { elem_type = resultType; array_type = !resultArrayType };
  resultType

(** [sem_expr (e : Ast.expr)] returns the type of the expression [e]. *)
and sem_expr : Ast.expr -> Types.t_type = function
  | E_const_int ci -> Types.T_int
  | E_const_char cc -> Types.T_char
  | E_lvalue lv -> sem_lvalue lv
  | E_func_call fc -> (
      let open Types in
      match sem_funcCall fc with
      | T_func T_none ->
          Error.handle_error Error.type_error_msg
            (Printf.sprintf
               "Function '%s' returns nothing and can't be used as an \
                expression."
               fc.id);
          T_none
      | T_func t -> t
      | T_none | T_int | T_char | T_array _ -> assert false)
  | E_sgn_expr (s, e) ->
      let typeExpr = sem_expr e in
      if not (Types.equal_types Types.T_int typeExpr) then
        Error.handle_type_error Types.T_int typeExpr
          "Operator `-` (minus sign) must be applied to an integer expression.";
      Types.T_int
  | E_op_expr_expr (e1, ao, e2) ->
      let typeExpr1, typeExpr2 = (sem_expr e1, sem_expr e2) in
      let open Types in
      (match (equal_types T_int typeExpr1, equal_types T_int typeExpr2) with
      | true, true -> ()
      | _ ->
          Error.handle_type_error T_int
            (if typeExpr1 <> T_int then typeExpr1 else typeExpr2)
            (Printf.sprintf
               "Arithmetic operators must be applied to integer expressions. \
                %s argument is of non-integer type."
               (if typeExpr1 <> T_int then "Left" else "Right")));
      T_int
  | E_expr_parenthesized e -> sem_expr e

(** [sem_cond (c : Ast.cond)] semantically analyses condition [c]. *)
and sem_cond : Ast.cond -> unit = function
  | C_not_cond (lo, c) -> sem_cond c
  | C_cond_cond (c1, lo, c2) ->
      sem_cond c1;
      sem_cond c2
  | C_expr_expr (e1, co, e2) ->
      let typeExpr1, typeExpr2 = (sem_expr e1, sem_expr e2) in
      if not (Types.equal_types typeExpr1 typeExpr2) then
        Error.handle_type_error typeExpr1 typeExpr2
          "Arguments of a logical operator have different types. Only \
           expressions of the same type can be compared with a logical \
           operator."
  | C_cond_parenthesized c -> sem_cond c

(** [sem_funcCall (fc : Ast.funcCall)] returns the return type of function call
    [fc]. Additionally, it checks if the types of its arguments match the
    expected ones defined in the function's header. The fields 'comp_id' and
    'ret_type' of [fc] are set. *)
and sem_funcCall fc : Types.t_type =
  let resultLookUpOption = look_up_entry fc.id in
  try
    if resultLookUpOption = None then raise Not_found;
    if not (List.mem fc.id Symbol.lib_function_names) then begin
      let postfix =
        let ancestorsNames =
          let rec get_ancestorsNames = function
            | None -> []
            | Some scope -> scope.name :: get_ancestorsNames scope.parent
          in
          get_ancestorsNames (Some (Option.get resultLookUpOption).scope)
        in
        Printf.sprintf "(%d)" (Hashtbl.hash (String.concat "" ancestorsNames))
      in
      fc.comp_id <- fc.id ^ postfix
    end;
    let functionEntry =
      match (Option.get resultLookUpOption).kind with
      | ENTRY_function ef -> ef
      | ENTRY_variable _ | ENTRY_parameter _ -> raise Error.Shared_name_func_var
    in
    set_entry_isUsed (Option.get resultLookUpOption);
    if fc.ret_type = None then
      fc.ret_type <- Some (Types.t_type_of_t_func functionEntry.return_type);

    if List.compare_lengths fc.expr_list functionEntry.parameters_list <> 0 then
      raise Error.Unexpected_number_of_parameters;

    let exprTypesListInFuncCall = List.map sem_expr fc.expr_list in
    let paramTypesListFromST =
      List.map
        (fun { parameter_type = pt; _ } -> pt)
        functionEntry.parameters_list
    in
    let typeListsAreEqual =
      List.for_all2 Types.equal_types exprTypesListInFuncCall
        paramTypesListFromST
    in
    if not typeListsAreEqual then
      raise (Error.Type_error (failwith "TODO", failwith "TODO"));

    let exprIsLValueList =
      let rec is_lvalue_of_expr = function
        | E_lvalue _ -> true
        | E_expr_parenthesized expr -> is_lvalue_of_expr expr
        | _ -> false
      in
      List.map is_lvalue_of_expr fc.expr_list
    in
    let paramIsByRefListFromST =
      let get_is_ref_of_param_entry pe = pe.passing = Symbol.BY_REFERENCE in
      List.map get_is_ref_of_param_entry functionEntry.parameters_list
    in
    let byRefListsAreEqual =
      let helper eLV pBR = eLV || not pBR in
      List.equal helper exprIsLValueList paramIsByRefListFromST
    in
    if not byRefListsAreEqual then raise Error.Passing_error;
    functionEntry.return_type
  with
  | Not_found ->
      Error.handle_error "Undeclared function called"
        ("Function '" ^ fc.id ^ "' is called, but never declared.");
      T_none
  | Error.Shared_name_func_var ->
      Error.handle_error "Function and variable share the same name"
        ("Name '" ^ fc.id ^ "' is shared with a function and a variable.");
      T_none
  | Error.Unexpected_number_of_parameters ->
      let functionEntry =
        match (Option.get resultLookUpOption).kind with
        | ENTRY_function ef -> ef
        | _ -> assert false
      in
      Error.handle_error "Unexpected number of parameters in function call"
        (Printf.sprintf
           "Function '%s' expected %d arguments, but instead got %d." fc.id
           (List.length functionEntry.parameters_list)
           (List.length fc.expr_list));
      Option.get fc.ret_type
  | Error.Type_error (expectedType, foundType) ->
      Error.handle_type_error expectedType foundType
        (Printf.sprintf "Arguments' types of function '%s' don't match." fc.id);
      Option.get fc.ret_type
  | Error.Passing_error ->
      Error.handle_error "r-value passed by reference"
        (Printf.sprintf
           "'%s' function call: Expression that is passed by reference isn't \
            an l-value."
           fc.id);
      Option.get fc.ret_type

(** [sem_on (ast : Ast.funcDef)] semantically analyses the root of the ast [ast]
    (produced by the parser). It also initializes the SymbolTable. *)
and sem_on asts : unit =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
