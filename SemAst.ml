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
  if Types.debugMode then
    Printf.printf "Opening new scope for '%s' function\n" fd.header.id;
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
      let rec get_par_names = function
        | [] -> []
        | { id_list = il; ref; fpar_type } :: tail -> il @ get_par_names tail
      in
      let resultList = get_par_names fd.header.fpar_def_list in
      let overloadedParNameOption = duplicate_element resultList in
      if overloadedParNameOption <> None then
        Error.handle_error "Two parameters share identifier"
          ("Parameter name '"
          ^ Option.get overloadedParNameOption
          ^ "' in function '" ^ fd.header.id ^ "' is used twice.");
      resultList
    in
    let varNames : string list =
      let rec get_var_names = function
        | [] -> []
        | L_funcDef _ :: tail | L_funcDecl _ :: tail -> get_var_names tail
        | L_varDef vd :: tail -> vd.id_list @ get_var_names tail
      in
      let resultList = get_var_names fd.local_def_list in
      let overloadedVarNameOption = duplicate_element resultList in
      if overloadedVarNameOption <> None then
        Error.handle_error "Two variables share identifier"
          ("Variable '"
          ^ Option.get overloadedVarNameOption
          ^ "' is declared twice in the function '" ^ fd.header.id ^ "'.");
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
      ("The name '"
      ^ Option.get overloadedParVarNameOption
      ^ "' is shared between a variable and a parameter in function '"
      ^ fd.header.id ^ "'.");

  let isMainProgram = !current_scope.depth = 1 in
  ignore isMainProgram;
  let expectedReturnType = Ast.t_type_of_retType fd.header.ret_type in
  let typeReturnedInBlock =
    Types.T_func (match sem_block fd.block with None -> T_none | Some t -> t)
  in
  if expectedReturnType <> typeReturnedInBlock then
    Error.handle_error Error.type_error_msg
      ("Function '" ^ fd.header.id ^ "' returns "
      ^ Types.string_of_t_type expectedReturnType
      ^ " type, but in its block "
      ^ Types.string_of_t_type typeReturnedInBlock
      ^ " type is returned.");

  let isMainProgram = !current_scope.depth = 1 in
  if isMainProgram then
    List.iter
      (fun id -> Error.handle_warning ("Unused name '" ^ id ^ "'."))
      (Symbol.get_unused_entries ());

  if Types.debugMode then
    Printf.printf "Closing scope for '%s' function's declarations.\n"
      fd.header.id;
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
      "(" ^ string_of_int (Hashtbl.hash (String.concat "" ancestorsNames)) ^ ")"
    in
    header.comp_id <- header.id ^ postfix
  end;
  let isMainProgram = !current_scope.depth = 0 in
  if isMainProgram then
    if header.ret_type <> Nothing then
      Error.handle_error Error.type_error_msg
        "Main function must return 'nothing' type."
    else if header.fpar_def_list <> [] then
      Error.handle_error "Main function shouldn't have parameters"
        "Main function shouldn't have parameters.";

  let resultLookUpOption = look_up_entry header.id in
  if
    resultLookUpOption = None
    || not
         (Symbol.equal_scopes (Option.get resultLookUpOption).scope
            !current_scope)
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
      let returnTypeFromHeader : Types.t_type =
        Ast.t_type_of_retType header.ret_type
      in
      let paramListFromHeader : (int * Types.t_type * bool) list =
        let rec helper : fparDef list -> (int * Types.t_type * bool) list =
          function
          | [] -> []
          | { ref = r; id_list = il; fpar_type = fpt } :: tail ->
              let paramType = Ast.t_type_of_fparType fpt in
              (List.length il, paramType, r) :: helper tail
        in
        let resultList = helper header.fpar_def_list in
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
      let matchingNumOfParams : bool =
        let lengthOfParamListHeader =
          let rec f accum = function
            | [] -> accum
            | (n, _, _) :: tail -> f (accum + n) tail
          in
          f 0 paramListFromHeader
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
        raise Error.Overloaded_functions
      else if functionEntry.return_type <> returnTypeFromHeader then
        raise Error.Expected_type_not_returned
      else if not matchingParamTypes then
        raise Error.Non_matching_parameter_types
      else if functionEntry.state = Symbol.DEFINED then
        raise Error.Redefined_function
      else
        Symbol.set_func_defined functionEntry
    with
    | Error.Shared_name_func_var ->
        Error.handle_error "Function and variable share the same name"
          ("Name '" ^ header.id ^ "' is shared with a function and a variable.")
    | Error.Overloaded_functions ->
        Error.handle_error "Function overload"
          ("Function '" ^ header.id ^ "' is overloaded.")
    | Error.Redefined_function ->
        Error.handle_error "Redefinition of function"
          ("Function '" ^ header.id ^ "' is defined twice.")
    | Error.Expected_type_not_returned ->
        Error.handle_error Error.type_error_msg
          ("Return type of function '" ^ header.id
         ^ "' differs between declarations")
    | Error.Non_matching_parameter_types ->
        Error.handle_error Error.type_error_msg
          ("Parameter types of function '" ^ header.id
         ^ "' differ between declarations.")

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
      let correspondingFuncDef =
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
        | Some (L_funcDef fd) -> fd
        | _ ->
            Error.handle_error
              ("Function '" ^ fdecl.header.id ^ "' declared but never defined.")
              "Function declared but never defined"
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
      | Types.T_array _ ->
          Error.handle_error "Assignment to array"
            "Assignment to an l-value of type array is not possible."
      | Types.T_func _ ->
          Error.handle_error "Assignment to function"
            "Assignment to a function call is not possible."
      | t ->
          if Types.debugMode then
            Printf.printf
              "... checking the types of an lvalue and an expression \
               (assignment)\n";
          let typeExpr = sem_expr e in
          if not (Types.equal_types t typeExpr) then
            Error.handle_error Error.type_error_msg
              ("The value of an expression of type " ^ Types.string_of_t_type t
             ^ " is tried to be assigned to an l-value of type "
              ^ Types.string_of_t_type typeExpr
              ^ ".");
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
              "In an if-then-else statement two different types are returned.")
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
            ("Undefined variable '" ^ id ^ "' is being used in function '"
            ^ Symbol.(!current_scope.name)
            ^ "'.");
        let entryFound = Option.get entryFoundOption in
        set_entry_isUsed entryFound;
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
        let resultType = Types.T_array (String.length s + 1, Types.T_char) in
        resultArrayType := Some resultType;
        resultType
        (* Note: the last character of a string literal is not the '\0' character. *)
    | L_comp (lv, e) -> (
        if Types.debugMode then
          Printf.printf
            "... checking the type of the content inside the brackets \
             (position in array must be an integer)\n";
        let typeExpr = sem_expr e in
        if not Types.(equal_types T_int typeExpr) then
          Error.handle_error Error.type_error_msg
            ("Expected type integer, but received "
            ^ Types.string_of_t_type typeExpr
            ^ ". Index of array elements must be of integer type.");
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
        | _ ->
            Error.handle_error "Iteration on non-array type of variable"
              ("Variable '" ^ get_name_of_lv lv
             ^ "' is either not an array or it is declared as an array with \
                less dimensions than as used."))
  in
  let resultType = sem_lvalue_kind lv.lv_kind in
  if lv.lv_type = None then
    lv.lv_type <- Some { elem_type = resultType; array_type = !resultArrayType };
  resultType

(** [sem_expr (e : Ast.expr)] returns the type of the expression [e]. *)
and sem_expr : Ast.expr -> Types.t_type = function
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
          Error.handle_error Error.type_error_msg
            ("Function '" ^ fc.id
           ^ "' returns nothing and can't be used as an expression.")
      | T_func t -> t
      | T_none | T_int | T_char | T_array _ -> assert false)
  | E_sgn_expr (s, e) ->
      if Types.debugMode then
        Printf.printf "... checking a signed expression (must be int)\n";
      let typeExpr = sem_expr e in
      if not (Types.equal_types Types.T_int typeExpr) then
        Error.handle_error Error.type_error_msg
          "Operator `-` (minus sign) is applied to a non-integer type of \
           argument.";
      Types.T_int
  | E_op_expr_expr (e1, ao, e2) ->
      if Types.debugMode then
        Printf.printf
          "... checking whether the arguments of an arithmOperator are of type \
           int\n";
      let typeExpr1, typeExpr2 = (sem_expr e1, sem_expr e2) in
      let open Types in
      if not (equal_types T_int typeExpr1) then
        Error.handle_error Error.type_error_msg
          ("Left argument of an arithmetic operator is an argument of type "
          ^ Types.string_of_t_type typeExpr1
          ^ ".");
      if not (equal_types Types.T_int typeExpr2) then
        Error.handle_error Error.type_error_msg
          ("Right argument of an arithmetic operator is an argument of type "
          ^ Types.string_of_t_type typeExpr2
          ^ ".");
      Types.T_int
  | E_expr_parenthesized e -> sem_expr e

(** [sem_cond (c : Ast.cond)] semantically analyses condition [c]. *)
and sem_cond : Ast.cond -> unit = function
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
      if not (Types.equal_types typeExpr1 typeExpr2) then
        Error.handle_error Error.type_error_msg
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
        "("
        ^ string_of_int (Hashtbl.hash (String.concat "" ancestorsNames))
        ^ ")"
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
    if not typeListsAreEqual then raise Error.Type_error;

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
        ("Function '" ^ fc.id ^ "' is called, but never declared.")
  | Error.Shared_name_func_var ->
      Error.handle_error "Function and variable share the same name"
        ("Name '" ^ fc.id ^ "' is shared with a function and a variable.")
  | Error.Unexpected_number_of_parameters ->
      let functionEntry =
        match (Option.get resultLookUpOption).kind with
        | ENTRY_function ef -> ef
        | _ -> assert false
      in
      Error.handle_error "Unexpected number of parameters in function call"
        ("Function '" ^ fc.id ^ "' expected "
        ^ string_of_int (List.length functionEntry.parameters_list)
        ^ " arguments, but instead got "
        ^ string_of_int (List.length fc.expr_list)
        ^ ".")
  | Error.Type_error ->
      Error.handle_error Error.type_error_msg
        ("Arguments' types of function '" ^ fc.id ^ "' don't match.")
  | Error.Passing_error ->
      Error.handle_error "r-value passed by reference"
        ("'" ^ fc.id
       ^ "' function call: Expression that is passed by reference isn't an \
          l-value.")

(** [sem_on (ast : Ast.funcDef)] semantically analyses the root of the ast [ast]
    (produced by the parser). It also initializes the SymbolTable. *)
and sem_on asts : unit =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
