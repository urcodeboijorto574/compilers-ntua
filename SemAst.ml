open Ast
open Symbol
open Printf

(** [funcDefAncestors] is a stack that stores all the ancestors of a funcDef in
    runtime. *)
let funcDefAncestors : funcDef option Stack.t = Stack.create ()

(** [iteri2 f l1 l2] Same as [List.iter2], but the function is applied to the
    index of the elements as first argument (counting from 0), and the two
    elements themselves as second and third arguments.
    @raise Failure if the two lists are determined to have different lengths. *)
let iteri2 f l1 l2 =
  let rec helper index = function
    | [], [] -> ()
    | [], _ | _, [] -> failwith "iteri2"
    | h1 :: t1, h2 :: t2 ->
        f index h1 h2;
        helper (index + 1) (t1, t2)
  in
  helper 0 (l1, l2)

(** [sem_funcDef (fd : Ast.funcDef)] semantically analyses the function
    definition [fd]. The field 'parent_func' of [fd] is set. After semantically
    analysing the header, local definitions list and the block, it is checked if
    in the function's block a value of the expected type is returned. *)
let rec sem_funcDef fd : unit =
  let isMainProgram = !current_scope.depth = Symbol.initialScopeDepthValue in
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
          (sprintf "Parameter name '%s' in function '%s' is used twice."
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
          (sprintf "Variable '%s' is declared twice in the function '%s'."
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
      (sprintf
         "The name '%s' is shared between a variable and a parameter in \
          function '%s'."
         (Option.get overloadedParVarNameOption)
         fd.header.id);

  let expectedReturnType = Ast.t_type_of_retType fd.header.ret_type in
  let typeReturnedInBlock =
    let open Types in
    T_func
      (try Option.get (sem_block (Types.get expectedReturnType) fd.block)
       with Invalid_argument _ -> T_none)
  in
  if expectedReturnType <> typeReturnedInBlock then
    Error.handle_type_error expectedReturnType typeReturnedInBlock
      (sprintf "Function '%s' doesn't return the expected type in its block."
         fd.header.id);
  if isMainProgram then
    List.iter
      (fun id -> Error.handle_warning (sprintf "Unused name '%s'." id))
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
    let ancestorsNames : string list =
      Stack.fold
        (fun acc (fdOption : Ast.funcDef option) ->
          Option.fold ~none:acc
            ~some:(fun (fd : funcDef) -> fd.header.id :: acc)
            fdOption)
        [] funcDefAncestors
      |> List.rev
    in
    String.concat "" ancestorsNames |> Hashtbl.hash |> sprintf "(%d)"
    |> fun postfix -> header.comp_id <- header.id ^ postfix
  end;
  let isMainProgram = !current_scope.depth = Symbol.initialScopeDepthValue in
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
      let functionEntry =
        match (Option.get resultLookUpOption).kind with
        | ENTRY_function ef -> ef
        | ENTRY_variable _ | ENTRY_parameter _ ->
            raise Error.Shared_name_func_var
      in
      if functionEntry.return_type <> Ast.t_type_of_retType header.ret_type then
        Error.handle_type_error functionEntry.return_type
          (Ast.t_type_of_retType header.ret_type)
          (sprintf "Return type of function '%s' differs between declarations."
             header.id);
      let paramListFromHeaderExtended : (Types.t_type * bool) list =
        let open List in
        header.fpar_def_list
        |> map (fun (fpd : Ast.fparDef) ->
               init (length fpd.id_list) (fun _ ->
                   (Ast.t_type_of_fparType fpd.fpar_type, fpd.ref)))
        |> flatten
      in
      let matchingNumOfParams : bool =
        List.compare_lengths functionEntry.parameters_list
          paramListFromHeaderExtended
        = 0
      in
      if not matchingNumOfParams then
        Error.handle_error "Function overload"
          (sprintf "Function '%s' is overloaded." header.id);
      let checkParams () : unit =
        iteri2
          (fun i pEntry pHeader ->
            if pEntry.parameter_type <> fst pHeader then
              Error.handle_type_error pEntry.parameter_type (fst pHeader)
                (sprintf
                   "The type of the parameter at position %d of the '%s' \
                    function's header differs from the one declared at its \
                    previous function header."
                   i header.id);
            if pEntry.passing = BY_REFERENCE <> snd pHeader then
              Error.handle_error Error.semantic_error_msg
                (sprintf
                   "The type of passing of the parameter at position %d of \
                    '%s' function's header differs from the one declared at \
                    its previous function header."
                   i header.id))
          functionEntry.parameters_list paramListFromHeaderExtended
      in
      if matchingNumOfParams then checkParams ();
      if functionEntry.state = Symbol.DEFINED then
        Error.handle_error "Redefinition of function"
          (sprintf "Function '%s' is defined twice." header.id);
      Symbol.set_func_defined functionEntry
    with Error.Shared_name_func_var ->
      Error.handle_error "Function and variable share the same name"
        (sprintf "Name '%s' is shared with a function and a variable." header.id)

(** [sem_fparDefList (fpdl : Ast.fparDef list)] semantically analyses the
    function's parameter definitions [fpdl]. *)
and sem_fparDefList fpdl : (int * Types.t_type * Symbol.param_passing) list =
  List.map sem_fparDef fpdl

(** [sem_fparDef (fpd : Ast.fparDef)] semantically analyses the function's
    parameter definition [fpd]. *)
and sem_fparDef fpd : int * Types.t_type * Symbol.param_passing =
  if List.exists (Int.equal 0) fpd.fpar_type.array_dimensions then
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
              (sprintf "Function '%s' declared but never defined."
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
      (sprintf "Function '%s' has redundant declarations" fd.header.id)

(** [sem_varDef (vd : Ast.varDef)] enters in the symbolTable every variable
    defined in the variable definition [vd]. *)
and sem_varDef vd : unit =
  if List.exists (Int.equal 0) vd.var_type.array_dimensions then
    Error.handle_error "Array of zero size"
      "Variable array declared to have size a non-positive number.";
  let typ = Ast.t_type_of_varType vd.var_type in
  List.iter (fun i -> Symbol.enter_variable i typ) vd.id_list

(** [sem_block (expT : Types.t_type) (bl : Ast.stmt list)] semantically analyses
    every statement of the block [bl]. [expT] is the expected return type of the
    block ([expT] is not encapsulated in [Types.T_func]). *)
and sem_block (expectedReturnType : Types.t_type) (bl : Ast.stmt list) :
    Types.t_type option =
  let isWarningRaised = ref false in
  let raiseWarning () =
    if not !isWarningRaised then
      Error.handle_warning "A section of a block is never reached.";
    isWarningRaised := true
  in
  List.fold_left
    (fun acc stmt ->
      let typeOfStmt = sem_stmt expectedReturnType stmt in
      match acc with
      | None -> typeOfStmt
      | Some _ ->
          raiseWarning ();
          acc)
    None bl

(** [sem_stmt (expT : Types.t_type) (s : Ast.stmt)] semantically analyses the
    statement [s] and returns [Some t] if [s] is a return statement or [None] if
    not. [expT] is the expected return type of the block of the statement
    ([expT] is not encapsulated in [Types.T_func]). *)
and sem_stmt (expectedReturnType : Types.t_type) :
    Ast.stmt -> Types.t_type option = function
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
          Some (Types.join t)
      | Types.T_func t ->
          Error.handle_error "Assignment to function"
            "Assignment to a function call is not possible.";
          Some t
      | t ->
          let typeExpr = sem_expr e in
          if not (Types.equal_types t typeExpr) then
            Error.handle_type_error t typeExpr
              (sprintf
                 "The value of an expression of type %s is tried to be \
                  assigned to an l-value of type %s."
                 (Types.string_of_t_type typeExpr)
                 (Types.string_of_t_type t));
          None)
  | S_block b -> sem_block expectedReturnType b
  | S_func_call fc -> (
      let open Types in
      match sem_funcCall fc with
      | T_func t ->
          if t <> T_none then
            Error.handle_warning
              (sprintf "The return value of the function '%s' is not used."
                 fc.id);
          None
      | T_none | T_int | T_char | T_array _ -> assert false)
  | S_if (c, s) ->
      let constCondValueOpt =
        if sem_cond c then Ast.get_const_cond_value c else None
      in
      let type_of_s = sem_stmt expectedReturnType s in
      Option.bind constCondValueOpt (fun v -> if v then type_of_s else None)
  | S_if_else (c, s1, s2) ->
      let constCondValueOpt =
        if sem_cond c then Ast.get_const_cond_value c else None
      in
      let type_of_s1 = sem_stmt expectedReturnType s1 in
      let type_of_s2 = sem_stmt expectedReturnType s2 in
      if type_of_s1 = type_of_s2 then
        type_of_s1
      else if type_of_s1 = None || type_of_s2 = None then
        Option.bind constCondValueOpt (fun v ->
            if v then type_of_s1 else type_of_s2)
      else (
        Error.handle_error "Multiple types returned in if-then-else statement"
          "In an if-then-else statement two different types are returned.";
        None)
  | S_while (c, s) -> (
      let constCondValueOpt =
        if sem_cond c then Ast.get_const_cond_value c else None
      in
      let type_of_s = sem_stmt expectedReturnType s in
      match constCondValueOpt with
      | Some false -> None
      | Some true ->
          let check_returnativeness () =
            let rec is_stmt_returnative = function
              | S_return e_opt -> true
              | S_block stmtList -> List.exists is_stmt_returnative stmtList
              | S_if_else (c, s1, s2) ->
                  Option.fold
                    ~none:(is_stmt_returnative s1 || is_stmt_returnative s2)
                    ~some:(fun v -> is_stmt_returnative (if v then s1 else s2))
                    (get_const_cond_value c)
              | S_if (c, s) | S_while (c, s) ->
                  Option.fold ~none:(is_stmt_returnative s)
                    ~some:(fun v -> v && is_stmt_returnative s)
                    (get_const_cond_value c)
              | S_assignment _ | S_func_call _ | S_semicolon -> false
            in
            if not (is_stmt_returnative s) then
              Error.handle_warning "Infinite loop."
          in
          Option.fold ~some:ignore ~none:(check_returnativeness ()) type_of_s;
          type_of_s
      | None -> None)
  | S_return x ->
      let returnedType = Option.fold ~none:Types.T_none ~some:sem_expr x in
      if expectedReturnType <> returnedType then
        Error.handle_type_error expectedReturnType returnedType
          "Return statement in block has unexpected return type.";
      Some returnedType
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
            (sprintf "Undefined variable '%s' is being used in function '%s'."
               id
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
            (sprintf "Index of arrays must be of integer type.");
        let rec get_name_of_lv = function
          | L_id id -> id
          | L_string s -> s
          | L_comp (lvalue, _) -> get_name_of_lv lvalue
        in
        match sem_lvalue_kind lv with
        | Types.T_array (n, t) ->
            if n <> -1 then begin
              match Ast.get_const_expr_value e with
              | Some (T_int, index) when index < 0 || index >= n ->
                  Error.handle_error "Segmentation fault"
                    (sprintf
                       "Attempt to access an out of bounds element of the \
                        array '%s'."
                       (get_name_of_lv lv))
              | _ -> ()
            end;
            t
        | t ->
            Error.handle_error "Iteration on non-array type of variable"
              (sprintf
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
            (sprintf
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
            (sprintf
               "Arithmetic operators must be applied to integer expressions. \
                %s argument is of non-integer type."
               (if typeExpr1 <> T_int then "Left" else "Right")));
      T_int
  | E_expr_parenthesized e -> sem_expr e

(** [sem_cond (c : Ast.cond)] semantically analyses condition [c]. [true] is
    returned if all comparison operators included are applied to arguments of
    the same type. *)
and sem_cond : Ast.cond -> bool = function
  | C_not_cond (lo, c) -> sem_cond c
  | C_cond_cond (c1, lo, c2) -> sem_cond c1 && sem_cond c2
  | C_expr_expr (e1, co, e2) ->
      let typeExpr1, typeExpr2 = (sem_expr e1, sem_expr e2) in
      let exprTypesMatch = Types.equal_types typeExpr1 typeExpr2 in
      if not exprTypesMatch then
        Error.handle_type_error typeExpr1 typeExpr2
          "Arguments of a logical operator have different types. Only \
           expressions of the same type can be compared with a logical \
           operator.";
      exprTypesMatch
  | C_cond_parenthesized c -> sem_cond c

(** [sem_funcCall (fc : Ast.funcCall)] returns the return type of function call
    [fc] (the result is encapsulated in [Types.T_func]). Additionally, it checks
    if the types of its arguments match the expected ones defined in the
    function's header. The fields 'comp_id' and 'ret_type' of [fc] are set. *)
and sem_funcCall fc : Types.t_type =
  let resultLookUpOption = look_up_entry fc.id in
  try
    if resultLookUpOption = None then raise Not_found;
    if not (List.mem fc.id Symbol.lib_function_names) then begin
      let ancestorsNames =
        let none = [] in
        let rec some sc = sc.name :: Option.fold ~none ~some sc.parent in
        Option.fold ~none ~some (Some (Option.get resultLookUpOption).scope)
      in
      String.concat "" ancestorsNames |> Hashtbl.hash |> sprintf "(%d)"
      |> fun postfix -> fc.comp_id <- fc.id ^ postfix
    end;
    let functionEntry =
      match (Option.get resultLookUpOption).kind with
      | ENTRY_function ef -> ef
      | ENTRY_variable _ | ENTRY_parameter _ -> raise Error.Shared_name_func_var
    in
    set_entry_isUsed (Option.get resultLookUpOption);
    if fc.ret_type = None then
      fc.ret_type <- Some (Types.get functionEntry.return_type);
    let isNumOfParamsOK =
      List.compare_lengths fc.expr_list functionEntry.parameters_list = 0
    in
    if not isNumOfParamsOK then
      Error.handle_error "Unexpected number of arguments in function call"
        (sprintf "Function '%s' expected %d arguments, but instead got %d."
           fc.id
           (List.length functionEntry.parameters_list)
           (List.length fc.expr_list));
    let checkArgs () : unit =
      let t_type_of_param_entry par = par.parameter_type in
      let is_by_ref_of_param_entry par = par.passing = Symbol.BY_REFERENCE in
      let rec is_lvalue_of_expr = function
        | E_lvalue _ -> true
        | E_expr_parenthesized e -> is_lvalue_of_expr e
        | _ -> false
      in
      iteri2
        (fun i param arg ->
          let typeOfParam = t_type_of_param_entry param in
          let typeOfArg = sem_expr arg in
          if not (Types.equal_types typeOfParam typeOfArg) then
            Error.handle_type_error typeOfParam typeOfArg
              (sprintf
                 "The type of the argument at position %d of the '%s' function \
                  call differs from the one declared at the function \
                  definition."
                 i fc.id);
          let isParamByRef = is_by_ref_of_param_entry param in
          let isExprLValue = is_lvalue_of_expr arg in
          if isParamByRef && not isExprLValue then
            Error.handle_error "r-value passed by reference"
              (sprintf
                 "'%s' function call: Argument at position %d that is passed \
                  by reference isn't an l-value."
                 fc.id i))
        functionEntry.parameters_list fc.expr_list
    in
    if isNumOfParamsOK then checkArgs ();
    functionEntry.return_type
  with
  | Not_found ->
      Error.handle_error "Undeclared function called"
        (sprintf "Function '%s' is called, but never declared." fc.id);
      T_none
  | Error.Shared_name_func_var ->
      Error.handle_error "Function and variable share the same name"
        (sprintf "Name '%s' is shared with a function and a variable." fc.id);
      T_none

(** [sem_on (ast : Ast.funcDef)] semantically analyses the root of the ast [ast]
    (produced by the parser). It also initializes the SymbolTable. *)
and sem_on asts : unit =
  Symbol.create_symbol_table 100;
  sem_funcDef asts
