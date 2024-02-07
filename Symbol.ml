let lib_function_names =
  [
    "writeInteger";
    "writeChar";
    "writeString";
    "readInteger";
    "readChar";
    "readString";
    "ascii";
    "chr";
    "strlen";
    "strcmp";
    "strcpy";
    "strcat";
  ]

type param_passing =
  | BY_VALUE
  | BY_REFERENCE

and scope = {
  name : string;
  parent : scope option;
  depth : int;
}

and entry = {
  id : string;
  scope : scope;
  mutable kind : entry_kind;
  mutable isUsed : bool;
}

and entry_kind =
  | ENTRY_variable of entry_variable
  | ENTRY_function of entry_function
  | ENTRY_parameter of entry_parameter

and entry_variable = { variable_type : Types.t_type }

and entry_parameter = {
  parameter_type : Types.t_type;
  passing : param_passing;
}

and entry_func_state =
  | DECLARED
  | DEFINED

and entry_function = {
  parameters_list : entry_parameter list;
  (* [return_type] is encapsulated in [Types.T_func]. *)
  return_type : Types.t_type;
  scope_depth : int;
  mutable state : entry_func_state;
}

let set_func_defined entryFunc = entryFunc.state <- DEFINED
and set_entry_isUsed entry = entry.isUsed <- true

let initialScopeDepthValue = 0

let current_scope =
  ref { name = ""; parent = None; depth = initialScopeDepthValue }

let open_scope str =
  current_scope :=
    {
      name = str;
      parent = Some !current_scope;
      depth = !current_scope.depth + 1;
    }

and close_scope () =
  try current_scope := Option.get !current_scope.parent
  with Invalid_argument _ -> failwith "Initial scope closed"

let rec equal_scopes s1 s2 : bool =
  s1.name = s2.name && s1.depth = s2.depth
  &&
  match (s1.parent, s2.parent) with
  | None, None -> true
  | Some s1', Some s2' -> equal_scopes s1' s2'
  | _ -> false

let rec different_scopes s1 s2 : bool =
  s1.name <> s2.name || s1.depth <> s2.depth
  ||
  match (s1.parent, s2.parent) with
  | None, None -> false
  | Some s1', Some s2' -> different_scopes s1' s2'
  | _ -> true

(** [symbolTable] is a Hashtbl ref that stores the current image of the
    SymbolTable. *)
let symbolTable = ref (Hashtbl.create 0)

let create_symbol_table numOfBuckets =
  symbolTable := Hashtbl.create numOfBuckets;
  current_scope := { name = ""; parent = None; depth = initialScopeDepthValue }

(** [enter_entry i e] takes an identifier [i] and an entry kind [e] and creates
    and adds a new entry in the symbolTable. *)
let enter_entry ident eKind =
  let isUsed =
    List.mem ident lib_function_names
    || !current_scope.depth = initialScopeDepthValue
  in
  let e = { id = ident; scope = !current_scope; kind = eKind; isUsed } in
  Hashtbl.add !symbolTable ident e

let enter_variable id typ =
  enter_entry id (ENTRY_variable { variable_type = typ })

let enter_parameter id typ isRef =
  let kind =
    ENTRY_parameter
      {
        parameter_type = typ;
        passing = (if isRef then BY_REFERENCE else BY_VALUE);
      }
  in
  enter_entry id kind

let enter_function (id : string)
    (paramList : (int * Types.t_type * param_passing) list) return_type state =
  let parameters_list : entry_parameter list =
    List.map
      (fun (n, t, pp) ->
        List.init n (fun _ -> { parameter_type = t; passing = pp }))
      paramList
    |> List.flatten
  in
  ENTRY_function
    { parameters_list; return_type; scope_depth = !current_scope.depth; state }
  |> enter_entry id

let add_standard_library () =
  let open Types in
  let add_func_lib name parList typ =
    let extract_passing (n, t) =
      ( n,
        t,
        match t with
        | T_array _ -> BY_REFERENCE
        | T_int | T_char -> BY_VALUE
        | T_none | T_func _ -> assert false )
    in
    enter_function name (List.map extract_passing parList) (T_func typ) DEFINED
  in
  add_func_lib "writeInteger" [ (1, T_int) ] T_none;
  add_func_lib "writeChar" [ (1, T_char) ] T_none;
  add_func_lib "writeString" [ (1, T_array (-1, T_char)) ] T_none;

  add_func_lib "readInteger" [] T_int;
  add_func_lib "readChar" [] T_char;
  add_func_lib "readString" [ (1, T_int); (1, T_array (-1, T_char)) ] T_none;

  add_func_lib "ascii" [ (1, T_char) ] T_int;
  add_func_lib "chr" [ (1, T_int) ] T_char;

  add_func_lib "strlen" [ (1, T_array (-1, T_char)) ] T_int;
  add_func_lib "strcmp" [ (2, T_array (-1, T_char)) ] T_int;
  add_func_lib "strcpy" [ (2, T_array (-1, T_char)) ] T_none;
  add_func_lib "strcat" [ (2, T_array (-1, T_char)) ] T_none

let look_up_entry (id : string) =
  let is_entry_from_ancestor_scope e =
    (* [is_ancestor s1 s2] returns [true] if [s2] is part of [s1]'s ancestor
       scopes or if [s1] and [s2] are the same scope. *)
    let rec is_ancestor referenceScope candidateScope =
      referenceScope.depth >= candidateScope.depth
      && (equal_scopes referenceScope candidateScope
         || is_ancestor (Option.get referenceScope.parent) candidateScope)
    in
    !current_scope.depth >= e.scope.depth
    && (e.scope.parent = None || is_ancestor !current_scope e.scope)
  in
  (* Entries with [id] as their name. *)
  Hashtbl.find_all !symbolTable id
  (* Entries of ancestor scopes of the current scope. *)
  |> List.filter is_entry_from_ancestor_scope
  (* Entries of ancestor scopes in decreasing order of depths. *)
  |> List.stable_sort (fun e1 e2 -> compare e1.scope.depth e2.scope.depth * -1)
  (* Entry closest to the current scope. *)
  |> fun entryList -> match entryList with [] -> None | e :: _ -> Some e

let get_unused_entries () =
  let unusedEntriesList = ref [] in
  Hashtbl.iter
    (fun id e ->
      if not e.isUsed then unusedEntriesList := id :: !unusedEntriesList)
    !symbolTable;
  !unusedEntriesList
