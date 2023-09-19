type param_passing =
  | BY_VALUE
  | BY_REFERENCE

and scope = {
  name : string;
  parent : scope option;
  depth : int;
  mutable scope_entries : entry list;
}

and entry = {
  id : string;
  scope : scope;
  mutable kind : entry_kind;
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
  return_type : Types.t_type;
  scope_depth : int;
  mutable state : entry_func_state;
}

let set_func_defined entryFunc = entryFunc.state <- DEFINED

let current_scope =
  ref { name = ""; parent = None; depth = 0; scope_entries = [] }

let open_scope str =
  current_scope :=
    {
      name = str;
      parent = Some !current_scope;
      depth = !current_scope.depth + 1;
      scope_entries = [];
    }

and close_scope () =
  let getV = function None -> failwith "Initial scope closed" | Some v -> v in
  current_scope := getV !current_scope.parent

(** [symbolTable] is a Hashtbl ref that stores the current image of the
    SymbolTable. *)
let symbolTable = ref (Hashtbl.create 0)

let create_symbol_table numOfBuckets =
  symbolTable := Hashtbl.create numOfBuckets;
  current_scope := { name = ""; parent = None; depth = 0; scope_entries = [] }

(** [enter_entry i e] takes an identifier [i] and an entry kind [e] and creates
    and adds a new entry in the symbolTable. *)
let enter_entry ident eKind =
  let e = { id = ident; scope = !current_scope; kind = eKind } in
  Hashtbl.add !symbolTable ident e;
  if Types.debugMode then
    Printf.printf "entering entry %s in current scope\n" e.id;
  !current_scope.scope_entries <- e :: !current_scope.scope_entries

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
    (paramList : (int * Types.t_type * param_passing) list) retTyp stat =
  let paramL : entry_parameter list =
    let rec convert_list paramList =
      match paramList with
      | [] -> []
      | (0, t, pp) :: tail -> convert_list tail
      | (n, t, pp) :: tail ->
          { parameter_type = t; passing = pp }
          :: convert_list ((n - 1, t, pp) :: tail)
    in
    convert_list paramList
  in
  let kind =
    ENTRY_function
      {
        parameters_list = paramL;
        return_type = retTyp;
        scope_depth = !current_scope.depth;
        state = stat;
      }
  in
  enter_entry id kind

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
  add_func_lib "writeString" [ (1, T_array (T_char, -1)) ] T_none;
  add_func_lib "readInteger" [] T_int;
  add_func_lib "readChar" [] T_char;
  add_func_lib "readString" [ (1, T_int); (1, T_array (T_char, -1)) ] T_none;
  add_func_lib "ascii" [ (1, T_char) ] T_int;
  add_func_lib "chr" [ (1, T_int) ] T_char;
  add_func_lib "strlen" [ (1, T_array (T_char, -1)) ] T_int;
  add_func_lib "strcmp" [ (2, T_array (T_char, -1)) ] T_int;
  add_func_lib "strcpy" [ (2, T_array (T_char, -1)) ] T_none;
  add_func_lib "strcat" [ (2, T_array (T_char, -1)) ] T_none

let look_up_entry (id : string) =
  let print_entries_list (sc : scope) =
    Printf.printf "Scope '%s':\n\t[ " sc.name;
    List.iter
      (fun e -> Printf.printf "%s(%s) " e.id e.scope.name)
      sc.scope_entries;
    Printf.printf "]\n"
  and print_hashtable () =
    let printedSmth = ref false in
    Printf.printf "\tSymbolTable contains the following:\n";
    let string_of_entry_kind = function
      | ENTRY_variable _ -> "ENTRY_variable"
      | ENTRY_parameter _ -> "ENTRY_parameter"
      | ENTRY_function _ -> "ENTRY_function"
    in
    Hashtbl.iter
      (fun key entry ->
        if key = id then (
          printedSmth := true;
          Printf.printf
            "\tKey: '%s', Value: { id = '%s'; scope = %s(%d); kind = %s }\n" key
            entry.id
            (match entry.scope.name with "" -> "(global)" | str -> str)
            entry.scope.depth
            (string_of_entry_kind entry.kind)))
      !symbolTable;
    if not !printedSmth then Printf.printf "\t(nothing)\n"
  in
  if Types.debugMode then (
    Printf.printf "Looking for name '%s':\n" id;
    print_hashtable ());

  try
    let resultEntryList =
      List.filter
        (fun e -> e.scope.depth <= !current_scope.depth)
        (Hashtbl.find_all !symbolTable id)
    in
    if Types.debugMode then
      print_entries_list
        {
          name = "result entries";
          parent = None;
          depth = 42;
          scope_entries = resultEntryList;
        };
    let resultEntry =
      try List.hd resultEntryList
      with Failure _ ->
        if Types.debugMode then
          Printf.printf "Entry '%s' not found in any scope.\n" id;
        raise Not_found
    in
    if Types.debugMode then
      Printf.printf "Entry '%s' found in scope '%s'!\n" id
        resultEntry.scope.name;
    if
      let rec equal_scopes s1 s2 : bool =
        s1.name = s2.name && s1.depth = s2.depth
        &&
        match (s1.parent, s2.parent) with
        | None, None -> true
        | Some s1', Some s2' -> equal_scopes s1' s2'
        | _ -> false
      in
      equal_scopes resultEntry.scope !current_scope
    then
      let rec get_entry_in_smaller_depth = function
        | [] -> raise Not_found
        | entry :: remainingList ->
            if entry.scope.depth > !current_scope.depth then
              get_entry_in_smaller_depth remainingList
            else (
              (* entry is defined in a parent scope *)
              if Types.debugMode then print_entries_list entry.scope;
              Some entry)
      in
      get_entry_in_smaller_depth resultEntryList
    else (
      (* entry got found *)
      if Types.debugMode then (
        Printf.printf "%s found in " id;
        print_entries_list resultEntry.scope);
      Some resultEntry)
  with Not_found -> None

let get_undefined_functions () =
  let undefinedFunctionsList = ref [] in
  begin
    Hashtbl.iter
      (fun i e ->
        match e.kind with
        | ENTRY_function ef ->
            if ef.state = DECLARED then
              undefinedFunctionsList := i :: !undefinedFunctionsList
        | ENTRY_variable _ | ENTRY_parameter _ -> ())
      !symbolTable
  end;
  match !undefinedFunctionsList with [] -> None | l -> Some l
