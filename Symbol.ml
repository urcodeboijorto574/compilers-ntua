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
let current_scope = ref { name = ""; parent = None; depth = 0 }

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

(** [symbolTable] is a Hashtbl ref that stores the current image of the
    SymbolTable. *)
let symbolTable = ref (Hashtbl.create 0)

let create_symbol_table numOfBuckets =
  symbolTable := Hashtbl.create numOfBuckets;
  current_scope := { name = ""; parent = None; depth = 0 }

(** [enter_entry i e] takes an identifier [i] and an entry kind [e] and creates
    and adds a new entry in the symbolTable. *)
let enter_entry ident eKind =
  let e = { id = ident; scope = !current_scope; kind = eKind } in
  Hashtbl.add !symbolTable ident e;
  if Types.debugMode then
    Printf.printf "entering entry %s in current scope\n" e.id

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
  if Types.debugMode then (
    Printf.printf "Looking for name '%s':\n" id;
    let print_hashtable () =
      let printedSmth = ref false in
      Printf.printf "\tSymbolTable contains the following:\n";
      let string_of_entry_kind = function
        | ENTRY_variable _ -> "ENTRY_variable"
        | ENTRY_parameter _ -> "ENTRY_parameter"
        | ENTRY_function _ -> "ENTRY_function"
      in
      Hashtbl.iter
        (fun key entry ->
          if not (List.mem key lib_function_names) then (
            printedSmth := true;
            Printf.printf
              "\tKey: '%s', Value: { id = '%s'; scope = %s(%d); kind = %s }\n"
              key entry.id
              (match entry.scope.name with "" -> "(global)" | str -> str)
              entry.scope.depth
              (string_of_entry_kind entry.kind)))
        !symbolTable;
      if not !printedSmth then Printf.printf "\t(nothing)\n"
    in
    print_hashtable ());

  let resultEntryList =
    (* Entries of ancestor scopes in decreasing order. *)
    List.stable_sort
      (fun e1 e2 -> compare e1.scope.depth e2.scope.depth * -1)
      ((* Entries of ancestor scopes of the current scope. *)
       List.filter
         (fun e ->
           (* [is_ancestor s1 s2] returns [true] if [s2] is part of [s1]'s
              ancestor scopes or if [s1] and [s2] are the same scope. *)
           let rec is_ancestor referenceScope candidateScope =
             if referenceScope.parent = None then
               candidateScope.parent = None
             else
               candidateScope.parent = None
               || equal_scopes referenceScope candidateScope
               || is_ancestor (Option.get referenceScope.parent) candidateScope
           in
           is_ancestor !current_scope e.scope)
         ((* Entries with [id] as their name. *)
          Hashtbl.find_all !symbolTable id))
  in
  if Types.debugMode then (
    let print_entry_list () =
      Printf.printf "resultEntryList contains the following\n\t[ ";
      if resultEntryList <> [] then (
        Printf.printf "\n\t";
        List.iter
          (fun e ->
            Printf.printf "  { id: %s, scope: %s, depth: %d}\n\t" e.id
              e.scope.name e.scope.depth)
          resultEntryList);
      Printf.printf "]\n"
    in
    print_entry_list ();
    if resultEntryList <> [] then
      Printf.printf
        "Entry '%s' found in scope '%s'(depth: %d)!(Current depth: %d)\n" id
        (List.hd resultEntryList).scope.name
        (List.hd resultEntryList).scope.depth !current_scope.depth
    else
      Printf.printf "Entry '%s' not found in any scope.\n" id);
  try Some (List.hd resultEntryList) with _ -> None

let get_undefined_functions () =
  let undefinedFunctionsList = ref [] in
  Hashtbl.iter
    (fun id e ->
      match e.kind with
      | ENTRY_function ef ->
          if ef.state = DECLARED then
            undefinedFunctionsList := id :: !undefinedFunctionsList
      | ENTRY_variable _ | ENTRY_parameter _ -> ())
    !symbolTable;
  !undefinedFunctionsList
