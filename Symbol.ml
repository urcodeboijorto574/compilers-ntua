module HT = Hashtbl.Make (struct
  type t = string (* Key type of the hashtable *)

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type param_passing =
  | BY_VALUE
  | BY_REFERENCE

and scope = {
  name : string;
  parent : scope option;
  mutable scope_entries : entry list;
}

and entry = {
  id : string;
  scope : scope;
  mutable kind : entry_kind;
}

and entry_kind =
  | ENTRY_none
  | ENTRY_variable of entry_variable
  | ENTRY_function of entry_function
  | ENTRY_parameter of entry_parameter

and entry_variable = {
  variable_type : Types.t_type;
  variable_array_size : int list;
}

and entry_function = {
  parameters_list : entry_parameter list;
  return_type : Types.t_type;
}

and entry_parameter = {
  parameter_type : Types.t_type;
  mutable parameter_array_size : int list;
  passing : param_passing;
}

(** [current_scope] is of type [scope ref]. *)
let current_scope = ref { name = ""; parent = None; scope_entries = [] }

(** [open_scope ()] updates the [current_scope] variable to a new one with
    [parent] the previous scope and an empty list for [scope_entries]. *)
let open_scope str =
  current_scope :=
    { name = str; parent = Some !current_scope; scope_entries = [] }

(** [close_scope ()] updates the [current_scope] variable to the scope
    saved as its [parent]. *)
and close_scope () =
  let getV = function None -> failwith "Initial scope closed" | Some v -> v in
  current_scope := getV !current_scope.parent

(** [symbolTable] is a Hashtbl ref that stores the current image of the SymbolTable. *)
let symbolTable = ref (HT.create 0)

(** [create_symbol_table n] initializes the symbolTable as a Hashtbl with [n]
    number of buckets. It also initializes the [current_scope]. *)
let create_symbol_table numOfBuckets =
  symbolTable := HT.create numOfBuckets;
  current_scope := { name = ""; parent = None; scope_entries = [] }

(** [enter_entry i e] takes an identifier [i] and an entry kind [e] and creates
    and adds a new entry in the symbolTable. *)
let enter_entry ident eKind =
  let e = { id = ident; scope = !current_scope; kind = eKind } in
  HT.add !symbolTable ident e;
  Printf.printf "entering entry %s in current scope\n" e.id;
  !current_scope.scope_entries <- e :: !current_scope.scope_entries

(** [enter_variable i t aS] takes an identifier [i], a type [t] and an array
    size [aS] and creates a new [entry_variable] that will be entered in the
    symbolTable via the [enter_entry] function. *)
let enter_variable id typ arrSize =
  let kind =
    ENTRY_variable { variable_type = typ; variable_array_size = arrSize }
  in
  enter_entry id kind

(** [enter_function i pL rt] enters a new function entry in the symbolTable.
    [i] is the function's identifier, [rt] is the return type of the function
    and [pL] is a list of type [(int * (Types.t_type * int list * param_passing))
    list]. Each element of the [pL] signifies the number of parameters in each
    parameter definition, with an exact type, array size and kind of parameter
    passing. *)
let enter_function (id : string)
    (paramList : (int * (Types.t_type * int list * param_passing)) list) retTyp
    =
  let paramL : entry_parameter list =
    let rec convert_list paramList =
      match paramList with
      | [] -> []
      | (0, (t, arrSz, pp)) :: tail -> convert_list tail
      | (n, (t, arrSz, pp)) :: tail ->
          { parameter_type = t; parameter_array_size = arrSz; passing = pp }
          :: convert_list ((n - 1, (t, arrSz, pp)) :: tail)
    in
    convert_list paramList
  in
  let kind =
    ENTRY_function { parameters_list = paramL; return_type = retTyp }
  in
  enter_entry id kind

(** [look_up_entry_temp] is a premature look_up function to search in the
    symbolTable. It exists only for debugging purpospes. *)
let look_up_entry_temp (id : string) =
  Printf.printf "\tLooking for name %s...\n" id;
  let rec look_up_entry_helper (sc : scope) =
    begin
      (* Print debug messages *)
      Printf.printf "Scope '%s':\n\t[ " sc.name;
      let rec print_entries_list = function
        | [] -> Printf.printf "]\n"
        | h :: t ->
            Printf.printf "%s " h.id;
            print_entries_list t
      in
      print_entries_list sc.scope_entries
    end;
    let isTarget e = e.id = id in
    try Some (List.find isTarget sc.scope_entries)
    with Not_found -> (
      match sc.parent with None -> None | Some s -> look_up_entry_helper s)
  in
  look_up_entry_helper !current_scope

(** [look_up_entry id] takes an [id : string] and checks if this name has been
    stored in the symbolTable.
    Returns [entry]. *)
let look_up_entry (id : string) =
  Printf.printf "\t Looking for name %s...\n" id;
  try HT.find !symbolTable id
  with Not_found -> failwith "(TODO) Undefined variable name"
