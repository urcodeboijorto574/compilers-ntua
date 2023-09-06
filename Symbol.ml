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
  | ENTRY_variable of entry_variable
  | ENTRY_function of entry_function
  | ENTRY_parameter of entry_parameter

and entry_variable = { variable_type : Types.t_type }

and entry_function = {
  parameters_list : entry_parameter list;
  return_type : Types.t_type;
}

and entry_parameter = {
  parameter_type : Types.t_type;
  passing : param_passing;
}

let current_scope = ref { name = ""; parent = None; scope_entries = [] }

let open_scope str =
  current_scope :=
    { name = str; parent = Some !current_scope; scope_entries = [] }

and close_scope () =
  let getV = function None -> failwith "Initial scope closed" | Some v -> v in
  current_scope := getV !current_scope.parent

(** [symbolTable] is a Hashtbl ref that stores the current image of the
    SymbolTable. *)
let symbolTable = ref (HT.create 0)

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

let enter_variable id typ =
  enter_entry id (ENTRY_variable { variable_type = typ })

(** [enter_parameter id t isRef] takes an identifier [id], a type [t] and
    [isRef : bool] that marks whether the parameter passing is by reference, in
    which case [true], or by value, [false]. Returns [unit]. *)
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
    (paramList : (int * Types.t_type * param_passing) list) retTyp =
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
    ENTRY_function { parameters_list = paramL; return_type = retTyp }
  in
  enter_entry id kind

let look_up_entry_temp (id : string) =
  Printf.printf "Looking for name %s...\n" id;
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

let look_up_entry (id : string) =
  Printf.printf "Looking for name %s...\n" id;
  try HT.find !symbolTable id
  with Not_found -> failwith "(TODO) Undefined variable name"
