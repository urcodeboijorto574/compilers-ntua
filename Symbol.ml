module HT = Hashtbl.Make (struct
  type t = string (* Key type of the hashtable *)

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type param_passing =
| BY_VALUE
| BY_REFERENCE

and scope = {
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

let current_scope = ref { parent = None; scope_entries = [] }

let open_scope () =
  current_scope := { parent = Some !current_scope; scope_entries = [] }

and close_scope () =
  let getV = function
  | None -> failwith "Can't close initial scope"
  | Some v -> v
  in
  current_scope := getV !current_scope.parent

let symbolTable = ref (HT.create 0)

let create_symbol_table numOfBuckets =
  symbolTable := HT.create numOfBuckets;
  current_scope := { parent = None; scope_entries = [] }

let enter_entry ident eKind =
  let e = { id = ident; scope = !current_scope; kind = eKind } in
  HT.add !symbolTable ident e;
  Printf.printf "entering entry %s in current scope\n" e.id;
  !current_scope.scope_entries <- e :: !current_scope.scope_entries

let enter_variable id typ arrSize =
  let kind =
    ENTRY_variable { variable_type = typ; variable_array_size = arrSize }
  in
  enter_entry id kind

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

(* let enter_parameter id typ arrSize isRef =
   let kind =
     ENTRY_parameter
       {
         parameter_type = typ;
         parameter_array_size = arrSize;
         passing = (if isRef then BY_REFERENCE else BY_VALUE);
       }
   in
   enter_entry id kind *)

let scope_name = ref [ "Init" ]

let look_up_entry (id : string) =
  Printf.printf "\tLooking for name %s...\n" id;
  let rec look_up_entry_helper (sc : scope) =
    (Printf.printf "Scope '%s':\n\t[ " (List.hd !scope_name);
     let rec printEntriesList = function
     | [] -> Printf.printf "]\n"
     | h :: t ->
         Printf.printf "%s " h.id;
         printEntriesList t
     in
     printEntriesList sc.scope_entries);
    let isTarget e = e.id = id in
    try Some (List.find isTarget sc.scope_entries)
    with Not_found -> (
      match sc.parent with None -> None | Some s -> look_up_entry_helper s)
  in
  look_up_entry_helper !current_scope

let add_scope_name str = scope_name := str :: !scope_name
let rem_scope_name () = scope_name := List.tl !scope_name
