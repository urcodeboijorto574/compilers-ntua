module HT = Hashtbl.Make (struct
  type t = string (* Key type of the hashtable *)

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type passing_params =
| BY_VALUE
| BY_REFERENCE

and scope = {
parent : scope option;
mutable symbol_entries : entry list;
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
variable_array_size : int option;
}

and entry_function = {
parameters_list : entry_parameter list;
return_type : Types.t_type;
}

and entry_parameter = {
parameter_type : Types.t_type;
passing : passing_params;
}

let current_scope = ref { parent = None; symbol_entries = [] }

let open_scope () =
  current_scope := { parent = Some !current_scope; symbol_entries = [] }

and close_scope () =
  let getV = function None -> failwith "no value" | Some v -> v in
  current_scope := getV !current_scope.parent

let symbolTable = ref (HT.create 0)

let create_symbol_table numOfBuckets =
  symbolTable := HT.create numOfBuckets;
  current_scope := { parent = None; symbol_entries = [] }
