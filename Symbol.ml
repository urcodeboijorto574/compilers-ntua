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
  let getV = function None -> failwith "no value" | Some v -> v in
  current_scope := getV !current_scope.parent

let symbolTable = ref (HT.create 0)

let create_symbol_table numOfBuckets =
  symbolTable := HT.create numOfBuckets;
  current_scope := { parent = None; scope_entries = [] }

let enter_entry ident eKind =
  let e = { id = ident; scope = !current_scope; kind = eKind } in
  HT.add !symbolTable ident e;
  !current_scope.scope_entries <- e :: !current_scope.scope_entries

let enter_variable id typ arrSize =
  let kind =
    ENTRY_variable { variable_type = typ; variable_array_size = arrSize }
  in
  enter_entry id kind

let enter_function id
    (paramList : (int * (Types.t_type * int list * param_passing)) list) retTyp
    =
  let paramL =
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

let look_up_entry id =
  let f : entry -> bool = function
  | { id = ident; scope = s; kind = k } -> id = ident
  in
  let rec helper (id : string) : scope option -> entry option = function
  | None -> raise Not_found
  | Some s -> (
      try Some (List.find f s.scope_entries) with Not_found -> raise Not_found)
  in
  try helper id (Some !current_scope)
  with Not_found ->
    (try match HT.find !symbolTable id with e -> Some e
     with Not_found -> None);
    None
