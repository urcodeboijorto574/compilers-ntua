open Types

type identifier = {
  id_name : string 
  id_type : Types.t_type
}

module HashTable = Hashtbl.Make (
  struct
    type t = int
    let equal = (==)
    let hash = Hashtbl.hash
)

type scope = {
  parent : scope option;
  symbol_entries : entry list;
}

and passing_params = BY_VALUE | BY_REFERENCE

and entry = {
  id : identifier;  (* maybe requires a string *)
  scope : scope;
  kind : entry_kind;
}

and entry_variable = {
  variable_type : Types.t_type;
}

and entry_parameter = {
  parameter_type : Types.t_type;
  passing : passing_params
}

and entry_function = {
  parameters_list : entry_parameter list;
  return_type : Types.t_type
}

and entry_kind = ENTRY_none
               | ENTRY_variable of entry_variable
               | ENTRY_function of entry_function
               | ENTRY_parameter of entry_parameter