type passing_params =
| BY_VALUE
| BY_REFERENCE

and scope = {
parent : scope option;
symbol_entries : entry list;
}

and entry = {
id : string;
scope : scope;
kind : entry_kind;
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

and entry_parameter = {
parameter_type : Types.t_type;
passing : passing_params;
}

and entry_function = {
parameters_list : entry_parameter list;
return_type : Types.t_type;
}

val current_scope : scope ref
