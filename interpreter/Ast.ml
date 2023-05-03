type func_def = {
  header: header;
  local_def: local_def list;
  block:  stmt list;
}

type header = {
  id: string;
  header_r: header_r;
  ret_type: ret_type;
}

type header_r = {
  fpar_def: fpar_def;
  header_rr: fpar_def list;
}

type fpar_def = {
  ref: bool;
  id: string;
  fpar_def_r: list string;
  fpar_type: fpar_type;
}

type data_type = int | char

type mytype = {
  data_type: data_type;
  my_type_r: list int;
}

type ret_type = data_type | nothing of string

type fpar_type = {
  data_type: data_type;
  rest: integer;
}

type local_def = func_def | func_decl | var_def

type func_decl = header;



