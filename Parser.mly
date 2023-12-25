%{
  open Ast
  open Types

  let rec string_list_of_char_list = function
    | [] -> []
    | h :: t -> (String.make 1 h) :: string_list_of_char_list t
%}

(* Token declarations *)
%token T_while T_do T_if T_then T_else
%token T_fun
%token T_int T_char T_var
%token T_nothing
%token T_ref
%token T_return

%token T_plus T_minus T_mul T_mod T_div
%token T_or T_not T_and
%token T_equal T_not_equal T_less_eq T_greater_eq T_less T_greater

%token T_left_par T_right_par T_left_sqr T_right_sqr T_left_br T_right_br
%token T_comma T_semicolon T_colon
%token T_assignment
%token <string> T_identifier
%token <int> T_integer
%token <char> T_chr
%token <char list> T_string

%token T_eof

(* Operators' hierarchy *)
%left T_or
%left T_and
%nonassoc T_not
%left T_plus T_minus
%left T_mul T_mod T_div

%nonassoc T_then
%nonassoc T_else

(* Types of non-terminal symbols *)
%start program
%type <Ast.funcDef> program
%type <funcDef> func_def
%type <header> header
%type <retType> ret_type
%type <fparDef list> fpar_def_list
%type <fparDef> fpar_def
%type <string list> id_list
%type <fparType> fpar_type
%type <dataType> data_type
%type <int list> array_dimensions
%type <localDef list> local_def_list
%type <localDef> local_def
%type <funcDecl> func_decl
%type <varDef> var_def
%type <varType> var_type
%type <block> block
%type <stmt list> stmt_list
%type <stmt> stmt
%type <lvalue> l_value
%type <lvalue_kind> l_value_comp
%type <expr> expr
%type <funcCall> func_call
%type <expr list> expr_list
%type <cond> cond

%%

program:
    func_def T_eof { $1 }

func_def:
    header local_def_list block { newFuncDef($1, $2, $3) }

header:
    T_fun T_identifier T_left_par T_right_par T_colon ret_type { newHeader($2, [], $6) }
  | T_fun T_identifier T_left_par fpar_def_list T_right_par T_colon ret_type { newHeader($2, $4, $7) }

ret_type:
    data_type { RetDataType($1) }
  | T_nothing { Nothing }

fpar_def_list:
    fpar_def { [$1] }
  | fpar_def T_semicolon fpar_def_list { $1 :: $3 }

fpar_def:
    T_ref T_identifier id_list T_colon fpar_type { newFparDef(true, $2 :: $3, $5) }
  | T_identifier id_list T_colon fpar_type { newFparDef(false, $1 :: $2, $4) }

id_list:
    (* nothing *) { [] }
  | T_comma T_identifier id_list { $2 :: $3 }

fpar_type:
    data_type array_dimensions { newFparType($1, $2) }
  | data_type T_left_sqr T_right_sqr array_dimensions { newFparType($1, -1 :: $4) }

data_type:
    T_int { ConstInt }
  | T_char { ConstChar }

array_dimensions:
    (* nothing *) { [] }
  | T_left_sqr T_integer T_right_sqr array_dimensions { $2 :: $4 }

local_def_list:
    (* nothing *)  { [] }
  | local_def local_def_list { $1 :: $2 }

local_def:
    func_def { L_funcDef($1) }
  | func_decl { L_funcDecl($1) }
  | var_def { L_varDef($1) }

func_decl:
    header T_semicolon { FuncDecl_Header($1) }

var_def:
    T_var T_identifier id_list T_colon var_type T_semicolon { newVarDef($2 :: $3, $5) }

var_type:
    data_type array_dimensions { newVarType($1, $2) }

block:
    T_left_br stmt_list T_right_br { Block($2) }

stmt_list:
    (* nothing *) { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
    T_semicolon { S_semicolon }
  | block { S_block($1) }
  | l_value T_assignment expr T_semicolon { S_assignment($1, $3) }
  | func_call T_semicolon { S_func_call($1) }
  | T_if cond T_then stmt { S_if($2, $4) }
  | T_if cond T_then stmt T_else stmt { S_if_else($2, $4, $6) }
  | T_while cond T_do stmt { S_while($2, $4) }
  | T_return T_semicolon { S_return(None) }
  | T_return expr T_semicolon { S_return(Some($2)) }

l_value:
    T_identifier { newLValue (L_id ($1)) }
  | T_string { newLValue (L_string (String.concat "" (string_list_of_char_list $1))) }
  | l_value_comp T_left_sqr expr T_right_sqr { newLValue (L_comp ($1, $3)) }

l_value_comp:
    T_identifier { L_id $1 }
  | T_string { L_string (String.concat "" (string_list_of_char_list $1)) }
  | l_value_comp T_left_sqr expr T_right_sqr { L_comp ($1, $3) }

expr:
    T_integer { E_const_int($1) }
  | T_chr { E_const_char($1) }
  | l_value { E_lvalue($1) }
  | T_left_par expr T_right_par { E_expr_parenthesized($2) }
  | func_call { E_func_call($1) }
  | T_plus expr { E_sgn_expr(O_plus, $2) }
  | T_minus expr { E_sgn_expr(O_minus, $2) }
  | expr T_plus expr { E_op_expr_expr($1, O_plus, $3) }
  | expr T_minus expr { E_op_expr_expr($1, O_minus, $3) }
  | expr T_mul expr { E_op_expr_expr($1, O_mul, $3) }
  | expr T_div expr { E_op_expr_expr($1, O_div, $3) }
  | expr T_mod expr { E_op_expr_expr($1, O_mod, $3) }

func_call:
    T_identifier T_left_par T_right_par { newFuncCall($1, []) }
  | T_identifier T_left_par expr_list T_right_par { newFuncCall($1, $3) }

expr_list:
    expr { [$1] }
  | expr T_comma expr_list { $1 :: $3 }

cond:
    T_left_par cond T_right_par { C_cond_parenthesized($2) }
  | T_not cond { C_not_cond(O_not, $2) }
  | cond T_and cond { C_cond_cond($1, O_and, $3) }
  | cond T_or cond { C_cond_cond($1, O_or, $3) }
  | expr T_equal expr { C_expr_expr($1, O_equal, $3) }
  | expr T_not_equal expr { C_expr_expr($1, O_not_equal, $3) }
  | expr T_less expr { C_expr_expr($1, O_less, $3) }
  | expr T_less_eq expr { C_expr_expr($1, O_less_eq, $3) }
  | expr T_greater expr { C_expr_expr($1, O_greater, $3) }
  | expr T_greater_eq expr { C_expr_expr($1, O_greater_eq, $3) }
