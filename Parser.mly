%{
	open Ast
  open Types
%}

%token T_while T_do
%token T_if T_then T_else
%token T_fun
%token T_int
%token T_nothing
%token T_ref
%token T_return

%token T_or
%token T_not
%token T_and
%token T_plus T_minus
%token T_mul T_mod T_div

%token T_char
%token T_var

%token T_left_par T_right_par T_left_sqr T_right_sqr T_left_br T_right_br
%token T_comma T_semicolon T_colon
%token T_assignment
%token <Ast.sem_id> T_identifier
%token <int> T_integer
%token <char> T_chr
%token <string> T_string

%token T_eof

(* TODO: check the operator hierarchy below, for re-assurance *)
%token T_equal T_not_equal T_less_eq T_greater_eq T_less T_greater
%left T_or
%left T_and
%left T_plus T_minus
%left T_mul T_mod T_div
%nonassoc T_not

%start program
%type <Ast.funcDef> program
%type <funcDef> func_def
%type <localDef list> local_def_list
%type <header> header
%type <fparDef> fpar_def
%type <fparDef list> fpar_def_list
%type <string list> id_list
%type <dataType> data_type
%type <myType> mytype
%type <int list> array_dimension
%type <retType> ret_type
%type <fparType> fpar_type
%type <localDef> local_def
%type <funcDecl> func_decl
%type <varDef> var_def
%type <stmt> stmt
%type <block> block
%type <stmt list> stmt_list
%type <funcCall> func_call
%type <sem_expr list> expr_list
%type <lvalue> l_value
%type <lvalue> l_value_id_or_array
%type <sem_expr> expr
%type <arithmOperator> arithm_oper
%type <cond> cond
%type <binOperator> bin_oper_cond
%type <binOperator> bin_oper_expr

%%

program:
  func_def T_eof { $1 }

func_def:
  header local_def_list block { newFuncDef($1, $2, $3) }

local_def_list:
  (* nothing *)  { [] }
| local_def local_def_list { $1 :: $2 }

header:
  T_fun T_identifier T_left_par T_right_par T_colon ret_type { newHeader($2, [], $6) }
| T_fun T_identifier T_left_par fpar_def_list T_right_par T_colon ret_type { newHeader($2, $4, $7) }

fpar_def_list:
  fpar_def { [$1] }
| fpar_def T_semicolon fpar_def_list { $1 :: $3 }

fpar_def:
  T_ref T_identifier id_list T_colon fpar_type { newFparDef(true, $2 :: $3, $5) }
| T_identifier id_list T_colon fpar_type { newFparDef(false, $1 :: $2, $4) }

id_list:
  (* nothing *) { [] }
| T_comma T_identifier id_list { $2 :: $3 }

data_type:
  T_int { ConstInt }
| T_char { ConstChar }

mytype:
  data_type array_dimension { newMyType($1, $2) }

array_dimension:
  (* nothing *) { [] }
| T_left_sqr T_integer T_right_sqr array_dimension { $2 :: $4 }

ret_type:
  data_type { RetDataType($1) }
| T_nothing { Nothing }

fpar_type:
  data_type array_dimension { newFparType($1, $2, false) }
| data_type T_left_sqr T_right_sqr array_dimension { newFparType($1, $4, true) }

local_def:
  func_def { L_FuncDef($1) }
| func_decl { L_FuncDecl($1) }
| var_def { L_varDef($1) }

func_decl:
  header T_semicolon { FuncDecl_Header($1) }

var_def:
  T_var T_identifier id_list T_colon mytype T_semicolon { newVarDef($2 :: $3, $5) }

stmt:
  T_semicolon { S_semicolon }
| l_value_id_or_array T_assignment expr T_semicolon { newAssignment($1, $3) }
| block { S_block($1) }
| func_call T_semicolon { S_func_call($1) }
| T_if cond T_then stmt { S_if($2, $4) }
| T_if cond T_then stmt T_else stmt { S_if_else($2, $4, $6) }
| T_while cond T_do stmt { S_while($2, $4) }
| T_return T_semicolon { S_semicolon }
| T_return expr T_semicolon { S_return($2) }

block:
  T_left_br stmt_list T_right_br { Block($2) }

stmt_list:
  (* nothing *) { [] }
| stmt stmt_list { $1 :: $2 }

func_call:
  T_identifier T_left_par T_right_par { newFuncCall($1, [], T_func(None)) }
| T_identifier T_left_par expr_list T_right_par { newFuncCall($1, $3, T_func(Some(T_int))) }

expr_list:
  expr { [$1] }
| expr T_comma expr_list { $1 :: $3 }

l_value:
  T_identifier { L_id($1) }
| T_string { L_string($1) }
| l_value T_left_sqr expr T_right_sqr {
    check_type_int $3.expr_type;
    L_comp($1, $3)
  }

l_value_id_or_array:
  T_identifier { L_id($1) }
| l_value T_left_sqr expr T_right_sqr {
    check_type_int $3.expr_type;
    L_comp($1, $3)
  }

expr:
  T_integer { newSemExpr(E_const_int($1), T_int) }
| T_chr { newSemExpr(E_const_char($1), T_char) }
| l_value { newSemExpr(E_lvalue($1), T_int(* TODO: missing value here, T_int is placeholder *)) }
| T_left_par expr T_right_par { newSemExpr(E_expr_parenthesized($2), $2.expr_type) }
| func_call { newSemExpr(E_func_call($1), $1.func_type) }
| T_plus expr {
    check_type_int $2.expr_type;
    newSemExpr(E_op_expr(O_plus, $2), $2.expr_type)
  }
| T_minus expr {
    check_type_int $2.expr_type;
    newSemExpr(E_op_expr(O_minus, $2), $2.expr_type)
  }
| expr arithm_oper expr {
    equal_type $1.expr_type $3.expr_type;
    check_type_int $1.expr_type;
    newSemExpr(E_op_expr_expr($1, $2, $3), T_int)
  }

arithm_oper:
  T_plus { O_plus }
| T_minus { O_minus }
| T_mul { O_mul }
| T_div { O_div }
| T_mod { O_mod }

cond:
  T_left_par cond T_right_par { C_cond_parenthesized($2) }
| T_not cond { C_not_cond(O_not, $2) }
| cond bin_oper_cond cond { C_cond_cond($1, $2, $3) }
| expr bin_oper_expr expr {
    equal_type $1.expr_type $3.expr_type;
    C_expr_expr($1, $2, $3)
  }

bin_oper_cond:
  T_and { O_and }
| T_or { O_or }

bin_oper_expr:
  T_equal { O_equal }
| T_less { O_less }
| T_less_eq { O_less_eq }
| T_greater { O_greater }
| T_greater_eq { O_greater_eq }
| T_not_equal { O_not_equal }
