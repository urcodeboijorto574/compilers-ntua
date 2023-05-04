%{
	open Ast
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
%token T_identifier
%token T_integer
%token T_chr
%token T_string

%token T_eof

%token T_equal T_not_equal T_less_eq T_greater_eq T_less T_greater

%left T_or
%left T_and
%left T_plus T_minus
%left T_mul T_mod T_div
%nonassoc T_not

%start program
%type <Ast.FuncDef> program
%type <FuncDef> func_def
%type <LocalDef list> local_def_list
%type <Header> header
%type <FparDef> fpar_def
%type <FparDef list> fpar_def_list
%type <string list> id_list
%type <DataType> data_type
%type <MyType> mytype
%type <int list> array_dimension
%type <RetType> ret_type
%type <FparType> fpar_type
%type <LocalDef> local_def
%type <FuncDecl> func_decl
%type <VarDef> var_def
%type <Stmt> stmt
%type <Stmt list> block
%type <Stmt list> stmt_list
%type <FuncCall> func_call
%type <Expr> expr_list
%type <Lvalue> l_value
%type <Expr> expr
%type <Cond> cond


%%


program     : func_def T_eof { $1 }

func_def    : header local_def_list block { FuncDef($1, $2, $3) }

local_def_list : { [] }
               | local_def local_def_list   { $1 :: $2 }

header      : T_fun T_identifier T_left_par T_right_par T_colon ret_type { Header($2, [], $6) }
            | T_fun T_identifier T_left_par fpar_def_list T_right_par T_colon ret_type { Header($2, $4, $7) }

fpar_def_list : fpar_def    { [$1] }
              | fpar_def T_semicolon fpar_def_list  { $1 :: $3 }


fpar_def    : T_ref T_identifier id_list T_colon fpar_type { FparDef("ref", $2 :: $3, $5) }
            | T_identifier id_list T_colon fpar_type { FparDef("", $2, $4) }

id_list  :  { [] } 
            | T_comma T_identifier id_list { $2 :: $3 }

data_type   : T_int { Const($1) }
            | T_char { Char($1) }

mytype      : data_type array_dimension { MyType($1, $2) }

array_dimension    : { [] }
                   | T_left_sqr T_integer T_right_sqr array_dimension { $2 :: $4 }

ret_type    : data_type { RetDataType($1) }
            | T_nothing { Nothing("nothing") }

fpar_type   : data_type array_dimension    { FparType($1, $2) }
            | data_type T_left_sqr T_right_sqr array_dimension { FparType($1, $4) }

local_def   : func_def { L_FuncDef($1) }
            | func_decl { L_FuncDecl($1) }
            | var_def { L_VarDef($1) }

func_decl   : header T_semicolon { FuncDecl_Header($1) }

var_def     : T_var T_identifier id_list T_colon mytype T_semicolon { VarDef($2 :: $3, $5) }

stmt        : T_semicolon { () }
            | l_value T_assignment expr T_semicolon { S_assignment($1, $3) }
            | block { S_block($1) }
            | func_call T_semicolon { S_func_call($1) }
            | T_if cond T_then stmt { S_if($2, $4) }
            | T_if cond T_then stmt T_else stmt{ S_if_else($2, $4, $6) }
            | T_while cond T_do stmt { S_while($2, $4) }
            | T_return T_semicolon { () }
            | T_return expr T_semicolon { S_return($2)  }

block       : T_left_br stmt_list T_right_br { $2 }

stmt_list   : { [] }
            | stmt stmt_list { $1 :: $2 }

func_call   : T_identifier T_left_par T_right_par { FuncCall($1, []) }
            | T_identifier T_left_par expr_list T_right_par { FuncCall($1, $3) }

expr_list   : expr { $1 }
            | expr T_comma expr_list { $1 :: $3 }


l_value     : T_identifier { L_id($1) }
            | T_string { L_string($1) }
            | l_value T_left_sqr expr T_right_sqr { L_comp($1, $3) }


expr        : T_integer { E_const($1) }
            | T_chr { E_char($1) }
            | l_value { E_lvalue($1) }
            | T_left_par expr T_right_par { $2 }
            | func_call { () }
            | T_plus expr { E_op_expr($1, $2) }
            | T_minus expr { E_op_expr($1, $2) }
            | expr T_plus expr { E_op_expr_exp($1, $3) }
            | expr T_minus expr { E_op_expr_exp($1, $3) }
            | expr T_mul expr { E_op_expr_exp($1, $3) }
            | expr T_div expr { E_op_expr_exp($1, $3) }
            | expr T_mod expr { E_op_expr_exp($1, $3) }

cond        : T_left_par cond T_right_par { $2 }
            | T_not cond { C_not_cond($1, $2) }
            | cond T_and cond { C_cond_cond($1, $3) }
            | cond T_or cond { C_cond_cond($1, $3) }
            | expr T_equal expr { C_expr_expr($1, $3) }
            | expr T_less expr { C_expr_expr($1, $3) }
            | expr T_less_eq expr { C_expr_expr($1, $3) }
            | expr T_greater expr { C_expr_expr($1, $3) }
            | expr T_greater_eq expr { C_expr_expr($1, $3) }
            | expr T_not_equal expr { C_expr_expr($1, $3) }