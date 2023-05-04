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
%type <local_def list> program
%type <local_def list> func_def

%%


program     : func_def T_eof { $1 }

func_def    : header local_def_list block { $2 }

local_def_list : { () }
               | local_def local_def_list   { $1 :: $2 }

header      : T_fun T_identifier T_left_par T_right_par T_colon ret_type { () }
            | T_fun T_identifier T_left_par fpar_def_list T_right_par T_colon ret_type { () }

fpar_def_list : fpar_def    { () }
              | fpar_def T_semicolon fpar_def_list  { () }


fpar_def    : T_ref T_identifier id_list T_colon fpar_type { () }
            | T_identifier id_list T_colon fpar_type { () }

id_list  :  { () } 
            | T_comma T_identifier id_list { () }

data_type   : T_int { () }
            | T_char { () }

mytype      : data_type array_dimension { () }

array_dimension    : { () }
                   | T_left_sqr T_integer T_right_sqr array_dimension { () }

ret_type    : data_type { () }
            | T_nothing { () }

fpar_type   : data_type array_dimension    { () }
            | data_type T_left_sqr T_right_sqr array_dimension { () }

local_def   : func_def { () }
            | func_decl { () }
            | var_def { () }

func_decl   : header T_semicolon { () }

var_def     : T_var T_identifier id_list T_colon mytype T_semicolon { () }

stmt        : T_semicolon { () }
            | l_value T_assignment expr T_semicolon { () }
            | block { () }
            | func_call T_semicolon { () }
            | T_if cond T_then stmt { () }
            | T_if cond T_then stmt T_else stmt{ () }
            | T_while cond T_do stmt { () }
            | T_return T_semicolon { () }
            | T_return expr T_semicolon { () }

block       : T_left_br stmt_list T_right_br { () }

stmt_list   : { () }
            | stmt stmt_list { () }

func_call   : T_identifier T_left_par T_right_par { () }
            | T_identifier T_left_par expr_list T_right_par { () }

expr_list   : expr { () }
            | expr T_comma expr_list { () }


l_value     : T_identifier { () }
            | T_string { () }
            | l_value T_left_sqr expr T_right_sqr { () }


expr        : T_integer { () }
            | T_chr { () }
            | l_value { () }
            | T_left_par expr T_right_par { () }
            | func_call { () }
            | T_plus expr { () }
            | T_minus expr { () }
            | expr T_plus expr { () }
            | expr T_minus expr { () }
            | expr T_mul expr { () }
            | expr T_div expr { () }
            | expr T_mod expr { () }

cond        : T_left_par cond T_right_par { () }
            | T_not cond { () }
            | cond T_and cond { () }
            | cond T_or cond { () }
            | expr T_equal expr { () }
            | expr T_less expr { () }
            | expr T_less_eq expr { () }
            | expr T_greater expr { () }
            | expr T_greater_eq expr { () }
            | expr T_not_equal expr { () }