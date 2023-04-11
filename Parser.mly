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

%nonassoc T_equal T_not_equal T_less_eq T_greater_eq T_less T_greater
%nonassoc T_then T_else


%start program
%type <unit> program

%%


program     : func_def T_eof { () }

func_def    : header local_def* block { () }

header      : T_fun T_identifier T_left_par header_r? T_right_par T_colon ret_type { () }

header_r    : fpar_def header_rr*  { () }

header_rr   : T_semicolon fpar_def { () }

fpar_def    : T_ref? T_identifier fpar_def_r T_colon fpar_type { () }

fpar_def_r  : (* nothing *) { () }      /* ---------------------------- */
            | T_comma T_identifier fpar_def_r { () }

data_type   : T_int { () }
            | T_char { () }

mytype      : data_type mytype_r { () }

mytype_r    : (* nothing *) { () }     /*  ---------------------------------------- */
            | T_left_sqr T_integer T_right_sqr mytype_r { () }

ret_type    : data_type { () }
            | T_nothing { () }

fpar_type   : data_type squares? fpar_type_r { () }

squares     : T_left_sqr T_right_sqr    { () }

fpar_type_r : (* nothing *) { () }
            | T_left_sqr T_integer T_right_sqr fpar_type_r { () }

local_def   : func_def { () }
            | func_decl { () }
            | var_def { () }

func_decl   : header T_semicolon { () }

var_def     : T_var T_identifier var_def_r T_colon mytype T_semicolon { () }

var_def_r   : (* nothing *) { () }      /* ----------------------------------------- */
            | T_comma T_identifier var_def_r { () }

stmt        : T_semicolon { () }
            | l_value T_assignment expr T_semicolon { () }
            | block { () }
            | func_call T_semicolon { () }
            | T_if cond T_then stmt T_else stmt { () }
            | T_if cond T_then stmt { () }
            | T_while cond T_do stmt { () }
            | T_return expr? T_semicolon { () }

block       : T_left_br stmt* T_right_br { () }

func_call   : T_identifier T_left_par func_call_rr? T_right_par { () }

func_call_rr :  expr func_call_r   { () }

func_call_r : (* nothing *) { () }  /* ------------------- */
            | T_comma expr func_call_r { () }


l_value     : T_identifier { () }
            | T_string { () }
            | l_value T_left_sqr expr T_right_sqr { () }

expr        : T_integer { () }
            | T_char { () }
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