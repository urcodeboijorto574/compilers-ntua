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

/* %left T_or
%left T_and
%left T_plus T_minus
%left T_mul T_mod T_div */

%left T_or
%left T_and
%left T_plus T_minus
%left T_mul T_mod T_div
%nonassoc T_not
/* %nonassoc T_then T_else */

/* %nonassoc T_equal T_not_equal T_less_eq T_greater_eq T_less T_greater */
/* %nonassoc T_then T_else */


%start program
%type <unit> program
/* %type <unit> func_def */

%%


program     : func_def T_eof { (Printf.printf("program\n")) }

func_def    : header local_def* block { (Printf.printf("func_def\n")) }

header      : T_fun T_identifier T_left_par header_r? T_right_par T_colon ret_type { (Printf.printf("header\n")) }

header_r    : fpar_def header_rr*  { () }

header_rr   /* : (* nothing *) { () } */
            : T_semicolon fpar_def { () }

fpar_def    : T_ref? T_identifier fpar_def_r* T_colon fpar_type { (Printf.printf("fpar_def\n")) }

fpar_def_r  : (* nothing *) { () }      /* ----------------------------------------- */
            | T_comma T_identifier fpar_def_r { () }

data_type   : T_int { (Printf.printf("data_type\n")) }
            | T_char { (Printf.printf("data_type\n")) }

mytype      : data_type mytype_r* { (Printf.printf("mytype\n")) }

mytype_r    : (* nothing *) { () }      /* ----------------------------------------- */
            | T_left_sqr T_integer T_right_sqr mytype_r { () }

ret_type    : data_type { (Printf.printf("ret_rype\n")) }
            | T_nothing { (Printf.printf("ret_type\n")) }

/* fpar_type   : data_type squares? fpar_type_r* { (Printf.printf("fpar_type\n")) } */
fpar_type   : data_type rest    {()}

rest        : rest_r*   {()}

rest_r      : T_left_sqr T_integer? T_right_sqr {()}

/* squares     : T_left_sqr T_right_sqr    { () } */

// fpar_type_r /* : (* nothing *) { () } */
//             : T_left_sqr T_integer T_right_sqr { () }

local_def   : func_def { (Printf.printf("local_def\n")) }
            | func_decl { (Printf.printf("local_def\n")) }
            | var_def { (Printf.printf("local_def\n")) }

func_decl   : header T_semicolon { (Printf.printf("func_decl\n")) }

var_def     : T_var T_identifier var_def_r* T_colon mytype T_semicolon { (Printf.printf("var_def\n")) }

var_def_r   /* : (* nothing *) { () }  */     /* ----------------------------------------- */
            : T_comma T_identifier { () }

stmt        : T_semicolon { () }
            | l_value T_assignment expr T_semicolon { (Printf.printf("stmt\n")) }
            | block { (Printf.printf("stmt\n")) }
            | func_call T_semicolon { (Printf.printf("stmt\n")) }
            | T_if cond T_then stmt else_stmt? { (Printf.printf("stmt\n")) }
            // | T_if cond T_then stmt T_else stmt { () }
            | T_while cond T_do stmt { (Printf.printf("stmt\n")) }
            | T_return expr? T_semicolon { (Printf.printf("stmt\n")) }

else_stmt   :   T_else stmt { () }

block       : T_left_br stmt* T_right_br { (Printf.printf("block\n")) }

func_call   : T_identifier T_left_par func_call_rr? T_right_par { (Printf.printf("func_call\n")) }

func_call_rr : expr func_call_r*   { () }

func_call_r /* : (* nothing *) { () }  */ /* ------------------- */
            : T_comma expr { () }


l_value     : T_identifier { (Printf.printf("l_value\n")) }
            | T_string { (Printf.printf("l_value\n")) }
            | l_value T_left_sqr expr T_right_sqr { (Printf.printf("l_value\n")) }


expr        : T_integer { (Printf.printf("expr\n")) }
            | T_chr { (Printf.printf("expr\n")) }
            | l_value { (Printf.printf("expr\n")) }
            | T_left_par expr T_right_par { (Printf.printf("expr\n")) }
            | func_call { (Printf.printf("expr\n")) }
            | T_plus expr { (Printf.printf("expr\n")) }
            | T_minus expr { (Printf.printf("expr\n")) }
            | expr T_plus expr { (Printf.printf("expr\n")) }
            | expr T_minus expr { (Printf.printf("expr\n")) }
            | expr T_mul expr { (Printf.printf("expr\n")) }
            | expr T_div expr { (Printf.printf("expr\n")) }
            | expr T_mod expr { (Printf.printf("expr\n")) }

cond        : T_left_par cond T_right_par { (Printf.printf("cond\n")) }
            | T_not cond { (Printf.printf("cond\n")) }
            | cond T_and cond { (Printf.printf("cond\n")) }
            | cond T_or cond { (Printf.printf("cond\n")) }
            | expr T_equal expr { (Printf.printf("cond\n")) }
            | expr T_less expr { (Printf.printf("cond\n")) }
            | expr T_less_eq expr { (Printf.printf("cond\n")) }
            | expr T_greater expr { (Printf.printf("cond\n")) }
            | expr T_greater_eq expr { (Printf.printf("cond\n")) }
            | expr T_not_equal expr { (Printf.printf("cond\n")) }
