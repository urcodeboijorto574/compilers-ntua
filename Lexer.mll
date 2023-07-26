{
  open Parser
  let num_lines = ref 1

  let multi_line_string_error_msg () =
    Printf.eprintf "String must close in the same line it starts. Line %d.\n" !num_lines;
    incr num_lines
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
(** [digit_hex] are the letters/digits that can be written in hex escape character. *)
let digit_hex = ['a'-'f' 'A'-'F' '0'-'9']
let char_hex = "\\x" digit_hex digit_hex
let white  = [' ' '\t' '\r']
let char_common = [^ '\\' '\'' '"']
let char_escape = '\\' ['n' 't' 'r' '0' '\\' '\'' '"'] | char_hex
(** [char_not_escape] are the characters that, if written next to a front-slash,
    the front-slash is considered redundant. For example '\a', '\1' and '\@'. *)
let char_not_escape = '\\' [^ 'n' 't' 'r' '0' '\\' '\'' '"' 'x']
let char_const = char_common | char_escape
(** [char_string] are the characters that can exist inside a string. *)
let char_string = char_common # ['"' '\n' '\\'] | char_escape
let identifier = letter (letter | digit | '_')*
let integer = digit+
let character = '\'' char_const '\''
let string = '"' (char_string | char_not_escape)* '"'

rule lexer = parse
| "and"       { T_and }
| "char"      { T_char }
| "div"       { T_div }
| "do"        { T_do }
| "else"      { T_else }
| "fun"       { T_fun }
| "if"        { T_if }
| "int"       { T_int }
| "mod"       { T_mod }
| "not"       { T_not }
| "nothing"   { T_nothing }
| "or"        { T_or }
| "ref"       { T_ref }
| "return"    { T_return }
| "then"      { T_then }
| "var"       { T_var }
| "while"     { T_while }

| '+'   { T_plus }
| '-'   { T_minus }
| '*'   { T_mul }
| '='   { T_equal }
| '#'   { T_not_equal }
| '<'   { T_less }
| '>'   { T_greater }
| "<="  { T_less_eq }
| ">="  { T_greater_eq }
| '('   { T_left_par }
| ')'   { T_right_par }
| '['   { T_left_sqr }
| ']'   { T_right_sqr }
| '{'   { T_left_br }
| '}'   { T_right_br }
| ','   { T_comma }
| ';'   { T_semicolon }
| ':'   { T_colon}
| "<-"  { T_assignment }

| "$$"  { multi_comments lexbuf }
| '$'   { comment lexbuf }

| identifier  { T_identifier { id_name = (Lexing.lexeme lexbuf); id_type = Types.T_int (* TODO: this is a placeholder *) } }
| integer     { T_integer (int_of_string (Lexing.lexeme lexbuf)) }

| '\n'                                  { incr num_lines; lexer lexbuf }
| white+                                { lexer lexbuf }
| character                             { T_chr ((Lexing.lexeme lexbuf).[0]) }
| string                                { T_string (Lexing.lexeme lexbuf) }
| '"' char_string* (('\n' | eof) as c)  { multi_line_string_error_msg (); if c = "\n" then strings lexbuf else T_eof }

| eof       { T_eof }
| _ as chr  { Printf.eprintf "Unknown character '%c' at line %d.\n" chr !num_lines; lexer lexbuf }

and multi_comments = parse
| '\n' { incr num_lines; multi_comments lexbuf }
| "$$" { lexer lexbuf }
| eof  { Printf.eprintf "Error! Unclosed comment at line: %d.\n" !num_lines; T_eof }
| _    { multi_comments lexbuf }

and comment = parse
| '\n' { incr num_lines; lexer lexbuf }
| eof  { T_eof }
| _    { comment lexbuf }

and strings = parse
| char_string* '\"' { lexer lexbuf }
| char_string* '\n' { incr num_lines; strings lexbuf }
| char_string* eof  { incr num_lines; T_eof}

{
  let string_of_token = function
  | T_eof         ->  "T_eof"
  | T_and         ->  "T_and"
  | T_char        ->  "T_char"
  | T_div         ->  "T_div"
  | T_do          ->  "T_do"
  | T_else        ->  "T_else"
  | T_fun         ->  "T_fun"
  | T_if          ->  "T_if"
  | T_int         ->  "T_int"
  | T_mod         ->  "T_mod"
  | T_not         ->  "T_not"
  | T_nothing     ->  "T_nothing"
  | T_or          ->  "T_or"
  | T_ref         ->  "T_ref"
  | T_return      ->  "T_return"
  | T_then        ->  "T_then"
  | T_var         ->  "T_var"
  | T_while       ->  "T_while"
  | T_plus        ->  "T_plus"
  | T_minus       ->  "T_minus"
  | T_mul         ->  "T_mul"
  | T_equal       ->  "T_equal"
  | T_not_equal   ->  "T_not_equal"
  | T_less        ->  "T_less"
  | T_greater     ->  "T_greater"
  | T_less_eq     ->  "T_less_eq"
  | T_greater_eq  ->  "T_greater_eq"
  | T_left_par    ->  "T_left_par"
  | T_right_par   ->  "T_right_par"
  | T_left_sqr    ->  "T_left_sqr"
  | T_right_sqr   ->  "T_right_sqr"
  | T_left_br     ->  "T_left_br"
  | T_right_br    ->  "T_right_br"
  | T_comma       ->  "T_comma"
  | T_semicolon   ->  "T_semicolon"
  | T_colon       ->  "T_colon"
  | T_assignment  ->  "T_assignment"
  | T_string _    ->  "T_string"
  | T_integer _   ->  "T_integer"
  | T_identifier _  -> "T_identifier"
  | T_chr _       -> "T_chr"
}
