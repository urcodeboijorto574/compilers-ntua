{
    type token = 
      | T_eof | T_and | T_char | T_div | T_do | T_else | T_fun | T_if
      | T_int | T_mod | T_not | T_nothing | T_or | T_ref | T_return
      | T_then | T_var | T_while | T_plus | T_minus | T_mul | T_equal
      | T_times | T_less | T_greater | T_less_eq | T_greater_eq
      | T_left_par | T_right_par | T_left_sqr | T_right_sqr | T_left_br | T_right_br
      | T_comma | T_semicolon | T_colon | T_assignment
      | T_identifier | T_integer | T_chr | T_string

    let num_lines = ref 1
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let hex = '\\' 'x' ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9']
let white  = [' ' '\t' '\r']
let char_common = [^ '\\' '\'' '\"']
let char_escape = '\\' ['n' 't' 'r' '0' '\\' '\'' '\"'] | hex
let char_const = char_common | char_escape
let char_string = [^ '\"' '\n'] | '\\' ['n' 't' 'r' '0' '\'' '\"'] | hex

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
  | '#'   { T_times }
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

  | letter (letter | digit | '_')*        { T_identifier }
  | digit+                                { T_integer }
  | '\'' char_const '\''                  { T_chr }
  | '\n'                                  { incr num_lines; lexer lexbuf }
  | white+                                { lexer lexbuf }
  | '\"' char_string* '\"'                { T_string }
  | '"' char_string* (('\n' | eof) as c)  {
                                            Printf.eprintf "String must close in the same line it starts. Line %d. \n" !num_lines;
                                            incr num_lines;
                                            if c = "\n" then strings lexbuf
                                            else             T_eof
                                          }

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
    | char_string* '\"'                   { lexer lexbuf }
    | char_string* (('\n' | eof) as c)    {
                                            Printf.eprintf "String must close in the same line it starts. Line %d. \n" !num_lines;
                                            incr num_lines;
                                            if c = "\n" then strings lexbuf
                                            else             T_eof
                                          }

{
  let string_of_token token = 
    match token with
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
      | T_times       ->  "T_times"
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
      | T_chr         ->  "T_chr"
      | T_identifier  ->  "T_identifier"
      | T_integer     ->  "T_integer"
      | T_string      ->  "T_string"

  let main =
    let lexbuf = Lexing.from_channel stdin
    in
      let rec loop () =
        let token = lexer lexbuf
        in
          Printf.printf "token=%s, lexeme=%s \n" (string_of_token token) (Lexing.lexeme lexbuf);
          if token <> T_eof then loop () in
          loop ()
}
