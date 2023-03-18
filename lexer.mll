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
let hex = ['a'-'f' 'A'-'F']
let white  = [' ' '\t' '\r']
let common = [^ '\\' '\'' '\"']
let escape =  ['\'' '\\' '\"' ] | ('\\' 'x' hex hex) | '\\' '0' | '\t' | '\r' | '\n'
let char = common | escape       (* const characters *)

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

  | "$$"              { multi_comments lexbuf }     (* multi-line comments *)

  (* TODO *)
  | '$' [^ '\n' '$']* { lexer lexbuf }              (* ignore one-line comments *)
  
    
  | letter (letter | digit | '_')*  { T_identifier }
  | digit+                  { T_integer }
  | '\'' char '\''          { T_chr }
  | '\n'                    { incr num_lines; lexer lexbuf }
  | white+                  { lexer lexbuf }
  | '"'                     { strings lexbuf }
  
  | eof       { T_eof } 
  | _ as chr  { Printf.eprintf "Unknown character '%c' at line %d.\n" chr !num_lines; lexer lexbuf }

  and multi_comments = parse
    | '\n' { incr num_lines; multi_comments lexbuf }
    | "$$" { lexer lexbuf }
    | eof  { Printf.eprintf "Error! Unclosed comment at line: %d.\n" !num_lines; T_eof }
    | _    { multi_comments lexbuf }

  and strings = parse
    | '"'                         { T_string }
    | '\n'                        { Printf.eprintf "String must close in the same line it starts.Line %d.\n" !num_lines; lexer lexbuf}
    | char*                       { strings lexbuf  }
    | _ as chr                    { Printf.eprintf "Illegal character '%c' at string, line : %d.\n" chr !num_lines; lexer lexbuf }
    
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
  


  