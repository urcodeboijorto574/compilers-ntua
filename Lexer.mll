{
  open Parser

  let int_of_hex digit1 digit2 =
    let int_of_hex digit =
      if digit >= '0' && digit <= '9' then
        (Char.code digit) - (Char.code '0')
      else if digit >= 'a' && digit <= 'f' then
        (Char.code digit) - (Char.code 'a') + 10
      else
        (Char.code digit) - (Char.code 'A') + 10
    in
    (int_of_hex digit1) * 16 + (int_of_hex digit2)

  let num_lines = ref 1
  let incrementNumLines lexbuf =
    incr num_lines;
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos_fname = pos.pos_fname;
        pos_lnum = pos.pos_lnum + 1;
        pos_bol = 0;
        pos_cnum = 0;
      }

  let char_list_in_string = ref []
  let add_in_list c = char_list_in_string := c :: !char_list_in_string
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

  | identifier  { T_identifier (Lexing.lexeme lexbuf) }
  | integer     { T_integer (int_of_string (Lexing.lexeme lexbuf)) }

  | '\n' { incrementNumLines lexbuf; lexer lexbuf }
  | white+ { lexer lexbuf }
  | '\'' { characters lexbuf }
  | '"' { char_list_in_string := []; strings lexbuf }
  | '"' char_string* eof {
      Error.handle_error_fatal Error.lexing_error_msg
        (Printf.sprintf "Strings must close in the same line they start. Line %d." !num_lines)
    }

  | eof { T_eof }
  | _ as chr {
      Error.handle_error Error.lexing_error_msg
        (Printf.sprintf "Unknown character '%c'. Line %d.\n" chr !num_lines);
      lexer lexbuf
    }

and strings = parse
  | "\n" {
      Error.handle_error Error.lexing_error_msg
        (Printf.sprintf "String must close in the same line it starts. Line %d." !num_lines);
      add_in_list '\n';
      incrementNumLines lexbuf;
      strings lexbuf
    }
  | "'" {
      Error.handle_error Error.lexing_error_msg
        (Printf.sprintf "Single quotes are not permitted in strings (maybe you forgot a \'\\\'?). Line %d." !num_lines);
      add_in_list '\''; strings lexbuf
    }
  | "\\x" (digit_hex as d1) (digit_hex as d2) {
      add_in_list (Char.chr (int_of_hex d1 d2));
      strings lexbuf
    }
  | char_escape as c {
      add_in_list (
        match c with
        | "\\n" -> '\n'
        | "\\t" -> '\t'
        | "\\r" -> '\r'
        | "\\0" -> '\000'
        | "\\\\" -> '\\'
        | "\\\'" -> '\''
        | "\\\"" -> '\"'
        | _ -> assert false
        (* hex case is caught in previous rule *));
      strings lexbuf
    }
  | char_not_escape as c { add_in_list (String.get c 1); strings lexbuf }
  | char_common as c { add_in_list c; strings lexbuf }
  | '"' { T_string (List.rev !char_list_in_string) }

and characters = parse
  | (char_common as c) '\'' { T_chr c }
  | "\\x" (digit_hex as d1) (digit_hex as d2) '\'' { T_chr (Char.chr (int_of_hex d1 d2)) }
  | (char_escape as c) '\'' {
      T_chr (
        match c with
        | "\\n" -> '\n'
        | "\\t" -> '\t'
        | "\\r" -> '\r'
        | "\\0" -> '\000'
        | "\\\\" -> '\\'
        | "\\\'" -> '\''
        | "\\\"" -> '\"'
        | _ -> assert false
        (* hex case is caught in previous rule *))
    }
  | (char_not_escape as c) '\'' { T_chr (String.get c 1) }
  | '\'' {
      Error.handle_error Error.lexing_error_msg
        (Printf.sprintf "Single quotes contain no character on the insides. Line %d." !num_lines);
      T_chr '\000'
    }
  | char_common* '\'' {
      Error.handle_error_fatal Error.lexing_error_msg
        (Printf.sprintf "Multiple characters found inside single quotes(\'\'). Line %d." !num_lines)
    }
  | _ {
      Error.handle_error_fatal Error.lexing_error_msg
        (Printf.sprintf "Single quotes opened and didn't close. Line %d." !num_lines)
    }

and multi_comments = parse
  | '\n' { incrementNumLines lexbuf; multi_comments lexbuf }
  | "$$" { lexer lexbuf }
  | eof  {
      Error.handle_error Error.lexing_error_msg
        (Printf.sprintf "Unclosed multi-line comment. Line %d." !num_lines);
      T_eof
    }
  | _    { multi_comments lexbuf }

and comment = parse
  | '\n' { incrementNumLines lexbuf; lexer lexbuf }
  | eof  { T_eof }
  | _    { comment lexbuf }

{}
