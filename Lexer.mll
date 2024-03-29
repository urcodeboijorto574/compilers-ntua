{
  open Parser
  open Printf

  let int_of_hex digit1 digit2 = int_of_string (sprintf "0x%c%c" digit1 digit2)

  let incrementNumLines (lexbuf : Lexing.lexbuf) =
    let lcp : Lexing.position = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos_fname = lcp.pos_fname;
        pos_lnum = lcp.pos_lnum + 1;
        pos_bol = lcp.pos_bol;
        pos_cnum = lcp.pos_cnum
      }
  let get_location_msg (lexbuf : Lexing.lexbuf) =
    sprintf "File \"%s\", line %d:\n" lexbuf.lex_curr_p.pos_fname lexbuf.lex_curr_p.pos_lnum

  let char_list_in_string = ref []
  let add_in_list c = char_list_in_string := c :: !char_list_in_string
  let string_of_rev_char_list revCharList =
    revCharList |> List.rev_map (String.make 1) |> String.concat ""

  let multiline_string_msg = "Strings must close in the same line they start."
  let handle_error lexbuf infoMsg =
    Error.(handle_error lexing_error_msg (get_location_msg lexbuf ^ infoMsg))
  let handle_error_fatal lexbuf infoMsg =
    Error.(handle_error_fatal lexing_error_msg (get_location_msg lexbuf ^ infoMsg))
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
  | '"' char_string* eof { handle_error_fatal lexbuf multiline_string_msg }

  | eof { T_eof }
  | _ as chr {
      handle_error lexbuf (sprintf "Unknown character '%c'.\n" chr);
      lexer lexbuf
    }

and strings = parse
  | "\n" {
      handle_error lexbuf multiline_string_msg;
      add_in_list '\n';
      incrementNumLines lexbuf;
      strings lexbuf
    }
  | "'" {
      handle_error lexbuf
        "Single quotes are not permitted in strings (maybe you forgot a \'\\\'?).";
      add_in_list '\'';
      strings lexbuf
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
  | '"' { T_string (string_of_rev_char_list !char_list_in_string) }

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
      handle_error lexbuf "Single quotes contain no character on the insides.";
      T_chr '\000'
    }
  | char_common* '\'' {
      handle_error_fatal lexbuf
        "Multiple characters found inside single quotes(\'\')."
    }
  | _ { handle_error_fatal lexbuf "Single quotes opened and didn't close." }

and multi_comments = parse
  | '\n' { incrementNumLines lexbuf; multi_comments lexbuf }
  | "$$" { lexer lexbuf }
  | eof  { handle_error lexbuf "Unclosed multi-line comment."; T_eof }
  | _    { multi_comments lexbuf }

and comment = parse
  | '\n' { incrementNumLines lexbuf; lexer lexbuf }
  | eof  { T_eof }
  | _    { comment lexbuf }

{}
