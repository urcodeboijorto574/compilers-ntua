
(* The type of tokens. *)

type token = 
  | T_while
  | T_var
  | T_then
  | T_string
  | T_semicolon
  | T_right_sqr
  | T_right_par
  | T_right_br
  | T_return
  | T_ref
  | T_plus
  | T_or
  | T_nothing
  | T_not_equal
  | T_not
  | T_mul
  | T_mod
  | T_minus
  | T_less_eq
  | T_less
  | T_left_sqr
  | T_left_par
  | T_left_br
  | T_integer
  | T_int
  | T_if
  | T_identifier
  | T_greater_eq
  | T_greater
  | T_fun
  | T_equal
  | T_eof
  | T_else
  | T_do
  | T_div
  | T_comma
  | T_colon
  | T_chr
  | T_char
  | T_assignment
  | T_and

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
