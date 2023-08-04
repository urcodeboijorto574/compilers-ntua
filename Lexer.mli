(** [lexer] reads from the lexbuf and returns a token.
    All tokens are defined in the parser. *)
val lexer : Lexing.lexbuf -> Parser.token
