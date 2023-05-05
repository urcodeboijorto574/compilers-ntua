
module MenhirBasics = struct
  
  exception Error
  
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
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState137
  | MenhirState135
  | MenhirState132
  | MenhirState123
  | MenhirState120
  | MenhirState118
  | MenhirState116
  | MenhirState112
  | MenhirState110
  | MenhirState106
  | MenhirState103
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState86
  | MenhirState80
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState63
  | MenhirState59
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState47
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState37
  | MenhirState34
  | MenhirState30
  | MenhirState28
  | MenhirState24
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState14
  | MenhirState12
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 1 "Parser.mly"
  
	open Ast

# 120 "Parser.ml"

let rec _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 125 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 155 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_goto_cond : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 63 "Parser.mly"
      (cond)
# 185 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv511 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 195 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | T_or ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv507 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 209 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv505 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 216 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 221 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 228 "Parser.ml"
            ) = 
# 149 "Parser.mly"
                                          ( _2 )
# 232 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv509 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 242 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv510)) : 'freshtv512)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv517 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 251 "Parser.ml"
        ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 255 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv513 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 267 "Parser.ml"
            ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 271 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 63 "Parser.mly"
      (cond)
# 276 "Parser.ml"
            ))), _, (_3 : (
# 63 "Parser.mly"
      (cond)
# 280 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 286 "Parser.ml"
            ) = 
# 152 "Parser.mly"
                             ( C_cond_cond(_1, O_or, _3)  )
# 290 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv514)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 300 "Parser.ml"
            ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 304 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv516)) : 'freshtv518)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv521 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 313 "Parser.ml"
        ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 317 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv519 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 323 "Parser.ml"
        ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 327 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 63 "Parser.mly"
      (cond)
# 332 "Parser.ml"
        ))), _, (_3 : (
# 63 "Parser.mly"
      (cond)
# 336 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 63 "Parser.mly"
      (cond)
# 342 "Parser.ml"
        ) = 
# 151 "Parser.mly"
                              ( C_cond_cond(_1, O_and, _3) )
# 346 "Parser.ml"
         in
        _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv520)) : 'freshtv522)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv525 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 354 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv523 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 360 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 365 "Parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 63 "Parser.mly"
      (cond)
# 371 "Parser.ml"
        ) = 
# 150 "Parser.mly"
                         ( C_not_cond(O_not, _2) )
# 375 "Parser.ml"
         in
        _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv524)) : 'freshtv526)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv531 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 383 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | T_do ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv527 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 395 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_if ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_left_br ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_return ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_semicolon ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_string ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_while ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv528)
        | T_or ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv529 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 427 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv530)) : 'freshtv532)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv537 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 436 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | T_or ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv533 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 450 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_if ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_left_br ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_return ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_semicolon ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_string ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_while ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv534)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv535 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 480 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv536)) : 'freshtv538)
    | _ ->
        _menhir_fail ()

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 490 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 518 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 546 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 574 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 602 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 630 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run83 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 658 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv503 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 666 "Parser.ml"
    )) = Obj.magic _menhir_stack in
    ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 62 "Parser.mly"
      (expr)
# 671 "Parser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (
# 62 "Parser.mly"
      (expr)
# 678 "Parser.ml"
    ) = 
# 139 "Parser.mly"
                                          ( _2 )
# 682 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)

and _menhir_goto_expr_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 60 "Parser.mly"
      (expr)
# 689 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv497 * _menhir_state)) * _menhir_state * (
# 60 "Parser.mly"
      (expr)
# 699 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv493 * _menhir_state)) * _menhir_state * (
# 60 "Parser.mly"
      (expr)
# 709 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv491 * _menhir_state)) * _menhir_state * (
# 60 "Parser.mly"
      (expr)
# 716 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 60 "Parser.mly"
      (expr)
# 721 "Parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 59 "Parser.mly"
      (funcCall)
# 729 "Parser.ml"
            ) = 
# 125 "Parser.mly"
                                                            ( newFuncCall(_1, _3) )
# 733 "Parser.ml"
             in
            _menhir_goto_func_call _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)) : 'freshtv494)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv495 * _menhir_state)) * _menhir_state * (
# 60 "Parser.mly"
      (expr)
# 743 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)) : 'freshtv498)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv501 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 752 "Parser.ml"
        ))) * _menhir_state * (
# 60 "Parser.mly"
      (expr)
# 756 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv499 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 762 "Parser.ml"
        ))) * _menhir_state * (
# 60 "Parser.mly"
      (expr)
# 766 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 771 "Parser.ml"
        ))), _, (_3 : (
# 60 "Parser.mly"
      (expr)
# 775 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 60 "Parser.mly"
      (expr)
# 781 "Parser.ml"
        ) = 
# 128 "Parser.mly"
                                     ( _1 :: _3 )
# 785 "Parser.ml"
         in
        _menhir_goto_expr_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)) : 'freshtv502)
    | _ ->
        _menhir_fail ()

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 794 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 822 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 850 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 878 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 906 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 934 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 62 "Parser.mly"
      (expr)
# 962 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv381 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 972 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 976 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_right_sqr ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv377 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 996 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1000 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 1007 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1011 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 61 "Parser.mly"
      (lvalue)
# 1016 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1020 "Parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 61 "Parser.mly"
      (lvalue)
# 1027 "Parser.ml"
            ) = 
# 133 "Parser.mly"
                                                  ( L_comp(_1, _3) )
# 1031 "Parser.ml"
             in
            _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)) : 'freshtv378)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv379 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 1041 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1045 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv387 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1054 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1058 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv383 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1074 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1078 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1083 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1087 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 1093 "Parser.ml"
            ) = 
# 143 "Parser.mly"
                               ( E_op_expr_exp(_1, O_plus, _3) )
# 1097 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv385 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1107 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1111 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)) : 'freshtv388)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1120 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1124 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv389 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1130 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1134 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1139 "Parser.ml"
        ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1143 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 62 "Parser.mly"
      (expr)
# 1149 "Parser.ml"
        ) = 
# 145 "Parser.mly"
                              ( E_op_expr_exp(_1, O_mul, _3) )
# 1153 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)) : 'freshtv392)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1161 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1165 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv393 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1171 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1175 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1180 "Parser.ml"
        ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1184 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 62 "Parser.mly"
      (expr)
# 1190 "Parser.ml"
        ) = 
# 147 "Parser.mly"
                              ( E_op_expr_exp(_1, O_mod, _3) )
# 1194 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)) : 'freshtv396)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1202 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1206 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1212 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1216 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1221 "Parser.ml"
        ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1225 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 62 "Parser.mly"
      (expr)
# 1231 "Parser.ml"
        ) = 
# 146 "Parser.mly"
                              ( E_op_expr_exp(_1, O_div, _3) )
# 1235 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)) : 'freshtv400)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv405 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1243 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1247 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1263 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1267 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1272 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1276 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 1282 "Parser.ml"
            ) = 
# 144 "Parser.mly"
                                ( E_op_expr_exp(_1, O_minus, _3) )
# 1286 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv403 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1296 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1300 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)) : 'freshtv406)
    | MenhirState80 | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1309 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1319 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_chr ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_identifier ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_integer ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_left_par ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_minus ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_plus ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_string ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv408)
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv409 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1357 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1362 "Parser.ml"
            ))) = _menhir_stack in
            let _v : (
# 60 "Parser.mly"
      (expr)
# 1367 "Parser.ml"
            ) = 
# 127 "Parser.mly"
                   ( _1 )
# 1371 "Parser.ml"
             in
            _menhir_goto_expr_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv410)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv411 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1381 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)) : 'freshtv414)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv417 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1390 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_right_par ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv415 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1414 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1423 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv419 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1439 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 62 "Parser.mly"
      (expr)
# 1444 "Parser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 1450 "Parser.ml"
            ) = 
# 142 "Parser.mly"
                           ( E_op_expr(O_minus, _2) )
# 1454 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv421 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1464 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)) : 'freshtv424)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1473 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv425 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1489 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 62 "Parser.mly"
      (expr)
# 1494 "Parser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 1500 "Parser.ml"
            ) = 
# 141 "Parser.mly"
                          ( E_op_expr(O_plus, _2) )
# 1504 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv427 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1514 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)) : 'freshtv430)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv433 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1523 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_equal ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | T_greater ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | T_greater_eq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | T_less ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | T_less_eq ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_not_equal ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_right_par ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv431 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1559 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)) : 'freshtv434)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1568 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1572 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv435 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1592 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1596 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1601 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1605 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1611 "Parser.ml"
            ) = 
# 158 "Parser.mly"
                                    ( C_expr_expr(_1, O_not_equal, _3)  )
# 1615 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1625 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1629 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv438)) : 'freshtv440)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv445 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1638 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1642 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv441 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1662 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1666 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1671 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1675 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1681 "Parser.ml"
            ) = 
# 155 "Parser.mly"
                                  ( C_expr_expr(_1, O_less_eq, _3)  )
# 1685 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv443 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1695 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1699 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv444)) : 'freshtv446)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv451 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1708 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1712 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv447 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1732 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1736 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1741 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1745 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1751 "Parser.ml"
            ) = 
# 154 "Parser.mly"
                               ( C_expr_expr(_1, O_less, _3)  )
# 1755 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv449 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1765 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1769 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)) : 'freshtv452)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv457 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1778 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1782 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv453 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1802 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1806 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1811 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1815 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1821 "Parser.ml"
            ) = 
# 157 "Parser.mly"
                                     ( C_expr_expr(_1, O_greater_eq, _3)  )
# 1825 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv455 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1835 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1839 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv456)) : 'freshtv458)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv463 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1848 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1852 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv459 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1872 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1876 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1881 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1885 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1891 "Parser.ml"
            ) = 
# 156 "Parser.mly"
                                  ( C_expr_expr(_1, O_greater, _3)  )
# 1895 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1905 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1909 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)) : 'freshtv464)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv469 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1918 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1922 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv465 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1942 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1946 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1951 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1955 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1961 "Parser.ml"
            ) = 
# 153 "Parser.mly"
                                ( C_expr_expr(_1, O_equal, _3) )
# 1965 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv466)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv467 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1975 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1979 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)) : 'freshtv470)
    | MenhirState116 | MenhirState52 | MenhirState86 | MenhirState106 | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1988 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_equal ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | T_greater ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | T_greater_eq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | T_less ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | T_less_eq ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_not_equal ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv471 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2022 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv472)) : 'freshtv474)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv481 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2031 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv477 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2051 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2058 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 62 "Parser.mly"
      (expr)
# 2063 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 56 "Parser.mly"
      (stmt)
# 2070 "Parser.ml"
            ) = 
# 117 "Parser.mly"
                                        ( S_return(_2)  )
# 2074 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv476)) : 'freshtv478)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv479 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2084 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv480)) : 'freshtv482)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv489 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2093 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2097 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_minus ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | T_mod ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | T_mul ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | T_plus ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv485 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2117 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2121 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv483 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2128 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2132 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 61 "Parser.mly"
      (lvalue)
# 2137 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 2141 "Parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 56 "Parser.mly"
      (stmt)
# 2148 "Parser.ml"
            ) = 
# 110 "Parser.mly"
                                                    ( S_assignment(_1, _3) )
# 2152 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv487 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2162 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2166 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)) : 'freshtv490)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 58 "Parser.mly"
      (stmt list)
# 2176 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2186 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_br ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv365 * _menhir_state) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2196 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * _menhir_state) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2203 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 58 "Parser.mly"
      (stmt list)
# 2208 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 57 "Parser.mly"
      (stmt list)
# 2215 "Parser.ml"
            ) = 
# 119 "Parser.mly"
                                             ( _2 )
# 2219 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv361) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 57 "Parser.mly"
      (stmt list)
# 2227 "Parser.ml"
            )) = _v in
            ((match _menhir_s with
            | MenhirState51 | MenhirState132 | MenhirState110 | MenhirState118 | MenhirState120 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv335) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 57 "Parser.mly"
      (stmt list)
# 2237 "Parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : (
# 57 "Parser.mly"
      (stmt list)
# 2245 "Parser.ml"
                )) : (
# 57 "Parser.mly"
      (stmt list)
# 2249 "Parser.ml"
                )) = _v in
                ((let _v : (
# 56 "Parser.mly"
      (stmt)
# 2254 "Parser.ml"
                ) = 
# 111 "Parser.mly"
                    ( S_block(_1) )
# 2258 "Parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)) : 'freshtv336)
            | MenhirState50 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv359 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 2266 "Parser.ml"
                )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 2270 "Parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 57 "Parser.mly"
      (stmt list)
# 2276 "Parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv357 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 2282 "Parser.ml"
                )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 2286 "Parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : (
# 57 "Parser.mly"
      (stmt list)
# 2292 "Parser.ml"
                )) : (
# 57 "Parser.mly"
      (stmt list)
# 2296 "Parser.ml"
                )) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : (
# 44 "Parser.mly"
      (header)
# 2301 "Parser.ml"
                ))), _, (_2 : (
# 43 "Parser.mly"
      (localDef list)
# 2305 "Parser.ml"
                ))) = _menhir_stack in
                let _v : (
# 42 "Parser.mly"
      (funcDef)
# 2310 "Parser.ml"
                ) = 
# 69 "Parser.mly"
                                          ( newFuncDef(_1, _2, _3) )
# 2314 "Parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv355) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 42 "Parser.mly"
      (funcDef)
# 2322 "Parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                match _menhir_s with
                | MenhirState40 | MenhirState135 | MenhirState137 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv339 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2331 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv337 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2337 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (_1 : (
# 42 "Parser.mly"
      (funcDef)
# 2342 "Parser.ml"
                    ))) = _menhir_stack in
                    let _v : (
# 53 "Parser.mly"
      (localDef)
# 2347 "Parser.ml"
                    ) = 
# 101 "Parser.mly"
                       ( L_FuncDef(_1) )
# 2351 "Parser.ml"
                     in
                    _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)) : 'freshtv340)
                | MenhirState0 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv353 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2359 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | T_eof ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv349 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2369 "Parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv347 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2375 "Parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, (_1 : (
# 42 "Parser.mly"
      (funcDef)
# 2380 "Parser.ml"
                        ))) = _menhir_stack in
                        let _2 = () in
                        let _v : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2386 "Parser.ml"
                        ) = 
# 67 "Parser.mly"
                             ( _1 )
# 2390 "Parser.ml"
                         in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv345) = _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let (_v : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2398 "Parser.ml"
                        )) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let (_v : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2406 "Parser.ml"
                        )) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let ((_1 : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2414 "Parser.ml"
                        )) : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2418 "Parser.ml"
                        )) = _v in
                        (Obj.magic _1 : 'freshtv342)) : 'freshtv344)) : 'freshtv346)) : 'freshtv348)) : 'freshtv350)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv351 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2428 "Parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv352)) : 'freshtv354)
                | _ ->
                    _menhir_fail ()) : 'freshtv356)) : 'freshtv358)) : 'freshtv360)
            | _ ->
                _menhir_fail ()) : 'freshtv362)) : 'freshtv364)) : 'freshtv366)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv367 * _menhir_state) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2443 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv373 * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2452 "Parser.ml"
        )) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2456 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv371 * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2462 "Parser.ml"
        )) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2466 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 56 "Parser.mly"
      (stmt)
# 2471 "Parser.ml"
        ))), _, (_2 : (
# 58 "Parser.mly"
      (stmt list)
# 2475 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 58 "Parser.mly"
      (stmt list)
# 2480 "Parser.ml"
        ) = 
# 122 "Parser.mly"
                             ( _1 :: _2 )
# 2484 "Parser.ml"
         in
        _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)) : 'freshtv374)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 56 "Parser.mly"
      (stmt)
# 2493 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv321 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2503 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2507 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_else ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv315 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2517 "Parser.ml"
            ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2521 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_if ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_left_br ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_return ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_semicolon ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_string ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_while ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv316)
        | T_identifier | T_if | T_left_br | T_return | T_right_br | T_semicolon | T_string | T_while ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv317 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2549 "Parser.ml"
            ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2553 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 2558 "Parser.ml"
            ))), _, (_4 : (
# 56 "Parser.mly"
      (stmt)
# 2562 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 56 "Parser.mly"
      (stmt)
# 2569 "Parser.ml"
            ) = 
# 113 "Parser.mly"
                                    ( S_if(_2, _4) )
# 2573 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv319 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2583 "Parser.ml"
            ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2587 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv325 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2596 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2600 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2604 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv323 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2610 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2614 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2618 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 2623 "Parser.ml"
        ))), _, (_4 : (
# 56 "Parser.mly"
      (stmt)
# 2627 "Parser.ml"
        ))), _, (_6 : (
# 56 "Parser.mly"
      (stmt)
# 2631 "Parser.ml"
        ))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 56 "Parser.mly"
      (stmt)
# 2639 "Parser.ml"
        ) = 
# 114 "Parser.mly"
                                               ( S_if_else(_2, _4, _6) )
# 2643 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)) : 'freshtv326)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv329 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2651 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2655 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv327 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2661 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2665 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 2670 "Parser.ml"
        ))), _, (_4 : (
# 56 "Parser.mly"
      (stmt)
# 2674 "Parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (
# 56 "Parser.mly"
      (stmt)
# 2681 "Parser.ml"
        ) = 
# 115 "Parser.mly"
                                     ( S_while(_2, _4) )
# 2685 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)) : 'freshtv330)
    | MenhirState132 | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331 * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2693 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_identifier ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_if ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_left_br ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_return ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_semicolon ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_string ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_while ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_right_br ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv332)
    | _ ->
        _menhir_fail ()

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_goto_l_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 61 "Parser.mly"
      (lvalue)
# 2778 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState123 | MenhirState116 | MenhirState112 | MenhirState52 | MenhirState86 | MenhirState106 | MenhirState103 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState54 | MenhirState55 | MenhirState56 | MenhirState80 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState63 | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2788 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_comma | T_div | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_mod | T_mul | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv303 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2800 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 61 "Parser.mly"
      (lvalue)
# 2805 "Parser.ml"
            ))) = _menhir_stack in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 2810 "Parser.ml"
            ) = 
# 138 "Parser.mly"
                      ( E_lvalue(_1) )
# 2814 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv305 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2824 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)
    | MenhirState51 | MenhirState132 | MenhirState110 | MenhirState118 | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2833 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_assignment ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv309 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2843 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_chr ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_identifier ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_integer ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_left_par ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_minus ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_plus ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_string ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv310)
        | T_left_sqr ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv311 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2875 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)
    | _ ->
        _menhir_fail ()

and _menhir_goto_func_call : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 59 "Parser.mly"
      (funcCall)
# 2885 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState123 | MenhirState116 | MenhirState112 | MenhirState52 | MenhirState86 | MenhirState106 | MenhirState103 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState54 | MenhirState55 | MenhirState56 | MenhirState80 | MenhirState59 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2895 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2901 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 59 "Parser.mly"
      (funcCall)
# 2906 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 62 "Parser.mly"
      (expr)
# 2911 "Parser.ml"
        ) = 
# 140 "Parser.mly"
                        ( () )
# 2915 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)) : 'freshtv294)
    | MenhirState51 | MenhirState132 | MenhirState110 | MenhirState118 | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2923 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv297 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2933 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv295 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2940 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 59 "Parser.mly"
      (funcCall)
# 2945 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 56 "Parser.mly"
      (stmt)
# 2951 "Parser.ml"
            ) = 
# 112 "Parser.mly"
                                    ( S_func_call(_1) )
# 2955 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv299 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2965 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)
    | _ ->
        _menhir_fail ()

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 62 "Parser.mly"
      (expr)
# 3057 "Parser.ml"
    ) = 
# 136 "Parser.mly"
                        ( E_const(_1) )
# 3061 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 62 "Parser.mly"
      (expr)
# 3075 "Parser.ml"
    ) = 
# 137 "Parser.mly"
                    ( E_char(_1) )
# 3079 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 58 "Parser.mly"
      (stmt list)
# 3088 "Parser.ml"
    ) = 
# 121 "Parser.mly"
              ( [] )
# 3092 "Parser.ml"
     in
    _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv285) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 61 "Parser.mly"
      (lvalue)
# 3133 "Parser.ml"
    ) = 
# 132 "Parser.mly"
                       ( L_string(_1) )
# 3137 "Parser.ml"
     in
    _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 56 "Parser.mly"
      (stmt)
# 3151 "Parser.ml"
    ) = 
# 109 "Parser.mly"
                          ( () )
# 3155 "Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_semicolon ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState112 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (
# 56 "Parser.mly"
      (stmt)
# 3191 "Parser.ml"
        ) = 
# 116 "Parser.mly"
                                   ( () )
# 3195 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run116 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_integer ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_left_par ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv273 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_chr ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_identifier ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_integer ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_left_par ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_minus ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_plus ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv271 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState59 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv269 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 59 "Parser.mly"
      (funcCall)
# 3271 "Parser.ml"
            ) = 
# 124 "Parser.mly"
                                                  ( newFuncCall(_1, []) )
# 3275 "Parser.ml"
             in
            _menhir_goto_func_call _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | T_string ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv274)
    | T_and | T_assignment | T_comma | T_div | T_do | T_equal | T_greater | T_greater_eq | T_left_sqr | T_less | T_less_eq | T_minus | T_mod | T_mul | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (
# 61 "Parser.mly"
      (lvalue)
# 3292 "Parser.ml"
        ) = 
# 131 "Parser.mly"
                           ( L_id(_1) )
# 3296 "Parser.ml"
         in
        _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)

and _menhir_goto_fpar_def_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 46 "Parser.mly"
      (fparDef list)
# 3310 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv263 * _menhir_state))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3320 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv259 * _menhir_state))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3330 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_colon ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv255 * _menhir_state))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3340 "Parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | T_char ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | T_int ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | T_nothing ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv256)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv257 * _menhir_state))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3362 "Parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv261 * _menhir_state))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3373 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3382 "Parser.ml"
        ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3386 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3392 "Parser.ml"
        ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3396 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 45 "Parser.mly"
      (fparDef)
# 3401 "Parser.ml"
        ))), _, (_3 : (
# 46 "Parser.mly"
      (fparDef list)
# 3405 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 46 "Parser.mly"
      (fparDef list)
# 3411 "Parser.ml"
        ) = 
# 78 "Parser.mly"
                                                    ( _1 :: _3 )
# 3415 "Parser.ml"
         in
        _menhir_goto_fpar_def_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)) : 'freshtv268)
    | _ ->
        _menhir_fail ()

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_if ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_left_br ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_return ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_semicolon ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_string ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_while ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_right_br ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_fpar_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 45 "Parser.mly"
      (fparDef)
# 3451 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv253 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3459 "Parser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_semicolon ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3469 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_identifier ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_ref ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv248)
    | T_right_par ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3487 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 45 "Parser.mly"
      (fparDef)
# 3492 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 46 "Parser.mly"
      (fparDef list)
# 3497 "Parser.ml"
        ) = 
# 77 "Parser.mly"
                            ( [_1] )
# 3501 "Parser.ml"
         in
        _menhir_goto_fpar_def_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3511 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)

and _menhir_goto_local_def_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 43 "Parser.mly"
      (localDef list)
# 3519 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState137 | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv241 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 3529 "Parser.ml"
        )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 3533 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_br ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv242)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv245 * _menhir_state * (
# 53 "Parser.mly"
      (localDef)
# 3549 "Parser.ml"
        )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 3553 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv243 * _menhir_state * (
# 53 "Parser.mly"
      (localDef)
# 3559 "Parser.ml"
        )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 3563 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 53 "Parser.mly"
      (localDef)
# 3568 "Parser.ml"
        ))), _, (_2 : (
# 43 "Parser.mly"
      (localDef list)
# 3572 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 43 "Parser.mly"
      (localDef list)
# 3577 "Parser.ml"
        ) = 
# 72 "Parser.mly"
                                            ( _1 :: _2 )
# 3581 "Parser.ml"
         in
        _menhir_goto_local_def_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)) : 'freshtv246)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fpar_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "Parser.mly"
      (fparType)
# 3590 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv235 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3599 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 52 "Parser.mly"
      (fparType)
# 3605 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv233 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3611 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : (
# 52 "Parser.mly"
      (fparType)
# 3617 "Parser.ml"
        )) : (
# 52 "Parser.mly"
      (fparType)
# 3621 "Parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 47 "Parser.mly"
      (string list)
# 3626 "Parser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 45 "Parser.mly"
      (fparDef)
# 3634 "Parser.ml"
        ) = 
# 81 "Parser.mly"
                                                           ( newFparDef("ref", _2 :: _3, _5) )
# 3638 "Parser.ml"
         in
        _menhir_goto_fpar_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv239 * _menhir_state) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3646 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 52 "Parser.mly"
      (fparType)
# 3652 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3658 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : (
# 52 "Parser.mly"
      (fparType)
# 3664 "Parser.ml"
        )) : (
# 52 "Parser.mly"
      (fparType)
# 3668 "Parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 47 "Parser.mly"
      (string list)
# 3673 "Parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (
# 45 "Parser.mly"
      (fparDef)
# 3680 "Parser.ml"
        ) = 
# 82 "Parser.mly"
                                                     ( newFparDef("", _2, _4) )
# 3684 "Parser.ml"
         in
        _menhir_goto_fpar_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)) : 'freshtv240)
    | _ ->
        _menhir_fail ()

and _menhir_goto_local_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "Parser.mly"
      (localDef)
# 3693 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv231 * _menhir_state * (
# 53 "Parser.mly"
      (localDef)
# 3701 "Parser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_fun ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | T_var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | T_left_br ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135) : 'freshtv232)

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 43 "Parser.mly"
      (localDef list)
# 3722 "Parser.ml"
    ) = 
# 71 "Parser.mly"
                 ( [] )
# 3726 "Parser.ml"
     in
    _menhir_goto_local_def_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | T_colon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv228)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)

and _menhir_goto_array_dimension : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 50 "Parser.mly"
      (int list)
# 3761 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 50 "Parser.mly"
      (int list)
# 3772 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * _menhir_state))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : (
# 50 "Parser.mly"
      (int list)
# 3780 "Parser.ml"
        )) : (
# 50 "Parser.mly"
      (int list)
# 3784 "Parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 50 "Parser.mly"
      (int list)
# 3793 "Parser.ml"
        ) = 
# 93 "Parser.mly"
                                                                      ( _2 :: _4 )
# 3797 "Parser.ml"
         in
        _menhir_goto_array_dimension _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)) : 'freshtv198)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv201 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 3805 "Parser.ml"
        )) * _menhir_state)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 50 "Parser.mly"
      (int list)
# 3811 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 3817 "Parser.ml"
        )) * _menhir_state)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : (
# 50 "Parser.mly"
      (int list)
# 3823 "Parser.ml"
        )) : (
# 50 "Parser.mly"
      (int list)
# 3827 "Parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 48 "Parser.mly"
      (dataType)
# 3832 "Parser.ml"
        ))), _) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (
# 52 "Parser.mly"
      (fparType)
# 3839 "Parser.ml"
        ) = 
# 99 "Parser.mly"
                                                               ( newFparType(_1, _4) )
# 3843 "Parser.ml"
         in
        _menhir_goto_fpar_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)) : 'freshtv202)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 3851 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 50 "Parser.mly"
      (int list)
# 3857 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 3863 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : (
# 50 "Parser.mly"
      (int list)
# 3869 "Parser.ml"
        )) : (
# 50 "Parser.mly"
      (int list)
# 3873 "Parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 48 "Parser.mly"
      (dataType)
# 3878 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 52 "Parser.mly"
      (fparType)
# 3883 "Parser.ml"
        ) = 
# 98 "Parser.mly"
                                           ( newFparType(_1, _2) )
# 3887 "Parser.ml"
         in
        _menhir_goto_fpar_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 3895 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 50 "Parser.mly"
      (int list)
# 3901 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 3907 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : (
# 50 "Parser.mly"
      (int list)
# 3913 "Parser.ml"
        )) : (
# 50 "Parser.mly"
      (int list)
# 3917 "Parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 48 "Parser.mly"
      (dataType)
# 3922 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 49 "Parser.mly"
      (myType)
# 3927 "Parser.ml"
        ) = 
# 90 "Parser.mly"
                                        ( newMyType(_1, _2) )
# 3931 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 49 "Parser.mly"
      (myType)
# 3939 "Parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv219 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3946 "Parser.ml"
        ))) * _menhir_state * (
# 49 "Parser.mly"
      (myType)
# 3950 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv215 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3960 "Parser.ml"
            ))) * _menhir_state * (
# 49 "Parser.mly"
      (myType)
# 3964 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv213 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3971 "Parser.ml"
            ))) * _menhir_state * (
# 49 "Parser.mly"
      (myType)
# 3975 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : (
# 47 "Parser.mly"
      (string list)
# 3980 "Parser.ml"
            ))), _, (_5 : (
# 49 "Parser.mly"
      (myType)
# 3984 "Parser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 55 "Parser.mly"
      (varDef)
# 3993 "Parser.ml"
            ) = 
# 107 "Parser.mly"
                                                                    ( newVarDef(_2 :: _3, _5) )
# 3997 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv211) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 55 "Parser.mly"
      (varDef)
# 4005 "Parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 55 "Parser.mly"
      (varDef)
# 4013 "Parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 55 "Parser.mly"
      (varDef)
# 4021 "Parser.ml"
            )) : (
# 55 "Parser.mly"
      (varDef)
# 4025 "Parser.ml"
            )) = _v in
            ((let _v : (
# 53 "Parser.mly"
      (localDef)
# 4030 "Parser.ml"
            ) = 
# 103 "Parser.mly"
                      ( L_VarDef(_1) )
# 4034 "Parser.ml"
             in
            _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv217 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4044 "Parser.ml"
            ))) * _menhir_state * (
# 49 "Parser.mly"
      (myType)
# 4048 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)) : 'freshtv226)
    | _ ->
        _menhir_fail ()

and _menhir_goto_header : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 44 "Parser.mly"
      (header)
# 4058 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 4068 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_fun ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | T_var ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | T_left_br ->
            _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv182)
    | MenhirState40 | MenhirState137 | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 4088 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_fun ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv191 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 4100 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState137 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 4108 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 44 "Parser.mly"
      (header)
# 4114 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 54 "Parser.mly"
      (funcDecl)
# 4120 "Parser.ml"
            ) = 
# 105 "Parser.mly"
                                 ( FuncDecl_Header(_1) )
# 4124 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 54 "Parser.mly"
      (funcDecl)
# 4132 "Parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 54 "Parser.mly"
      (funcDecl)
# 4140 "Parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 54 "Parser.mly"
      (funcDecl)
# 4148 "Parser.ml"
            )) : (
# 54 "Parser.mly"
      (funcDecl)
# 4152 "Parser.ml"
            )) = _v in
            ((let _v : (
# 53 "Parser.mly"
      (localDef)
# 4157 "Parser.ml"
            ) = 
# 102 "Parser.mly"
                        ( L_FuncDecl(_1) )
# 4161 "Parser.ml"
             in
            _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)) : 'freshtv188)) : 'freshtv190)) : 'freshtv192)
        | T_var ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_left_br ->
            _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137) : 'freshtv194)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 50 "Parser.mly"
      (int list)
# 4185 "Parser.ml"
    ) = 
# 92 "Parser.mly"
                     ( [] )
# 4189 "Parser.ml"
     in
    _menhir_goto_array_dimension _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_integer ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_right_sqr ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | T_right_par | T_semicolon ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv176)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)

and _menhir_goto_id_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 47 "Parser.mly"
      (string list)
# 4239 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv155 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4249 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4255 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 47 "Parser.mly"
      (string list)
# 4260 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (
# 47 "Parser.mly"
      (string list)
# 4267 "Parser.ml"
        ) = 
# 85 "Parser.mly"
                                           ( _2 :: _3 )
# 4271 "Parser.ml"
         in
        _menhir_goto_id_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)) : 'freshtv156)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4279 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv157 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4289 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | T_int ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv159 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4309 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4318 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv163 * _menhir_state) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4328 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | T_int ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv165 * _menhir_state) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4348 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv173 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4357 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv169 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4367 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | T_int ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv170)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv171 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4387 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)) : 'freshtv174)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ret_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 51 "Parser.mly"
      (retType)
# 4397 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv147 * _menhir_state))) * _menhir_state)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 51 "Parser.mly"
      (retType)
# 4408 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv145 * _menhir_state))) * _menhir_state)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : (
# 51 "Parser.mly"
      (retType)
# 4416 "Parser.ml"
        )) : (
# 51 "Parser.mly"
      (retType)
# 4420 "Parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        let _5 = () in
        let _4 = () in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 44 "Parser.mly"
      (header)
# 4431 "Parser.ml"
        ) = 
# 74 "Parser.mly"
                                                                         ( newHeader(_2, [], _6) )
# 4435 "Parser.ml"
         in
        _menhir_goto_header _menhir_env _menhir_stack _menhir_s _v) : 'freshtv146)) : 'freshtv148)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv151 * _menhir_state))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 4443 "Parser.ml"
        )))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 51 "Parser.mly"
      (retType)
# 4449 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv149 * _menhir_state))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 4455 "Parser.ml"
        )))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_7 : (
# 51 "Parser.mly"
      (retType)
# 4461 "Parser.ml"
        )) : (
# 51 "Parser.mly"
      (retType)
# 4465 "Parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s), _, (_4 : (
# 46 "Parser.mly"
      (fparDef list)
# 4470 "Parser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 44 "Parser.mly"
      (header)
# 4480 "Parser.ml"
        ) = 
# 75 "Parser.mly"
                                                                                       ( newHeader(_2, _4, _7) )
# 4484 "Parser.ml"
         in
        _menhir_goto_header _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)
    | _ ->
        _menhir_fail ()

and _menhir_goto_data_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 48 "Parser.mly"
      (dataType)
# 4493 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4503 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4509 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 48 "Parser.mly"
      (dataType)
# 4514 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 51 "Parser.mly"
      (retType)
# 4519 "Parser.ml"
        ) = 
# 95 "Parser.mly"
                        ( RetDataType(_1) )
# 4523 "Parser.ml"
         in
        _menhir_goto_ret_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv132)) : 'freshtv134)
    | MenhirState30 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4531 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4541 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState19 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_integer ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
            | T_right_sqr ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv135 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4555 "Parser.ml"
                )) * _menhir_state) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | T_left_sqr ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                | T_right_par | T_semicolon ->
                    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv136)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv137 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4575 "Parser.ml"
                )) * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)) : 'freshtv140)
        | T_right_par | T_semicolon ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv142)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4590 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_semicolon ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv144)
    | _ ->
        _menhir_fail ()

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 47 "Parser.mly"
      (string list)
# 4611 "Parser.ml"
    ) = 
# 84 "Parser.mly"
            ( [] )
# 4615 "Parser.ml"
     in
    _menhir_goto_id_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | T_colon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv128)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 51 "Parser.mly"
      (retType)
# 4657 "Parser.ml"
    ) = 
# 96 "Parser.mly"
                        ( Nothing("nothing") )
# 4661 "Parser.ml"
     in
    _menhir_goto_ret_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 48 "Parser.mly"
      (dataType)
# 4675 "Parser.ml"
    ) = 
# 87 "Parser.mly"
                    ( Const(_1) )
# 4679 "Parser.ml"
     in
    _menhir_goto_data_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 48 "Parser.mly"
      (dataType)
# 4693 "Parser.ml"
    ) = 
# 88 "Parser.mly"
                     ( Char(_1) )
# 4697 "Parser.ml"
     in
    _menhir_goto_data_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | T_colon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv118)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_comma ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | T_colon ->
        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 4752 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * (
# 53 "Parser.mly"
      (localDef)
# 4761 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 4770 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 4779 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv25 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 4788 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 4792 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv27 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 4801 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv33 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 4820 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 4829 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 4838 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4847 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4856 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4865 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4874 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4883 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4892 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4911 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4920 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4929 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4938 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4947 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 4956 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 4965 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 5004 "Parser.ml"
        )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 5008 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 5017 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv85 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 5026 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 5040 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 5049 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv93 * _menhir_state))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 5058 "Parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 5067 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv99 * _menhir_state))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv101 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 5086 "Parser.ml"
        )) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 5095 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv105 * _menhir_state)) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 5104 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv111 * _menhir_state))) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv116)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv9 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | T_ref ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | T_right_par ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv7 * _menhir_state))) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState3 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | T_colon ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv3 * _menhir_state))) * _menhir_state) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | T_char ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                    | T_int ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                    | T_nothing ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv4)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv5 * _menhir_state))) * _menhir_state) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)) : 'freshtv8)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv10)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv11 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)) : 'freshtv14)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 41 "Parser.mly"
      (Ast.funcDef)
# 5220 "Parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_fun ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 5249 "Parser.ml"
