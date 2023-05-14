
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | T_while
    | T_var
    | T_then
    | T_string of (
# 28 "Parser.mly"
       (string)
# 14 "Parser.ml"
  )
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
    | T_integer of (
# 26 "Parser.mly"
       (int)
# 38 "Parser.ml"
  )
    | T_int
    | T_if
    | T_identifier of (
# 25 "Parser.mly"
       (string)
# 45 "Parser.ml"
  )
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
    | T_chr of (
# 27 "Parser.mly"
       (char)
# 60 "Parser.ml"
  )
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

# 136 "Parser.ml"

let rec _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 141 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 171 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_goto_cond : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 63 "Parser.mly"
      (cond)
# 201 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv511 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 211 "Parser.ml"
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
# 225 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv505 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 232 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 237 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 244 "Parser.ml"
            ) = 
# 149 "Parser.mly"
                                          ( C_cond_parenthesized(_2) )
# 248 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv509 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 258 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv510)) : 'freshtv512)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv517 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 267 "Parser.ml"
        ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 271 "Parser.ml"
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
# 283 "Parser.ml"
            ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 287 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 63 "Parser.mly"
      (cond)
# 292 "Parser.ml"
            ))), _, (_3 : (
# 63 "Parser.mly"
      (cond)
# 296 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 302 "Parser.ml"
            ) = 
# 152 "Parser.mly"
                             ( C_cond_cond(_1, O_or, _3)  )
# 306 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv514)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 316 "Parser.ml"
            ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 320 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv516)) : 'freshtv518)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv521 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 329 "Parser.ml"
        ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 333 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv519 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 339 "Parser.ml"
        ))) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 343 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 63 "Parser.mly"
      (cond)
# 348 "Parser.ml"
        ))), _, (_3 : (
# 63 "Parser.mly"
      (cond)
# 352 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 63 "Parser.mly"
      (cond)
# 358 "Parser.ml"
        ) = 
# 151 "Parser.mly"
                              ( C_cond_cond(_1, O_and, _3) )
# 362 "Parser.ml"
         in
        _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv520)) : 'freshtv522)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv525 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 370 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv523 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 376 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 381 "Parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 63 "Parser.mly"
      (cond)
# 387 "Parser.ml"
        ) = 
# 150 "Parser.mly"
                         ( C_not_cond(O_not, _2) )
# 391 "Parser.ml"
         in
        _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv524)) : 'freshtv526)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv531 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 399 "Parser.ml"
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
# 411 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | T_if ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_left_br ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_return ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_semicolon ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | T_string _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
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
# 443 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv530)) : 'freshtv532)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv537 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 452 "Parser.ml"
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
# 466 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | T_if ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_left_br ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_return ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_semicolon ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_string _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
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
# 496 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv536)) : 'freshtv538)
    | _ ->
        _menhir_fail ()

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 506 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 534 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 562 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 590 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 618 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 646 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run83 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 674 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv503 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 682 "Parser.ml"
    )) = Obj.magic _menhir_stack in
    ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 62 "Parser.mly"
      (expr)
# 687 "Parser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (
# 62 "Parser.mly"
      (expr)
# 694 "Parser.ml"
    ) = 
# 139 "Parser.mly"
                                          ( E_expr_parenthesized(_2) )
# 698 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)

and _menhir_goto_expr_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 60 "Parser.mly"
      (expr list)
# 705 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv497 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 715 "Parser.ml"
        ))) * _menhir_state * (
# 60 "Parser.mly"
      (expr list)
# 719 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv493 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 729 "Parser.ml"
            ))) * _menhir_state * (
# 60 "Parser.mly"
      (expr list)
# 733 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv491 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 740 "Parser.ml"
            ))) * _menhir_state * (
# 60 "Parser.mly"
      (expr list)
# 744 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 25 "Parser.mly"
       (string)
# 749 "Parser.ml"
            ))), _, (_3 : (
# 60 "Parser.mly"
      (expr list)
# 753 "Parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 59 "Parser.mly"
      (funcCall)
# 760 "Parser.ml"
            ) = 
# 125 "Parser.mly"
                                                            ( newFuncCall(_1, _3) )
# 764 "Parser.ml"
             in
            _menhir_goto_func_call _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)) : 'freshtv494)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv495 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 774 "Parser.ml"
            ))) * _menhir_state * (
# 60 "Parser.mly"
      (expr list)
# 778 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)) : 'freshtv498)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv501 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 787 "Parser.ml"
        ))) * _menhir_state * (
# 60 "Parser.mly"
      (expr list)
# 791 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv499 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 797 "Parser.ml"
        ))) * _menhir_state * (
# 60 "Parser.mly"
      (expr list)
# 801 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 806 "Parser.ml"
        ))), _, (_3 : (
# 60 "Parser.mly"
      (expr list)
# 810 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 60 "Parser.mly"
      (expr list)
# 816 "Parser.ml"
        ) = 
# 128 "Parser.mly"
                                     ( _1 :: _3 )
# 820 "Parser.ml"
         in
        _menhir_goto_expr_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)) : 'freshtv502)
    | _ ->
        _menhir_fail ()

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 829 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 857 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 885 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 913 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 941 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 969 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 62 "Parser.mly"
      (expr)
# 997 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv381 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 1007 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1011 "Parser.ml"
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
# 1031 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1035 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 1042 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1046 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 61 "Parser.mly"
      (lvalue)
# 1051 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1055 "Parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 61 "Parser.mly"
      (lvalue)
# 1062 "Parser.ml"
            ) = 
# 133 "Parser.mly"
                                                  ( L_comp(_1, _3) )
# 1066 "Parser.ml"
             in
            _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)) : 'freshtv378)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv379 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 1076 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1080 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv387 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1089 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1093 "Parser.ml"
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
# 1109 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1113 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1118 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1122 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 1128 "Parser.ml"
            ) = 
# 143 "Parser.mly"
                               ( E_op_expr_expr(_1, O_plus, _3) )
# 1132 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv385 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1142 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1146 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)) : 'freshtv388)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1155 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1159 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv389 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1165 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1169 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1174 "Parser.ml"
        ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1178 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 62 "Parser.mly"
      (expr)
# 1184 "Parser.ml"
        ) = 
# 145 "Parser.mly"
                              ( E_op_expr_expr(_1, O_mul, _3) )
# 1188 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)) : 'freshtv392)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1196 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1200 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv393 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1206 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1210 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1215 "Parser.ml"
        ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1219 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 62 "Parser.mly"
      (expr)
# 1225 "Parser.ml"
        ) = 
# 147 "Parser.mly"
                              ( E_op_expr_expr(_1, O_mod, _3) )
# 1229 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)) : 'freshtv396)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1237 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1241 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1247 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1251 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1256 "Parser.ml"
        ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1260 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 62 "Parser.mly"
      (expr)
# 1266 "Parser.ml"
        ) = 
# 146 "Parser.mly"
                              ( E_op_expr_expr(_1, O_div, _3) )
# 1270 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)) : 'freshtv400)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv405 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1278 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1282 "Parser.ml"
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
# 1298 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1302 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1307 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1311 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 1317 "Parser.ml"
            ) = 
# 144 "Parser.mly"
                                ( E_op_expr_expr(_1, O_minus, _3) )
# 1321 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv403 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1331 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1335 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)) : 'freshtv406)
    | MenhirState80 | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1344 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1354 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_chr _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | T_identifier _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | T_integer _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | T_left_par ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_minus ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_plus ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | T_string _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
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
# 1392 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1397 "Parser.ml"
            ))) = _menhir_stack in
            let _v : (
# 60 "Parser.mly"
      (expr list)
# 1402 "Parser.ml"
            ) = 
# 127 "Parser.mly"
                   ( [_1] )
# 1406 "Parser.ml"
             in
            _menhir_goto_expr_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv410)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv411 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1416 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)) : 'freshtv414)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv417 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1425 "Parser.ml"
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
# 1449 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1458 "Parser.ml"
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
# 1474 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 62 "Parser.mly"
      (expr)
# 1479 "Parser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 1485 "Parser.ml"
            ) = 
# 142 "Parser.mly"
                           ( E_op_expr(O_minus, _2) )
# 1489 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv421 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1499 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)) : 'freshtv424)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1508 "Parser.ml"
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
# 1524 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 62 "Parser.mly"
      (expr)
# 1529 "Parser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 1535 "Parser.ml"
            ) = 
# 141 "Parser.mly"
                          ( E_op_expr(O_plus, _2) )
# 1539 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv427 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1549 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)) : 'freshtv430)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv433 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1558 "Parser.ml"
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
# 1594 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)) : 'freshtv434)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1603 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1607 "Parser.ml"
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
# 1627 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1631 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1636 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1640 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1646 "Parser.ml"
            ) = 
# 158 "Parser.mly"
                                    ( C_expr_expr(_1, O_not_equal, _3)  )
# 1650 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1660 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1664 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv438)) : 'freshtv440)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv445 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1673 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1677 "Parser.ml"
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
# 1697 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1701 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1706 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1710 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1716 "Parser.ml"
            ) = 
# 155 "Parser.mly"
                                  ( C_expr_expr(_1, O_less_eq, _3)  )
# 1720 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv443 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1730 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1734 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv444)) : 'freshtv446)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv451 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1743 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1747 "Parser.ml"
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
# 1767 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1771 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1776 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1780 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1786 "Parser.ml"
            ) = 
# 154 "Parser.mly"
                               ( C_expr_expr(_1, O_less, _3)  )
# 1790 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv449 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1800 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1804 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)) : 'freshtv452)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv457 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1813 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1817 "Parser.ml"
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
# 1837 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1841 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1846 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1850 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1856 "Parser.ml"
            ) = 
# 157 "Parser.mly"
                                     ( C_expr_expr(_1, O_greater_eq, _3)  )
# 1860 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv455 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1870 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1874 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv456)) : 'freshtv458)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv463 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1883 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1887 "Parser.ml"
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
# 1907 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1911 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1916 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1920 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1926 "Parser.ml"
            ) = 
# 156 "Parser.mly"
                                  ( C_expr_expr(_1, O_greater, _3)  )
# 1930 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1940 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1944 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)) : 'freshtv464)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv469 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1953 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1957 "Parser.ml"
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
# 1977 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 1981 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "Parser.mly"
      (expr)
# 1986 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 1990 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 63 "Parser.mly"
      (cond)
# 1996 "Parser.ml"
            ) = 
# 153 "Parser.mly"
                                ( C_expr_expr(_1, O_equal, _3) )
# 2000 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv466)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv467 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2010 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2014 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)) : 'freshtv470)
    | MenhirState116 | MenhirState52 | MenhirState86 | MenhirState106 | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2023 "Parser.ml"
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
# 2057 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv472)) : 'freshtv474)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv481 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2066 "Parser.ml"
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
# 2086 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2093 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 62 "Parser.mly"
      (expr)
# 2098 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 56 "Parser.mly"
      (stmt)
# 2105 "Parser.ml"
            ) = 
# 117 "Parser.mly"
                                        ( S_return(_2)  )
# 2109 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv476)) : 'freshtv478)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv479 * _menhir_state) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2119 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv480)) : 'freshtv482)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv489 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2128 "Parser.ml"
        ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2132 "Parser.ml"
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
# 2152 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2156 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv483 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2163 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2167 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 61 "Parser.mly"
      (lvalue)
# 2172 "Parser.ml"
            ))), _, (_3 : (
# 62 "Parser.mly"
      (expr)
# 2176 "Parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 56 "Parser.mly"
      (stmt)
# 2183 "Parser.ml"
            ) = 
# 110 "Parser.mly"
                                                    ( S_assignment(_1, _3) )
# 2187 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv487 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2197 "Parser.ml"
            ))) * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 2201 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)) : 'freshtv490)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 58 "Parser.mly"
      (stmt list)
# 2211 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2221 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_br ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv365 * _menhir_state) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2231 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * _menhir_state) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2238 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 58 "Parser.mly"
      (stmt list)
# 2243 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 57 "Parser.mly"
      (block)
# 2250 "Parser.ml"
            ) = 
# 119 "Parser.mly"
                                             ( Block(_2) )
# 2254 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv361) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 57 "Parser.mly"
      (block)
# 2262 "Parser.ml"
            )) = _v in
            ((match _menhir_s with
            | MenhirState51 | MenhirState132 | MenhirState110 | MenhirState118 | MenhirState120 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv335) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 57 "Parser.mly"
      (block)
# 2272 "Parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : (
# 57 "Parser.mly"
      (block)
# 2280 "Parser.ml"
                )) : (
# 57 "Parser.mly"
      (block)
# 2284 "Parser.ml"
                )) = _v in
                ((let _v : (
# 56 "Parser.mly"
      (stmt)
# 2289 "Parser.ml"
                ) = 
# 111 "Parser.mly"
                    ( S_block(_1) )
# 2293 "Parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)) : 'freshtv336)
            | MenhirState50 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv359 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 2301 "Parser.ml"
                )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 2305 "Parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 57 "Parser.mly"
      (block)
# 2311 "Parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv357 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 2317 "Parser.ml"
                )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 2321 "Parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : (
# 57 "Parser.mly"
      (block)
# 2327 "Parser.ml"
                )) : (
# 57 "Parser.mly"
      (block)
# 2331 "Parser.ml"
                )) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : (
# 44 "Parser.mly"
      (header)
# 2336 "Parser.ml"
                ))), _, (_2 : (
# 43 "Parser.mly"
      (localDef list)
# 2340 "Parser.ml"
                ))) = _menhir_stack in
                let _v : (
# 42 "Parser.mly"
      (funcDef)
# 2345 "Parser.ml"
                ) = 
# 69 "Parser.mly"
                                          ( newFuncDef(_1, _2, _3) )
# 2349 "Parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv355) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 42 "Parser.mly"
      (funcDef)
# 2357 "Parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                match _menhir_s with
                | MenhirState40 | MenhirState135 | MenhirState137 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv339 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2366 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv337 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2372 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (_1 : (
# 42 "Parser.mly"
      (funcDef)
# 2377 "Parser.ml"
                    ))) = _menhir_stack in
                    let _v : (
# 53 "Parser.mly"
      (localDef)
# 2382 "Parser.ml"
                    ) = 
# 101 "Parser.mly"
                       ( L_FuncDef(_1) )
# 2386 "Parser.ml"
                     in
                    _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)) : 'freshtv340)
                | MenhirState0 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv353 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2394 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | T_eof ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv349 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2404 "Parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv347 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2410 "Parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, (_1 : (
# 42 "Parser.mly"
      (funcDef)
# 2415 "Parser.ml"
                        ))) = _menhir_stack in
                        let _2 = () in
                        let _v : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2421 "Parser.ml"
                        ) = 
# 67 "Parser.mly"
                             ( _1 )
# 2425 "Parser.ml"
                         in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv345) = _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let (_v : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2433 "Parser.ml"
                        )) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let (_v : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2441 "Parser.ml"
                        )) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let ((_1 : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2449 "Parser.ml"
                        )) : (
# 41 "Parser.mly"
      (Ast.funcDef)
# 2453 "Parser.ml"
                        )) = _v in
                        (Obj.magic _1 : 'freshtv342)) : 'freshtv344)) : 'freshtv346)) : 'freshtv348)) : 'freshtv350)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv351 * _menhir_state * (
# 42 "Parser.mly"
      (funcDef)
# 2463 "Parser.ml"
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
# 2478 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv373 * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2487 "Parser.ml"
        )) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2491 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv371 * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2497 "Parser.ml"
        )) * _menhir_state * (
# 58 "Parser.mly"
      (stmt list)
# 2501 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 56 "Parser.mly"
      (stmt)
# 2506 "Parser.ml"
        ))), _, (_2 : (
# 58 "Parser.mly"
      (stmt list)
# 2510 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 58 "Parser.mly"
      (stmt list)
# 2515 "Parser.ml"
        ) = 
# 122 "Parser.mly"
                             ( _1 :: _2 )
# 2519 "Parser.ml"
         in
        _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)) : 'freshtv374)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 56 "Parser.mly"
      (stmt)
# 2528 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv321 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2538 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2542 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_else ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv315 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2552 "Parser.ml"
            ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2556 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | T_if ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_left_br ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_return ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_semicolon ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | T_string _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | T_while ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv316)
        | T_identifier _ | T_if | T_left_br | T_return | T_right_br | T_semicolon | T_string _ | T_while ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv317 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2584 "Parser.ml"
            ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2588 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 2593 "Parser.ml"
            ))), _, (_4 : (
# 56 "Parser.mly"
      (stmt)
# 2597 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 56 "Parser.mly"
      (stmt)
# 2604 "Parser.ml"
            ) = 
# 113 "Parser.mly"
                                    ( S_if(_2, _4) )
# 2608 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv319 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2618 "Parser.ml"
            ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2622 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv325 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2631 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2635 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2639 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv323 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2645 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2649 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2653 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 2658 "Parser.ml"
        ))), _, (_4 : (
# 56 "Parser.mly"
      (stmt)
# 2662 "Parser.ml"
        ))), _, (_6 : (
# 56 "Parser.mly"
      (stmt)
# 2666 "Parser.ml"
        ))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 56 "Parser.mly"
      (stmt)
# 2674 "Parser.ml"
        ) = 
# 114 "Parser.mly"
                                               ( S_if_else(_2, _4, _6) )
# 2678 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)) : 'freshtv326)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv329 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2686 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2690 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv327 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 2696 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2700 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 63 "Parser.mly"
      (cond)
# 2705 "Parser.ml"
        ))), _, (_4 : (
# 56 "Parser.mly"
      (stmt)
# 2709 "Parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (
# 56 "Parser.mly"
      (stmt)
# 2716 "Parser.ml"
        ) = 
# 115 "Parser.mly"
                                     ( S_while(_2, _4) )
# 2720 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)) : 'freshtv330)
    | MenhirState132 | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331 * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 2728 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_identifier _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
        | T_if ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_left_br ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_return ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_semicolon ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | T_string _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
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
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
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
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_goto_l_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 61 "Parser.mly"
      (lvalue)
# 2813 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState123 | MenhirState116 | MenhirState112 | MenhirState52 | MenhirState86 | MenhirState106 | MenhirState103 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState54 | MenhirState55 | MenhirState56 | MenhirState80 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState63 | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2823 "Parser.ml"
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
# 2835 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 61 "Parser.mly"
      (lvalue)
# 2840 "Parser.ml"
            ))) = _menhir_stack in
            let _v : (
# 62 "Parser.mly"
      (expr)
# 2845 "Parser.ml"
            ) = 
# 138 "Parser.mly"
                      ( E_lvalue(_1) )
# 2849 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv305 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2859 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)
    | MenhirState51 | MenhirState132 | MenhirState110 | MenhirState118 | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2868 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_assignment ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv309 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 2878 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_chr _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | T_identifier _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | T_integer _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | T_left_par ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_minus ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_plus ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | T_string _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
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
# 2910 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)
    | _ ->
        _menhir_fail ()

and _menhir_goto_func_call : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 59 "Parser.mly"
      (funcCall)
# 2920 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState123 | MenhirState116 | MenhirState112 | MenhirState52 | MenhirState86 | MenhirState106 | MenhirState103 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState54 | MenhirState55 | MenhirState56 | MenhirState80 | MenhirState59 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2930 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2936 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 59 "Parser.mly"
      (funcCall)
# 2941 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 62 "Parser.mly"
      (expr)
# 2946 "Parser.ml"
        ) = 
# 140 "Parser.mly"
                        ( E_func_call(_1) )
# 2950 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)) : 'freshtv294)
    | MenhirState51 | MenhirState132 | MenhirState110 | MenhirState118 | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2958 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv297 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2968 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv295 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 2975 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 59 "Parser.mly"
      (funcCall)
# 2980 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 56 "Parser.mly"
      (stmt)
# 2986 "Parser.ml"
            ) = 
# 112 "Parser.mly"
                                    ( S_func_call(_1) )
# 2990 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv299 * _menhir_state * (
# 59 "Parser.mly"
      (funcCall)
# 3000 "Parser.ml"
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
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
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
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
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
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_left_par ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 26 "Parser.mly"
       (int)
# 3085 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 26 "Parser.mly"
       (int)
# 3095 "Parser.ml"
    )) : (
# 26 "Parser.mly"
       (int)
# 3099 "Parser.ml"
    )) = _v in
    ((let _v : (
# 62 "Parser.mly"
      (expr)
# 3104 "Parser.ml"
    ) = 
# 136 "Parser.mly"
                        ( E_const(_1) )
# 3108 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 27 "Parser.mly"
       (char)
# 3115 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 27 "Parser.mly"
       (char)
# 3125 "Parser.ml"
    )) : (
# 27 "Parser.mly"
       (char)
# 3129 "Parser.ml"
    )) = _v in
    ((let _v : (
# 62 "Parser.mly"
      (expr)
# 3134 "Parser.ml"
    ) = 
# 137 "Parser.mly"
                    ( E_char(_1) )
# 3138 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 58 "Parser.mly"
      (stmt list)
# 3147 "Parser.ml"
    ) = 
# 121 "Parser.mly"
              ( [] )
# 3151 "Parser.ml"
     in
    _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "Parser.mly"
       (string)
# 3185 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv285) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 28 "Parser.mly"
       (string)
# 3195 "Parser.ml"
    )) : (
# 28 "Parser.mly"
       (string)
# 3199 "Parser.ml"
    )) = _v in
    ((let _v : (
# 61 "Parser.mly"
      (lvalue)
# 3204 "Parser.ml"
    ) = 
# 132 "Parser.mly"
                       ( L_string(_1) )
# 3208 "Parser.ml"
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
# 3222 "Parser.ml"
    ) = 
# 109 "Parser.mly"
                          ( S_semicolon(";") )
# 3226 "Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
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
# 3262 "Parser.ml"
        ) = 
# 116 "Parser.mly"
                                   (  S_semicolon(";") )
# 3266 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
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
    | T_chr _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | T_integer _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | T_left_par ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_minus ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_not ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_plus ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "Parser.mly"
       (string)
# 3306 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_left_par ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv273 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 3318 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_chr _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | T_identifier _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | T_integer _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | T_left_par ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_minus ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_plus ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv271 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 3340 "Parser.ml"
            ))) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState59 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv269 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 3348 "Parser.ml"
            ))) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 25 "Parser.mly"
       (string)
# 3354 "Parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (
# 59 "Parser.mly"
      (funcCall)
# 3361 "Parser.ml"
            ) = 
# 124 "Parser.mly"
                                                  ( newFuncCall(_1, []) )
# 3365 "Parser.ml"
             in
            _menhir_goto_func_call _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | T_string _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv274)
    | T_and | T_assignment | T_comma | T_div | T_do | T_equal | T_greater | T_greater_eq | T_left_sqr | T_less | T_less_eq | T_minus | T_mod | T_mul | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 3379 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 25 "Parser.mly"
       (string)
# 3384 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 61 "Parser.mly"
      (lvalue)
# 3389 "Parser.ml"
        ) = 
# 131 "Parser.mly"
                           ( L_id(_1) )
# 3393 "Parser.ml"
         in
        _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 3403 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)

and _menhir_goto_fpar_def_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 46 "Parser.mly"
      (fparDef list)
# 3411 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv263 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 3421 "Parser.ml"
        ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3425 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv259 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 3435 "Parser.ml"
            ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3439 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_colon ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv255 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 3449 "Parser.ml"
                ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3453 "Parser.ml"
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
                let (_menhir_stack : (((('freshtv257 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 3475 "Parser.ml"
                ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3479 "Parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv261 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 3490 "Parser.ml"
            ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3494 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3503 "Parser.ml"
        ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3507 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3513 "Parser.ml"
        ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 3517 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 45 "Parser.mly"
      (fparDef)
# 3522 "Parser.ml"
        ))), _, (_3 : (
# 46 "Parser.mly"
      (fparDef list)
# 3526 "Parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 46 "Parser.mly"
      (fparDef list)
# 3532 "Parser.ml"
        ) = 
# 78 "Parser.mly"
                                                    ( _1 :: _3 )
# 3536 "Parser.ml"
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
    | T_identifier _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | T_if ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_left_br ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_return ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_semicolon ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_string _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
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
# 3572 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv253 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3580 "Parser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_semicolon ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3590 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_identifier _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
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
# 3608 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 45 "Parser.mly"
      (fparDef)
# 3613 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 46 "Parser.mly"
      (fparDef list)
# 3618 "Parser.ml"
        ) = 
# 77 "Parser.mly"
                            ( [_1] )
# 3622 "Parser.ml"
         in
        _menhir_goto_fpar_def_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 3632 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)

and _menhir_goto_local_def_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 43 "Parser.mly"
      (localDef list)
# 3640 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState137 | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv241 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 3650 "Parser.ml"
        )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 3654 "Parser.ml"
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
# 3670 "Parser.ml"
        )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 3674 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv243 * _menhir_state * (
# 53 "Parser.mly"
      (localDef)
# 3680 "Parser.ml"
        )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 3684 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 53 "Parser.mly"
      (localDef)
# 3689 "Parser.ml"
        ))), _, (_2 : (
# 43 "Parser.mly"
      (localDef list)
# 3693 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 43 "Parser.mly"
      (localDef list)
# 3698 "Parser.ml"
        ) = 
# 72 "Parser.mly"
                                            ( _1 :: _2 )
# 3702 "Parser.ml"
         in
        _menhir_goto_local_def_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)) : 'freshtv246)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fpar_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "Parser.mly"
      (fparType)
# 3711 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv235 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 3720 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3724 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 52 "Parser.mly"
      (fparType)
# 3730 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv233 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 3736 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3740 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : (
# 52 "Parser.mly"
      (fparType)
# 3746 "Parser.ml"
        )) : (
# 52 "Parser.mly"
      (fparType)
# 3750 "Parser.ml"
        )) = _v in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 25 "Parser.mly"
       (string)
# 3755 "Parser.ml"
        ))), _, (_3 : (
# 47 "Parser.mly"
      (string list)
# 3759 "Parser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (
# 45 "Parser.mly"
      (fparDef)
# 3766 "Parser.ml"
        ) = 
# 81 "Parser.mly"
                                                           ( newFparDef("ref", _2 :: _3, _5) )
# 3770 "Parser.ml"
         in
        _menhir_goto_fpar_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv239 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 3778 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3782 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 52 "Parser.mly"
      (fparType)
# 3788 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 3794 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 3798 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : (
# 52 "Parser.mly"
      (fparType)
# 3804 "Parser.ml"
        )) : (
# 52 "Parser.mly"
      (fparType)
# 3808 "Parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 25 "Parser.mly"
       (string)
# 3813 "Parser.ml"
        ))), _, (_2 : (
# 47 "Parser.mly"
      (string list)
# 3817 "Parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _v : (
# 45 "Parser.mly"
      (fparDef)
# 3823 "Parser.ml"
        ) = 
# 82 "Parser.mly"
                                                     ( newFparDef("", _2, _4) )
# 3827 "Parser.ml"
         in
        _menhir_goto_fpar_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)) : 'freshtv240)
    | _ ->
        _menhir_fail ()

and _menhir_goto_local_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "Parser.mly"
      (localDef)
# 3836 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv231 * _menhir_state * (
# 53 "Parser.mly"
      (localDef)
# 3844 "Parser.ml"
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
# 3865 "Parser.ml"
    ) = 
# 71 "Parser.mly"
                 ( [] )
# 3869 "Parser.ml"
     in
    _menhir_goto_local_def_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 25 "Parser.mly"
       (string)
# 3885 "Parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
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
# 3910 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state) * (
# 26 "Parser.mly"
       (int)
# 3919 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 50 "Parser.mly"
      (int list)
# 3925 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * _menhir_state) * (
# 26 "Parser.mly"
       (int)
# 3931 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : (
# 50 "Parser.mly"
      (int list)
# 3937 "Parser.ml"
        )) : (
# 50 "Parser.mly"
      (int list)
# 3941 "Parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s), (_2 : (
# 26 "Parser.mly"
       (int)
# 3946 "Parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (
# 50 "Parser.mly"
      (int list)
# 3953 "Parser.ml"
        ) = 
# 93 "Parser.mly"
                                                                      ( _2 :: _4 )
# 3957 "Parser.ml"
         in
        _menhir_goto_array_dimension _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)) : 'freshtv198)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv201 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 3965 "Parser.ml"
        )) * _menhir_state)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 50 "Parser.mly"
      (int list)
# 3971 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 3977 "Parser.ml"
        )) * _menhir_state)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : (
# 50 "Parser.mly"
      (int list)
# 3983 "Parser.ml"
        )) : (
# 50 "Parser.mly"
      (int list)
# 3987 "Parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 48 "Parser.mly"
      (dataType)
# 3992 "Parser.ml"
        ))), _) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (
# 52 "Parser.mly"
      (fparType)
# 3999 "Parser.ml"
        ) = 
# 99 "Parser.mly"
                                                               ( newFparType(_1, _4, true) )
# 4003 "Parser.ml"
         in
        _menhir_goto_fpar_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)) : 'freshtv202)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4011 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 50 "Parser.mly"
      (int list)
# 4017 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4023 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : (
# 50 "Parser.mly"
      (int list)
# 4029 "Parser.ml"
        )) : (
# 50 "Parser.mly"
      (int list)
# 4033 "Parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 48 "Parser.mly"
      (dataType)
# 4038 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 52 "Parser.mly"
      (fparType)
# 4043 "Parser.ml"
        ) = 
# 98 "Parser.mly"
                                           ( newFparType(_1, _2, false) )
# 4047 "Parser.ml"
         in
        _menhir_goto_fpar_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4055 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 50 "Parser.mly"
      (int list)
# 4061 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4067 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : (
# 50 "Parser.mly"
      (int list)
# 4073 "Parser.ml"
        )) : (
# 50 "Parser.mly"
      (int list)
# 4077 "Parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 48 "Parser.mly"
      (dataType)
# 4082 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 49 "Parser.mly"
      (myType)
# 4087 "Parser.ml"
        ) = 
# 90 "Parser.mly"
                                        ( newMyType(_1, _2) )
# 4091 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 49 "Parser.mly"
      (myType)
# 4099 "Parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv219 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4106 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4110 "Parser.ml"
        ))) * _menhir_state * (
# 49 "Parser.mly"
      (myType)
# 4114 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv215 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4124 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4128 "Parser.ml"
            ))) * _menhir_state * (
# 49 "Parser.mly"
      (myType)
# 4132 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv213 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4139 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4143 "Parser.ml"
            ))) * _menhir_state * (
# 49 "Parser.mly"
      (myType)
# 4147 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 25 "Parser.mly"
       (string)
# 4152 "Parser.ml"
            ))), _, (_3 : (
# 47 "Parser.mly"
      (string list)
# 4156 "Parser.ml"
            ))), _, (_5 : (
# 49 "Parser.mly"
      (myType)
# 4160 "Parser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (
# 55 "Parser.mly"
      (varDef)
# 4168 "Parser.ml"
            ) = 
# 107 "Parser.mly"
                                                                    ( newVarDef(_2 :: _3, _5) )
# 4172 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv211) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 55 "Parser.mly"
      (varDef)
# 4180 "Parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 55 "Parser.mly"
      (varDef)
# 4188 "Parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 55 "Parser.mly"
      (varDef)
# 4196 "Parser.ml"
            )) : (
# 55 "Parser.mly"
      (varDef)
# 4200 "Parser.ml"
            )) = _v in
            ((let _v : (
# 53 "Parser.mly"
      (localDef)
# 4205 "Parser.ml"
            ) = 
# 103 "Parser.mly"
                      ( L_varDef(_1) )
# 4209 "Parser.ml"
             in
            _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv217 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4219 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4223 "Parser.ml"
            ))) * _menhir_state * (
# 49 "Parser.mly"
      (myType)
# 4227 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)) : 'freshtv226)
    | _ ->
        _menhir_fail ()

and _menhir_goto_header : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 44 "Parser.mly"
      (header)
# 4237 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 4247 "Parser.ml"
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
# 4267 "Parser.ml"
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
# 4279 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState137 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 4287 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 44 "Parser.mly"
      (header)
# 4293 "Parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 54 "Parser.mly"
      (funcDecl)
# 4299 "Parser.ml"
            ) = 
# 105 "Parser.mly"
                                 ( FuncDecl_Header(_1) )
# 4303 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 54 "Parser.mly"
      (funcDecl)
# 4311 "Parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 54 "Parser.mly"
      (funcDecl)
# 4319 "Parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 54 "Parser.mly"
      (funcDecl)
# 4327 "Parser.ml"
            )) : (
# 54 "Parser.mly"
      (funcDecl)
# 4331 "Parser.ml"
            )) = _v in
            ((let _v : (
# 53 "Parser.mly"
      (localDef)
# 4336 "Parser.ml"
            ) = 
# 102 "Parser.mly"
                        ( L_FuncDecl(_1) )
# 4340 "Parser.ml"
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
# 4364 "Parser.ml"
    ) = 
# 92 "Parser.mly"
                     ( [] )
# 4368 "Parser.ml"
     in
    _menhir_goto_array_dimension _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_integer _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state -> (
# 26 "Parser.mly"
       (int)
# 4391 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_right_sqr ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state) * (
# 26 "Parser.mly"
       (int)
# 4403 "Parser.ml"
        )) = Obj.magic _menhir_stack in
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
        let (_menhir_stack : ('freshtv177 * _menhir_state) * (
# 26 "Parser.mly"
       (int)
# 4423 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)

and _menhir_goto_id_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 47 "Parser.mly"
      (string list)
# 4431 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv155 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4441 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4445 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4451 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4455 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 25 "Parser.mly"
       (string)
# 4460 "Parser.ml"
        ))), _, (_3 : (
# 47 "Parser.mly"
      (string list)
# 4464 "Parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 47 "Parser.mly"
      (string list)
# 4470 "Parser.ml"
        ) = 
# 85 "Parser.mly"
                                           ( _2 :: _3 )
# 4474 "Parser.ml"
         in
        _menhir_goto_id_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)) : 'freshtv156)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4482 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4486 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv157 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4496 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4500 "Parser.ml"
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
            let (_menhir_stack : (('freshtv159 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4520 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4524 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 4533 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4537 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv163 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 4547 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4551 "Parser.ml"
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
            let (_menhir_stack : ('freshtv165 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 4571 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4575 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv173 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4584 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4588 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv169 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4598 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4602 "Parser.ml"
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
            let (_menhir_stack : (('freshtv171 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4622 "Parser.ml"
            )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 4626 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)) : 'freshtv174)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ret_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 51 "Parser.mly"
      (retType)
# 4636 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv147 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4645 "Parser.ml"
        ))) * _menhir_state)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 51 "Parser.mly"
      (retType)
# 4651 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv145 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4657 "Parser.ml"
        ))) * _menhir_state)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : (
# 51 "Parser.mly"
      (retType)
# 4663 "Parser.ml"
        )) : (
# 51 "Parser.mly"
      (retType)
# 4667 "Parser.ml"
        )) = _v in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 25 "Parser.mly"
       (string)
# 4672 "Parser.ml"
        ))), _) = _menhir_stack in
        let _5 = () in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 44 "Parser.mly"
      (header)
# 4681 "Parser.ml"
        ) = 
# 74 "Parser.mly"
                                                                         ( newHeader(_2, [], _6) )
# 4685 "Parser.ml"
         in
        _menhir_goto_header _menhir_env _menhir_stack _menhir_s _v) : 'freshtv146)) : 'freshtv148)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv151 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4693 "Parser.ml"
        ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 4697 "Parser.ml"
        )))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 51 "Parser.mly"
      (retType)
# 4703 "Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv149 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 4709 "Parser.ml"
        ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 4713 "Parser.ml"
        )))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_7 : (
# 51 "Parser.mly"
      (retType)
# 4719 "Parser.ml"
        )) : (
# 51 "Parser.mly"
      (retType)
# 4723 "Parser.ml"
        )) = _v in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 25 "Parser.mly"
       (string)
# 4728 "Parser.ml"
        ))), _, (_4 : (
# 46 "Parser.mly"
      (fparDef list)
# 4732 "Parser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 44 "Parser.mly"
      (header)
# 4741 "Parser.ml"
        ) = 
# 75 "Parser.mly"
                                                                                       ( newHeader(_2, _4, _7) )
# 4745 "Parser.ml"
         in
        _menhir_goto_header _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)
    | _ ->
        _menhir_fail ()

and _menhir_goto_data_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 48 "Parser.mly"
      (dataType)
# 4754 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4764 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4770 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 48 "Parser.mly"
      (dataType)
# 4775 "Parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 51 "Parser.mly"
      (retType)
# 4780 "Parser.ml"
        ) = 
# 95 "Parser.mly"
                        ( RetDataType(_1) )
# 4784 "Parser.ml"
         in
        _menhir_goto_ret_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv132)) : 'freshtv134)
    | MenhirState30 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4792 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4802 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState19 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_integer _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _v
            | T_right_sqr ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv135 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 4816 "Parser.ml"
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
# 4836 "Parser.ml"
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
# 4851 "Parser.ml"
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
# 4872 "Parser.ml"
    ) = 
# 84 "Parser.mly"
            ( [] )
# 4876 "Parser.ml"
     in
    _menhir_goto_id_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 25 "Parser.mly"
       (string)
# 4892 "Parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
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
# 4924 "Parser.ml"
    ) = 
# 96 "Parser.mly"
                        ( Nothing("nothing") )
# 4928 "Parser.ml"
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
# 4942 "Parser.ml"
    ) = 
# 87 "Parser.mly"
                    ( Const("int") )
# 4946 "Parser.ml"
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
# 4960 "Parser.ml"
    ) = 
# 88 "Parser.mly"
                     ( Char("char") )
# 4964 "Parser.ml"
     in
    _menhir_goto_data_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 25 "Parser.mly"
       (string)
# 4980 "Parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
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

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "Parser.mly"
       (string)
# 5005 "Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
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
# 5029 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * (
# 53 "Parser.mly"
      (localDef)
# 5038 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 5047 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 5056 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv25 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 5065 "Parser.ml"
        ))) * _menhir_state * (
# 56 "Parser.mly"
      (stmt)
# 5069 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv27 * _menhir_state) * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 5078 "Parser.ml"
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
# 5097 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 5106 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 63 "Parser.mly"
      (cond)
# 5115 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5124 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5133 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5142 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5151 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5160 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5169 "Parser.ml"
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
# 5188 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5197 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5206 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5215 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5224 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * (
# 62 "Parser.mly"
      (expr)
# 5233 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * (
# 61 "Parser.mly"
      (lvalue)
# 5242 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 5251 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
# 5285 "Parser.ml"
        )) * _menhir_state * (
# 43 "Parser.mly"
      (localDef list)
# 5289 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 5298 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv85 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5307 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 5311 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5320 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * (
# 44 "Parser.mly"
      (header)
# 5329 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 45 "Parser.mly"
      (fparDef)
# 5338 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv93 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5347 "Parser.ml"
        ))) * _menhir_state * (
# 46 "Parser.mly"
      (fparDef list)
# 5351 "Parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 5360 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 5364 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state * (
# 25 "Parser.mly"
       (string)
# 5373 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv99 * _menhir_state) * (
# 26 "Parser.mly"
       (int)
# 5382 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv101 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 5391 "Parser.ml"
        )) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state * (
# 48 "Parser.mly"
      (dataType)
# 5400 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv105 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5409 "Parser.ml"
        )) * _menhir_state * (
# 47 "Parser.mly"
      (string list)
# 5413 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5422 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5431 "Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv111 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5440 "Parser.ml"
        ))) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5449 "Parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
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
    | T_identifier _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 25 "Parser.mly"
       (string)
# 5470 "Parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv9 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5481 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | T_ref ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | T_right_par ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv7 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5495 "Parser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState3 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | T_colon ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv3 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5507 "Parser.ml"
                    ))) * _menhir_state) = Obj.magic _menhir_stack in
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
                    let (_menhir_stack : ((('freshtv5 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5529 "Parser.ml"
                    ))) * _menhir_state) = Obj.magic _menhir_stack in
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
            let (_menhir_stack : ('freshtv11 * _menhir_state) * (
# 25 "Parser.mly"
       (string)
# 5544 "Parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
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
# 5571 "Parser.ml"
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
  

# 5600 "Parser.ml"
