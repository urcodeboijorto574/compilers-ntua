
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
  | MenhirState142
  | MenhirState134
  | MenhirState133
  | MenhirState130
  | MenhirState127
  | MenhirState126
  | MenhirState123
  | MenhirState121
  | MenhirState117
  | MenhirState115
  | MenhirState114
  | MenhirState111
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState93
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState86
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState68
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState57
  | MenhirState55
  | MenhirState51
  | MenhirState48
  | MenhirState45
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState28
  | MenhirState25
  | MenhirState19
  | MenhirState16
  | MenhirState14
  | MenhirState7
  | MenhirState3
  | MenhirState0

let rec _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_cond -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_left_par ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_not ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run117 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_cond -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | T_left_par ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | T_not ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_goto_func_call_r : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_func_call_r -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv581 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_call_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv579 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_func_call_r) : 'tv_func_call_r) = _v in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_func_call_r = 
# 109 "Parser.mly"
                                       ( () )
# 202 "Parser.ml"
         in
        _menhir_goto_func_call_r _menhir_env _menhir_stack _menhir_s _v) : 'freshtv580)) : 'freshtv582)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv591 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_call_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_func_call_r) : 'tv_func_call_r) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_expr)) = _menhir_stack in
        let _v : 'tv_func_call_rr = 
# 106 "Parser.mly"
                                  ( () )
# 218 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv587) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_call_rr) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv585) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_call_rr) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv583) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_func_call_rr) : 'tv_func_call_rr) = _v in
        ((let _v : 'tv_option_func_call_rr_ = 
# 116 "<standard.mly>"
    ( Some x )
# 235 "Parser.ml"
         in
        _menhir_goto_option_func_call_rr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv584)) : 'freshtv586)) : 'freshtv588)) : 'freshtv590)) : 'freshtv592)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cond : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_cond -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv543 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | T_or ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv539 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv537 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_cond)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_cond = 
# 129 "Parser.mly"
                                          ( () )
# 267 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv538)) : 'freshtv540)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv541 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv542)) : 'freshtv544)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv549 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv545 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_cond)), _, (_3 : 'tv_cond)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 132 "Parser.mly"
                             ( () )
# 293 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv546)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv547 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv548)) : 'freshtv550)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv553 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv551 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_cond)), _, (_3 : 'tv_cond)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_cond = 
# 131 "Parser.mly"
                              ( () )
# 313 "Parser.ml"
         in
        _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)) : 'freshtv554)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv559 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | T_or ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | T_do | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv555 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_cond)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_cond = 
# 130 "Parser.mly"
                         ( () )
# 334 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv556)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv557 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv558)) : 'freshtv560)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv565 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | T_do ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv561 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | T_if ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | T_left_br ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | T_return ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | T_semicolon ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | T_string ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | T_while ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv562)
        | T_or ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv563 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv564)) : 'freshtv566)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv577 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | T_or ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv573 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_else ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv567) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | T_identifier ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | T_if ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | T_left_br ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | T_return ->
                    _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | T_semicolon ->
                    _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | T_string ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | T_while ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130) : 'freshtv568)
            | T_identifier | T_if | T_left_br | T_return | T_right_br | T_semicolon | T_string | T_while ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv569) = Obj.magic _menhir_stack in
                ((let _v : 'tv_option_else_stmt_ = 
# 114 "<standard.mly>"
    ( None )
# 431 "Parser.ml"
                 in
                _menhir_goto_option_else_stmt_ _menhir_env _menhir_stack _v) : 'freshtv570)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv571 * _menhir_state) * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv572)) : 'freshtv574)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv575 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv576)) : 'freshtv578)
    | _ ->
        _menhir_fail ()

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run94 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv535 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_expr = 
# 119 "Parser.mly"
                                          ( () )
# 613 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv536)

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_func_call_r = 
# 108 "Parser.mly"
                            ( () )
# 622 "Parser.ml"
     in
    _menhir_goto_func_call_r _menhir_env _menhir_stack _menhir_s _v

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_goto_option_else_stmt_ : _menhir_env -> 'ttv_tail -> 'tv_option_else_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv533 * _menhir_state) * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
    let (_v : 'tv_option_else_stmt_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv531 * _menhir_state) * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
    let ((_4 : 'tv_option_else_stmt_) : 'tv_option_else_stmt_) = _v in
    ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_cond)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_stmt = 
# 95 "Parser.mly"
                                          ( () )
# 790 "Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv532)) : 'freshtv534)

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_l_value -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv457 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_right_sqr ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv455 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState75 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv453 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_l_value)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_l_value = 
# 114 "Parser.mly"
                                                  ( () )
# 852 "Parser.ml"
             in
            _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)) : 'freshtv456)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv458)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv461 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv459 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 123 "Parser.mly"
                               ( () )
# 879 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv462)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv465 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv463 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 125 "Parser.mly"
                              ( () )
# 896 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv464)) : 'freshtv466)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv469 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv467 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 127 "Parser.mly"
                              ( () )
# 909 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv468)) : 'freshtv470)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv473 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv471 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 126 "Parser.mly"
                              ( () )
# 922 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)) : 'freshtv474)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv477 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv475 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 124 "Parser.mly"
                                ( () )
# 945 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv476)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv478)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_right_par ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv480)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv481 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_right_par ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv482)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv483 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_right_par ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv484)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv487 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv485 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 122 "Parser.mly"
                           ( () )
# 1042 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv486)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv488)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv491 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv489 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 121 "Parser.mly"
                          ( () )
# 1069 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv490)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv492)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv493 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_equal ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_greater ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_greater_eq ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_less ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_less_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_not_equal ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_right_par ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv494)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv497 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv495 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 138 "Parser.mly"
                                    ( () )
# 1134 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv496)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv498)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv501 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv499 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 135 "Parser.mly"
                                  ( () )
# 1165 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv502)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv505 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv503 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 134 "Parser.mly"
                               ( () )
# 1196 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv506)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv509 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv507 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 137 "Parser.mly"
                                     ( () )
# 1227 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv508)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv510)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv511 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 136 "Parser.mly"
                                  ( () )
# 1258 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv512)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv514)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv517 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 133 "Parser.mly"
                                ( () )
# 1289 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv516)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv518)
    | MenhirState127 | MenhirState61 | MenhirState97 | MenhirState117 | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv519 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_equal ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_greater ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_greater_eq ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_less ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_less_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_not_equal ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv520)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv523 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv521 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_option_expr_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1351 "Parser.ml"
             in
            _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv522)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv524)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv529 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_minus ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_mod ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_mul ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_plus ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv527 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState134 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv525 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_l_value)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_stmt = 
# 92 "Parser.mly"
                                                    ( () )
# 1388 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)) : 'freshtv528)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv530)
    | _ ->
        _menhir_fail ()

and _menhir_goto_header_rr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_header_rr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state) * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_header_rr) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * _menhir_state) * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_header_rr) : 'tv_header_rr) = _v in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_fpar_def)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_header_rr = 
# 55 "Parser.mly"
                                             ( () )
# 1415 "Parser.ml"
         in
        _menhir_goto_header_rr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv440)) : 'freshtv442)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_header_rr) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_header_rr) : 'tv_header_rr) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_fpar_def)) = _menhir_stack in
        let _v : 'tv_header_r = 
# 52 "Parser.mly"
                                  ( () )
# 1431 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv447) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_header_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_header_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_header_r) : 'tv_header_r) = _v in
        ((let _v : 'tv_option_header_r_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1448 "Parser.ml"
         in
        _menhir_goto_option_header_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv444)) : 'freshtv446)) : 'freshtv448)) : 'freshtv450)) : 'freshtv452)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv399 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv397 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_stmt)), _, (xs : 'tv_list_stmt_)) = _menhir_stack in
        let _v : 'tv_list_stmt_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1467 "Parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)) : 'freshtv400)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv437 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_br ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv433 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv431 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_list_stmt_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_block = 
# 102 "Parser.mly"
                                         ( () )
# 1488 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv429) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((match _menhir_s with
            | MenhirState60 | MenhirState142 | MenhirState121 | MenhirState130 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv403) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_block) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv401) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_block) : 'tv_block) = _v in
                ((let _v : 'tv_stmt = 
# 93 "Parser.mly"
                    ( () )
# 1507 "Parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)) : 'freshtv404)
            | MenhirState59 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv427 * _menhir_state * 'tv_header) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_block) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv425 * _menhir_state * 'tv_header) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_block) : 'tv_block) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_header)), _, (_2 : 'tv_list_local_def_)) = _menhir_stack in
                let _v : (
# 41 "Parser.mly"
     (unit)
# 1523 "Parser.ml"
                ) = 
# 48 "Parser.mly"
                                      ( () )
# 1527 "Parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv423) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 41 "Parser.mly"
     (unit)
# 1535 "Parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                match _menhir_s with
                | MenhirState38 | MenhirState55 | MenhirState57 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv407 * _menhir_state * (
# 41 "Parser.mly"
     (unit)
# 1544 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv405 * _menhir_state * (
# 41 "Parser.mly"
     (unit)
# 1550 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (_1 : (
# 41 "Parser.mly"
     (unit)
# 1555 "Parser.ml"
                    ))) = _menhir_stack in
                    let _v : 'tv_local_def = 
# 80 "Parser.mly"
                       ( () )
# 1560 "Parser.ml"
                     in
                    _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)) : 'freshtv408)
                | MenhirState0 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv421 * _menhir_state * (
# 41 "Parser.mly"
     (unit)
# 1568 "Parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | T_eof ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv417 * _menhir_state * (
# 41 "Parser.mly"
     (unit)
# 1578 "Parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv415 * _menhir_state * (
# 41 "Parser.mly"
     (unit)
# 1584 "Parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, (_1 : (
# 41 "Parser.mly"
     (unit)
# 1589 "Parser.ml"
                        ))) = _menhir_stack in
                        let _2 = () in
                        let _v : (
# 40 "Parser.mly"
      (unit)
# 1595 "Parser.ml"
                        ) = 
# 46 "Parser.mly"
                             ( () )
# 1599 "Parser.ml"
                         in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv413) = _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let (_v : (
# 40 "Parser.mly"
      (unit)
# 1607 "Parser.ml"
                        )) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv411) = Obj.magic _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let (_v : (
# 40 "Parser.mly"
      (unit)
# 1615 "Parser.ml"
                        )) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv409) = Obj.magic _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let ((_1 : (
# 40 "Parser.mly"
      (unit)
# 1623 "Parser.ml"
                        )) : (
# 40 "Parser.mly"
      (unit)
# 1627 "Parser.ml"
                        )) = _v in
                        (Obj.magic _1 : 'freshtv410)) : 'freshtv412)) : 'freshtv414)) : 'freshtv416)) : 'freshtv418)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv419 * _menhir_state * (
# 41 "Parser.mly"
     (unit)
# 1637 "Parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
                | _ ->
                    _menhir_fail ()) : 'freshtv424)) : 'freshtv426)) : 'freshtv428)
            | _ ->
                _menhir_fail ()) : 'freshtv430)) : 'freshtv432)) : 'freshtv434)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv435 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)) : 'freshtv438)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv389) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv387) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, (_2 : 'tv_stmt)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_else_stmt = 
# 100 "Parser.mly"
                          ( () )
# 1669 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385) = _menhir_stack in
        let (_v : 'tv_else_stmt) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383) = Obj.magic _menhir_stack in
        let (_v : 'tv_else_stmt) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv381) = Obj.magic _menhir_stack in
        let ((x : 'tv_else_stmt) : 'tv_else_stmt) = _v in
        ((let _v : 'tv_option_else_stmt_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1683 "Parser.ml"
         in
        _menhir_goto_option_else_stmt_ _menhir_env _menhir_stack _v) : 'freshtv382)) : 'freshtv384)) : 'freshtv386)) : 'freshtv388)) : 'freshtv390)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv393 * _menhir_state) * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv391 * _menhir_state) * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_cond)), _, (_4 : 'tv_stmt)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_stmt = 
# 97 "Parser.mly"
                                     ( () )
# 1697 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)) : 'freshtv394)
    | MenhirState142 | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv395 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_identifier ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | T_if ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | T_left_br ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | T_return ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | T_semicolon ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | T_string ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | T_while ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | T_right_br ->
            _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv396)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv379 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_semicolon ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv375 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv373 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_option_expr_)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_stmt = 
# 98 "Parser.mly"
                                         ( () )
# 1749 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)) : 'freshtv376)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv377 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)) : 'freshtv380)

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_left_par ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_not ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | T_left_par ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | T_not ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_goto_l_value : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_l_value -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState133 | MenhirState127 | MenhirState123 | MenhirState61 | MenhirState97 | MenhirState117 | MenhirState114 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState63 | MenhirState64 | MenhirState65 | MenhirState89 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState73 | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv365 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_comma | T_div | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_mod | T_mul | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv361 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_l_value)) = _menhir_stack in
            let _v : 'tv_expr = 
# 118 "Parser.mly"
                      ( () )
# 1833 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv363 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv364)) : 'freshtv366)
    | MenhirState60 | MenhirState142 | MenhirState121 | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv371 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_assignment ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv367 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_chr ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_identifier ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_integer ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_left_par ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_minus ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_plus ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_string ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv368)
        | T_left_sqr ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv370)) : 'freshtv372)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_func_call_rr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_func_call_rr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv359 * _menhir_state)) * _menhir_state * 'tv_option_func_call_rr_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_right_par ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv355 * _menhir_state)) * _menhir_state * 'tv_option_func_call_rr_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv353 * _menhir_state)) * _menhir_state * 'tv_option_func_call_rr_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_option_func_call_rr_)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_func_call = 
# 104 "Parser.mly"
                                                                ( () )
# 1906 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_call) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState133 | MenhirState127 | MenhirState123 | MenhirState61 | MenhirState97 | MenhirState117 | MenhirState114 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState63 | MenhirState64 | MenhirState65 | MenhirState89 | MenhirState68 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState73 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv341 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv339 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_func_call)) = _menhir_stack in
            let _v : 'tv_expr = 
# 120 "Parser.mly"
                        ( () )
# 1923 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)) : 'freshtv342)
        | MenhirState60 | MenhirState142 | MenhirState121 | MenhirState130 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv349 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_semicolon ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv345 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv343 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_func_call)) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_stmt = 
# 94 "Parser.mly"
                                    ( () )
# 1943 "Parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv344)) : 'freshtv346)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv347 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)
        | _ ->
            _menhir_fail ()) : 'freshtv352)) : 'freshtv354)) : 'freshtv356)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv357 * _menhir_state)) * _menhir_state * 'tv_option_func_call_rr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv358)) : 'freshtv360)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv337) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 116 "Parser.mly"
                        ( () )
# 2048 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv335) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 117 "Parser.mly"
                    ( () )
# 2062 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_header_rr = 
# 54 "Parser.mly"
                            ( () )
# 2071 "Parser.ml"
     in
    _menhir_goto_header_rr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_ref ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_identifier ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_reduce48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_stmt_ = 
# 211 "<standard.mly>"
    ( [] )
# 2095 "Parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_left_par ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_not ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_l_value = 
# 113 "Parser.mly"
                       ( () )
# 2136 "Parser.ml"
     in
    _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)

and _menhir_run122 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv331) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_stmt = 
# 91 "Parser.mly"
                          ( () )
# 2150 "Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)

and _menhir_run123 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | T_left_par ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | T_semicolon ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv329) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState123 in
        ((let _v : 'tv_option_expr_ = 
# 114 "<standard.mly>"
    ( None )
# 2181 "Parser.ml"
         in
        _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_integer ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_left_par ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_minus ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_not ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_plus ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_left_par ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_chr ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_identifier ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_integer ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_left_par ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_minus ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_plus ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_string ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv321) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState68 in
            ((let _v : 'tv_option_func_call_rr_ = 
# 114 "<standard.mly>"
    ( None )
# 2249 "Parser.ml"
             in
            _menhir_goto_option_func_call_rr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv322)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv324)
    | T_and | T_assignment | T_comma | T_div | T_do | T_equal | T_greater | T_greater_eq | T_left_sqr | T_less | T_less_eq | T_minus | T_mod | T_mul | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_l_value = 
# 112 "Parser.mly"
                           ( () )
# 2264 "Parser.ml"
         in
        _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)

and _menhir_goto_fpar_type_r : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fpar_type_r -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * _menhir_state))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_type_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv301 * _menhir_state))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : 'tv_fpar_type_r) : 'tv_fpar_type_r) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_fpar_type_r = 
# 78 "Parser.mly"
                                                           ( () )
# 2294 "Parser.ml"
         in
        _menhir_goto_fpar_type_r _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)) : 'freshtv304)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv319 * _menhir_state * 'tv_data_type) * 'tv_option_squares_) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_type_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv317 * _menhir_state * 'tv_data_type) * 'tv_option_squares_) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_fpar_type_r) : 'tv_fpar_type_r) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_data_type)), (_2 : 'tv_option_squares_)) = _menhir_stack in
        let _v : 'tv_fpar_type = 
# 73 "Parser.mly"
                                             ( () )
# 2310 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_type) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv313 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_fpar_def_r)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_type) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv311 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_fpar_def_r)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : 'tv_fpar_type) : 'tv_fpar_type) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_option_T_ref_)), _, (_3 : 'tv_fpar_def_r)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : 'tv_fpar_def = 
# 57 "Parser.mly"
                                                               ( () )
# 2330 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_def) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState3 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_semicolon ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | T_right_par ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv306)
        | MenhirState33 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv307 * _menhir_state) * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_semicolon ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | T_right_par ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv308)
        | _ ->
            _menhir_fail ()) : 'freshtv310)) : 'freshtv312)) : 'freshtv314)) : 'freshtv316)) : 'freshtv318)) : 'freshtv320)
    | _ ->
        _menhir_fail ()

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_if ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_left_br ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_return ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_semicolon ->
        _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_string ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_while ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_right_br ->
        _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_goto_var_def_r : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_var_def_r -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv293 * _menhir_state)) * _menhir_state * 'tv_var_def_r) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv291 * _menhir_state)) * _menhir_state * 'tv_var_def_r) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_var_def_r)) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_var_def_r = 
# 89 "Parser.mly"
                                             ( () )
# 2413 "Parser.ml"
         in
        _menhir_goto_var_def_r _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)) : 'freshtv294)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv299 * _menhir_state)) * _menhir_state * 'tv_var_def_r) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv295 * _menhir_state)) * _menhir_state * 'tv_var_def_r) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | T_int ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv296)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv297 * _menhir_state)) * _menhir_state * 'tv_var_def_r) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)) : 'freshtv300)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mytype_r : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mytype_r -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv269 * _menhir_state))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_mytype_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : 'tv_mytype_r) : 'tv_mytype_r) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_mytype_r = 
# 68 "Parser.mly"
                                                        ( () )
# 2465 "Parser.ml"
         in
        _menhir_goto_mytype_r _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)) : 'freshtv270)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_mytype_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_mytype_r) : 'tv_mytype_r) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_data_type)) = _menhir_stack in
        let _v : 'tv_mytype = 
# 65 "Parser.mly"
                                 ( () )
# 2481 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_mytype) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv283 * _menhir_state)) * _menhir_state * 'tv_var_def_r)) * _menhir_state * 'tv_mytype) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv279 * _menhir_state)) * _menhir_state * 'tv_var_def_r)) * _menhir_state * 'tv_mytype) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv277 * _menhir_state)) * _menhir_state * 'tv_var_def_r)) * _menhir_state * 'tv_mytype) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : 'tv_var_def_r)), _, (_5 : 'tv_mytype)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_var_def = 
# 86 "Parser.mly"
                                                                      ( () )
# 2507 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_var_def) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv273) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_var_def) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_var_def) : 'tv_var_def) = _v in
            ((let _v : 'tv_local_def = 
# 82 "Parser.mly"
                      ( () )
# 2524 "Parser.ml"
             in
            _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)) : 'freshtv274)) : 'freshtv276)) : 'freshtv278)) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv281 * _menhir_state)) * _menhir_state * 'tv_var_def_r)) * _menhir_state * 'tv_mytype) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)) : 'freshtv286)) : 'freshtv288)) : 'freshtv290)
    | _ ->
        _menhir_fail ()

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_fpar_type_r = 
# 77 "Parser.mly"
                            ( () )
# 2542 "Parser.ml"
     in
    _menhir_goto_fpar_type_r _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_integer ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv263 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_sqr ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_left_sqr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | T_right_par | T_semicolon ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv260)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv265 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)

and _menhir_goto_list_local_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_local_def_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv255 * _menhir_state * 'tv_local_def) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state * 'tv_local_def) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_local_def)), _, (xs : 'tv_list_local_def_)) = _menhir_stack in
        let _v : 'tv_list_local_def_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2600 "Parser.ml"
         in
        _menhir_goto_list_local_def_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)) : 'freshtv256)
    | MenhirState38 | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state * 'tv_header) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_br ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv258)
    | _ ->
        _menhir_fail ()

and _menhir_reduce80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_var_def_r = 
# 88 "Parser.mly"
                            ( () )
# 2623 "Parser.ml"
     in
    _menhir_goto_var_def_r _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | T_colon ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv250)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_mytype_r = 
# 67 "Parser.mly"
                            ( () )
# 2660 "Parser.ml"
     in
    _menhir_goto_mytype_r _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_integer ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_sqr ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv241 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_left_sqr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | T_semicolon ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv242)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv243 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)) : 'freshtv246)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)

and _menhir_goto_option_squares_ : _menhir_env -> 'ttv_tail -> 'tv_option_squares_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv239 * _menhir_state * 'tv_data_type) * 'tv_option_squares_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_left_sqr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_right_par | T_semicolon ->
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv240)

and _menhir_goto_fpar_def_r : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fpar_def_r -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state)) * _menhir_state * 'tv_fpar_def_r) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv229 * _menhir_state)) * _menhir_state * 'tv_fpar_def_r) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_fpar_def_r)) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_fpar_def_r = 
# 60 "Parser.mly"
                                              ( () )
# 2737 "Parser.ml"
         in
        _menhir_goto_fpar_def_r _menhir_env _menhir_stack _menhir_s _v) : 'freshtv230)) : 'freshtv232)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_fpar_def_r) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_fpar_def_r) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | T_int ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv234)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_fpar_def_r) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)) : 'freshtv238)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_local_def : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_local_def -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_local_def) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_fun ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_var ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_left_br ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv228)

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_local_def_ = 
# 211 "<standard.mly>"
    ( [] )
# 2799 "Parser.ml"
     in
    _menhir_goto_list_local_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | T_colon ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv224)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)

and _menhir_goto_data_type : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_data_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_data_type)) = _menhir_stack in
        let _v : 'tv_ret_type = 
# 70 "Parser.mly"
                        ( () )
# 2844 "Parser.ml"
         in
        _menhir_goto_ret_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)) : 'freshtv200)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_right_sqr ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv207) = Obj.magic _menhir_stack in
                ((let _2 = () in
                let _1 = () in
                let _v : 'tv_squares = 
# 75 "Parser.mly"
                                        ( () )
# 2870 "Parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv205) = _menhir_stack in
                let (_v : 'tv_squares) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
                let (_v : 'tv_squares) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
                let ((x : 'tv_squares) : 'tv_squares) = _v in
                ((let _v : 'tv_option_squares_ = 
# 116 "<standard.mly>"
    ( Some x )
# 2884 "Parser.ml"
                 in
                _menhir_goto_option_squares_ _menhir_env _menhir_stack _v) : 'freshtv202)) : 'freshtv204)) : 'freshtv206)) : 'freshtv208)) : 'freshtv210)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv211) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv212)) : 'freshtv214)
        | T_right_par | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215) = Obj.magic _menhir_stack in
            ((let _v : 'tv_option_squares_ = 
# 114 "<standard.mly>"
    ( None )
# 2899 "Parser.ml"
             in
            _menhir_goto_option_squares_ _menhir_env _menhir_stack _v) : 'freshtv216)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | T_semicolon ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv222)
    | _ ->
        _menhir_fail ()

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_fpar_def_r = 
# 59 "Parser.mly"
                            ( () )
# 2931 "Parser.ml"
     in
    _menhir_goto_fpar_def_r _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_colon ->
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16) : 'freshtv194)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)

and _menhir_goto_ret_type : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ret_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv191 * _menhir_state))) * _menhir_state * 'tv_option_header_r_))) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_ret_type) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv189 * _menhir_state))) * _menhir_state * 'tv_option_header_r_))) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((_7 : 'tv_ret_type) : 'tv_ret_type) = _v in
    ((let ((_menhir_stack, _menhir_s), _, (_4 : 'tv_option_header_r_)) = _menhir_stack in
    let _6 = () in
    let _5 = () in
    let _3 = () in
    let _2 = () in
    let _1 = () in
    let _v : 'tv_header = 
# 50 "Parser.mly"
                                                                                   ( () )
# 2982 "Parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv187) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_header) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_fun ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | T_var ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | T_left_br ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv174)
    | MenhirState38 | MenhirState57 | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_fun ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState57 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_header)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_func_decl = 
# 84 "Parser.mly"
                                 ( () )
# 3027 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_func_decl) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_func_decl) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_func_decl) : 'tv_func_decl) = _v in
            ((let _v : 'tv_local_def = 
# 81 "Parser.mly"
                        ( () )
# 3044 "Parser.ml"
             in
            _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)) : 'freshtv182)) : 'freshtv184)
        | T_var ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_left_br ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv186)
    | _ ->
        _menhir_fail ()) : 'freshtv188)) : 'freshtv190)) : 'freshtv192)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_data_type = 
# 62 "Parser.mly"
                    ( () )
# 3068 "Parser.ml"
     in
    _menhir_goto_data_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_data_type = 
# 63 "Parser.mly"
                     ( () )
# 3082 "Parser.ml"
     in
    _menhir_goto_data_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv170)

and _menhir_goto_option_T_ref_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_T_ref_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_option_T_ref_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_option_T_ref_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | T_colon ->
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv164)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state * 'tv_option_T_ref_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_T_ref_ = 
# 114 "<standard.mly>"
    ( None )
# 3121 "Parser.ml"
     in
    _menhir_goto_option_T_ref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_header_r_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_header_r_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv161 * _menhir_state))) * _menhir_state * 'tv_option_header_r_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_right_par ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv157 * _menhir_state))) * _menhir_state * 'tv_option_header_r_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv153 * _menhir_state))) * _menhir_state * 'tv_option_header_r_)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | T_int ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | T_nothing ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState7 in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                ((let _1 = () in
                let _v : 'tv_ret_type = 
# 71 "Parser.mly"
                        ( () )
# 3161 "Parser.ml"
                 in
                _menhir_goto_ret_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv154)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv155 * _menhir_state))) * _menhir_state * 'tv_option_header_r_)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)) : 'freshtv158)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv159 * _menhir_state))) * _menhir_state * 'tv_option_header_r_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv147) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let x = () in
    let _v : 'tv_option_T_ref_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3193 "Parser.ml"
     in
    _menhir_goto_option_T_ref_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv15 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * 'tv_l_value)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv20)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv27 * _menhir_state) * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv39 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv43 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv55 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv77 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * 'tv_l_value)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state * 'tv_header) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state * 'tv_local_def) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv117 * _menhir_state)) * _menhir_state * 'tv_var_def_r)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * _menhir_state) * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv131 * _menhir_state))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state * 'tv_data_type) * 'tv_option_squares_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv135 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_fpar_def_r)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv137 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state * 'tv_option_T_ref_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv141 * _menhir_state))) * _menhir_state * 'tv_option_header_r_))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv143 * _menhir_state))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv146)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_ref ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | T_right_par ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState3 in
                ((let _v : 'tv_option_header_r_ = 
# 114 "<standard.mly>"
    ( None )
# 3561 "Parser.ml"
                 in
                _menhir_goto_option_header_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)
            | T_identifier ->
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv7 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)

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
# 40 "Parser.mly"
      (unit)
# 3600 "Parser.ml"
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
  

# 3629 "Parser.ml"
