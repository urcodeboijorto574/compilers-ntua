
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
  | MenhirState147
  | MenhirState139
  | MenhirState138
  | MenhirState135
  | MenhirState133
  | MenhirState131
  | MenhirState130
  | MenhirState127
  | MenhirState125
  | MenhirState121
  | MenhirState119
  | MenhirState118
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState112
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
  | MenhirState97
  | MenhirState95
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState89
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState71
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState54
  | MenhirState50
  | MenhirState47
  | MenhirState44
  | MenhirState41
  | MenhirState39
  | MenhirState36
  | MenhirState33
  | MenhirState32
  | MenhirState29
  | MenhirState25
  | MenhirState20
  | MenhirState18
  | MenhirState14
  | MenhirState7
  | MenhirState3
  | MenhirState0

let rec _menhir_run118 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_cond -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf stderr "State 118:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 102\n%!";
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_not ->
        Printf.fprintf stderr "Shifting (T_not) to state 101\n%!";
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_run121 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_cond -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf stderr "State 121:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 102\n%!";
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_not ->
        Printf.fprintf stderr "Shifting (T_not) to state 101\n%!";
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_goto_list_func_call_r_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_func_call_r_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv619 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_func_call_r_) = _v in
        ((Printf.fprintf stderr "State 94:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv617 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_list_func_call_r_) : 'tv_list_func_call_r_) = _v in
        ((Printf.fprintf stderr "Reducing production func_call_rr -> expr list(func_call_r) \n%!";
        let (_menhir_stack, _menhir_s, (_1 : 'tv_expr)) = _menhir_stack in
        let _v : 'tv_func_call_rr = 
# 118 "Parser.mly"
                                   ( () )
# 225 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv615) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_call_rr) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv613) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_call_rr) = _v in
        ((Printf.fprintf stderr "State 90:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv611) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_func_call_rr) : 'tv_func_call_rr) = _v in
        ((Printf.fprintf stderr "Reducing production option(func_call_rr) -> func_call_rr \n%!";
        let _v : 'tv_option_func_call_rr_ = 
# 116 "<standard.mly>"
    ( Some x )
# 244 "Parser.ml"
         in
        _menhir_goto_option_func_call_rr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv612)) : 'freshtv614)) : 'freshtv616)) : 'freshtv618)) : 'freshtv620)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv623 * _menhir_state * 'tv_func_call_r) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_func_call_r_) = _v in
        ((Printf.fprintf stderr "State 96:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv621 * _menhir_state * 'tv_func_call_r) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_func_call_r_) : 'tv_list_func_call_r_) = _v in
        ((Printf.fprintf stderr "Reducing production list(func_call_r) -> func_call_r list(func_call_r) \n%!";
        let (_menhir_stack, _menhir_s, (x : 'tv_func_call_r)) = _menhir_stack in
        let _v : 'tv_list_func_call_r_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 262 "Parser.ml"
         in
        _menhir_goto_list_func_call_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv622)) : 'freshtv624)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cond : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_cond -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv583 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 116:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            Printf.fprintf stderr "Shifting (T_and) to state 121\n%!";
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | T_or ->
            Printf.fprintf stderr "Shifting (T_or) to state 118\n%!";
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | T_right_par ->
            Printf.fprintf stderr "Shifting (T_right_par) to state 117\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv579 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 117:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv577 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production cond -> T_left_par cond T_right_par \n%!";
            let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_cond)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_cond = 
# 142 "Parser.mly"
                                          ( (Printf.printf("cond\n")) )
# 300 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)) : 'freshtv580)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv581 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv582)) : 'freshtv584)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv589 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 120:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            Printf.fprintf stderr "Shifting (T_and) to state 121\n%!";
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv585 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production cond -> cond T_or cond \n%!";
            let ((_menhir_stack, _menhir_s, (_1 : 'tv_cond)), _, (_3 : 'tv_cond)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 145 "Parser.mly"
                             ( (Printf.printf("cond\n")) )
# 330 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv587 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv588)) : 'freshtv590)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv593 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 122:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv591 * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production cond -> cond T_and cond \n%!";
        let ((_menhir_stack, _menhir_s, (_1 : 'tv_cond)), _, (_3 : 'tv_cond)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_cond = 
# 144 "Parser.mly"
                              ( (Printf.printf("cond\n")) )
# 353 "Parser.ml"
         in
        _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)) : 'freshtv594)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv597 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 123:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv595 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production cond -> T_not cond \n%!";
        let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_cond)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_cond = 
# 143 "Parser.mly"
                         ( (Printf.printf("cond\n")) )
# 368 "Parser.ml"
         in
        _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv596)) : 'freshtv598)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv603 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 124:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            Printf.fprintf stderr "Shifting (T_and) to state 121\n%!";
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | T_do ->
            Printf.fprintf stderr "Shifting (T_do) to state 125\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv599 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 125:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier ->
                Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | T_if ->
                Printf.fprintf stderr "Shifting (T_if) to state 131\n%!";
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | T_left_br ->
                Printf.fprintf stderr "Shifting (T_left_br) to state 63\n%!";
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | T_return ->
                Printf.fprintf stderr "Shifting (T_return) to state 127\n%!";
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | T_semicolon ->
                Printf.fprintf stderr "Shifting (T_semicolon) to state 126\n%!";
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | T_string ->
                Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | T_while ->
                Printf.fprintf stderr "Shifting (T_while) to state 64\n%!";
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv600)
        | T_or ->
            Printf.fprintf stderr "Shifting (T_or) to state 118\n%!";
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv601 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv602)) : 'freshtv604)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv609 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 132:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_and ->
            Printf.fprintf stderr "Shifting (T_and) to state 121\n%!";
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | T_or ->
            Printf.fprintf stderr "Shifting (T_or) to state 118\n%!";
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | T_then ->
            Printf.fprintf stderr "Shifting (T_then) to state 133\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv605 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 133:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier ->
                Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_if ->
                Printf.fprintf stderr "Shifting (T_if) to state 131\n%!";
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_left_br ->
                Printf.fprintf stderr "Shifting (T_left_br) to state 63\n%!";
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_return ->
                Printf.fprintf stderr "Shifting (T_return) to state 127\n%!";
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_semicolon ->
                Printf.fprintf stderr "Shifting (T_semicolon) to state 126\n%!";
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_string ->
                Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | T_while ->
                Printf.fprintf stderr "Shifting (T_while) to state 64\n%!";
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv606)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv607 * _menhir_state) * _menhir_state * 'tv_cond) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv608)) : 'freshtv610)
    | _ ->
        _menhir_fail ()

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 104:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 106:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 108:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 110:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run112 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 112:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 114:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run98 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 98:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv575 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    ((Printf.fprintf stderr "Reducing production expr -> T_left_par expr T_right_par \n%!";
    let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_expr = 
# 132 "Parser.mly"
                                          ( (Printf.printf("expr\n")) )
# 702 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv576)

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production list(func_call_r) -> \n%!";
    let _v : 'tv_list_func_call_r_ = 
# 211 "<standard.mly>"
    ( [] )
# 712 "Parser.ml"
     in
    _menhir_goto_list_func_call_r_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 92:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 80:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 82:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 84:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 88:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 86:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_goto_option_else_stmt_ : _menhir_env -> 'ttv_tail -> 'tv_option_else_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv573 * _menhir_state) * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
    let (_v : 'tv_option_else_stmt_) = _v in
    ((Printf.fprintf stderr "State 144:\n%!";
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv571 * _menhir_state) * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
    let ((_5 : 'tv_option_else_stmt_) : 'tv_option_else_stmt_) = _v in
    ((Printf.fprintf stderr "Reducing production stmt -> T_if cond T_then stmt option(else_stmt) \n%!";
    let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_cond)), _, (_4 : 'tv_stmt)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_stmt = 
# 107 "Parser.mly"
                                               ( (Printf.printf("stmt\n")) )
# 936 "Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv572)) : 'freshtv574)

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_l_value -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf stderr "State 76:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv491 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 78:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_right_sqr ->
            Printf.fprintf stderr "Shifting (T_right_sqr) to state 79\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv489 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState78 in
            ((Printf.fprintf stderr "State 79:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv487 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((Printf.fprintf stderr "Reducing production l_value -> l_value T_left_sqr expr T_right_sqr \n%!";
            let ((_menhir_stack, _menhir_s, (_1 : 'tv_l_value)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_l_value = 
# 126 "Parser.mly"
                                                  ( (Printf.printf("l_value\n")) )
# 1016 "Parser.ml"
             in
            _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv488)) : 'freshtv490)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv492)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv495 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 81:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv493 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production expr -> expr T_plus expr \n%!";
            let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 136 "Parser.mly"
                               ( (Printf.printf("expr\n")) )
# 1049 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv496)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv499 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 83:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv497 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production expr -> expr T_mul expr \n%!";
        let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 138 "Parser.mly"
                              ( (Printf.printf("expr\n")) )
# 1069 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)) : 'freshtv500)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv503 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 85:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv501 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production expr -> expr T_mod expr \n%!";
        let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 140 "Parser.mly"
                              ( (Printf.printf("expr\n")) )
# 1084 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv502)) : 'freshtv504)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv507 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 87:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv505 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production expr -> expr T_div expr \n%!";
        let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 139 "Parser.mly"
                              ( (Printf.printf("expr\n")) )
# 1099 "Parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv511 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 89:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv509 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production expr -> expr T_minus expr \n%!";
            let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 137 "Parser.mly"
                                ( (Printf.printf("expr\n")) )
# 1127 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv512)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv513 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 91:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            Printf.fprintf stderr "Shifting (T_comma) to state 92\n%!";
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_right_par ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv514)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv521 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 93:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_comma | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv519 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production func_call_r -> T_comma expr \n%!";
            let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_func_call_r = 
# 121 "Parser.mly"
                           ( () )
# 1198 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv517) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_func_call_r) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv515 * _menhir_state * 'tv_func_call_r) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 95:\n%!";
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_comma ->
                Printf.fprintf stderr "Shifting (T_comma) to state 92\n%!";
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | T_right_par ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv516)) : 'freshtv518)) : 'freshtv520)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv522)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv523 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 97:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_right_par ->
            Printf.fprintf stderr "Shifting (T_right_par) to state 98\n%!";
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv524)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv527 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 99:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv525 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production expr -> T_minus expr \n%!";
            let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 135 "Parser.mly"
                           ( (Printf.printf("expr\n")) )
# 1281 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv528)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv531 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 100:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_and | T_comma | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv529 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production expr -> T_plus expr \n%!";
            let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 134 "Parser.mly"
                          ( (Printf.printf("expr\n")) )
# 1314 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv530)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv532)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv533 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 103:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_equal ->
            Printf.fprintf stderr "Shifting (T_equal) to state 114\n%!";
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_greater ->
            Printf.fprintf stderr "Shifting (T_greater) to state 112\n%!";
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_greater_eq ->
            Printf.fprintf stderr "Shifting (T_greater_eq) to state 110\n%!";
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_less ->
            Printf.fprintf stderr "Shifting (T_less) to state 108\n%!";
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_less_eq ->
            Printf.fprintf stderr "Shifting (T_less_eq) to state 106\n%!";
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_not_equal ->
            Printf.fprintf stderr "Shifting (T_not_equal) to state 104\n%!";
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_right_par ->
            Printf.fprintf stderr "Shifting (T_right_par) to state 98\n%!";
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv534)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv537 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 105:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv535 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production cond -> expr T_not_equal expr \n%!";
            let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 151 "Parser.mly"
                                    ( (Printf.printf("cond\n")) )
# 1401 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv536)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv538)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 107:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv539 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production cond -> expr T_less_eq expr \n%!";
            let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 148 "Parser.mly"
                                  ( (Printf.printf("cond\n")) )
# 1440 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv540)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv542)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv545 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 109:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv543 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production cond -> expr T_less expr \n%!";
            let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 147 "Parser.mly"
                               ( (Printf.printf("cond\n")) )
# 1479 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv544)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv546)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv549 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 111:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv547 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production cond -> expr T_greater_eq expr \n%!";
            let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 150 "Parser.mly"
                                     ( (Printf.printf("cond\n")) )
# 1518 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv550)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv553 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 113:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv551 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production cond -> expr T_greater expr \n%!";
            let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 149 "Parser.mly"
                                  ( (Printf.printf("cond\n")) )
# 1557 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv554)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv557 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 115:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_and | T_do | T_or | T_right_par | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv555 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production cond -> expr T_equal expr \n%!";
            let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cond = 
# 146 "Parser.mly"
                                ( (Printf.printf("cond\n")) )
# 1596 "Parser.ml"
             in
            _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv556)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv558)
    | MenhirState131 | MenhirState64 | MenhirState101 | MenhirState121 | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 119:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_equal ->
            Printf.fprintf stderr "Shifting (T_equal) to state 114\n%!";
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_greater ->
            Printf.fprintf stderr "Shifting (T_greater) to state 112\n%!";
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_greater_eq ->
            Printf.fprintf stderr "Shifting (T_greater_eq) to state 110\n%!";
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_less ->
            Printf.fprintf stderr "Shifting (T_less) to state 108\n%!";
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_less_eq ->
            Printf.fprintf stderr "Shifting (T_less_eq) to state 106\n%!";
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_not_equal ->
            Printf.fprintf stderr "Shifting (T_not_equal) to state 104\n%!";
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119) : 'freshtv560)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv563 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 130:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_semicolon ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv561 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production option(expr) -> expr \n%!";
            let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_option_expr_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1679 "Parser.ml"
             in
            _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv562)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130) : 'freshtv564)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv569 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 139:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_div ->
            Printf.fprintf stderr "Shifting (T_div) to state 86\n%!";
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 88\n%!";
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_mod ->
            Printf.fprintf stderr "Shifting (T_mod) to state 84\n%!";
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_mul ->
            Printf.fprintf stderr "Shifting (T_mul) to state 82\n%!";
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 80\n%!";
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_semicolon ->
            Printf.fprintf stderr "Shifting (T_semicolon) to state 140\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv567 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState139 in
            ((Printf.fprintf stderr "State 140:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv565 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((Printf.fprintf stderr "Reducing production stmt -> l_value T_assignment expr T_semicolon \n%!";
            let ((_menhir_stack, _menhir_s, (_1 : 'tv_l_value)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_stmt = 
# 104 "Parser.mly"
                                                    ( (Printf.printf("stmt\n")) )
# 1726 "Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv566)) : 'freshtv568)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv570)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 148:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production list(stmt) -> stmt list(stmt) \n%!";
        let ((_menhir_stack, _menhir_s, (x : 'tv_stmt)), _, (xs : 'tv_list_stmt_)) = _menhir_stack in
        let _v : 'tv_list_stmt_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1752 "Parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv485 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 149:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_br ->
            Printf.fprintf stderr "Shifting (T_right_br) to state 150\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv481 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 150:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv479 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production block -> T_left_br list(stmt) T_right_br \n%!";
            let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_list_stmt_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_block = 
# 114 "Parser.mly"
                                         ( (Printf.printf("block\n")) )
# 1777 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv477) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((match _menhir_s with
            | MenhirState63 | MenhirState147 | MenhirState125 | MenhirState133 | MenhirState135 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_block) = _v in
                ((Printf.fprintf stderr "State 143:\n%!";
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_block) : 'tv_block) = _v in
                ((Printf.fprintf stderr "Reducing production stmt -> block \n%!";
                let _v : 'tv_stmt = 
# 105 "Parser.mly"
                    ( (Printf.printf("stmt\n")) )
# 1798 "Parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv450)) : 'freshtv452)
            | MenhirState62 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv475 * _menhir_state * 'tv_header) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_block) = _v in
                ((Printf.fprintf stderr "State 151:\n%!";
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv473 * _menhir_state * 'tv_header) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_block) : 'tv_block) = _v in
                ((Printf.fprintf stderr "Reducing production func_def -> header list(local_def) block \n%!";
                let ((_menhir_stack, _menhir_s, (_1 : 'tv_header)), _, (_2 : 'tv_list_local_def_)) = _menhir_stack in
                let _v : 'tv_func_def = 
# 55 "Parser.mly"
                                      ( (Printf.printf("func_def\n")) )
# 1816 "Parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv471) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_func_def) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                match _menhir_s with
                | MenhirState39 | MenhirState58 | MenhirState60 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv455 * _menhir_state * 'tv_func_def) = Obj.magic _menhir_stack in
                    ((Printf.fprintf stderr "State 152:\n%!";
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv453 * _menhir_state * 'tv_func_def) = Obj.magic _menhir_stack in
                    ((Printf.fprintf stderr "Reducing production local_def -> func_def \n%!";
                    let (_menhir_stack, _menhir_s, (_1 : 'tv_func_def)) = _menhir_stack in
                    let _v : 'tv_local_def = 
# 92 "Parser.mly"
                       ( (Printf.printf("local_def\n")) )
# 1835 "Parser.ml"
                     in
                    _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)) : 'freshtv456)
                | MenhirState0 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv469 * _menhir_state * 'tv_func_def) = Obj.magic _menhir_stack in
                    ((Printf.fprintf stderr "State 154:\n%!";
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | T_eof ->
                        Printf.fprintf stderr "Shifting (T_eof) to state 155\n%!";
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv465 * _menhir_state * 'tv_func_def) = Obj.magic _menhir_stack in
                        ((Printf.fprintf stderr "State 155:\n%!";
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv463 * _menhir_state * 'tv_func_def) = Obj.magic _menhir_stack in
                        ((Printf.fprintf stderr "Reducing production program -> func_def T_eof \n%!";
                        let (_menhir_stack, _menhir_s, (_1 : 'tv_func_def)) = _menhir_stack in
                        let _2 = () in
                        let _v : (
# 47 "Parser.mly"
      (unit)
# 1858 "Parser.ml"
                        ) = 
# 53 "Parser.mly"
                             ( (Printf.printf("program\n")) )
# 1862 "Parser.ml"
                         in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv461) = _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let (_v : (
# 47 "Parser.mly"
      (unit)
# 1870 "Parser.ml"
                        )) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv459) = Obj.magic _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let (_v : (
# 47 "Parser.mly"
      (unit)
# 1878 "Parser.ml"
                        )) = _v in
                        ((Printf.fprintf stderr "State 38:\n%!";
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv457) = Obj.magic _menhir_stack in
                        let (_menhir_s : _menhir_state) = _menhir_s in
                        let ((_1 : (
# 47 "Parser.mly"
      (unit)
# 1887 "Parser.ml"
                        )) : (
# 47 "Parser.mly"
      (unit)
# 1891 "Parser.ml"
                        )) = _v in
                        ((Printf.fprintf stderr "Accepting\n%!";
                        Obj.magic _1) : 'freshtv458)) : 'freshtv460)) : 'freshtv462)) : 'freshtv464)) : 'freshtv466)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        Printf.fprintf stderr "Initiating error handling\n%!";
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv467 * _menhir_state * 'tv_func_def) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)) : 'freshtv470)
                | _ ->
                    _menhir_fail ()) : 'freshtv472)) : 'freshtv474)) : 'freshtv476)
            | _ ->
                _menhir_fail ()) : 'freshtv478)) : 'freshtv480)) : 'freshtv482)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv483 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv484)) : 'freshtv486)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv427 * _menhir_state) * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 134:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_else ->
            Printf.fprintf stderr "Shifting (T_else) to state 135\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv421) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 135:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_identifier ->
                Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | T_if ->
                Printf.fprintf stderr "Shifting (T_if) to state 131\n%!";
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | T_left_br ->
                Printf.fprintf stderr "Shifting (T_left_br) to state 63\n%!";
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | T_return ->
                Printf.fprintf stderr "Shifting (T_return) to state 127\n%!";
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | T_semicolon ->
                Printf.fprintf stderr "Shifting (T_semicolon) to state 126\n%!";
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | T_string ->
                Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | T_while ->
                Printf.fprintf stderr "Shifting (T_while) to state 64\n%!";
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135) : 'freshtv422)
        | T_identifier | T_if | T_left_br | T_return | T_right_br | T_semicolon | T_string | T_while ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv423) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production option(else_stmt) -> \n%!";
            let _v : 'tv_option_else_stmt_ = 
# 114 "<standard.mly>"
    ( None )
# 1970 "Parser.ml"
             in
            _menhir_goto_option_else_stmt_ _menhir_env _menhir_stack _v) : 'freshtv424)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv425 * _menhir_state) * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv437) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 136:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv435) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production else_stmt -> T_else stmt \n%!";
        let (_menhir_stack, _, (_2 : 'tv_stmt)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_else_stmt = 
# 112 "Parser.mly"
                            ( () )
# 1993 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433) = _menhir_stack in
        let (_v : 'tv_else_stmt) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv431) = Obj.magic _menhir_stack in
        let (_v : 'tv_else_stmt) = _v in
        ((Printf.fprintf stderr "State 145:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv429) = Obj.magic _menhir_stack in
        let ((x : 'tv_else_stmt) : 'tv_else_stmt) = _v in
        ((Printf.fprintf stderr "Reducing production option(else_stmt) -> else_stmt \n%!";
        let _v : 'tv_option_else_stmt_ = 
# 116 "<standard.mly>"
    ( Some x )
# 2009 "Parser.ml"
         in
        _menhir_goto_option_else_stmt_ _menhir_env _menhir_stack _v) : 'freshtv430)) : 'freshtv432)) : 'freshtv434)) : 'freshtv436)) : 'freshtv438)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv441 * _menhir_state) * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 146:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv439 * _menhir_state) * _menhir_state * 'tv_cond)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production stmt -> T_while cond T_do stmt \n%!";
        let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_cond)), _, (_4 : 'tv_stmt)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_stmt = 
# 109 "Parser.mly"
                                     ( (Printf.printf("stmt\n")) )
# 2025 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv440)) : 'freshtv442)
    | MenhirState147 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 147:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_identifier ->
            Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_if ->
            Printf.fprintf stderr "Shifting (T_if) to state 131\n%!";
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_left_br ->
            Printf.fprintf stderr "Shifting (T_left_br) to state 63\n%!";
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_return ->
            Printf.fprintf stderr "Shifting (T_return) to state 127\n%!";
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_semicolon ->
            Printf.fprintf stderr "Shifting (T_semicolon) to state 126\n%!";
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_string ->
            Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_while ->
            Printf.fprintf stderr "Shifting (T_while) to state 64\n%!";
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_right_br ->
            _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv444)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv419 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
    ((Printf.fprintf stderr "State 128:\n%!";
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_semicolon ->
        Printf.fprintf stderr "Shifting (T_semicolon) to state 129\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv415 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 129:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv413 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production stmt -> T_return option(expr) T_semicolon \n%!";
        let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_option_expr_)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_stmt = 
# 110 "Parser.mly"
                                         ( (Printf.printf("stmt\n")) )
# 2090 "Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)) : 'freshtv416)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv417 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)) : 'freshtv420)

and _menhir_run101 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 101:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 102\n%!";
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_not ->
        Printf.fprintf stderr "Shifting (T_not) to state 101\n%!";
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 102:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 102\n%!";
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_not ->
        Printf.fprintf stderr "Shifting (T_not) to state 101\n%!";
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_goto_l_value : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_l_value -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState138 | MenhirState131 | MenhirState127 | MenhirState64 | MenhirState101 | MenhirState121 | MenhirState118 | MenhirState114 | MenhirState112 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState66 | MenhirState67 | MenhirState68 | MenhirState92 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState76 | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv405 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 75:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            Printf.fprintf stderr "Shifting (T_left_sqr) to state 76\n%!";
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | T_and | T_comma | T_div | T_do | T_equal | T_greater | T_greater_eq | T_less | T_less_eq | T_minus | T_mod | T_mul | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv401 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production expr -> l_value \n%!";
            let (_menhir_stack, _menhir_s, (_1 : 'tv_l_value)) = _menhir_stack in
            let _v : 'tv_expr = 
# 131 "Parser.mly"
                      ( (Printf.printf("expr\n")) )
# 2198 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv403 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)) : 'freshtv406)
    | MenhirState63 | MenhirState147 | MenhirState125 | MenhirState133 | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 137:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_assignment ->
            Printf.fprintf stderr "Shifting (T_assignment) to state 138\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 138:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_chr ->
                Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_identifier ->
                Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_integer ->
                Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_left_par ->
                Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_minus ->
                Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_plus ->
                Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_string ->
                Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138) : 'freshtv408)
        | T_left_sqr ->
            Printf.fprintf stderr "Shifting (T_left_sqr) to state 76\n%!";
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv409 * _menhir_state * 'tv_l_value) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)) : 'freshtv412)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_func_call_rr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_func_call_rr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv399 * _menhir_state)) * _menhir_state * 'tv_option_func_call_rr_) = Obj.magic _menhir_stack in
    ((Printf.fprintf stderr "State 73:\n%!";
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_right_par ->
        Printf.fprintf stderr "Shifting (T_right_par) to state 74\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state)) * _menhir_state * 'tv_option_func_call_rr_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 74:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv393 * _menhir_state)) * _menhir_state * 'tv_option_func_call_rr_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production func_call -> T_identifier T_left_par option(func_call_rr) T_right_par \n%!";
        let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_option_func_call_rr_)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_func_call = 
# 116 "Parser.mly"
                                                                ( (Printf.printf("func_call\n")) )
# 2289 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_call) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState138 | MenhirState131 | MenhirState127 | MenhirState64 | MenhirState101 | MenhirState121 | MenhirState118 | MenhirState114 | MenhirState112 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState66 | MenhirState67 | MenhirState68 | MenhirState92 | MenhirState71 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState76 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv381 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 77:\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production expr -> func_call \n%!";
            let (_menhir_stack, _menhir_s, (_1 : 'tv_func_call)) = _menhir_stack in
            let _v : 'tv_expr = 
# 133 "Parser.mly"
                        ( (Printf.printf("expr\n")) )
# 2308 "Parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
        | MenhirState63 | MenhirState147 | MenhirState125 | MenhirState133 | MenhirState135 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv389 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 141:\n%!";
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_semicolon ->
                Printf.fprintf stderr "Shifting (T_semicolon) to state 142\n%!";
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv385 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
                ((Printf.fprintf stderr "State 142:\n%!";
                let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv383 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
                ((Printf.fprintf stderr "Reducing production stmt -> func_call T_semicolon \n%!";
                let (_menhir_stack, _menhir_s, (_1 : 'tv_func_call)) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_stmt = 
# 106 "Parser.mly"
                                    ( (Printf.printf("stmt\n")) )
# 2332 "Parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)) : 'freshtv386)
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv387 * _menhir_state * 'tv_func_call) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)
        | _ ->
            _menhir_fail ()) : 'freshtv392)) : 'freshtv394)) : 'freshtv396)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state)) * _menhir_state * 'tv_option_func_call_rr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)) : 'freshtv400)

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 66:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 67:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 68:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 69:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv377) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf stderr "Reducing production expr -> T_integer \n%!";
    let _1 = () in
    let _v : 'tv_expr = 
# 129 "Parser.mly"
                        ( (Printf.printf("expr\n")) )
# 2468 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 72:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv375) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf stderr "Reducing production expr -> T_chr \n%!";
    let _1 = () in
    let _v : 'tv_expr = 
# 130 "Parser.mly"
                    ( (Printf.printf("expr\n")) )
# 2484 "Parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)

and _menhir_goto_list_header_rr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_header_rr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_header_rr_) = _v in
        ((Printf.fprintf stderr "State 35:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv367 * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_list_header_rr_) : 'tv_list_header_rr_) = _v in
        ((Printf.fprintf stderr "Reducing production header_r -> fpar_def list(header_rr) \n%!";
        let (_menhir_stack, _menhir_s, (_1 : 'tv_fpar_def)) = _menhir_stack in
        let _v : 'tv_header_r = 
# 59 "Parser.mly"
                                   ( () )
# 2506 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv365) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_header_r) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_header_r) = _v in
        ((Printf.fprintf stderr "State 31:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv361) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_header_r) : 'tv_header_r) = _v in
        ((Printf.fprintf stderr "Reducing production option(header_r) -> header_r \n%!";
        let _v : 'tv_option_header_r_ = 
# 116 "<standard.mly>"
    ( Some x )
# 2525 "Parser.ml"
         in
        _menhir_goto_option_header_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)) : 'freshtv364)) : 'freshtv366)) : 'freshtv368)) : 'freshtv370)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373 * _menhir_state * 'tv_header_rr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_header_rr_) = _v in
        ((Printf.fprintf stderr "State 37:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv371 * _menhir_state * 'tv_header_rr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_header_rr_) : 'tv_list_header_rr_) = _v in
        ((Printf.fprintf stderr "Reducing production list(header_rr) -> header_rr list(header_rr) \n%!";
        let (_menhir_stack, _menhir_s, (x : 'tv_header_rr)) = _menhir_stack in
        let _v : 'tv_list_header_rr_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2543 "Parser.ml"
         in
        _menhir_goto_list_header_rr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)) : 'freshtv374)
    | _ ->
        _menhir_fail ()

and _menhir_reduce53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production list(stmt) -> \n%!";
    let _v : 'tv_list_stmt_ = 
# 211 "<standard.mly>"
    ( [] )
# 2555 "Parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 64:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 102\n%!";
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_not ->
        Printf.fprintf stderr "Shifting (T_not) to state 101\n%!";
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 65:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf stderr "Reducing production l_value -> T_string \n%!";
    let _1 = () in
    let _v : 'tv_l_value = 
# 125 "Parser.mly"
                       ( (Printf.printf("l_value\n")) )
# 2608 "Parser.ml"
     in
    _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 126:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv357) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf stderr "Reducing production stmt -> T_semicolon \n%!";
    let _1 = () in
    let _v : 'tv_stmt = 
# 103 "Parser.mly"
                          ( () )
# 2624 "Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 127:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_semicolon ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState127 in
        ((Printf.fprintf stderr "Reducing production option(expr) -> \n%!";
        let _v : 'tv_option_expr_ = 
# 114 "<standard.mly>"
    ( None )
# 2664 "Parser.ml"
         in
        _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 131:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_chr ->
        Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 102\n%!";
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_minus ->
        Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_not ->
        Printf.fprintf stderr "Shifting (T_not) to state 101\n%!";
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_plus ->
        Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 70:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_left_par ->
        Printf.fprintf stderr "Shifting (T_left_par) to state 71\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 71:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_chr ->
            Printf.fprintf stderr "Shifting (T_chr) to state 72\n%!";
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_identifier ->
            Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_integer ->
            Printf.fprintf stderr "Shifting (T_integer) to state 69\n%!";
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_left_par ->
            Printf.fprintf stderr "Shifting (T_left_par) to state 68\n%!";
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_minus ->
            Printf.fprintf stderr "Shifting (T_minus) to state 67\n%!";
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_plus ->
            Printf.fprintf stderr "Shifting (T_plus) to state 66\n%!";
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_string ->
            Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_right_par ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState71 in
            ((Printf.fprintf stderr "Reducing production option(func_call_rr) -> \n%!";
            let _v : 'tv_option_func_call_rr_ = 
# 114 "<standard.mly>"
    ( None )
# 2754 "Parser.ml"
             in
            _menhir_goto_option_func_call_rr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv348)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv350)
    | T_and | T_assignment | T_comma | T_div | T_do | T_equal | T_greater | T_greater_eq | T_left_sqr | T_less | T_less_eq | T_minus | T_mod | T_mul | T_not_equal | T_or | T_plus | T_right_par | T_right_sqr | T_semicolon | T_then ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production l_value -> T_identifier \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_l_value = 
# 124 "Parser.mly"
                           ( (Printf.printf("l_value\n")) )
# 2771 "Parser.ml"
         in
        _menhir_goto_l_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production list(header_rr) -> \n%!";
    let _v : 'tv_list_header_rr_ = 
# 211 "<standard.mly>"
    ( [] )
# 2789 "Parser.ml"
     in
    _menhir_goto_list_header_rr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 33:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_ref ->
        Printf.fprintf stderr "Shifting (T_ref) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_identifier ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 63:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 70\n%!";
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_if ->
        Printf.fprintf stderr "Shifting (T_if) to state 131\n%!";
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_left_br ->
        Printf.fprintf stderr "Shifting (T_left_br) to state 63\n%!";
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_return ->
        Printf.fprintf stderr "Shifting (T_return) to state 127\n%!";
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_semicolon ->
        Printf.fprintf stderr "Shifting (T_semicolon) to state 126\n%!";
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_string ->
        Printf.fprintf stderr "Shifting (T_string) to state 65\n%!";
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_while ->
        Printf.fprintf stderr "Shifting (T_while) to state 64\n%!";
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_right_br ->
        _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_goto_list_var_def_r_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_var_def_r_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv339 * _menhir_state * 'tv_var_def_r) * _menhir_state * 'tv_list_var_def_r_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 45:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv337 * _menhir_state * 'tv_var_def_r) * _menhir_state * 'tv_list_var_def_r_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production list(var_def_r) -> var_def_r list(var_def_r) \n%!";
        let ((_menhir_stack, _menhir_s, (x : 'tv_var_def_r)), _, (xs : 'tv_list_var_def_r_)) = _menhir_stack in
        let _v : 'tv_list_var_def_r_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2862 "Parser.ml"
         in
        _menhir_goto_list_var_def_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)) : 'freshtv340)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv345 * _menhir_state)) * _menhir_state * 'tv_list_var_def_r_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 46:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            Printf.fprintf stderr "Shifting (T_colon) to state 47\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv341 * _menhir_state)) * _menhir_state * 'tv_list_var_def_r_) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 47:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                Printf.fprintf stderr "Shifting (T_char) to state 10\n%!";
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | T_int ->
                Printf.fprintf stderr "Shifting (T_int) to state 9\n%!";
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv342)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv343 * _menhir_state)) * _menhir_state * 'tv_list_var_def_r_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)) : 'freshtv346)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_mytype_r_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_mytype_r_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315 * _menhir_state * 'tv_mytype_r) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_mytype_r_) = _v in
        ((Printf.fprintf stderr "State 55:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313 * _menhir_state * 'tv_mytype_r) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_mytype_r_) : 'tv_list_mytype_r_) = _v in
        ((Printf.fprintf stderr "Reducing production list(mytype_r) -> mytype_r list(mytype_r) \n%!";
        let (_menhir_stack, _menhir_s, (x : 'tv_mytype_r)) = _menhir_stack in
        let _v : 'tv_list_mytype_r_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2920 "Parser.ml"
         in
        _menhir_goto_list_mytype_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv335 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_mytype_r_) = _v in
        ((Printf.fprintf stderr "State 56:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv333 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_list_mytype_r_) : 'tv_list_mytype_r_) = _v in
        ((Printf.fprintf stderr "Reducing production mytype -> data_type list(mytype_r) \n%!";
        let (_menhir_stack, _menhir_s, (_1 : 'tv_data_type)) = _menhir_stack in
        let _v : 'tv_mytype = 
# 72 "Parser.mly"
                                  ( (Printf.printf("mytype\n")) )
# 2938 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_mytype) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv329 * _menhir_state)) * _menhir_state * 'tv_list_var_def_r_)) * _menhir_state * 'tv_mytype) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 48:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_semicolon ->
            Printf.fprintf stderr "Shifting (T_semicolon) to state 49\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv325 * _menhir_state)) * _menhir_state * 'tv_list_var_def_r_)) * _menhir_state * 'tv_mytype) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 49:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv323 * _menhir_state)) * _menhir_state * 'tv_list_var_def_r_)) * _menhir_state * 'tv_mytype) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production var_def -> T_var T_identifier list(var_def_r) T_colon mytype T_semicolon \n%!";
            let (((_menhir_stack, _menhir_s), _, (_3 : 'tv_list_var_def_r_)), _, (_5 : 'tv_mytype)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_var_def = 
# 98 "Parser.mly"
                                                                       ( (Printf.printf("var_def\n")) )
# 2968 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv321) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_var_def) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv319) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_var_def) = _v in
            ((Printf.fprintf stderr "State 57:\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv317) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_var_def) : 'tv_var_def) = _v in
            ((Printf.fprintf stderr "Reducing production local_def -> var_def \n%!";
            let _v : 'tv_local_def = 
# 94 "Parser.mly"
                      ( (Printf.printf("local_def\n")) )
# 2987 "Parser.ml"
             in
            _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)) : 'freshtv326)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv327 * _menhir_state)) * _menhir_state * 'tv_list_var_def_r_)) * _menhir_state * 'tv_mytype) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)) : 'freshtv330)) : 'freshtv332)) : 'freshtv334)) : 'freshtv336)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_rest_r_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_rest_r_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * 'tv_rest_r) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rest_r_) = _v in
        ((Printf.fprintf stderr "State 26:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * 'tv_rest_r) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_rest_r_) : 'tv_list_rest_r_) = _v in
        ((Printf.fprintf stderr "Reducing production list(rest_r) -> rest_r list(rest_r) \n%!";
        let (_menhir_stack, _menhir_s, (x : 'tv_rest_r)) = _menhir_stack in
        let _v : 'tv_list_rest_r_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3019 "Parser.ml"
         in
        _menhir_goto_list_rest_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)) : 'freshtv284)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv311) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rest_r_) = _v in
        ((Printf.fprintf stderr "State 28:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_list_rest_r_) : 'tv_list_rest_r_) = _v in
        ((Printf.fprintf stderr "Reducing production rest -> list(rest_r) \n%!";
        let _v : 'tv_rest = 
# 83 "Parser.mly"
                        (())
# 3036 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_rest) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_rest) = _v in
        ((Printf.fprintf stderr "State 27:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_rest) : 'tv_rest) = _v in
        ((Printf.fprintf stderr "Reducing production fpar_type -> data_type rest \n%!";
        let (_menhir_stack, _menhir_s, (_1 : 'tv_data_type)) = _menhir_stack in
        let _v : 'tv_fpar_type = 
# 81 "Parser.mly"
                                (())
# 3056 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_type) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv299 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_list_fpar_def_r_)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_type) = _v in
        ((Printf.fprintf stderr "State 19:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv297 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_list_fpar_def_r_)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : 'tv_fpar_type) : 'tv_fpar_type) = _v in
        ((Printf.fprintf stderr "Reducing production fpar_def -> option(T_ref) T_identifier list(fpar_def_r) T_colon fpar_type \n%!";
        let ((_menhir_stack, _menhir_s, (_1 : 'tv_option_T_ref_)), _, (_3 : 'tv_list_fpar_def_r_)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : 'tv_fpar_def = 
# 64 "Parser.mly"
                                                                ( (Printf.printf("fpar_def\n")) )
# 3078 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv295) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_def) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState3 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv285 * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 32:\n%!";
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_semicolon ->
                Printf.fprintf stderr "Shifting (T_semicolon) to state 33\n%!";
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | T_right_par ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv286)
        | MenhirState33 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv293 * _menhir_state) * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 34:\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv291 * _menhir_state) * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production header_rr -> T_semicolon fpar_def \n%!";
            let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_fpar_def)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_header_rr = 
# 62 "Parser.mly"
                                   ( () )
# 3115 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv289) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_header_rr) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv287 * _menhir_state * 'tv_header_rr) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 36:\n%!";
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_semicolon ->
                Printf.fprintf stderr "Shifting (T_semicolon) to state 33\n%!";
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | T_right_par ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv288)) : 'freshtv290)) : 'freshtv292)) : 'freshtv294)
        | _ ->
            _menhir_fail ()) : 'freshtv296)) : 'freshtv298)) : 'freshtv300)) : 'freshtv302)) : 'freshtv304)) : 'freshtv306)) : 'freshtv308)) : 'freshtv310)) : 'freshtv312)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_T_integer_ : _menhir_env -> 'ttv_tail -> 'tv_option_T_integer_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv279 * _menhir_state) * 'tv_option_T_integer_) = Obj.magic _menhir_stack in
    ((Printf.fprintf stderr "State 23:\n%!";
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_right_sqr ->
        Printf.fprintf stderr "Shifting (T_right_sqr) to state 24\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv275 * _menhir_state) * 'tv_option_T_integer_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 24:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv273 * _menhir_state) * 'tv_option_T_integer_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production rest_r -> T_left_sqr option(T_integer) T_right_sqr \n%!";
        let ((_menhir_stack, _menhir_s), (_2 : 'tv_option_T_integer_)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_rest_r = 
# 85 "Parser.mly"
                                                (())
# 3167 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv271) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_rest_r) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv269 * _menhir_state * 'tv_rest_r) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 25:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            Printf.fprintf stderr "Shifting (T_left_sqr) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | T_right_par | T_semicolon ->
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv270)) : 'freshtv272)) : 'freshtv274)) : 'freshtv276)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv277 * _menhir_state) * 'tv_option_T_integer_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)

and _menhir_goto_list_local_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_local_def_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * _menhir_state * 'tv_local_def) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 59:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state * 'tv_local_def) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production list(local_def) -> local_def list(local_def) \n%!";
        let ((_menhir_stack, _menhir_s, (x : 'tv_local_def)), _, (xs : 'tv_list_local_def_)) = _menhir_stack in
        let _v : 'tv_list_local_def_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3214 "Parser.ml"
         in
        _menhir_goto_list_local_def_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)) : 'freshtv266)
    | MenhirState39 | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * _menhir_state * 'tv_header) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 62:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_br ->
            Printf.fprintf stderr "Shifting (T_left_br) to state 63\n%!";
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv268)
    | _ ->
        _menhir_fail ()

and _menhir_reduce55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production list(var_def_r) -> \n%!";
    let _v : 'tv_list_var_def_r_ = 
# 211 "<standard.mly>"
    ( [] )
# 3241 "Parser.ml"
     in
    _menhir_goto_list_var_def_r_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 42:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 43\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 43:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv257 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production var_def_r -> T_comma T_identifier \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_var_def_r = 
# 101 "Parser.mly"
                                   ( () )
# 3267 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_var_def_r) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_var_def_r) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 44:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            Printf.fprintf stderr "Shifting (T_comma) to state 42\n%!";
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | T_colon ->
            _menhir_reduce55 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv254)) : 'freshtv256)) : 'freshtv258)) : 'freshtv260)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production list(mytype_r) -> \n%!";
    let _v : 'tv_list_mytype_r_ = 
# 211 "<standard.mly>"
    ( [] )
# 3305 "Parser.ml"
     in
    _menhir_goto_list_mytype_r_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 51:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 52\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 52:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_right_sqr ->
            Printf.fprintf stderr "Shifting (T_right_sqr) to state 53\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv245 * _menhir_state)) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 53:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv243 * _menhir_state)) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "Reducing production mytype_r -> T_left_sqr T_integer T_right_sqr \n%!";
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_mytype_r = 
# 75 "Parser.mly"
                                               ( () )
# 3340 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_mytype_r) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_mytype_r) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 54:\n%!";
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_left_sqr ->
                Printf.fprintf stderr "Shifting (T_left_sqr) to state 51\n%!";
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | T_semicolon ->
                _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv240)) : 'freshtv242)) : 'freshtv244)) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv247 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)

and _menhir_reduce51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production list(rest_r) -> \n%!";
    let _v : 'tv_list_rest_r_ = 
# 211 "<standard.mly>"
    ( [] )
# 3386 "Parser.ml"
     in
    _menhir_goto_list_rest_r_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 21:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_integer ->
        Printf.fprintf stderr "Shifting (T_integer) to state 22\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 22:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production option(T_integer) -> T_integer \n%!";
        let x = () in
        let _v : 'tv_option_T_integer_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3410 "Parser.ml"
         in
        _menhir_goto_option_T_integer_ _menhir_env _menhir_stack _v) : 'freshtv232)) : 'freshtv234)
    | T_right_sqr ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production option(T_integer) -> \n%!";
        let _v : 'tv_option_T_integer_ = 
# 114 "<standard.mly>"
    ( None )
# 3420 "Parser.ml"
         in
        _menhir_goto_option_T_integer_ _menhir_env _menhir_stack _v) : 'freshtv236)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)

and _menhir_goto_list_fpar_def_r_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_fpar_def_r_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_list_fpar_def_r_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 17:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            Printf.fprintf stderr "Shifting (T_colon) to state 18\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_list_fpar_def_r_) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 18:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                Printf.fprintf stderr "Shifting (T_char) to state 10\n%!";
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | T_int ->
                Printf.fprintf stderr "Shifting (T_int) to state 9\n%!";
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv222)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_list_fpar_def_r_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * _menhir_state * 'tv_fpar_def_r) * _menhir_state * 'tv_list_fpar_def_r_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 30:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv227 * _menhir_state * 'tv_fpar_def_r) * _menhir_state * 'tv_list_fpar_def_r_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production list(fpar_def_r) -> fpar_def_r list(fpar_def_r) \n%!";
        let ((_menhir_stack, _menhir_s, (x : 'tv_fpar_def_r)), _, (xs : 'tv_list_fpar_def_r_)) = _menhir_stack in
        let _v : 'tv_list_fpar_def_r_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3481 "Parser.ml"
         in
        _menhir_goto_list_fpar_def_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)) : 'freshtv230)
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
    let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_local_def) = Obj.magic _menhir_stack in
    ((Printf.fprintf stderr "State 58:\n%!";
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_fun ->
        Printf.fprintf stderr "Shifting (T_fun) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_var ->
        Printf.fprintf stderr "Shifting (T_var) to state 40\n%!";
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_left_br ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv220)

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production list(local_def) -> \n%!";
    let _v : 'tv_list_local_def_ = 
# 211 "<standard.mly>"
    ( [] )
# 3521 "Parser.ml"
     in
    _menhir_goto_list_local_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 40:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 41\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 41:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            Printf.fprintf stderr "Shifting (T_comma) to state 42\n%!";
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_colon ->
            _menhir_reduce55 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv216)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)

and _menhir_goto_data_type : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_data_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 12:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production ret_type -> data_type \n%!";
        let (_menhir_stack, _menhir_s, (_1 : 'tv_data_type)) = _menhir_stack in
        let _v : 'tv_ret_type = 
# 77 "Parser.mly"
                        ( (Printf.printf("ret_rype\n")) )
# 3574 "Parser.ml"
         in
        _menhir_goto_ret_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 20:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            Printf.fprintf stderr "Shifting (T_left_sqr) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | T_right_par | T_semicolon ->
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv212)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 50:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_sqr ->
            Printf.fprintf stderr "Shifting (T_left_sqr) to state 51\n%!";
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | T_semicolon ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv214)
    | _ ->
        _menhir_fail ()

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production list(fpar_def_r) -> \n%!";
    let _v : 'tv_list_fpar_def_r_ = 
# 211 "<standard.mly>"
    ( [] )
# 3620 "Parser.ml"
     in
    _menhir_goto_list_fpar_def_r_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 15:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 16\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 16:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "Reducing production fpar_def_r -> T_comma T_identifier \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_fpar_def_r = 
# 67 "Parser.mly"
                                    ( () )
# 3646 "Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fpar_def_r) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_fpar_def_r) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 29:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            Printf.fprintf stderr "Shifting (T_comma) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | T_colon ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv198)) : 'freshtv200)) : 'freshtv202)) : 'freshtv204)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)

and _menhir_goto_ret_type : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ret_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv195 * _menhir_state))) * _menhir_state * 'tv_option_header_r_))) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_ret_type) = _v in
    ((Printf.fprintf stderr "State 11:\n%!";
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv193 * _menhir_state))) * _menhir_state * 'tv_option_header_r_))) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((_7 : 'tv_ret_type) : 'tv_ret_type) = _v in
    ((Printf.fprintf stderr "Reducing production header -> T_fun T_identifier T_left_par option(header_r) T_right_par T_colon ret_type \n%!";
    let ((_menhir_stack, _menhir_s), _, (_4 : 'tv_option_header_r_)) = _menhir_stack in
    let _6 = () in
    let _5 = () in
    let _3 = () in
    let _2 = () in
    let _1 = () in
    let _v : 'tv_header = 
# 57 "Parser.mly"
                                                                                   ( (Printf.printf("header\n")) )
# 3699 "Parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv191) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_header) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 39:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_fun ->
            Printf.fprintf stderr "Shifting (T_fun) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_var ->
            Printf.fprintf stderr "Shifting (T_var) to state 40\n%!";
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_left_br ->
            _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv178)
    | MenhirState39 | MenhirState60 | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 60:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_fun ->
            Printf.fprintf stderr "Shifting (T_fun) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_semicolon ->
            Printf.fprintf stderr "Shifting (T_semicolon) to state 61\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState60 in
            ((Printf.fprintf stderr "State 61:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((Printf.fprintf stderr "Reducing production func_decl -> header T_semicolon \n%!";
            let (_menhir_stack, _menhir_s, (_1 : 'tv_header)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_func_decl = 
# 96 "Parser.mly"
                                 ( (Printf.printf("func_decl\n")) )
# 3753 "Parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_func_decl) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv181) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_func_decl) = _v in
            ((Printf.fprintf stderr "State 153:\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_func_decl) : 'tv_func_decl) = _v in
            ((Printf.fprintf stderr "Reducing production local_def -> func_decl \n%!";
            let _v : 'tv_local_def = 
# 93 "Parser.mly"
                        ( (Printf.printf("local_def\n")) )
# 3772 "Parser.ml"
             in
            _menhir_goto_local_def _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)) : 'freshtv182)) : 'freshtv184)) : 'freshtv186)) : 'freshtv188)
        | T_var ->
            Printf.fprintf stderr "Shifting (T_var) to state 40\n%!";
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_left_br ->
            _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv190)
    | _ ->
        _menhir_fail ()) : 'freshtv192)) : 'freshtv194)) : 'freshtv196)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 9:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf stderr "Reducing production data_type -> T_int \n%!";
    let _1 = () in
    let _v : 'tv_data_type = 
# 69 "Parser.mly"
                    ( (Printf.printf("data_type\n")) )
# 3800 "Parser.ml"
     in
    _menhir_goto_data_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 10:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf stderr "Reducing production data_type -> T_char \n%!";
    let _1 = () in
    let _v : 'tv_data_type = 
# 70 "Parser.mly"
                     ( (Printf.printf("data_type\n")) )
# 3816 "Parser.ml"
     in
    _menhir_goto_data_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)

and _menhir_goto_option_T_ref_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_T_ref_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_option_T_ref_) = Obj.magic _menhir_stack in
    ((Printf.fprintf stderr "State 13:\n%!";
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 14\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_option_T_ref_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 14:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_comma ->
            Printf.fprintf stderr "Shifting (T_comma) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | T_colon ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv168)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state * 'tv_option_T_ref_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)

and _menhir_reduce64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "Reducing production option(T_ref) -> \n%!";
    let _v : 'tv_option_T_ref_ = 
# 114 "<standard.mly>"
    ( None )
# 3862 "Parser.ml"
     in
    _menhir_goto_option_T_ref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_header_r_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_header_r_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv165 * _menhir_state))) * _menhir_state * 'tv_option_header_r_) = Obj.magic _menhir_stack in
    ((Printf.fprintf stderr "State 5:\n%!";
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_right_par ->
        Printf.fprintf stderr "Shifting (T_right_par) to state 6\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv161 * _menhir_state))) * _menhir_state * 'tv_option_header_r_) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 6:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_colon ->
            Printf.fprintf stderr "Shifting (T_colon) to state 7\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv157 * _menhir_state))) * _menhir_state * 'tv_option_header_r_)) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 7:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_char ->
                Printf.fprintf stderr "Shifting (T_char) to state 10\n%!";
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | T_int ->
                Printf.fprintf stderr "Shifting (T_int) to state 9\n%!";
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | T_nothing ->
                Printf.fprintf stderr "Shifting (T_nothing) to state 8\n%!";
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState7 in
                ((Printf.fprintf stderr "State 8:\n%!";
                let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                ((Printf.fprintf stderr "Reducing production ret_type -> T_nothing \n%!";
                let _1 = () in
                let _v : 'tv_ret_type = 
# 78 "Parser.mly"
                        ( (Printf.printf("ret_type\n")) )
# 3912 "Parser.ml"
                 in
                _menhir_goto_ret_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)) : 'freshtv156)
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv159 * _menhir_state))) * _menhir_state * 'tv_option_header_r_)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv163 * _menhir_state))) * _menhir_state * 'tv_option_header_r_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)) : 'freshtv166)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 4:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf stderr "Reducing production option(T_ref) -> T_ref \n%!";
    let x = () in
    let _v : 'tv_option_T_ref_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3949 "Parser.ml"
     in
    _menhir_goto_option_T_ref_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv15 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * 'tv_l_value)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv20)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv21 * _menhir_state) * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv29 * _menhir_state) * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * 'tv_cond)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv37 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv41 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv53 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_func_call_r) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv81 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state * 'tv_l_value)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * 'tv_l_value)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_header) * _menhir_state * 'tv_list_local_def_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state * 'tv_local_def) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * 'tv_mytype_r) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv121 * _menhir_state)) * _menhir_state * 'tv_list_var_def_r_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_var_def_r) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_header) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_header_rr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_fpar_def) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state * 'tv_fpar_def_r) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_rest_r) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_data_type) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv141 * _menhir_state * 'tv_option_T_ref_)) * _menhir_state * 'tv_list_fpar_def_r_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state * 'tv_option_T_ref_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv145 * _menhir_state))) * _menhir_state * 'tv_option_header_r_))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv147 * _menhir_state))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv150)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf stderr "State 1:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_identifier ->
        Printf.fprintf stderr "Shifting (T_identifier) to state 2\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((Printf.fprintf stderr "State 2:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | T_left_par ->
            Printf.fprintf stderr "Shifting (T_left_par) to state 3\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state)) = Obj.magic _menhir_stack in
            ((Printf.fprintf stderr "State 3:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | T_ref ->
                Printf.fprintf stderr "Shifting (T_ref) to state 4\n%!";
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | T_right_par ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState3 in
                ((Printf.fprintf stderr "Reducing production option(header_r) -> \n%!";
                let _v : 'tv_option_header_r_ = 
# 114 "<standard.mly>"
    ( None )
# 4334 "Parser.ml"
                 in
                _menhir_goto_option_header_r_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)
            | T_identifier ->
                _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv7 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
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
    Printf.fprintf stderr "Lookahead token is now %s (%d-%d)\n%!" (let (_tok : token) = _tok in
    ((match _tok with
    | T_and ->
        "T_and"
    | T_assignment ->
        "T_assignment"
    | T_char ->
        "T_char"
    | T_chr ->
        "T_chr"
    | T_colon ->
        "T_colon"
    | T_comma ->
        "T_comma"
    | T_div ->
        "T_div"
    | T_do ->
        "T_do"
    | T_else ->
        "T_else"
    | T_eof ->
        "T_eof"
    | T_equal ->
        "T_equal"
    | T_fun ->
        "T_fun"
    | T_greater ->
        "T_greater"
    | T_greater_eq ->
        "T_greater_eq"
    | T_identifier ->
        "T_identifier"
    | T_if ->
        "T_if"
    | T_int ->
        "T_int"
    | T_integer ->
        "T_integer"
    | T_left_br ->
        "T_left_br"
    | T_left_par ->
        "T_left_par"
    | T_left_sqr ->
        "T_left_sqr"
    | T_less ->
        "T_less"
    | T_less_eq ->
        "T_less_eq"
    | T_minus ->
        "T_minus"
    | T_mod ->
        "T_mod"
    | T_mul ->
        "T_mul"
    | T_not ->
        "T_not"
    | T_not_equal ->
        "T_not_equal"
    | T_nothing ->
        "T_nothing"
    | T_or ->
        "T_or"
    | T_plus ->
        "T_plus"
    | T_ref ->
        "T_ref"
    | T_return ->
        "T_return"
    | T_right_br ->
        "T_right_br"
    | T_right_par ->
        "T_right_par"
    | T_right_sqr ->
        "T_right_sqr"
    | T_semicolon ->
        "T_semicolon"
    | T_string ->
        "T_string"
    | T_then ->
        "T_then"
    | T_var ->
        "T_var"
    | T_while ->
        "T_while") : string)) lexbuf.Lexing.lex_start_p.Lexing.pos_cnum lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 47 "Parser.mly"
      (unit)
# 4460 "Parser.ml"
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
    ((Printf.fprintf stderr "State 0:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_fun ->
        Printf.fprintf stderr "Shifting (T_fun) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 4492 "Parser.ml"
