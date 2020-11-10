
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNTUPLE
    | TIMES
    | THEN
    | SND
    | SETREF
    | SET
    | RPAREN
    | RBRACE
    | RANGLE
    | PROC
    | PLUS
    | PAIR
    | NEWREF
    | MINUS
    | LPAREN
    | LETREC
    | LET
    | LBRACE
    | LANGLE
    | ISZERO
    | INT of (
# 22 "src/parser.mly"
       (int)
# 31 "src/parser.ml"
  )
    | IN
    | IF
    | ID of (
# 23 "src/parser.mly"
       (string)
# 38 "src/parser.ml"
  )
    | FST
    | EQUALS
    | EOF
    | END
    | ELSE
    | DIVIDED
    | DEREF
    | DEBUG
    | COMMA
    | BEGIN
  
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
  | MenhirState112
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState106
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState56
  | MenhirState55
  | MenhirState50
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState37
  | MenhirState34
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState17
  | MenhirState14
  | MenhirState12
  | MenhirState10
  | MenhirState4
  | MenhirState2
  | MenhirState0

# 8 "src/parser.mly"
  
open Ast

# 132 "src/parser.ml"

let rec _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_ID_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 142 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv427 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 150 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_ID_) : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 23 "src/parser.mly"
       (string)
# 157 "src/parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_ID_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 163 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv428)) : 'freshtv430)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv431) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_ID_) : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_ID__ = 
# 144 "<standard.mly>"
    ( x )
# 178 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)) : 'freshtv434)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState38 | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv421) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 204 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)) : 'freshtv422)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv425 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_expr)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 221 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv424)) : 'freshtv426)
    | _ ->
        _menhir_fail ()

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv417) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_COMMA_expr__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv415) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs : 'tv_loption_separated_nonempty_list_COMMA_expr__) : 'tv_loption_separated_nonempty_list_COMMA_expr__) = _v in
    ((let _v : 'tv_exprs = let es = 
# 232 "<standard.mly>"
    ( xs )
# 436 "src/parser.ml"
     in
    
# 170 "src/parser.mly"
                                        ( es )
# 441 "src/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv413) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_exprs) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv403 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv399 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv397 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (es : 'tv_exprs)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 161 "src/parser.mly"
                             ( BeginEnd(es) )
# 467 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)) : 'freshtv400)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv401 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)) : 'freshtv404)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv411 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RANGLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv407 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv405 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (es : 'tv_exprs)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 165 "src/parser.mly"
                                 ( Tuple(es) )
# 495 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)) : 'freshtv408)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv409 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)) : 'freshtv412)
    | _ ->
        _menhir_fail ()) : 'freshtv414)) : 'freshtv416)) : 'freshtv418)

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_ID__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_COMMA_ID__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv393) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs : 'tv_loption_separated_nonempty_list_COMMA_ID__) : 'tv_loption_separated_nonempty_list_COMMA_ID__) = _v in
    ((let _v : 'tv_ids = let is = 
# 232 "<standard.mly>"
    ( xs )
# 521 "src/parser.ml"
     in
    
# 173 "src/parser.mly"
                                      ( is )
# 526 "src/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv391) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_ids) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv389 * _menhir_state)) * _menhir_state * 'tv_ids) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RANGLE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv385 * _menhir_state)) * _menhir_state * 'tv_ids) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv381 * _menhir_state)) * _menhir_state * 'tv_ids)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv382)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv383 * _menhir_state)) * _menhir_state * 'tv_ids)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv387 * _menhir_state)) * _menhir_state * 'tv_ids) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)) : 'freshtv392)) : 'freshtv394)) : 'freshtv396)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/parser.mly"
       (string)
# 610 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 622 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv376)
    | RANGLE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv377 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 638 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 23 "src/parser.mly"
       (string)
# 643 "src/parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_ID_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 648 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv379 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 658 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 | MenhirState64 | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState55 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv222)
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | END | RANGLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 738 "src/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv226)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 146 "src/parser.mly"
                                  ( Mul(e1,e2) )
# 755 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)) : 'freshtv230)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LANGLE | LET | LETREC | LPAREN | MINUS | NEWREF | PAIR | PLUS | PROC | RANGLE | RBRACE | RPAREN | SET | SETREF | SND | THEN | UNTUPLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 144 "src/parser.mly"
                                 ( Add(e1,e2) )
# 776 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv234)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 147 "src/parser.mly"
                                    ( Div(e1,e2) )
# 793 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)) : 'freshtv238)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv241 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LANGLE | LET | LETREC | LPAREN | MINUS | NEWREF | PAIR | PLUS | PROC | RANGLE | RBRACE | RPAREN | SET | SETREF | SND | THEN | UNTUPLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 145 "src/parser.mly"
                                  ( Sub(e1,e2) )
# 814 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv242)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv247 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState66 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv243 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 143 "src/parser.mly"
                                   ( Debug(e) )
# 848 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)) : 'freshtv246)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv248)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv253 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv251 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState68 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv249 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 157 "src/parser.mly"
                                      ( DeRef(e) )
# 884 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)) : 'freshtv252)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv254)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv259 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv257 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState70 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv255 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 149 "src/parser.mly"
                                  ( Fst(e) )
# 920 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)) : 'freshtv258)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv260)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState72 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv262)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv264)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv267 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv265 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState74 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv266)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv268)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv271 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LANGLE | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RANGLE | RBRACE | RPAREN | SET | SETREF | SND | THEN | UNTUPLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv269 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)), _), _, (e3 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 159 "src/parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 1089 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv272)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv277 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv275 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState77 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv273 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 155 "src/parser.mly"
                                       ( IsZero(e) )
# 1123 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)) : 'freshtv276)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv278)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv281 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1137 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv279 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1149 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState81 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv280)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv282)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv285 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1213 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LANGLE | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RANGLE | RBRACE | RPAREN | SET | SETREF | SND | THEN | UNTUPLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv283 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1231 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1236 "src/parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 151 "src/parser.mly"
                                                    ( Let(x,e1,e2) )
# 1244 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv286)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv289 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1256 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 1260 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv287 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1272 "src/parser.ml"
            ))) * (
# 23 "src/parser.mly"
       (string)
# 1276 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState84 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv288)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv290)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv293 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1340 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 1344 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LANGLE | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RANGLE | RBRACE | RPAREN | SET | SETREF | SND | THEN | UNTUPLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv291 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1362 "src/parser.ml"
            ))) * (
# 23 "src/parser.mly"
       (string)
# 1366 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1371 "src/parser.ml"
            ))), (y : (
# 23 "src/parser.mly"
       (string)
# 1375 "src/parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 152 "src/parser.mly"
                                                                               ( Letrec(x,y,e1,e2) )
# 1385 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv294)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv299 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv297 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState87 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv295 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 164 "src/parser.mly"
                                      ( Sub(Int 0, e) )
# 1419 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)) : 'freshtv298)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv300)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv305 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv303 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState89 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 162 "src/parser.mly"
                               (e)
# 1484 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)) : 'freshtv304)
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv306)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState91 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv307 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 154 "src/parser.mly"
                                           ( App(e1,e2) )
# 1527 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv312)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState93 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 156 "src/parser.mly"
                                       ( NewRef(e) )
# 1563 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv318)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv319 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState95 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv320)
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv322)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv327 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv325 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState97 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv323 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 148 "src/parser.mly"
                                                    ( Pair(e1,e2) )
# 1668 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)) : 'freshtv326)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv328)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv333 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 1682 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv331 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 1698 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState99 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv329 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 1706 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1712 "src/parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 153 "src/parser.mly"
                                                             ( Proc(x,e) )
# 1722 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)) : 'freshtv332)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv334)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv337 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1736 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LANGLE | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RANGLE | RBRACE | RPAREN | SET | SETREF | SND | THEN | UNTUPLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv335 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1754 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1759 "src/parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 160 "src/parser.mly"
                                    ( Set(x,e) )
# 1766 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv338)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv341 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv339 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState102 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv340)
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv342)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv347 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv345 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState104 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv343 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 158 "src/parser.mly"
                                                          ( SetRef(e1,e2) )
# 1869 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv344)) : 'freshtv346)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv348)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv353 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv351 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState106 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv349 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 150 "src/parser.mly"
                                  ( Snd(e) )
# 1905 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv354)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv357 * _menhir_state)) * _menhir_state * 'tv_ids))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv355 * _menhir_state)) * _menhir_state * 'tv_ids))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState108 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv356)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv358)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv361 * _menhir_state)) * _menhir_state * 'tv_ids))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LANGLE | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RANGLE | RBRACE | RPAREN | SET | SETREF | SND | THEN | UNTUPLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv359 * _menhir_state)) * _menhir_state * 'tv_ids))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _, (is : 'tv_ids)), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 166 "src/parser.mly"
                                                                          ( Untuple(is,e1,e2) )
# 2008 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv362)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv371 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState112 in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 80 "src/parser.mly"
       (Ast.prog)
# 2035 "src/parser.ml"
            ) = 
# 112 "src/parser.mly"
                 ( AProg e )
# 2039 "src/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv367) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 80 "src/parser.mly"
       (Ast.prog)
# 2047 "src/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv365) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 80 "src/parser.mly"
       (Ast.prog)
# 2055 "src/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 80 "src/parser.mly"
       (Ast.prog)
# 2063 "src/parser.ml"
            )) : (
# 80 "src/parser.mly"
       (Ast.prog)
# 2067 "src/parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv364)) : 'freshtv366)) : 'freshtv368)) : 'freshtv370)) : 'freshtv372)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112) : 'freshtv374)
    | _ ->
        _menhir_fail ()

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 2088 "src/parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv101 * _menhir_state)) * _menhir_state * 'tv_ids))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv103 * _menhir_state)) * _menhir_state * 'tv_ids))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv105 * _menhir_state)) * _menhir_state * 'tv_ids))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv109 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv111 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv115 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2140 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv117 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2149 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv119 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv121 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv127 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv131 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv133 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2193 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2197 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv135 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2206 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2210 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv137 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2219 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2223 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv139 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2232 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv141 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2241 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv143 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2250 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv147 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv149 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv151 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv159 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv165 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv193 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2379 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv195 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2388 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2392 "src/parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv201 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv205 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2421 "src/parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv207 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2430 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv209 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv211 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv213 * _menhir_state)) * _menhir_state * 'tv_ids))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv215 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 2454 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv217 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv220)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGLE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | RANGLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _v : 'tv_loption_separated_nonempty_list_COMMA_ID__ = 
# 142 "<standard.mly>"
    ( [] )
# 2489 "src/parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv96)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv90)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv86)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "src/parser.mly"
       (string)
# 2640 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv77 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2651 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv79 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2705 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)) : 'freshtv82)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv69 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "src/parser.mly"
       (string)
# 2735 "src/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv65 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2746 "src/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv61 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2756 "src/parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BEGIN ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | DEBUG ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | DEREF ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | FST ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | ID _v ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
                    | IF ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | INT _v ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
                    | ISZERO ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | LANGLE ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | LET ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | LETREC ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | LPAREN ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | NEWREF ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | PAIR ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | PROC ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | SET ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | SETREF ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | SND ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | UNTUPLE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv62)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv63 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2810 "src/parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)) : 'freshtv66)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv67 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2821 "src/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)) : 'freshtv70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv71 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv58)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv54)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MINUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState27 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv52)
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "src/parser.mly"
       (string)
# 3075 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv43 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3086 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv39 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3096 "src/parser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_v : (
# 23 "src/parser.mly"
       (string)
# 3101 "src/parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RPAREN ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv35 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3112 "src/parser.ml"
                    ))) * (
# 23 "src/parser.mly"
       (string)
# 3116 "src/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EQUALS ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv31 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3126 "src/parser.ml"
                        ))) * (
# 23 "src/parser.mly"
       (string)
# 3130 "src/parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | BEGIN ->
                            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | DEBUG ->
                            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | DEREF ->
                            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | FST ->
                            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | ID _v ->
                            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
                        | IF ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | INT _v ->
                            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
                        | ISZERO ->
                            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | LANGLE ->
                            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | LET ->
                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | LETREC ->
                            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | LPAREN ->
                            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | NEWREF ->
                            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | PAIR ->
                            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | PROC ->
                            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | SET ->
                            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | SETREF ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | SND ->
                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | UNTUPLE ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv32)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv33 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3184 "src/parser.ml"
                        ))) * (
# 23 "src/parser.mly"
       (string)
# 3188 "src/parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)) : 'freshtv36)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv37 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3199 "src/parser.ml"
                    ))) * (
# 23 "src/parser.mly"
       (string)
# 3203 "src/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)) : 'freshtv40)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv41 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3214 "src/parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)) : 'freshtv44)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv45 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3225 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)) : 'freshtv48)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "src/parser.mly"
       (string)
# 3249 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv23 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3260 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | DEBUG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | DEREF ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | FST ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | ID _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
            | IF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | INT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
            | ISZERO ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | LANGLE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | LETREC ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | NEWREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | PAIR ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | PROC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | SET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | SETREF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | SND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | UNTUPLE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv24)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv25 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3314 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)) : 'freshtv28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | RANGLE ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv20)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "src/parser.mly"
       (int)
# 3442 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 22 "src/parser.mly"
       (int)
# 3452 "src/parser.ml"
    )) : (
# 22 "src/parser.mly"
       (int)
# 3456 "src/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 141 "src/parser.mly"
              ( Int i )
# 3461 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv18)

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/parser.mly"
       (string)
# 3517 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 23 "src/parser.mly"
       (string)
# 3527 "src/parser.ml"
    )) : (
# 23 "src/parser.mly"
       (string)
# 3531 "src/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 142 "src/parser.mly"
             ( Var x )
# 3536 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv16)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | DEBUG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | DEREF ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FST ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ID _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | IF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | ISZERO ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LANGLE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LETREC ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NEWREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PAIR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PROC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SETREF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | UNTUPLE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | END ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

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

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 80 "src/parser.mly"
       (Ast.prog)
# 3792 "src/parser.ml"
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
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEBUG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FST ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LANGLE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PAIR ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNTUPLE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 3857 "src/parser.ml"
