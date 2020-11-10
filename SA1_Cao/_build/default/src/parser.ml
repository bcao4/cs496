
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TL
    | TIMES
    | THEN
    | SETREF
    | SET
    | SEMICOLON
    | RPAREN
    | RBRACE
    | PROC
    | PLUS
    | NEWREF
    | MINUS
    | LPAREN
    | LETREC
    | LET
    | LBRACE
    | ISZERO
    | INT of (
# 22 "src/parser.mly"
       (int)
# 28 "src/parser.ml"
  )
    | IN
    | IF
    | ID of (
# 23 "src/parser.mly"
       (string)
# 35 "src/parser.ml"
  )
    | HD
    | EQUALS
    | EOF
    | END
    | EMPTYLIST
    | EMPTY
    | ELSE
    | DOT
    | DIVIDED
    | DEREF
    | DEBUG
    | CONS
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
  | MenhirState121
  | MenhirState118
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState111
  | MenhirState110
  | MenhirState106
  | MenhirState104
  | MenhirState102
  | MenhirState100
  | MenhirState98
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState87
  | MenhirState82
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState55
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState45
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState35
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState25
  | MenhirState22
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState13
  | MenhirState11
  | MenhirState7
  | MenhirState4
  | MenhirState2
  | MenhirState0

# 8 "src/parser.mly"
  
open Ast

# 138 "src/parser.ml"

let rec _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_nonempty_list_letrecdec_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_letrecdec_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv471 * _menhir_state) * _menhir_state * 'tv_nonempty_list_letrecdec_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv467 * _menhir_state) * _menhir_state * 'tv_nonempty_list_letrecdec_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv468)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv469 * _menhir_state) * _menhir_state * 'tv_nonempty_list_letrecdec_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)) : 'freshtv472)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv475 * _menhir_state * 'tv_letrecdec) * _menhir_state * 'tv_nonempty_list_letrecdec_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv473 * _menhir_state * 'tv_letrecdec) * _menhir_state * 'tv_nonempty_list_letrecdec_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_letrecdec)), _, (xs : 'tv_nonempty_list_letrecdec_)) = _menhir_stack in
        let _v : 'tv_nonempty_list_letrecdec_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 221 "src/parser.ml"
         in
        _menhir_goto_nonempty_list_letrecdec_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv474)) : 'freshtv476)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_field_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_field_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_field_) : 'tv_separated_nonempty_list_SEMICOLON_field_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_field__ = 
# 144 "<standard.mly>"
    ( x )
# 242 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_field__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)) : 'freshtv462)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv465 * _menhir_state * 'tv_field)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_field_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv463 * _menhir_state * 'tv_field)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_field_) : 'tv_separated_nonempty_list_SEMICOLON_field_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_field)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_field_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 259 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_field_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv464)) : 'freshtv466)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_expr_) : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 280 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)) : 'freshtv454)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv457 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv455 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_expr_) : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_expr)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_expr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 297 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv456)) : 'freshtv458)
    | _ ->
        _menhir_fail ()

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | CONS ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEBUG ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | EMPTY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | EMPTYLIST ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | HD ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LBRACE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LETREC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEWREF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | CONS ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DEBUG ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DEREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | EMPTY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | EMPTYLIST ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | HD ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LBRACE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LETREC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEWREF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | CONS ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | DEBUG ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | DEREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | EMPTY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | EMPTYLIST ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | HD ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LBRACE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LETREC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEWREF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "src/parser.mly"
       (string)
# 468 "src/parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let ((id : (
# 23 "src/parser.mly"
       (string)
# 476 "src/parser.ml"
        )) : (
# 23 "src/parser.mly"
       (string)
# 480 "src/parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 175 "src/parser.mly"
                          ( Proj(e1,id) )
# 487 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv449 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | CONS ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | DEBUG ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | DEREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | EMPTY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | EMPTYLIST ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | HD ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LBRACE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LETREC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEWREF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/parser.mly"
       (string)
# 552 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv441 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 564 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv437 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 574 "src/parser.ml"
            ))) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "src/parser.mly"
       (string)
# 579 "src/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv433 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 590 "src/parser.ml"
                ))) * (
# 23 "src/parser.mly"
       (string)
# 594 "src/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQUALS ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv429 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 604 "src/parser.ml"
                    ))) * (
# 23 "src/parser.mly"
       (string)
# 608 "src/parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BEGIN ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | CONS ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | DEBUG ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | DEREF ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | EMPTY ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | EMPTYLIST ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | HD ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | ID _v ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
                    | IF ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | INT _v ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
                    | ISZERO ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | LBRACE ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | LET ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | LETREC ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | LPAREN ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | NEWREF ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | PROC ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | SET ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | SETREF ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | TL ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv430)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv431 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 664 "src/parser.ml"
                    ))) * (
# 23 "src/parser.mly"
       (string)
# 668 "src/parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)) : 'freshtv434)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv435 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 679 "src/parser.ml"
                ))) * (
# 23 "src/parser.mly"
       (string)
# 683 "src/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)) : 'freshtv438)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv439 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 694 "src/parser.ml"
            ))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv440)) : 'freshtv442)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 705 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv444)

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_field__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv427 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_field__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_field__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv421 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_field__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_field__)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_expr = let fs = 
# 232 "<standard.mly>"
    ( xs )
# 730 "src/parser.ml"
         in
        
# 174 "src/parser.mly"
                                                            ( Record(fs) )
# 735 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)) : 'freshtv424)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv425 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_field__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/parser.mly"
       (string)
# 749 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv417 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 761 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv418)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 817 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState55 | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState50 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv240)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_expr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 901 "src/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv244)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv247 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTY | EMPTYLIST | END | EOF | HD | ID _ | IF | IN | INT _ | ISZERO | LBRACE | LET | LETREC | LPAREN | MINUS | NEWREF | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 148 "src/parser.mly"
                                  ( Mul(e1,e2) )
# 924 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv248)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv251 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTY | EMPTYLIST | END | EOF | HD | ID _ | IF | IN | INT _ | ISZERO | LBRACE | LET | LETREC | LPAREN | MINUS | NEWREF | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 146 "src/parser.mly"
                                 ( Add(e1,e2) )
# 951 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv252)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv255 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTY | EMPTYLIST | END | EOF | HD | ID _ | IF | IN | INT _ | ISZERO | LBRACE | LET | LETREC | LPAREN | MINUS | NEWREF | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 149 "src/parser.mly"
                                    ( Div(e1,e2) )
# 974 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv256)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv259 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTY | EMPTYLIST | END | EOF | HD | ID _ | IF | IN | INT _ | ISZERO | LBRACE | LET | LETREC | LPAREN | MINUS | NEWREF | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv257 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 147 "src/parser.mly"
                                  ( Sub(e1,e2) )
# 1001 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv260)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv263 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState63 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv262)
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv264)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv269 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv267 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState65 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv265 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 171 "src/parser.mly"
                                                        ( Cons(e1, e2) )
# 1110 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)) : 'freshtv268)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv270)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv273 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState67 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv271 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 150 "src/parser.mly"
                                   ( Debug(e) )
# 1148 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)) : 'freshtv274)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv276)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv281 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv279 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState69 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv277 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 160 "src/parser.mly"
                                      ( DeRef(e) )
# 1186 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)) : 'freshtv280)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69) : 'freshtv282)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv287 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv285 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState71 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv283 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 170 "src/parser.mly"
                                      ( Empty(e) )
# 1224 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)) : 'freshtv286)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv288)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv293 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv291 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState73 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv289 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 168 "src/parser.mly"
                                   ( Hd(e) )
# 1262 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)) : 'freshtv292)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv294)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState75 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv296)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv298)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv301 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv299 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState77 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv300)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv302)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv305 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTY | EMPTYLIST | END | EOF | HD | ID _ | IF | IN | INT _ | ISZERO | LBRACE | LET | LETREC | LPAREN | NEWREF | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv303 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)), _), _, (e3 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 162 "src/parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 1441 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv306)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState80 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv307 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 158 "src/parser.mly"
                                       ( IsZero(e) )
# 1477 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv312)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv325 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 1491 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv323 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 1511 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (id : (
# 23 "src/parser.mly"
       (string)
# 1516 "src/parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field = 
# 179 "src/parser.mly"
                              ( (id,e) )
# 1522 "src/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv321) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_field) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv319 * _menhir_state * 'tv_field) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv313 * _menhir_state * 'tv_field) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ID _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv314)
            | RBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv315 * _menhir_state * 'tv_field) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_field)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_SEMICOLON_field_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1553 "src/parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMICOLON_field_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv317 * _menhir_state * 'tv_field) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv326)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv329 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1572 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv327 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1586 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState89 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv328)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv330)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv333 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1652 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTY | EMPTYLIST | END | EOF | HD | ID _ | IF | IN | INT _ | ISZERO | LBRACE | LET | LETREC | LPAREN | NEWREF | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv331 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1672 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1677 "src/parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 151 "src/parser.mly"
                                                    ( Let(x,e1,e2) )
# 1685 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv334)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv343 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 1697 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 1701 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ID _ | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv341 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 1721 "src/parser.ml"
            ))) * (
# 23 "src/parser.mly"
       (string)
# 1725 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (x : (
# 23 "src/parser.mly"
       (string)
# 1730 "src/parser.ml"
            ))), (y : (
# 23 "src/parser.mly"
       (string)
# 1734 "src/parser.ml"
            ))), _, (e1 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_letrecdec = 
# 183 "src/parser.mly"
                                                    ( (x, y, e1) )
# 1742 "src/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv339) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_letrecdec) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv337 * _menhir_state * 'tv_letrecdec) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | IN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv335 * _menhir_state * 'tv_letrecdec) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_letrecdec)) = _menhir_stack in
                let _v : 'tv_nonempty_list_letrecdec_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 1763 "src/parser.ml"
                 in
                _menhir_goto_nonempty_list_letrecdec_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv338)) : 'freshtv340)) : 'freshtv342)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv344)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv347 * _menhir_state) * _menhir_state * 'tv_nonempty_list_letrecdec_)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTY | EMPTYLIST | END | EOF | HD | ID _ | IF | IN | INT _ | ISZERO | LBRACE | LET | LETREC | LPAREN | NEWREF | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv345 * _menhir_state) * _menhir_state * 'tv_nonempty_list_letrecdec_)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (decs : 'tv_nonempty_list_letrecdec_)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 154 "src/parser.mly"
                                                             ( Letrec(decs, e2) )
# 1799 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv348)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv353 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv351 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState98 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv349 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 173 "src/parser.mly"
                                      ( Sub(Int 0, e) )
# 1835 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv354)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv359 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv357 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState100 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv355 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 166 "src/parser.mly"
                               (e)
# 1906 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)) : 'freshtv358)
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv360)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv365 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv363 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState102 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv361 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 157 "src/parser.mly"
                                           ( App(e1,e2) )
# 1949 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)) : 'freshtv364)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv366)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv371 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState104 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv367 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 159 "src/parser.mly"
                                       ( NewRef(e) )
# 1987 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv368)) : 'freshtv370)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv372)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv377 * _menhir_state)))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv375 * _menhir_state)))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState106 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv373 * _menhir_state)))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 155 "src/parser.mly"
                                                     ( Proc("_",e) )
# 2027 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)) : 'freshtv376)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv378)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv383 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2041 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv381 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2059 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState111 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv379 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2067 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 2073 "src/parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 156 "src/parser.mly"
                                                             ( Proc(x,e) )
# 2083 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv384)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv387 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2097 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTY | EMPTYLIST | END | EOF | HD | ID _ | IF | IN | INT _ | ISZERO | LBRACE | LET | LETREC | LPAREN | NEWREF | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv385 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2117 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 2122 "src/parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 163 "src/parser.mly"
                                    ( Set(x,e) )
# 2129 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv388)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState114 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv390)
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv392)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv397 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv395 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState116 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv393 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 161 "src/parser.mly"
                                                          ( SetRef(e1,e2) )
# 2238 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)) : 'freshtv396)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv398)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState118 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 169 "src/parser.mly"
                                   ( Tl(e) )
# 2276 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)) : 'freshtv402)
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv404)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv415 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState121 in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 83 "src/parser.mly"
       (Ast.prog)
# 2307 "src/parser.ml"
            ) = 
# 115 "src/parser.mly"
                 ( AProg e )
# 2311 "src/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv409) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 83 "src/parser.mly"
       (Ast.prog)
# 2319 "src/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 83 "src/parser.mly"
       (Ast.prog)
# 2327 "src/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv405) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 83 "src/parser.mly"
       (Ast.prog)
# 2335 "src/parser.ml"
            )) : (
# 83 "src/parser.mly"
       (Ast.prog)
# 2339 "src/parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv406)) : 'freshtv408)) : 'freshtv410)) : 'freshtv412)) : 'freshtv414)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | TIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv416)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv237) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv235) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) = _v in
    ((let _v : 'tv_exprs = let es = 
# 232 "<standard.mly>"
    ( xs )
# 2368 "src/parser.ml"
     in
    
# 186 "src/parser.mly"
                                            ( es )
# 2373 "src/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv233) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_exprs) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv231 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv227 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv225 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (es : 'tv_exprs)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_expr = 
# 164 "src/parser.mly"
                             ( BeginEnd(es) )
# 2397 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv226)) : 'freshtv228)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv97 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv99 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv101 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv103 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2441 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv105 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2450 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv107 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2459 "src/parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv109 * _menhir_state)))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv111 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv117 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_letrecdec) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv121 * _menhir_state) * _menhir_state * 'tv_nonempty_list_letrecdec_)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state) * _menhir_state * 'tv_nonempty_list_letrecdec_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv125 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 2508 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2512 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv127 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2521 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv129 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2530 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv131 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2539 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state * 'tv_field)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv135 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 2553 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv137 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv139 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv141 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv143 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv151 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv155 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv157 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv159 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv177 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv199 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 2717 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv203 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2731 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv205 * _menhir_state * (
# 23 "src/parser.mly"
       (string)
# 2740 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2744 "src/parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv209 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv213 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv215 * _menhir_state)))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2778 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv219 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv224)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv90)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv86)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
# 2937 "src/parser.ml"
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
# 2948 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv79 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3004 "src/parser.ml"
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

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            let (_menhir_stack : ('freshtv63 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "src/parser.mly"
       (string)
# 3034 "src/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv59 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 3045 "src/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv55 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 3055 "src/parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BEGIN ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | CONS ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | DEBUG ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | DEREF ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | EMPTY ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | EMPTYLIST ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | HD ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | ID _v ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                    | IF ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | INT _v ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                    | ISZERO ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | LBRACE ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | LET ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | LETREC ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | LPAREN ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | NEWREF ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | PROC ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | SET ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | SETREF ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | TL ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv56)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv57 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 3111 "src/parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv61 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 3122 "src/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)) : 'freshtv64)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv69 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv65 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BEGIN ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | CONS ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | DEBUG ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | DEREF ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | EMPTY ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | EMPTYLIST ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | HD ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | ID _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | IF ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | INT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | ISZERO ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | LBRACE ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | LET ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | LETREC ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | LPAREN ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | NEWREF ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | PROC ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | SET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | SETREF ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | TL ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv66)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv67 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
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

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv52)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | CONS ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | DEBUG ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | DEREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | EMPTY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | EMPTYLIST ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | HD ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LBRACE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LETREC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | MINUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState14 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16) : 'freshtv46)
    | NEWREF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState14 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_expr = 
# 165 "src/parser.mly"
                     ( Unit )
# 3374 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv48)) : 'freshtv50)
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "src/parser.mly"
       (string)
# 3413 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv37 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3424 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | CONS ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | DEBUG ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | DEREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | EMPTY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | EMPTYLIST ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | HD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LBRACE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LETREC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NEWREF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv38)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv39 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3480 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)) : 'freshtv42)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState26 in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_field__ = 
# 142 "<standard.mly>"
    ( [] )
# 3507 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_field__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "src/parser.mly"
       (int)
# 3582 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 22 "src/parser.mly"
       (int)
# 3592 "src/parser.ml"
    )) : (
# 22 "src/parser.mly"
       (int)
# 3596 "src/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 144 "src/parser.mly"
              ( Int i )
# 3601 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv30)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | CONS ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | DEBUG ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | DEREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | EMPTY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | EMPTYLIST ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | HD ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LBRACE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LETREC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NEWREF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/parser.mly"
       (string)
# 3659 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 23 "src/parser.mly"
       (string)
# 3669 "src/parser.ml"
    )) : (
# 23 "src/parser.mly"
       (string)
# 3673 "src/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 145 "src/parser.mly"
             ( Var x )
# 3678 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 167 "src/parser.mly"
                ( EmptyList )
# 3756 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv22)

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv14)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | CONS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | DEBUG ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | DEREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | EMPTY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | EMPTYLIST ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | HD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LBRACE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LETREC ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NEWREF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv6)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | CONS ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | DEBUG ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | DEREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | EMPTY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | EMPTYLIST ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | HD ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LBRACE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LETREC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEWREF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState45 in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 4069 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

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
# 83 "src/parser.mly"
       (Ast.prog)
# 4092 "src/parser.ml"
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
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CONS ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEBUG ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTYLIST ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | HD ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LBRACE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 4159 "src/parser.ml"
