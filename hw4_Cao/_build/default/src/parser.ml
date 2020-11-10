
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNPAIR
    | UNITTYPE
    | UNIT
    | TREETYPE
    | TL
    | TIMES
    | THEN
    | SETREF
    | SET
    | SEMICOLON
    | RPAREN
    | REFTYPE
    | RBRACE
    | PROC
    | PLUS
    | PAIR
    | NULL_QUESTION
    | NULLT_QUESTION
    | NODE
    | NEWREF
    | MINUS
    | LPAREN
    | LISTTYPE
    | LETREC
    | LET
    | LESS_THAN
    | LBRACE
    | ISZERO
    | INTTYPE
    | INT of (
# 22 "src/parser.mly"
       (int)
# 40 "src/parser.ml"
  )
    | IN
    | IF
    | ID of (
# 23 "src/parser.mly"
       (string)
# 47 "src/parser.ml"
  )
    | HD
    | GREATER_THAN
    | GETRST
    | GETLST
    | GETDATA
    | EQUALS
    | EOF
    | END
    | EMPTYTREE
    | EMPTYLIST
    | ELSE
    | DIVIDED
    | DEREF
    | DEBUG
    | CONS
    | COMMA
    | COLON
    | BOOLTYPE
    | BEGIN
    | ARROW
  
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
  | MenhirState174
  | MenhirState172
  | MenhirState171
  | MenhirState170
  | MenhirState168
  | MenhirState166
  | MenhirState165
  | MenhirState164
  | MenhirState163
  | MenhirState161
  | MenhirState159
  | MenhirState158
  | MenhirState157
  | MenhirState155
  | MenhirState153
  | MenhirState151
  | MenhirState150
  | MenhirState149
  | MenhirState148
  | MenhirState147
  | MenhirState145
  | MenhirState143
  | MenhirState141
  | MenhirState139
  | MenhirState138
  | MenhirState137
  | MenhirState136
  | MenhirState135
  | MenhirState134
  | MenhirState133
  | MenhirState131
  | MenhirState130
  | MenhirState129
  | MenhirState128
  | MenhirState127
  | MenhirState126
  | MenhirState124
  | MenhirState122
  | MenhirState120
  | MenhirState118
  | MenhirState116
  | MenhirState114
  | MenhirState112
  | MenhirState111
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState102
  | MenhirState100
  | MenhirState99
  | MenhirState94
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState86
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState67
  | MenhirState65
  | MenhirState62
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState34
  | MenhirState31
  | MenhirState27
  | MenhirState26
  | MenhirState24
  | MenhirState23
  | MenhirState22
  | MenhirState19
  | MenhirState15
  | MenhirState12
  | MenhirState10
  | MenhirState7
  | MenhirState0

# 8 "src/parser.mly"
  
open Ast

# 180 "src/parser.ml"

let rec _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INTTYPE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LESS_THAN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LISTTYPE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LPAREN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | REFTYPE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 223 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 235 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | CONS ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | DEBUG ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | DEREF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | EMPTYLIST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | EMPTYTREE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | GETDATA ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | GETLST ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | GETRST ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | HD ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | IF ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | ISZERO ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LET ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LETREC ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LPAREN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NEWREF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NODE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NULLT_QUESTION ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NULL_QUESTION ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | PAIR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | CONS ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | DEBUG ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | DEREF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | EMPTYLIST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | EMPTYTREE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | GETDATA ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | GETLST ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | GETRST ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | HD ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | IF ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | ISZERO ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LET ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LETREC ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LPAREN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NEWREF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NODE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NULLT_QUESTION ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NULL_QUESTION ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | PAIR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | CONS ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | DEBUG ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | DEREF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | EMPTYLIST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | EMPTYTREE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | GETDATA ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | GETLST ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | GETRST ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | HD ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | IF ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | ISZERO ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LET ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LETREC ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LPAREN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NEWREF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NODE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NULLT_QUESTION ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NULL_QUESTION ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | PAIR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | CONS ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | DEBUG ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | DEREF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | EMPTYLIST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | EMPTYTREE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | GETDATA ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | GETLST ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | GETRST ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | HD ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | IF ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | ISZERO ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LET ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LETREC ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LPAREN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NEWREF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NODE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NULLT_QUESTION ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NULL_QUESTION ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | PAIR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_goto_texpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOLTYPE ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | INTTYPE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LESS_THAN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LISTTYPE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LPAREN ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | REFTYPE ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | TREETYPE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | UNITTYPE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | GREATER_THAN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))), _, (t2 : (Ast.texpr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 217 "src/parser.mly"
                                                             ( PairType(t1, t2) )
# 569 "src/parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | EQUALS | GETDATA | GETLST | GETRST | GREATER_THAN | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Ast.texpr))), _, (t2 : (Ast.texpr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.texpr) = 
# 213 "src/parser.mly"
                                    ( FuncType(t1,t2) )
# 592 "src/parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Ast.texpr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 219 "src/parser.mly"
                                          ( ListType(t) )
# 619 "src/parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 214 "src/parser.mly"
                                 ( t1 )
# 645 "src/parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.texpr) = 
# 215 "src/parser.mly"
                          ( RefType(t1) )
# 662 "src/parser.ml"
         in
        _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Ast.texpr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 221 "src/parser.mly"
                                          ( TreeType(t) )
# 683 "src/parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | CONS ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | DEBUG ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | DEREF ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | EMPTYLIST ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | EMPTYTREE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | GETDATA ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | GETLST ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | GETRST ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | HD ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | ID _v ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
                | IF ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | INT _v ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
                | ISZERO ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | LET ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | LETREC ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | LPAREN ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | NEWREF ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | NODE ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | NULLT_QUESTION ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | NULL_QUESTION ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | PAIR ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | PROC ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | SET ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | SETREF ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | TL ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | UNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | UNPAIR ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOLTYPE ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | INTTYPE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | LESS_THAN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | LISTTYPE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | LPAREN ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | REFTYPE ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | TREETYPE ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | UNITTYPE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (ele_type : (Ast.texpr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expr) = 
# 197 "src/parser.mly"
                                  ( EmptyTree(ele_type) )
# 922 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (ele_type : (Ast.texpr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expr) = 
# 191 "src/parser.mly"
                                  ( EmptyList(ele_type) )
# 945 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState102 | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState99 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1047 "src/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 172 "src/parser.mly"
                                  ( Mul(e1,e2) )
# 1062 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 170 "src/parser.mly"
                                 ( Add(e1,e2) )
# 1081 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 173 "src/parser.mly"
                                    ( Div(e1,e2) )
# 1096 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 171 "src/parser.mly"
                                  ( Sub(e1,e2) )
# 1115 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState110 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState112 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (he : (Ast.expr))), _), _, (te : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 192 "src/parser.mly"
                                                        ( Cons(he, te) )
# 1230 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState114 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 169 "src/parser.mly"
                                    ( Debug(e) )
# 1262 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState116 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 180 "src/parser.mly"
                                      ( DeRef(e) )
# 1294 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState118 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 200 "src/parser.mly"
                                        ( GetData(e) )
# 1326 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState120 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 201 "src/parser.mly"
                                       ( GetLST(e) )
# 1358 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState122 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 202 "src/parser.mly"
                                       ( GetRST(e) )
# 1390 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState124 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 194 "src/parser.mly"
                                   ( Hd(e) )
# 1422 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState126 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState128 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 182 "src/parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 1621 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState131 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 178 "src/parser.mly"
                                       ( IsZero(e) )
# 1651 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState133 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1762 "src/parser.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 174 "src/parser.mly"
                                                    ( Let(x,e1,e2) )
# 1770 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState136 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1879 "src/parser.ml"
            ))), (y : (
# 23 "src/parser.mly"
       (string)
# 1883 "src/parser.ml"
            ))), _, (targ : (Ast.texpr))), _, (tr : (Ast.texpr))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _12 = () in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 175 "src/parser.mly"
                                                                                                                   ( Letrec(x,y,targ,tr,e1,e2) )
# 1895 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState139 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 186 "src/parser.mly"
                                      ( Sub(Int 0, e) )
# 1925 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState141 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 185 "src/parser.mly"
                               (e)
# 2002 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState143 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 177 "src/parser.mly"
                                           ( App(e1,e2) )
# 2043 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState145 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 179 "src/parser.mly"
                                       ( NewRef(e) )
# 2075 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState147 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState149 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState151 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (data_exp : (Ast.expr))), _), _, (lst_exp : (Ast.expr))), _), _, (rst_exp : (Ast.expr))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 198 "src/parser.mly"
                                                                                          ( Node(data_exp, lst_exp, rst_exp) )
# 2277 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState153 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 199 "src/parser.mly"
                                               ( NullT(e) )
# 2309 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState155 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 193 "src/parser.mly"
                                              ( Null(e) )
# 2341 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState157 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState159 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 188 "src/parser.mly"
                                                        ( Pair(e1, e2) )
# 2458 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState161 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 2486 "src/parser.ml"
            ))), _, (t : (Ast.texpr))), _, (e : (Ast.expr))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 176 "src/parser.mly"
                                                                             ( Proc(x,t,e) )
# 2497 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 2524 "src/parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 183 "src/parser.mly"
                                    ( Set(x,e) )
# 2531 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState164 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165)
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState166 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 181 "src/parser.mly"
                                                          ( SetRef(e1,e2) )
# 2646 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState168 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 195 "src/parser.mly"
                                   ( Tl(e) )
# 2678 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState170 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (id1 : (
# 23 "src/parser.mly"
       (string)
# 2789 "src/parser.ml"
            ))), (id2 : (
# 23 "src/parser.mly"
       (string)
# 2793 "src/parser.ml"
            ))), _, (e_pair : (Ast.expr))), _), _, (e_body : (Ast.expr))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 189 "src/parser.mly"
                                                                                                  ( Unpair(id1, id2, e_pair, e_body) )
# 2804 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState174 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 104 "src/parser.mly"
       (Ast.prog)
# 2827 "src/parser.ml"
            ) = 
# 136 "src/parser.mly"
                 ( AProg e )
# 2831 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 104 "src/parser.mly"
       (Ast.prog)
# 2838 "src/parser.ml"
            )) = _v in
            Obj.magic _1
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | TIMES ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
    | _ ->
        _menhir_fail ()

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 212 "src/parser.mly"
               ( UnitType )
# 2862 "src/parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOLTYPE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | INTTYPE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LESS_THAN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LISTTYPE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LPAREN ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | REFTYPE ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | TREETYPE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | UNITTYPE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | INTTYPE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LESS_THAN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LISTTYPE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LPAREN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | REFTYPE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | INTTYPE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | LESS_THAN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | LISTTYPE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | LPAREN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | REFTYPE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOLTYPE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INTTYPE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LESS_THAN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LISTTYPE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LPAREN ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | REFTYPE ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | TREETYPE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | UNITTYPE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INTTYPE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LESS_THAN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LISTTYPE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAREN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | REFTYPE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 209 "src/parser.mly"
              ( IntType )
# 3031 "src/parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 210 "src/parser.mly"
               ( BoolType )
# 3043 "src/parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs : (Ast.expr list)) = _v in
    let _v : (Ast.expr list) = let es = 
# 232 "<standard.mly>"
    ( xs )
# 3055 "src/parser.ml"
     in
    
# 206 "src/parser.mly"
                                            ( es )
# 3060 "src/parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (es : (Ast.expr list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expr) = 
# 184 "src/parser.mly"
                             ( BeginEnd(es) )
# 3077 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | RPAREN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | EQUALS ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | BEGIN ->
                                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | CONS ->
                                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | DEBUG ->
                                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | DEREF ->
                                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | EMPTYLIST ->
                                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | EMPTYTREE ->
                                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | GETDATA ->
                                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | GETLST ->
                                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | GETRST ->
                                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | HD ->
                                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | ID _v ->
                                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
                            | IF ->
                                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | INT _v ->
                                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
                            | ISZERO ->
                                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LET ->
                                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LETREC ->
                                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LPAREN ->
                                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NEWREF ->
                                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NODE ->
                                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NULLT_QUESTION ->
                                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NULL_QUESTION ->
                                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | PAIR ->
                                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | PROC ->
                                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | SET ->
                                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | SETREF ->
                                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | TL ->
                                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | UNIT ->
                                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | UNPAIR ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 168 "src/parser.mly"
           ( Unit )
# 3593 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOLTYPE ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | INTTYPE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | LESS_THAN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | LISTTYPE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | LPAREN ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | REFTYPE ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | TREETYPE ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | UNITTYPE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | CONS ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEBUG ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEREF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | EMPTYLIST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | EMPTYTREE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | GETDATA ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | GETLST ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | GETRST ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | HD ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IF ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | ISZERO ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LET ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LETREC ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAREN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MINUS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState56 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | NEWREF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NODE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NULLT_QUESTION ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NULL_QUESTION ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PAIR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOLTYPE ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | INTTYPE ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | LESS_THAN ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | LISTTYPE ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | LPAREN ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | REFTYPE ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | TREETYPE ->
                        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | UNITTYPE ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | CONS ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | DEBUG ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | DEREF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | EMPTYLIST ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | EMPTYTREE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | GETDATA ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | GETLST ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | GETRST ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | HD ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | ISZERO ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LET ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LETREC ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LPAREN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NEWREF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NODE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NULLT_QUESTION ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NULL_QUESTION ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | PAIR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "src/parser.mly"
       (int)
# 4672 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 22 "src/parser.mly"
       (int)
# 4680 "src/parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 165 "src/parser.mly"
              ( Int i )
# 4685 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | CONS ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | DEBUG ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | DEREF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | EMPTYLIST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | EMPTYTREE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | GETDATA ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | GETLST ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | GETRST ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | HD ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IF ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | ISZERO ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LET ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LETREC ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LPAREN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NEWREF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NODE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NULLT_QUESTION ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NULL_QUESTION ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | PAIR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/parser.mly"
       (string)
# 4759 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 23 "src/parser.mly"
       (string)
# 4767 "src/parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 166 "src/parser.mly"
             ( Var x )
# 4772 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | INTTYPE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LESS_THAN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LISTTYPE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LPAREN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | REFTYPE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | INTTYPE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LESS_THAN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LISTTYPE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LPAREN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | REFTYPE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | CONS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | DEBUG ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | DEREF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | EMPTYLIST ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | EMPTYTREE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | GETDATA ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | GETLST ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | GETRST ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | HD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | ID _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | INT _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | ISZERO ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LET ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LETREC ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LPAREN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NEWREF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NODE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NULLT_QUESTION ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NULL_QUESTION ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PAIR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | CONS ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | DEBUG ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | DEREF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | EMPTYLIST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | EMPTYTREE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | GETDATA ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | GETLST ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | GETRST ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | HD ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | IF ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | ISZERO ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | LET ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | LETREC ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | LPAREN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NEWREF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NODE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NULLT_QUESTION ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NULL_QUESTION ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | PAIR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState94 in
        let _v : (Ast.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 5444 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

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
# 104 "src/parser.mly"
       (Ast.prog)
# 5467 "src/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CONS ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEBUG ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTYLIST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTYTREE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GETDATA ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GETLST ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GETRST ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | HD ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NODE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NULLT_QUESTION ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NULL_QUESTION ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PAIR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 5545 "src/parser.ml"
