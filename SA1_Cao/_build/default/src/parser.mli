
(* The type of tokens. *)

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
  | INT of (int)
  | IN
  | IF
  | ID of (string)
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)
