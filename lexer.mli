open Utils

(** All the possibles tokens *)
type token =
  | VAR of string
  | INTEGER of int
  | STRING of string
  | IDENTIFIER of string
  | SUB
  | RETURN
  | CALL_MARK
  | LBRACE
  | RBRACE
  | LPAR
  | RPAR
  | SEMICOLON
  | COMMA
  | IF
  | UNLESS
  | ELSE
  | ELSEIF
  | NOT
  | NOT_WORD
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | CONCAT
  | LAZY_OR
  | LAZY_AND
  | EQUALS
  | DIFFERENT
  | GREATER
  | LOWER
  | GREATER_EQUALS
  | LOWER_EQUALS
  | STRING_EQUALS
  | STRING_DIFFERENT
  | STRING_GREATER
  | STRING_LOWER
  | STRING_GREATER_EQUALS
  | STRING_LOWER_EQUALS
  | EOF

(** Return a string describing the given token *)
val string_of_token : token -> string

(** Lex a token from a stream of characters. Return a token on
    success, and a string describing the error on failure. *)
val lexer : char Stream.t -> (string, token) either
