open Utils
open Tokens

(** The lexer state *)
type lexer_state

(** Create a lexer state from an input channel *)
val state_of_channel : in_channel -> lexer_state

(** Return a string describing the given token *)
val string_of_token : token -> string

(** Lex a token from a state. Return a token on
    success, and a string describing the error on failure. *)
val lexer : lexer_state -> (string, token) either

(** Return a stream generating all the lexemes for a lexer state *)
val to_stream : lexer_state -> (string, token) either Stream.t

(** Combine state_of_channel and to_stream *)
val lex : in_channel -> (string, token) either Stream.t
