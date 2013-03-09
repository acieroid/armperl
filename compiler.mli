open Utils
open Tokens

module type Lexer = sig 
  val lex : in_channel -> (string, token) either Stream.t
end

module type Parser =
  sig 
    val parse : token Stream.t -> Expression.expr
  end

module type CodeGenerator =
  sig
    val gen : Expression.expr -> char Stream.t
  end
