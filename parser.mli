(** The parser *)
open Expression
open Tokens

(** Parse a program from token stream (returned by the lexer).
    Return (fns, instrs), symtable where:
      - fns contains the function definitions of the program
      - instrs contains the instruction of the program which aren't in
        any function
      - symtable is the symbol table filled with the information
        extracted during the parsing *)
val parse : token Stream.t -> (expr list * expr list) * Symtable.t
