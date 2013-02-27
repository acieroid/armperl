(** This module implements immutable symbol tables
    TODO: separate the functions and the variables, as in Perl (a
    function and a variable can have the same name)
 *)

(** The symbol table *)
type t

(** The empty symbol table *)
val empty : t

(** Find an entry in a symbol table, if it exists *)
val find : t -> string -> Expression.value option

(** Add an entry in a symbol table. Return the new symbol table *)
val add : t -> string -> Expression.value -> t
