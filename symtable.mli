(** This module implements immutable symbol tables
    TODO: separate the functions and the variables, as in Perl (a
    function and a variable can have the same name)
 *)

(** The symbol table *)
type t

(** Return a new empty symbol table *)
val empty : unit -> t

(** Find an variable in a symbol table. If it does not exists, return Undef *)
val find_var : t -> string -> Expression.value

(** Set an variable in a symbol table. Return the new symbol table *)
val set_var : t -> string -> Expression.value -> t

(** Find a function in a symbol table *)
val find_fun : t -> string -> Expression.perl_function

(** Set a function in a symbol table *)
val set_fun : t -> string -> Expression.perl_function -> t

(** Find a global variable in a symbol table (or undef) *)
val find_global : t -> string -> Expression.value

(** Set a global variable in a symbol table *)
val set_global : t -> string -> Expression.value -> t
