(** This module implements an immutable symbol table, that is used by
    the evaluator *)

(** The symbol table *)
type t

(** Type representing a perl function *)
type perl_function = {
    name: string;                 (** The name of the function *)
    defined: bool;                (** Is this function already defined? *)
    args: string list;            (** The arguments of the function *)
    body: Expression.expr list;   (** The body of the function *)
  }

(** Return a new empty symbol table *)
val empty : unit -> t

(** Find an variable in a symbol table. If it does not exists, return Undef *)
val find_var : t -> string -> Expression.value

(** Set an variable in a symbol table. Return the new symbol table *)
val set_var : t -> string -> Expression.value -> t

(** Find a function in a symbol table *)
val find_fun : t -> string -> perl_function

(** Set a function in a symbol table *)
val set_fun : t -> string -> perl_function -> t

(** Find a global variable in a symbol table (or undef) *)
val find_global : t -> string -> Expression.value

(** Set a global variable in a symbol table *)
val set_global : t -> string -> Expression.value -> t
