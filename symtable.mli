(** This module implements immutable symbol tables *)

(** The symbol table *)
type t

(** The empty symbol table *)
val empty : t

(** Find an entry in a symbol table, if it exists *)
val find : t -> string -> Some Expression.value

(** Add an entry in a symbol table. Return the new symbol table *)
val add : t -> string -> Expression.value -> t
