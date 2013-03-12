(** Implement symbol table used by the code generator to hold the
    addresses of the global variables *)

(** The symbol table *)
type t

(** Return a new empty symbol table *)
val empty : unit -> t

(** Add a variable to the symbol table. If the value is already in the
    table, don't overwrite it (the previous value is kept) *)
val add : t -> string -> Expression.value -> unit

(** Find a variable in the symbol table *)
val find : t -> string -> Expression.value

(** Get the address of a variable in the symbol table. Addresses are
    4-bit aligned (ie. are multiple of 4) *)
val get_addr : t -> string -> int

(** Do a traversal of the symbol table, in the order in which the
    elements where added *)
val iter : t -> (int -> string -> Expression.value -> unit) -> unit
