(** Implement symbol table used by the code generator to hold the
    addresses of the global variables *)

(** Raised when tryig to redefine a function that is already defined *)
exception Already_defined

(** The symbol table *)
type t

(** Return a new empty symbol table *)
val create : unit -> t

(** Add a global variable to the symbol table. *)
val add_global : t -> string -> unit

(** Get the address of a variable in the symbol table. Addresses are
    4-bit aligned (ie. are multiple of 4). *)
val get_global_addr : t -> string -> int

(** Do a traversal of the globals variables of the symbol table, in
    the order in which the elements where added *)
val iter_globals : t -> (int -> string -> unit) -> unit

(** Change the local variables of the symbol table *)
val set_locals : t -> string list -> unit

(** Return the current local variables (or None) *)
val get_locals : t -> string list option

(** Clear the local variables *)
val clear_locals : t -> unit

(** Return true if the given variable is a local variable *)
val is_local : t -> string -> bool

(** Add a variable to the global variables if it is not already in the
    local variables *)
val add_var : t -> string -> unit

(** Add a function (and its number of parameters) to the symbol
    table. Raise Already_defined if the function is already defined. *)
val add_fun : t -> string -> int -> unit

(** Get the number of parameters of a function. Raise Not_found if the
    function is not defined *)
val get_fun : t ->  string -> int
