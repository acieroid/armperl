(** Implement a string table, used to store the strings during
    compilation *)

(** The string table *)
type t

(** Return a new empty string table *)
val create : unit -> t

(** Add a string to the string table *)
val add : t -> string -> unit

(** Get the address of a string. Raise Not_found if the string is not
    in the string table *)
val get_addr : t -> string -> int

(** Iterate over the values of the string table *)
val iter : t -> (int -> string -> unit) -> unit
