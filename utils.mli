(** Type to handle errors. If an either value is Right, then the
computation suceeded and returned a 'b, if it is Left, the computation
failed with error described by a 'a *)
type ('a,'b) either = Left of 'a | Right of 'b

(** Convert a list of character to a string *)
val implode : char list -> string

(** Convert a string to a list of characters *)
val explode : string -> char list

(** Return a stream that reads an input channel character by character *)
val stream_of_input : in_channel -> char Stream.t
