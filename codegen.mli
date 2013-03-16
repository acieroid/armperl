(** Code generator that produces ARM assembly code *)
open Expression

(** Generate the code given the list of the functions, the
    instructions, and the symbol table constructed by the parser *)
val gen : out_channel -> expr list * expr list -> Symtable.t -> unit
