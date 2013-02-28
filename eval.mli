(** Evaluate a sequence of Perl expressions *)
val eval_sequence : Symtable.t -> bool -> Expression.expr list -> Expression.value * Symtable.t

(** Evaluate a Perl expression *)
val eval : Symtable.t -> bool -> Expression.expr -> Expression.value * Symtable.t
