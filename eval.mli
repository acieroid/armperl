(** Evaluate a Perl expression *)
val eval : Symtable.t -> Expression.expr -> Expression.value * Symtable.t
