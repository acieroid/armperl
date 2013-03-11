(** Evaluate a sequence of Perl expressions *)
val eval_sequence : Eval_symtable.t -> bool -> Expression.expr list -> Expression.value * Eval_symtable.t

(** Evaluate a Perl expression *)
val eval : Eval_symtable.t -> bool -> Expression.expr -> Expression.value * Eval_symtable.t
