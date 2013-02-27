(* TODO: return the new symbol table *)
let rec eval symtable = function
  | Value x -> x
  | Variable name ->
      (match Symtable.find with
      | Some v -> (v, symtable)
      | None -> failwith ("No such variable: " ^ name))
  | BinOp (op, left, right) ->
      let symtable', left' = eval symtable left in
      let symtable'', right' = eval symtable' right
      eval_binop symtable'' op left' right'
  | UnOp (op, x) ->
      let symtable', x' = eval symtable x
      eval_unop symtable' op x'
  | Funcall (f, args) ->
      let symtable' f' = eval symtable f in
      let symtable'', args_rev =
        List.fold_left
          (fun (st, l) x ->
            let st', x' = eval st x in
            (st', x::l))
          args
      eval_fun symtable'' f' (List.rev args_rev)
  | Fundef (name, args, body) ->
      (Symtable.add name (Function args, body), Function (args, body))
  | Cond (test, consequent, alternative) ->
      (* Non-0 is treated as true *)
      (match eval symtable test with
      | (Integer 0, symtable') -> eval symtable' alternative
      | (_, symtable') -> eval symtable' consequent) 
