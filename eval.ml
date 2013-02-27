open Expression
open Symtable

(* TODO: complete *)
let eval_binop op left right =
  match op with
  | Plus -> 
      (match left, right with
      | (Integer x, Integer y) -> Integer (x + y)
      | (Integer x, _) | (_, Integer x)-> Integer x
      | _ -> Integer 0)

let eval_unop op x =
  match op with
  | Not ->
      (match x with
      | Integer 0 -> Integer 1
      | _ -> Integer 0)
  | UnaryPlus -> x
  | UnaryMinus ->
      (* This is Perl's behaviour *)
      (match x with
      | Integer x -> Integer (-x)
      | String s -> String ("-" ^ s)
      | Function _ -> Integer 0)

let eval_fun symtable f args =
  match f with
  | _ -> failwith "TODO"

let rec eval symtable = function
  | Value x -> (x, symtable)
  | Variable name ->
      (match find symtable name with
      | Some v -> (v, symtable)
      | None -> failwith ("No such variable: " ^ name))
  | Assign (name, right) ->
      let value, symtable' = eval symtable right in
      value, add symtable name value
  | BinOp (op, left, right) ->
      let left', symtable' = eval symtable left in
      let right', symtable'' = eval symtable' right in
      eval_binop op left' right', symtable''
  | UnOp (op, x) ->
      let x', symtable' = eval symtable x in
      eval_unop op x', symtable'
  | Funcall (f, args) ->
      let f', symtable' = eval symtable f in
      let args_rev, symtable'' =
        List.fold_left
          (fun (l, st) x ->
            let x', st' = eval st x in
            (x::l, st'))
          ([], symtable') args in
      eval_fun symtable'' f' (List.rev args_rev)
  | Fundef (name, args, body) ->
      let f = Function (args, body) in
      f, (add symtable name f)
  | Cond (test, consequent, alternative) ->
      (* Non-0 is treated as true *)
      (match eval symtable test with
      | (Integer 0, symtable') -> eval symtable' alternative
      | (_, symtable') -> eval symtable' consequent) 

