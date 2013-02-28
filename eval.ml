open Expression
open Symtable

(* TODO: int_repr "4foo" -> 4, float *)
let int_repr = function
    | Integer x -> x
    | True -> 1
    | False -> 0
    | _ -> 0

let int_op op left right =
  Integer (op (int_repr left) (int_repr right))

let int_comp comp left right =
  if comp (int_repr left) (int_repr right) then  True else False

let str_repr = function
  | String x -> x
  | Integer x -> string_of_int x
  | Float x -> string_of_float x
  | True -> "1"
  | False -> ""
  | Function _ -> ""

let str_op op left right =
  String (op (str_repr left) (str_repr right))

let str_comp comp left right =
  if comp (str_repr left) (str_repr right) then True else False

(* TODO: complete *)
let eval_binop op left right =
  match op with
  | Plus -> int_op (+) left right
  | Minus -> int_op (-) left right
  | Times -> int_op ( * ) left right
  | Divide -> int_op (/) left right
  | Concat -> str_op (^) left right
  | Equals -> int_comp (=) left right
  | Different -> int_comp (<>) left right
  | Greater -> int_comp (>) left right
  | Lower -> int_comp (<) left right
  | GreaterEquals -> int_comp (>=) left right
  | LowerEquals -> int_comp (<=) left right
  | StrEquals -> str_comp (=) left right
  | StrDifferent -> str_comp (<>) left right
  | StrGreater -> str_comp (>) left right
  | StrLower -> str_comp (<) left right
  | StrGreaterEquals -> str_comp (>=) left right
  | StrLowerEquals -> str_comp (<=) left right

let eval_unop op x =
  match op with
  | Not ->
      (match x with
      | Integer 0 -> Integer 1
      | Float 0. -> Float 1.
      | _ -> Integer 0)
  | UnaryPlus -> x
  | UnaryMinus ->
      (* This is Perl's behaviour *)
      (match x with
      | Integer x -> Integer (-x)
      | Float x -> Float (-.x)
      | False -> False
      | True -> Integer (-1)
      | String s -> String ("-" ^ s)
      | Function _ -> Integer 0)

let rec bind_params symtable names args =
  match (names, args) with
  | [], [] -> symtable
  | [], _ -> failwith "too many arguments"
  | _, [] -> failwith "not enough arguments"
  | (name::tl_names), (arg::tl_args) ->
      bind_params (add symtable name arg) tl_names tl_args

let rec eval_sequence symtable = function
  | [] -> Integer 0, symtable
  | [last] -> eval symtable last
  | hd::tl ->
      let _, symtable' = eval symtable hd in
      eval_sequence symtable' tl

and eval_fun symtable f args =
  match f with
  | Function (names, body) ->
      let symtable' = bind_params symtable names args in
      (* TODO: the function might change global variables. We need a
      separate symbol table for global variables. *)
      let v, _ = eval_sequence symtable' body in
      v, symtable
  | _ ->  failwith "Cannot call a non-function"

and eval symtable = function
  | Value x -> (x, symtable)
  | Variable name ->
      (match find symtable name with
      | Some v -> (v, symtable)
      | None -> failwith ("No such variable: " ^ name))
  | Assign (name, right) ->
      let value, symtable' = eval symtable right in
      value, add symtable name value
  | Or (left, right) ->
      let left', symtable' = eval symtable left in
      (match left' with
      | False -> eval symtable' right
      | _ -> left', symtable')
  | And (left, right) -> 
      let left', symtable' = eval symtable left in
      (match left' with
      | False -> False, symtable'
      | _ ->
          let right', symtable'' = eval symtable' right in
          (match right' with
          | False -> False, symtable''
          | _ -> left', symtable''))
  | BinOp (op, left, right) ->
      let left', symtable' = eval symtable left in
      let right', symtable'' = eval symtable' right in
      eval_binop op left' right', symtable''
  | UnOp (op, x) ->
      let x', symtable' = eval symtable x in
      eval_unop op x', symtable'
  | Funcall (f, args) ->
      let f' =
        (match find symtable f with
        | Some x -> x
        | None -> failwith ("Undefined function: " ^ f)) in
      let args_rev, symtable' =
        List.fold_left
          (fun (l, st) x ->
            let x', st' = eval st x in
            (x'::l, st'))
          ([], symtable) args in
      eval_fun symtable' f' (List.rev args_rev)
  | Fundef (name, args, body) ->
      let f = Function (args, body) in
      f, (add symtable name f)
  | Cond (test, consequent, alternative) ->
      (* Non-0 is treated as true *)
      (match eval symtable test with
      | (Integer 0, symtable') -> eval symtable' alternative
      | (_, symtable') -> eval symtable' consequent) 

