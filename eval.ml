open Expression
open Symtable

let int_repr = function
    | Integer x -> x
    | True -> 1
    | False -> 0
    | String x -> Scanf.sscanf x "%d" (fun x -> x)
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
  | Undef -> ""

let str_op op left right =
  String (op (str_repr left) (str_repr right))

let str_comp comp left right =
  if comp (str_repr left) (str_repr right) then True else False

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
      | Undef -> Integer 0)

let rec bind_params symtable names args =
  match (names, args) with
  | [], [] -> symtable
  | [], _ -> failwith "too many arguments"
  | _, [] -> failwith "not enough arguments"
  | (name::tl_names), (arg::tl_args) ->
      bind_params (set_var symtable name arg) tl_names tl_args

let rec eval_sequence symtable local = function
  | [] -> Integer 0, symtable
  | [last] -> eval symtable local last
  | hd::tl ->
      let _, symtable' = eval symtable local hd in
      eval_sequence symtable' local tl

and eval_fun symtable f args =
  let symtable' = bind_params symtable f.args args in
  let v, _ = eval_sequence symtable' true f.body in
  v, symtable

and eval symtable local = function
  | Value x -> (x, symtable)
  | Variable name ->
      let local = find_var symtable name in
      (match local with
      | Undef -> find_global symtable name
      | _ -> local), symtable
  | Assign (name, right) ->
      let local_exists = (find_var symtable name) <> Undef and
          value, symtable' = eval symtable local right in
        if local && local_exists then
          value, set_var symtable name value
        else
          value, set_global symtable name value
  | Or (left, right) ->
      let left', symtable' = eval symtable local left in
      (match left' with
      | False -> eval symtable' local right
      | _ -> left', symtable')
  | And (left, right) -> 
      let left', symtable' = eval symtable local left in
      (match left' with
      | False -> False, symtable'
      | _ ->
          let right', symtable'' = eval symtable' local right in
          (match right' with
          | False -> False, symtable''
          | _ -> left', symtable''))
  | BinOp (op, left, right) ->
      let left', symtable' = eval symtable local left in
      let right', symtable'' = eval symtable' local right in
      eval_binop op left' right', symtable''
  | UnOp (op, x) ->
      let x', symtable' = eval symtable local x in
      eval_unop op x', symtable'
  | Funcall (f, args) ->
      let f' =
        (match find_fun symtable f with
        | {defined=false} -> failwith ("Undefined function: " ^ f)
        | x -> x) in
      let args_rev, symtable' =
        List.fold_left
          (fun (l, st) x ->
            let x', st' = eval st local x in
            (x'::l, st'))
          ([], symtable) args in
      eval_fun symtable' f' (List.rev args_rev)
  | Fundef (name, args, body) ->
      let f = {name=name; args=args; body=body; defined=true} in
      Undef, (set_fun symtable name f)
  | Cond (test, consequents, alternative) ->
      (* Non-0 is treated as true *)
      (match eval symtable local test with
      | (Integer 0, symtable') -> eval symtable' local alternative
      | (_, symtable') -> eval_sequence symtable' local consequents) 

