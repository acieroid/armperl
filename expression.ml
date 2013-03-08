type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Concat
  | Equals
  | Different
  | Greater
  | Lower
  | GreaterEquals
  | LowerEquals
  | StrEquals
  | StrDifferent
  | StrGreater
  | StrLower
  | StrGreaterEquals
  | StrLowerEquals

type unop =
  | Not
  | UnaryPlus
  | UnaryMinus

type value =
  | Integer of int
  | Float of float
  | String of string
  | True | False
  | Undef

(* TODO: standard functions *)
type perl_function = {
    name: string;
    defined: bool;
    args: string list;
    body: expr list;
  }

and expr =
  | Value of value
  | Variable of string
  | BinOp of binop * expr * expr
  | Assign of string * expr
  | Or of expr * expr
  | And of expr * expr
  | UnOp of unop * expr
  | Funcall of string * expr list
  | Fundef of string * string list * expr list
  | Cond of expr * expr list * expr
  | Return of expr

let string_of_value = function
  | Integer x -> string_of_int x
  | Float x -> string_of_float x
  | True -> "1"
  | False -> ""
  | String x -> x
  | Undef -> "undef"

let string_of_unop = function
  | Not -> "not"
  | UnaryPlus -> "+"
  | UnaryMinus -> "-"

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Concat -> "."
  | Equals -> "=="
  | Different -> "!="
  | Greater -> ">"
  | Lower -> "<"
  | GreaterEquals -> ">="
  | LowerEquals -> "<="
  | StrEquals -> "eq"
  | StrDifferent -> "ne"
  | StrGreater -> "gt"
  | StrLower -> "lt"
  | StrGreaterEquals -> "ge"
  | StrLowerEquals -> "le"

let string_of_expression = function
  | Value v -> string_of_value
  | Variable v -> v
  | BinOp (op, left, right) ->
      (string_of_binop op) ^ "(" ^
      (string_of_expression left) ^ ", "
      (string_of_expression right) ^ ")"
  | Assign (name, v) ->
      "Assign(" ^ name ^ ", " ^ (string_of_value v)
  | Or (e1, e2) -> "Or(" ^ e1 ^ ", " ^ e2 ^ ")"
  | And (e1, e2) -> "And(" ^ e1 ^ ", " ^ e2 ^ ")"
  | UnOp (op, e) -> "UnOp(" ^ (string_of_unop op) ^ ", " ^ (string_of_expression e)
  | Funcall (fn, args) -> "Funcall(" ^ fn ^ ", [" ^
      (List.fold_left (fun x, y -> x ^ ", " ^ (string_of_expression y)) args) ^
      "])"
  | Fundef (name, args, body) -> "Fundef(" ^ fn ^ ", [" ^
      (List.fold_left (fun x, y -> x ^ ", " ^ y) args) ^ "], ["
      (List.fold_left (fun x, y -> x ^ ", " ^ (string_of_expression y)) body) ^
      "])"
  | Cond (cond, consequent, alternative) -> "Cond(" ^
      (string_of_expression cond) ^ ", [" ^
      (List.fold_left (fun x, y -> x ^ ", " ^ (string_of_expression y)) body) ^ ", " ^
      (string_of_expression alternative) ^ ")"
  | Return e -> "Return(" ^ (string_of_expression e) ^ ")"
      
