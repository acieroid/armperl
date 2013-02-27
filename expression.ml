type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Concat
  | Or
  | And
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
  | String of string
  | Function of string list * expr list

and expr =
  | Value of value
  | Variable of string
  | BinOp of binop * expr * expr
  | Assign of string * expr
  | UnOp of unop * expr
  | Funcall of expr * expr list
  | Fundef of string * string list * expr list
        (* TODO: consequent can have multiple expressions *)
  | Cond of expr * expr * expr

let string_of_value = function
  | Integer x -> string_of_int x
  | String x -> x
  | Function (args, body) -> "<fun/" ^ (string_of_int (List.length args)) ^ ">"
