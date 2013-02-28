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

(* TODO: add Bool and Float *)
type value =
  | Integer of int
  | Float of float
  | String of string
  | True | False
  | Function of string list * expr list

and expr =
  | Value of value
  | Variable of string
  | BinOp of binop * expr * expr
  | Assign of string * expr
  | Or of expr * expr
  | And of expr * expr
  | UnOp of unop * expr
  | Funcall of expr * expr list
  | Fundef of string * string list * expr list
        (* TODO: consequent can have multiple expressions *)
  | Cond of expr * expr * expr

let string_of_value = function
  | Integer x -> string_of_int x
  | Float x -> string_of_float x
  | True -> "1"
  | False -> ""
  | String x -> x
  | Function (args, body) -> "<fun/" ^ (string_of_int (List.length args)) ^ ">"
