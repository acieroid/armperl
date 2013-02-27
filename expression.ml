type value =
  | Integer of int
  | String of string
  | Function of string list * expr list

type expr =
  | Value of value
  | Variable of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Funcall of expr * expr list
  | Fundef of string * string list * expr list
  | Cond of expr * expr list * expr

type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Assign
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
