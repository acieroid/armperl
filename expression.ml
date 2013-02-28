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

(* TODO: add Float *)
type value =
  | Integer of int
  | Float of float
  | String of string
  | True | False
  | Undef

type perl_function = {
    name: string;
    defined: bool;
    args: string list;
    body: expr list;
  }

(* TODO: return *)
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
        (* TODO: consequent can have multiple expressions *)
  | Cond of expr * expr * expr

let string_of_value = function
  | Integer x -> string_of_int x
  | Float x -> string_of_float x
  | True -> "1"
  | False -> ""
  | String x -> x
  | Undef -> "undef"
