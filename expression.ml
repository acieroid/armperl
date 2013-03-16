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
  | CondEnd
  | Return of expr

let string_of_value = function
  | Integer x -> string_of_int x
  | Float x -> string_of_float x
  | True -> "1"
  | False -> ""
  | String x -> x
  | Undef -> ""

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

let rec string_of_expression = function
  | Value v -> string_of_value v
  | Variable v -> v
  | BinOp (op, left, right) ->
      "BinOp(" ^
      (string_of_binop op) ^ ", " ^
      (string_of_expression left) ^ ", " ^
      (string_of_expression right) ^ ")"
  | Assign (name, v) ->
      "Assign(" ^ name ^ ", " ^ (string_of_expression v)
  | Or (e1, e2) ->
      "Or(" ^
      (string_of_expression e1) ^ ", " ^
      (string_of_expression e2) ^ ")"
  | And (e1, e2) ->
      "And(" ^
      (string_of_expression e1) ^ ", " ^
      (string_of_expression e2) ^ ")"
  | UnOp (op, e) ->
      "UnOp(" ^ (string_of_unop op) ^ ", " ^ (string_of_expression e)
  | Funcall (fn, args) ->
      "Funcall(" ^ fn ^ ", [" ^
      (String.concat ", " (List.map string_of_expression args)) ^
      "])"
  | Fundef (name, args, body) ->
      "Fundef(" ^ name ^ ", [" ^
      (String.concat ", " args) ^ "], [" ^
      (String.concat ", " (List.map string_of_expression body)) ^
      "])"
  | Cond (cond, consequent, alternative) ->
      "Cond(" ^
      (string_of_expression cond) ^ ", [" ^
      (String.concat ", " (List.map string_of_expression consequent)) ^ ", " ^
      (string_of_expression alternative) ^ ")"
  | CondEnd ->
      "CondEnd"
  | Return e -> "Return(" ^ (string_of_expression e) ^ ")"
      
