(** A perl expression *)
type expr =
  | Integer of int (** A simple integer *)
  | String of string (** A simple string *)
  | Variable of string (** A variable, represented by its name *)
  | BinOp of binop * expr * expr (** A binary operation *)
  | UnOp of unop * expr (** An unary operation *)
   (** A function call, contains the function name and the arguments *)
  | Funcall of string * expr list
   (** A function definition, contains: the function name, the arguments names, and the body *)
  | Fundef of string * string list * expr list
   (** A condition, contains: the test, the consequent body, the alternative *)
  | Cond of expr * expr list * expr

(** The possible binary operators *)
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

(** The possible unary operators *)
type unop =
  | Not
  | UnaryPlus
  | UnaryMinus
