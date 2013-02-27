(** A Perl value *)
type value =
  | Integer of int (** A simple integer *)
  | String of string (** A simple string *)
  | Function of string list * expr list (** A function *)
  | Void (** no value *)

(** A Perl expression *)
type expr =
  | Value of value (** A Perl value *)
  | Variable of string (** A variable, represented by its name *)
  | BinOp of binop * expr * expr (** A binary operation *)
  | UnOp of unop * expr (** An unary operation *)
   (** A function call, contains the expression describing the
   function and the arguments *)
  | Funcall of expr * expr list
   (** A function definition, contains: the function name, the
   arguments names, and the body *)
  | Fundef of string * string list * expr list
   (** A condition, contains: the test, the consequent body (executed
   if the test is true), the alternative (executed if the test is
   false) *)
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

(** Convert a value to its representation *)
val string_of_value : Expression.value -> String
