(** Define the different expression that represents constructs of the language *)

(** Binary operators *)
type binop =
  | Plus               (** +  *)
  | Minus              (** -  *)
  | Times              (** *  *)
  | Divide             (** /  *)
  | Concat             (** .  *)
  | Equals             (** == *)
  | Different          (** != *)
  | Greater            (** >  *)
  | Lower              (** <  *)
  | GreaterEquals      (** >= *)
  | LowerEquals        (** <= *)
  | StrEquals          (** eq *)
  | StrDifferent       (** ne *)
  | StrGreater         (** gt *)
  | StrLower           (** lt *)
  | StrGreaterEquals   (** ge *)
  | StrLowerEquals     (** le *)

(** Unary operators *)
type unop =
  | Not                (** !, not *)
  | UnaryPlus          (** +      *)
  | UnaryMinus         (** -      *)

(** Different values *)
type value =
  | Integer of int
  | Float of float
  | String of string
  | True | False
  | Undef

(** Different expressions *)
and expr =
  | Value of value                  (** A value *)
  | Variable of string              (** A variable (local or global *)
  | BinOp of binop * expr * expr    (** A binary operation *)
  | Assign of string * expr         (** An assignment *)
  | Or of expr * expr               (** An 'or' operation *)
  | And of expr * expr              (** An 'and' operation *)
  | UnOp of unop * expr             (** An unary operation *)
  | Funcall of string * expr list   (** A function call *)
  | Fundef of string * string list * expr list (** A function definition *)
  | Cond of expr * expr list * expr (** A condition *)
  | CondEnd                         (** The last clause of a condition *)
  | Return of expr                  (** A return expression *)

(** Return the representation of a value *)
val string_of_value : value -> string

(** Return the representation of an unary operator *)
val string_of_unop : unop -> string

(** Return the representation of a binary operator *)
val string_of_binop : binop -> string

(** Return the representation of an expression *)
val string_of_expression : expr -> string
      
