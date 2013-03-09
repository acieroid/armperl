type token =
  | VAR of string
  | INTEGER of int
  | STRING of string
  | IDENTIFIER of string
  | SUB
  | RETURN
  | CALL_MARK
  | LBRACE
  | RBRACE
  | LPAR
  | RPAR
  | SEMICOLON
  | COMMA
  | IF
  | UNLESS
  | ELSE
  | ELSEIF
  | NOT
  | NOT_WORD
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | CONCAT
  | LAZY_OR
  | LAZY_AND
  | EQUALS
  | DIFFERENT
  | GREATER
  | LOWER
  | GREATER_EQUALS
  | LOWER_EQUALS
  | STRING_EQUALS
  | STRING_DIFFERENT
  | STRING_GREATER
  | STRING_LOWER
  | STRING_GREATER_EQUALS
  | STRING_LOWER_EQUALS
  | EOF

let string_of_token = function
  | VAR s -> "VAR($" ^ s ^ ")"
  | INTEGER n -> "INTEGER(" ^ string_of_int n ^ ")"
  | STRING s -> "STRING(\"" ^ s ^ "\")"
  | IDENTIFIER s -> "IDENTIFIER(" ^ s ^ ")"
  | SUB -> "'sub'"
  | RETURN -> "'return'"
  | CALL_MARK -> "'&'"
  | LBRACE -> "'{'"
  | RBRACE -> "'}'"
  | LPAR -> "'('"
  | RPAR -> "')'"
  | SEMICOLON -> "';'"
  | COMMA -> "','"
  | IF -> "'if'"
  | UNLESS -> "'unless'"
  | ELSE -> "'else'"
  | ELSEIF -> "'elsif'"
  | NOT -> "'!'"
  | NOT_WORD -> "'not'"
  | PLUS -> "'+'"
  | MINUS -> "'-'"
  | TIMES -> "'*'"
  | DIVIDE -> "'/'"
  | ASSIGN -> "'='"
  | CONCAT -> "'.'"
  | LAZY_OR -> "'||'"
  | LAZY_AND -> "'&&'"
  | EQUALS -> "'=='"
  | DIFFERENT -> "'!='"
  | GREATER -> "'>'"
  | LOWER -> "'<'"
  | GREATER_EQUALS -> "'>='"
  | LOWER_EQUALS -> "'<='"
  | STRING_EQUALS -> "'eq'"
  | STRING_DIFFERENT -> "'ne'"
  | STRING_GREATER -> "'gt'"
  | STRING_LOWER -> "'lt'"
  | STRING_GREATER_EQUALS -> "'ge'"
  | STRING_LOWER_EQUALS -> "'le'"
  | EOF -> "EOF"
