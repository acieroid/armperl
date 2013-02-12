(* 3, 18, 24, 35, 37
   4, 7 *)

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
  | NOT2
  | SPACE

let implode l =
  let s = String.create (List.length l) in
  let rec f n = function
    | x :: xs -> s.[n] <- x; f (n+1) xs
    | [] ->  s
  in f 0 l

let rec lex_identifier s l =
  match s with parser
    | [< ''a'..'z' as c >] -> lex_identifier s ([c] @ l)
    | [< ''A'..'Z' as c >] -> lex_identifier s ([c] @ l)
    | [< ''0'..'9' as c >] -> lex_identifier s ([c] @ l)
    | [< ''_' >] -> lex_identifier s (['_'] @ l)
    | [< >] -> (implode (List.rev l))

let rec lex_integer s n =
  match s with parser
    | [< ''0'..'9' as x >] -> lex_integer s (n*10 + ((int_of_char x) - int_of_char '0'))
    | [< >] -> n

let rec lex_string stream last l =
  match stream with parser
    | [< 'c when c == last >] -> implode (List.rev l)
    | [< 'c >] -> lex_string stream last ([c] @ l)

(* TODO: ignore comments and spaces *)
let lexer stream =
  match stream with parser
    | [< '' ' >] -> SPACE
    (* Simple symbols *)
    | [< ''{' >] -> LBRACE
    | [< ''}' >] -> RBRACE
    | [< ''(' >] -> LPAR
    | [< '')' >] -> RPAR
    | [< '';' >] -> SEMICOLON
    | [< '',' >] -> COMMA
    | [< ''+' >] -> PLUS
    | [< ''-' >] -> MINUS
    | [< ''*' >] -> TIMES
    | [< ''/' >] -> DIVIDE
    | [< ''.' >] -> CONCAT
    (* Keywords *)
    (* TODO: handle identifiers starting with keywords *)
    | [< ''r'; ''e'; ''t'; ''u'; ''r'; ''n' >] -> RETURN
    | [< ''s'; ''u'; ''b' >] -> SUB
    | [< ''i'; ''f' >] -> IF
    | [< ''u'; ''n'; ''l'; ''e'; ''s'; ''s' >] -> UNLESS
    | [< ''e' >] ->
      (match stream with parser
        | [< ''q' >] -> STRING_EQUALS
        | [< ''l'; ''s' >] ->
          (match stream with parser
            | [< ''e' >] -> ELSE
            | [< ''i'; ''f' >] -> ELSEIF))
    | [< ''n' >] ->
      (match stream with parser
        | [< ''e' >] -> STRING_DIFFERENT
        | [< ''o'; ''t' >] -> NOT2)
    | [< ''g' >] ->
      (match stream with parser
        | [< ''t' >] -> STRING_GREATER
        | [< ''e' >] -> STRING_GREATER_EQUALS)
    | [< ''l' >] ->
      (match stream with parser
        | [< ''t' >] -> STRING_LOWER
        | [< ''e' >] -> STRING_LOWER_EQUALS)
    (* Multi-character symbols *)
    | [< ''|'; ''|' >] -> LAZY_OR
    | [< ''&' >] ->
      (match stream with parser
        | [< ''&' >] -> LAZY_AND
        | [< >] -> CALL_MARK)
    | [< ''=' >] ->
      (match stream with parser
        | [< ''=' >] -> EQUALS
        | [< >] -> ASSIGN)
    | [< ''!' >] ->
      (match stream with parser
        | [< ''=' >] -> DIFFERENT
        | [< >] -> NOT)
    | [< ''>' >] ->
      (match stream with parser
        | [< ''=' >] -> GREATER_EQUALS
        | [< >] -> GREATER)
    | [< ''<' >] ->
      (match stream with parser
        | [< ''=' >] -> LOWER_EQUALS
        | [< >] -> LOWER)
    (* More complex tokens *)
    | [< ''0'..'9' as n >] -> 
        INTEGER (lex_integer stream ((int_of_char n) - int_of_char '0'))
    | [< 'c when c == '"' >] -> STRING (lex_string stream '"' [])
    | [< 'c when c == '\'' >] -> STRING (lex_string stream '\'' [])
    | [< ''$' >] -> VAR (lex_identifier stream [])
    | [< 'c >] -> IDENTIFIER (lex_identifier stream [c]) (* TODO: change to a-zA-Z or something like that *)
