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
  | END

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
    | [< >] -> END

let print_token = function
  | VAR s -> print_string "VAR("; print_string s; print_string ")"
  | INTEGER n -> print_string "INTEGER("; print_int n; print_string ")"
  | STRING s -> print_string "STRING("; print_string s; print_string ")"
  | IDENTIFIER s -> print_string "IDENTIFIER("; print_string s; print_string ")"
  | SUB -> print_string "SUB"
  | RETURN -> print_string "RETURN"
  | CALL_MARK -> print_string "CALL_MARK"
  | LBRACE -> print_string "LBRACE"
  | RBRACE -> print_string "RBRACE"
  | LPAR -> print_string "LPAR"
  | RPAR -> print_string "RPAR"
  | SEMICOLON -> print_string "SEMICOLON"
  | COMMA -> print_string "COMMA"
  | IF -> print_string "IF"
  | UNLESS -> print_string "UNLESS"
  | ELSE -> print_string "ELSE"
  | ELSEIF -> print_string "ELSEIF"
  | NOT -> print_string "NOT"
  | PLUS -> print_string "PLUS"
  | MINUS -> print_string "MINUS"
  | TIMES -> print_string "TIMES"
  | DIVIDE -> print_string "DIVIDE"
  | ASSIGN -> print_string "ASSIGN"
  | CONCAT -> print_string "CONCAT"
  | LAZY_OR -> print_string "LAZY_OR"
  | LAZY_AND -> print_string "LAZY_AND"
  | EQUALS -> print_string "EQUALS"
  | DIFFERENT -> print_string "DIFFERENT"
  | GREATER -> print_string "GREATER"
  | LOWER -> print_string "LOWER"
  | GREATER_EQUALS -> print_string "GREATER_EQUALS"
  | LOWER_EQUALS -> print_string "LOWER_EQUALS"
  | STRING_EQUALS -> print_string "STRING_EQUALS"
  | STRING_DIFFERENT -> print_string "STRING_DIFFERENT"
  | STRING_GREATER -> print_string "STRING_GREATER"
  | STRING_LOWER -> print_string "STRING_LOWER"
  | STRING_GREATER_EQUALS -> print_string "STRING_GREATER_EQUALS"
  | STRING_LOWER_EQUALS -> print_string "STRING_LOWER_EQUALS"
  | NOT2 -> print_string "NOT2"
  | SPACE -> print_string "SPACE"
  | END -> print_string "END"

let rec loop s =
  match (lexer s) with
    | END -> ()
    | SPACE -> loop s
    | x -> print_token x; print_string " "; loop s

let () =
  while true do
    loop (Stream.of_string (input_line stdin))
 done

