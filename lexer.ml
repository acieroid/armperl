(* TODO in this module:

    - Count the lines, in order to do better error reporting. This
      would probably need a state structure that keeps track of the
      current line number
*)
open Utils

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

let is_identifier_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

let is_space = function
  | ' ' | '\n' | '\t' -> true
  | _ -> false

let rec ignore_comment stream =
  match stream with parser
  | [< ''\n' >] -> ()
  | [< 'c >] -> ignore_comment stream
  | [< >] -> ()

let rec lex_identifier s l =
  match s with parser
  | [< 'c when is_identifier_char c >] -> lex_identifier s ([c] @ l)
  | [< >] -> (implode (List.rev l))

let rec lex_integer s n =
  match s with parser
  | [< ''0'..'9' as x >] -> lex_integer s (n*10 + ((int_of_char x) - int_of_char '0'))
  | [< >] -> n

let rec lex_string stream last l =
  match stream with parser
  | [< 'c when c == last >] -> implode (List.rev l)
  | [< 'c >] -> lex_string stream last ([c] @ l)

let lex_keyword stream start kwd =
  match stream with parser
  | [< 'c when is_identifier_char c >] ->
      IDENTIFIER (lex_identifier stream (List.rev (explode start)))
  | [< >] -> kwd

let rec try_lex_keyword stream start kwd acc =
  match start with
  | [] -> lex_keyword stream (implode start) kwd
  | (hd::tl) ->
      (match stream with parser
      | [< 'c when c == hd >] -> try_lex_keyword stream tl kwd ([c] @ acc)
      | [< 'c >] -> IDENTIFIER (lex_identifier stream ([c] @ acc))
      | [< >] -> IDENTIFIER (implode (List.rev acc)))

let rec lexer stream =
  let ret x = Right x and
      err x = Left x in
  match stream with parser
    (* Drop spaces and comment *)
  | [< 'c when is_space c >] -> lexer stream
  | [< ''#' >] -> ignore_comment stream; lexer stream
    (* Simple symbols *)
  | [< ''{' >] -> ret LBRACE
  | [< ''}' >] -> ret RBRACE
  | [< ''(' >] -> ret LPAR
  | [< '')' >] -> ret RPAR
  | [< '';' >] -> ret SEMICOLON
  | [< '',' >] -> ret COMMA
  | [< ''+' >] -> ret PLUS
  | [< ''-' >] -> ret MINUS
  | [< ''*' >] -> ret TIMES
  | [< ''/' >] -> ret DIVIDE
  | [< ''.' >] -> ret CONCAT
    (* Keywords *)
  | [< ''r' >] -> ret (try_lex_keyword stream ['e'; 't'; 'u'; 'r'; 'n'] RETURN ['r'])
  | [< ''s' >] -> ret (try_lex_keyword stream ['u'; 'b'] SUB ['s'])
  | [< ''i' >] -> ret (try_lex_keyword stream ['f'] IF ['i'])
  | [< ''u' >] -> ret (try_lex_keyword stream ['n'; 'l'; 'e'; 's'; 's'] UNLESS ['u'])
  | [< ''e' >] -> ret
        (match stream with parser
        | [< ''q' >] -> lex_keyword stream "eq" STRING_EQUALS
        | [< ''l' >] ->
            (match stream with parser
              [< ''s' >] ->
                (match stream with parser
                | [< ''e' >] -> lex_keyword stream "else" ELSE
                | [< ''i' >] -> try_lex_keyword stream ['f'] ELSEIF ['i'; 's'; 'l'; 'e']
                | [< 'c >] -> IDENTIFIER (lex_identifier stream [c; 's'; 'l'; 'e']))
            | [< 'c >] -> IDENTIFIER (lex_identifier stream [c; 'l'; 'e']))
        | [< 'c >] -> IDENTIFIER (lex_identifier stream [c; 'e']))
  | [< ''n' >] -> ret
        (match stream with parser
        | [< ''e' >] -> lex_keyword stream "ne" STRING_DIFFERENT
        | [< ''o' >] -> try_lex_keyword stream ['t'] NOT_WORD ['o'; 'n']
        | [< 'c >] -> IDENTIFIER (lex_identifier stream [c; 'n']))
  | [< ''g' >] -> ret
        (match stream with parser
        | [< ''t' >] -> lex_keyword stream "gt" STRING_GREATER
        | [< ''e' >] -> lex_keyword stream "ge" STRING_GREATER_EQUALS
        | [< 'c >] -> IDENTIFIER (lex_identifier stream [c; 'g']))
  | [< ''l' >] -> ret
        (match stream with parser
        | [< ''t' >] -> lex_keyword stream "lt" STRING_LOWER
        | [< ''e' >] -> lex_keyword stream "le" STRING_LOWER_EQUALS
        | [< 'c >] -> IDENTIFIER (lex_identifier stream [c; 'l']))
    (* Multi-character symbols *)
  | [< ''|'; ''|' >] -> ret LAZY_OR
  | [< ''&' >] -> ret
        (match stream with parser
        | [< ''&' >] -> LAZY_AND
        | [< >] -> CALL_MARK)
  | [< ''=' >] -> ret
        (match stream with parser
        | [< ''=' >] -> EQUALS
        | [< >] -> ASSIGN)
  | [< ''!' >] -> ret
        (match stream with parser
        | [< ''=' >] -> DIFFERENT
        | [< >] -> NOT)
  | [< ''>' >] -> ret
        (match stream with parser
        | [< ''=' >] -> GREATER_EQUALS
        | [< >] -> GREATER)
  | [< ''<' >] -> ret
        (match stream with parser
        | [< ''=' >] -> LOWER_EQUALS
        | [< >] -> LOWER)
    (* More complex tokens *)
  | [< ''0'..'9' as n >] ->
      ret (INTEGER (lex_integer stream ((int_of_char n) - int_of_char '0')))
  | [< 'c when c == '"' >] -> ret (STRING (lex_string stream '"' []))
  | [< 'c when c == '\'' >] -> ret (STRING (lex_string stream '\'' []))
  | [< ''$' >] -> ret (VAR (lex_identifier stream []))
  | [< 'c when is_identifier_char c >] -> ret (IDENTIFIER (lex_identifier stream [c]))
  | [< 'c >] -> err ("no match: " ^ Char.escaped c)
  | [< >] -> ret EOF

