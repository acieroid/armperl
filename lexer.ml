type ('a,'b) either = Left of 'a | Right of 'b;;

let implode l =
  let s = String.create (List.length l) in
  let rec f n = function
    | x :: xs -> s.[n] <- x; f (n+1) xs
    | [] ->  s
  in f 0 l

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

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

let is_identifier_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

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

let rec ignore_comment stream =
  match stream with parser
    | [< ''\n' >] -> ()
    | [< 'c >] -> ignore_comment stream
    | [< >] -> ()

let is_space = function
  | ' ' | '\n' | '\t' -> true
  | _ -> false

let lex_keyword stream start kwd =
  match stream with parser
    | [< 'c when is_identifier_char c >] ->
      IDENTIFIER (lex_identifier stream (List.rev (explode start)))
    | [< >] -> kwd

let rec lexer stream =
  let ret x = Some (Right x) in
  match stream with parser
    | [< 'c when is_space c >] -> lexer stream (* drop spaces *)
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
    (* TODO: fail with for example ssub *)
    | [< ''r'; ''e'; ''t'; ''u'; ''r'; ''n' >] ->
      ret (lex_keyword stream "return" RETURN)
    | [< ''s'; ''u'; ''b' >] ->
      ret (lex_keyword stream "sub" SUB)
    | [< ''i'; ''f' >] ->
      ret (lex_keyword stream "if" IF)
    | [< ''u'; ''n'; ''l'; ''e'; ''s'; ''s' >] ->
      ret (lex_keyword stream "unless" UNLESS)
    | [< ''e' >] -> ret
      (match stream with parser
        | [< ''q' >] -> lex_keyword stream "eq" STRING_EQUALS
        | [< ''l'; ''s' >] ->
          (match stream with parser
            | [< ''e' >] -> lex_keyword stream "else" ELSE
            | [< ''i'; ''f' >] -> lex_keyword stream "elsif" ELSEIF
            | [< 'c >] -> IDENTIFIER (lex_identifier stream [c; 's'; 'l'; 'e']))
        | [< 'c >] -> IDENTIFIER (lex_identifier stream [c; 'e']))
    | [< ''n' >] -> ret
      (match stream with parser
        | [< ''e' >] -> lex_keyword stream "ne" STRING_DIFFERENT
        | [< ''o'; ''t' >] -> lex_keyword stream "not" NOT2
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
    | [< 'c >] -> Some (Left ("no match: " ^ Char.escaped c))
    | [< >] -> None

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

let rec loop line s =
  match (lexer s) with
    | None -> ()
    | Some (Left err) ->
      (* TODO: print the error like gcc ? or ocaml ? *)
      print_newline ();
      print_string "error when lexing character on line ";
      print_int line;
      print_string " at position ";
      print_int (Stream.count s);
      print_string ": ";
      print_string err;
      print_newline ();
      ()
    | Some (Right x) -> print_token x; print_string " "; loop line s

let () =
  let line = ref 0 in
  try
    while true do
      line := !line + 1;
      loop !line (Stream.of_string (input_line stdin))
    done
  with
    | End_of_file -> ()

