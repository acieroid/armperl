open Utils
open Tokens

type lexer_state = {
    line: int ref; (* the current line number *)
    count: int ref; (* the number of character parsed before the current line *)
    stream: char Stream.t (* the current stream *)
  }

let count_line state =
  state.line := !(state.line) + 1;
  state.count := Stream.count state.stream

let state_of_channel channel = {
  line = ref 1;
  count = ref 0;
  stream = Stream.of_channel channel
}

let is_identifier_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

let is_space = function
  | ' ' | '\n' | '\t' -> true
  | _ -> false

let rec ignore_comment state =
  match state.stream with parser
  | [< ''\n' >] -> count_line state
  | [< 'c >] -> ignore_comment state
  | [< >] -> ()

let rec lex_identifier state l =
  match state.stream with parser
  | [< 'c when is_identifier_char c >] -> lex_identifier state ([c] @ l)
  | [< >] -> (implode (List.rev l))

let rec lex_integer state n =
  match state.stream with parser
  | [< ''0'..'9' as x >] ->
      lex_integer state (n*10 + ((int_of_char x) - int_of_char '0'))
  | [< >] -> n

let rec lex_string state last l =
  match state.stream with parser
  | [< 'c when c == last >] ->
      implode (List.rev l)
  | [< ''\n' >] ->
      count_line state;
      lex_string state last (['\n'] @ l)
  | [< 'c >] ->
      lex_string state last ([c] @ l)

let lex_keyword state start kwd =
  match state.stream with parser
  | [< 'c when is_identifier_char c >] ->
      IDENTIFIER (lex_identifier state
                    ([c] @ (List.rev (explode start))))
  | [< >] -> kwd

let rec try_lex_keyword state start kwd acc =
  match start with
  | [] -> lex_keyword state (implode start) kwd
  | (hd::tl) ->
      (match state.stream with parser
      | [< 'c when c == hd >] ->
          try_lex_keyword state tl kwd ([c] @ acc)
      | [< 'c when is_identifier_char c >] ->
          IDENTIFIER (lex_identifier state ([c] @ acc))
      | [< >] -> IDENTIFIER (implode (List.rev acc)))

let rec lexer state =
  let ret x = Right x and
      err x = Left x in
  match state.stream with parser
    (* Drop spaces and comment *)
  | [< 'c when is_space c >] -> 
      if c == '\n' then count_line state else ();
      lexer state
  | [< ''#' >] -> ignore_comment state; lexer state
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
  | [< ''r' >] ->
      ret (try_lex_keyword state ['e'; 't'; 'u'; 'r'; 'n'] RETURN ['r'])
  | [< ''s' >] ->
      ret (try_lex_keyword state ['u'; 'b'] SUB ['s'])
  | [< ''i' >] -> ret (try_lex_keyword state ['f'] IF ['i'])
  | [< ''u' >] -> ret (try_lex_keyword state ['n'; 'l'; 'e'; 's'; 's'] UNLESS ['u'])
  | [< ''e' >] -> ret
        (match state.stream with parser
        | [< ''q' >] -> lex_keyword state "eq" STRING_EQUALS
        | [< ''l' >] ->
            (match state.stream with parser
              [< ''s' >] ->
                (match state.stream with parser
                | [< ''e' >] -> lex_keyword state "else" ELSE
                | [< ''i' >] -> try_lex_keyword state ['f'] ELSEIF ['i'; 's'; 'l'; 'e']
                | [< >] -> IDENTIFIER (lex_identifier state ['s'; 'l'; 'e']))
            | [< >] -> IDENTIFIER (lex_identifier state ['l'; 'e']))
        | [< >] -> IDENTIFIER (lex_identifier state ['e']))
  | [< ''n' >] -> ret
        (match state.stream with parser
        | [< ''e' >] -> lex_keyword state "ne" STRING_DIFFERENT
        | [< ''o' >] -> try_lex_keyword state ['t'] NOT_WORD ['o'; 'n']
        | [< >] -> IDENTIFIER (lex_identifier state ['n']))
  | [< ''g' >] -> ret
        (match state.stream with parser
        | [< ''t' >] -> lex_keyword state "gt" STRING_GREATER
        | [< ''e' >] -> lex_keyword state "ge" STRING_GREATER_EQUALS
        | [< >] -> IDENTIFIER (lex_identifier state ['g']))
  | [< ''l' >] -> ret
        (match state.stream with parser
        | [< ''t' >] -> lex_keyword state "lt" STRING_LOWER
        | [< ''e' >] -> lex_keyword state "le" STRING_LOWER_EQUALS
        | [< >] -> IDENTIFIER (lex_identifier state ['l']))
    (* Multi-character symbols *)
  | [< ''|'; ''|' >] -> ret LAZY_OR
  | [< ''&' >] -> ret
        (match state.stream with parser
        | [< ''&' >] -> LAZY_AND
        | [< >] -> CALL_MARK)
  | [< ''=' >] -> ret
        (match state.stream with parser
        | [< ''=' >] -> EQUALS
        | [< >] -> ASSIGN)
  | [< ''!' >] -> ret
        (match state.stream with parser
        | [< ''=' >] -> DIFFERENT
        | [< >] -> NOT)
  | [< ''>' >] -> ret
        (match state.stream with parser
        | [< ''=' >] -> GREATER_EQUALS
        | [< >] -> GREATER)
  | [< ''<' >] -> ret
        (match state.stream with parser
        | [< ''=' >] -> LOWER_EQUALS
        | [< >] -> LOWER)
    (* More complex tokens *)
  | [< ''0'..'9' as n >] ->
      ret (INTEGER (lex_integer state ((int_of_char n) - int_of_char '0')))
  | [< 'c when c == '"' >] -> ret (STRING (lex_string state '"' []))
  | [< 'c when c == '\'' >] -> ret (STRING (lex_string state '\'' []))
  | [< ''$' >] -> ret (VAR (lex_identifier state []))
  | [< 'c when is_identifier_char c >] -> ret (IDENTIFIER (lex_identifier state [c]))
  | [< 'c >] -> err ("no match: '" ^ Char.escaped c ^
                     "' on line " ^ (string_of_int !(state.line)) ^
                     ", character " ^ (string_of_int
                                         ((Stream.count state.stream) -
                                            !(state.count))))
  | [< >] -> ret EOF

let rec to_stream state =
  [< 'lexer state; to_stream state >]

let lex channel = to_stream (state_of_channel channel)
