{
 open Tokens
 open Utils
 open Lexing
 exception LexingError of position
}

let digits = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let space = ['\n' '\t' '\r' ' ']

rule lexer = parse
| eof                          { EOF }
| '#' [^'\n']* '\n'?           { (* drop comments *) lexer lexbuf }
| space+                       { (* drop spaces *)lexer lexbuf }
| '{'                          { LBRACE }
| '}'                          { RBRACE }
| '('                          { LPAR }
| ')'                          { RPAR }
| ';'                          { SEMICOLON }
| ','                          { COMMA }
| '+'                          { PLUS }
| '-'                          { MINUS }
| '*'                          { TIMES }
| '/'                          { DIVIDE }
| '.'                          { CONCAT }
| "return"                     { RETURN }
| "sub"                        { SUB }
| "if"                         { IF }
| "unless"                     { UNLESS }
| "eq"                         { STRING_EQUALS }
| "else"                       { ELSE }
| "elsif"                      { ELSEIF }
| "ne"                         { STRING_DIFFERENT }
| "not"                        { NOT_WORD }
| "gt"                         { STRING_GREATER }
| "ge"                         { STRING_GREATER_EQUALS }
| "lt"                         { STRING_LOWER }
| "le"                         { STRING_LOWER_EQUALS }
| "||"                         { LAZY_OR }
| "&"                          { CALL_MARK }
| "&&"                         { LAZY_AND }
| "="                          { ASSIGN }
| "=="                         { EQUALS }
| "!"                          { NOT }
| "!="                         { DIFFERENT }
| ">"                          { GREATER }
| "<"                          { LOWER }
| ">="                         { GREATER_EQUALS }
| "<="                         { LOWER_EQUALS }
| digits+ as n                 { INTEGER (int_of_string n) }
| '"' ([^'"']* as s) '"'       { STRING s }
| '\'' ([^'\'']* as s) '\''    { STRING s }
| '$' ((alpha | digits)* as s) { VAR s }
| (alpha | digits)* as s       { IDENTIFIER s }
| _                            { raise (LexingError lexbuf.lex_curr_p) }

{
 let rec to_stream lexbuf =
   let item = try
     Right (lexer lexbuf)
   with
     LexingError p -> Left ("no match at line " ^ (string_of_int p.pos_lnum) ^
                            " on column " ^ (string_of_int p.pos_cnum))
   in
   [< 'item; to_stream lexbuf >]

 let lex channel = to_stream (Lexing.from_channel channel)
}
