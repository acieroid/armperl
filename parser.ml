let unoption = function
  | Some x -> x
  | None -> failwith "Unexpected end of file"

let unexpected stream =
  failwith ("unexpected symbol at position " ^
            (string_of_int (Stream.count stream) ^
             ": " ^ (string_of_token (Stream.next stream))))


let rec parse =
  
  let rec parseFactor inh stream = match unoption (Stream.peek stream) with
  | (VAR _) | (INTEGER _) | (STRING _) | (IDENTIFIER _)
  | CALL_MARK > parseSimpleExpr inh stream
  | LPAR ->
      (match stream with parser
      | [< 'LPAR; e = parseExpr inh; 'RPAR >] -> e)
  | _ -> unexpected stream 
  and parseTerm' inh stream = match unoption (Stream.peek stream) with
  | LBRACE | RPAR | SEMICOLON | COMMA | PLUS | MINUS
  | ASSIGN_MARK | CONCAT | OR | AND | EQUALS | DIFFERENT
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_EQUALS | STRING_DIFFERENT
  | STRING_GREATER | STRING_LOWER | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS -> inh
  | TIMES | DIVIDES ->
      (match stream with parser
      | [< 'TIMES; f = parseFactor inh; t' = parseTerm' (BinOp (Times, inh, f)); >] -> t'
      | [< 'DIVIDES; f = parseFactor inh; t' = parseTerm' (BinOp (Divides, inh, f)); >] -> t')
  | _ -> unexpected stream
  and parseTerm inh stream = match unoption (Stream.peek stream) with
  | (VAR _) | (INTEGER _) | (STRING _) | (IDENTIFIER _)
  | CALL_MARK | LPAR ->
      (match stream with parser
      | [< f = parseFactor inh; t' = parseTerm' f >] -> t')
  | _ -> unexpected stream

