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
  and parseCalc' inh stream = (* TODO *)
  and parseCalc inh stream = (* TODO *)
  and parseComp' inh stream = (* TODO *)
  and parseComp inh stream = (* TODO *)
  and parseEq' inh stream = (* TODO *)
  and parseEq inh stream = (* TODO *)
  and parseOr' inh stream = (* TODO *)
  and parseOr inh stream = (* TODO *)
  and parseExpr inh stream = (* TODO *)
  and parseSimpleExpr inh stream = (* TODO *)
  and parseCondEnd inh stream = (* TODO *)
  and parseCond inh stream = (* TODO *)
  and parseInstr' inh stream = (* TODO *)
  and parseInstr inh stream = (* TODO *)
  and parseArgsCallList' inh stream = (* TODO *)
  and parseArgsCallList inh stream = (* TODO *)
  and parseFuncallArgs inh stream = (* TODO *)
  and parseFuncall inh stream = (* TODO *)
  and parseInstrList' inh stream = (* TODO *)
  and parseInstrList inh stream = (* TODO *)
  and parseArgsList' inh stream = (* TODO *)
  and parseArgsList inh stream = (* TODO *)
  and parseFunctionArgs inh stream = (* TODO *)
  and parseFunction inh stream = (* TODO *)
  and parseFunctionList' inh stream = (* TODO *)
  and parseFunctionList inh stream = (* TODO *)
  and parseProgram' inh stream = (* TODO *)
  and parseProgram inh stream = (* TODO *)
  in
  parseProgram (Value Undef)
