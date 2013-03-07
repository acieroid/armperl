let unoption = function
  | Some x -> x
  | None -> failwith "Unexpected end of file"

let peek stream = unoption (Stream.peek stream)

let unexpected stream =
  failwith ("unexpected symbol at position " ^
            (string_of_int (Stream.count stream) ^
             ": " ^ (string_of_token (Stream.next stream))))


let rec parse =
  (* <factor> *)
  let rec parseFactor inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | CALL_MARK ->
      (* <factor> → <simple expr> *)
      parseSimpleExpr inh stream
  | LPAR ->
      (* <factor> → '(' <expr> ')' *)
      (match stream with parser
      | [< 'LPAR; e = parseExpr inh; 'RPAR >] -> e)
  | _ -> unexpected stream 
  (* <term> *)
  and parseTerm' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | PLUS | MINUS
  | ASSIGN_MARK | CONCAT | OR | AND | EQUALS | DIFFERENT
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_EQUALS | STRING_DIFFERENT
  | STRING_GREATER | STRING_LOWER | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS ->
      (* <term'> → ε *)
      inh
  | TIMES | DIVIDES ->
      (* <term'> → '*' <factor> <term'> *)
      (* <term'> → '/' <factor> <term'> *)
      (match stream with parser
      | [< 'TIMES; f = parseFactor inh; t' = parseTerm' (BinOp (Times, inh, f)); >] -> t'
      | [< 'DIVIDES; f = parseFactor inh; t' = parseTerm' (BinOp (Divides, inh, f)); >] -> t')
  | _ -> unexpected stream
  (* <term'> *)  
  and parseTerm inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR ->
      (* <term> → <factor> <term'> *)
      (match stream with parser
      | [< f = parseFactor inh; t' = parseTerm' f >] -> t')
  | _ -> unexpected stream
  (* <calc'> *)
  and parseCalc' inh stream = (* TODO *)
  (* <calc> *)
  and parseCalc inh stream = (* TODO *)
  (* <comp'> *)
  and parseComp' inh stream = (* TODO *)
  (* <comp> *)
  and parseComp inh stream = (* TODO *)
  (* <expr-eq'> *)
  and parseExprEq' inh stream = (* TODO *)
  (* <expr-eq> *)
  and parseExprEq inh stream = (* TODO *)
  (* <expr-or'> *)
  and parseExprOr' inh stream = (* TODO *)
  (* <expr-or> *)
  and parseExprOr inh stream = (* TODO *)
  (* <expr> *)
  and parseExpr inh stream = (* TODO *)
  (* <simple-expr> *)
  and parseSimpleExpr inh stream = (* TODO *)
  (* <cond-end> *)
  and parseCondEnd inh stream = (* TODO *)
  (* <cond> *)
  and parseCond inh stream = (* TODO *)
  (* <instr'> *)
  and parseInstr' inh stream = (* TODO *)
  (* <instr> *)
  and parseInstr inh stream = (* TODO *)
  (* <args call list'> *)
  and parseArgsCallList' inh stream = (* TODO *)
  (* <args call list> *)
  and parseArgsCallList inh stream = (* TODO *)
  (* <funcall args> *)
  and parseFuncallArgs inh stream = (* TODO *)
  (* <funcall> *)
  and parseFuncall inh stream = (* TODO *)
  (* <instr list'> *)
  and parseInstrList' inh stream = (* TODO *)
  (* <instr list> *)
  and parseInstrList inh stream = (* TODO *)
  (* <args list'> *)
  and parseArgsList' inh stream = (* TODO *)
  (* <args list> *)
  and parseArgsList inh stream = (* TODO *)
  (* <function args> *)
  and parseFunctionArgs inh stream = (* TODO *)
  (* <function> *)
  and parseFunction inh stream = (* TODO *)
  (* <function list'> *)
  and parseFunctionList' inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | LBRACE | LPAR | IF | UNLESS | NOT | EOF ->
      (* <function list'> → ε *)
      List []
  | SUB ->
      (* <function list'> → <function> <function list'> *)
      (match stream with parser
      | [< f = parseFunction inh; l = parseFunctionList' inh >] ->
          f::l)
  | _ -> unexpected stream
  (* <function list> *)
  and parseFunctionList inh stream = match peek stream with
  | SUB ->
      (* <function list> → <function> <function list'> *)
      (match stream with parser
      | [< f = parseFunction inh; l = parseFunctionList' inh >] ->
          f::l)
  | _ -> unexpected stream
  (* <program'> *)
  and parseProgram' inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING | IDENTIFIER _ | RETURN | CALL_MARK
  | LBRACE | LPAR | IF | UNLESS | NOT ->
      (* <program'> → <instr list> *)
      (match stream with parser
      | [< l = parseInstrList inh >] -> l)
  | EOF ->
      (* <program'> → ε *)
      List []
  | _ -> unexpected stream
  (* <program> *)
  and parseProgram inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | LBRACE | LPAR | IF | UNLESS | NOT ->
      (* <program> → <instr list> *)
      (match stream with parser
      | [< l = parseInstrList inh >] -> l)
  | SUB ->
      (*  <program> →  <function list> <program'>  *)
      (match stream with parser
      | [< l = parseFunctionList inh; p = parseProgram' >] ->
          (match l, p with
          | (List l', List p') -> Program (l', p'))
  | _ unexpected stream
  in
  parseProgram (Value Undef)
