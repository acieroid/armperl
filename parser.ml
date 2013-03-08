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
  | ASSIGN_MARK | CONCAT | LAZY_OR | LAZY_AND | EQUALS | DIFFERENT
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
  and parseCalc' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | ASSIGN | LAZY_OR | LAZY_AND | EQUALS | DIFFERENT
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_EQUALS | STRING_DIFFERENT
  | STRING_GREATER | STRING_LOWER | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS  ->
     (* <calc'> → ε *)
      inh
  | CONCAT | PLUS | MINUS ->
      (* <calc'> → '.' <term> <calc'>*)
      (* <calc'> → '+' <term> <calc'>*)
      (* <calc'> → '-' <term> <calc'>*)
      (match stream with parser
      | [< 'CONCAT; t = parseTerm inh; c' = parseCalc' (BinOp (Concat, inh, t)); >] -> c'
      | [< 'PLUS; t = parseTerm inh; c' = parseCalc' (BinOp (Plus, inh, t)); >] -> c'
      | [< 'MINUS; t = parseTerm inh; c' = parseCalc' (BinOp (Minus, inh, t)); >] -> c')

  (* <calc> *)
  and parseCalc inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | CALL_MARK | LPAR ->
      (* <calc> → <term> <calc'> *)
        (match stream with parser
	| [< t = parseTerm inh; c' = parseCalc' t >] -> c'
	| [< 'CALL_MARK; t = parseTerm inh; c' = parseCalc' (BinOp (Call_mark, inh, t)); >] -> c'
	| [< 'LPAR; t = parseTerm inh; c' = parseCalc' (BinOp (Lpar, inh, t)); >] -> c')

  (* <comp'> *)
  and parseComp' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | ASSIGN |LAZY_OR |LAZY_AND | EQUALS | DIFFERENT
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_EQUALS | STRING_DIFFERENT
  | STRING_GREATER | STRING_LOWER | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS
      
  (* <calc> *)
  and parseCalc inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | CALL_MARK | LPAR ->
      (* <calc> → <term> <calc'> *)
        (match stream with parser
	| [< t = parseTerm inh; c' = parseCalc' t >] -> c'
	| [< 'CALL_MARK; t = parseTerm inh; c' = parseCalc' (BinOp (Call_mark, inh, t)); >] -> c'
	| [< 'LPAR; t = parseTerm inh; c' = parseCalc' (BinOp (Lpar, inh, t)); >] -> c')

  (* <comp'> *)
  and parseComp' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | ASSIGN |LAZY_OR |LAZY_AND | EQUALS | DIFFERENT
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_EQUALS | STRING_DIFFERENT
  | STRING_GREATER | STRING_LOWER | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS ->
      (* <comp'> → ε *)
      inh

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
  and parseFuncall inh stream = match peek stream with
  | IDENTIFIER _ | CALL_MARK ->
	(* <funcall> → identifier <funcall args> *)
	(*  <funcall> → '&' identifier <funcall args> *)
	(match stream with parser
	| [< '(IDENTIFIER name); args = parseFuncallArgs inh >] ->
		Funcall (name, args)
	| [< 'CALL_MARK; '(IDENTIFIER name); args = parseFuncallArgs inh >] ->
	    Funcall (name, args)) 
  | _ -> unexpected stream
  (* <instr list'> *)
  and parseInstrList' inh stream = (* TODO *)
  (* <instr list> *)
  and parseInstrList inh stream = (* TODO *)
  (* <arg list'> *)
  and parseArgList' inh stream = match peek stream with
  | COMMA ->
      (* <arg list'> → ',' var <arg list'> *)
      (match stream with parser
      | [< 'COMMA; '(VAR v); args = parseArgList' inh >] ->
          v::args)
  | RPAR ->
      (* <arg list'> → ε *)
      []
  (* <arg list> *)
  and parseArgList inh stream = match peek stream with
  | VAR _ ->
    (* <arg list> → var <arg list'> *)
      (match stream with parser
        [< VAR v; args = parseArgList' inh >] ->
          v::args)
  | RPAR ->
    (* <arg list> → ε *)
      []
  (* <function args> *)
  and parseFunctionArgs inh stream = match peek stream with
  | LPAR ->
      (* <function args>  → '(' <arg list> ')' *)
      (match stream with parser
        [< LPAR; args = parseArgList inh; RPAR >] ->
          args)
  | _ -> unexpected stream
  (* <function> *)
  and parseFunction inh stream = match peek stream with
  | SUB ->
      (* <function> → 'sub' identifier <function args> '{' <instr list> '}' *)
      (match stream with parser
        [< SUB; IDENTIFIER name; args = parseFunctionArgs inh;
           LBRACE; body = parseInstrList inh; RBRACE >] ->
             Fundef (name, args, body))
  | _ -> unexpected stream
  (* <function list'> *)
  and parseFunctionList' inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | LBRACE | LPAR | IF | UNLESS | NOT | EOF ->
      (* <function list'> → ε *)
      []
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
      []
  | _ -> unexpected stream
  (* <program> *)
  and parseProgram inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | LBRACE | LPAR | IF | UNLESS | NOT ->
      (* <program> → <instr list> *)
      (match stream with parser
      | [< l = parseInstrList inh >] -> ([], l))
  | SUB ->
      (*  <program> →  <function list> <program'>  *)
      (match stream with parser
      | [< l = parseFunctionList inh; p = parseProgram' >] ->
          (l, p))
  | _ unexpected stream
  in
  (* TODO: the inh argument is not needed everywhere *)
  parseProgram (Value Undef)
