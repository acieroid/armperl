open Tokens
open Expression

let unoption = function
  | Some x -> x
  | None -> failwith "Unexpected end of file"

let peek stream = unoption (Stream.peek stream)

let unexpected stream =
  failwith ("unexpected symbol at position " ^
            (string_of_int (Stream.count stream) ^
             ": " ^ (string_of_token (Stream.next stream))))

let rec parse =
  (** <factor> *)
  let rec parseFactor inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | CALL_MARK ->
      (* <factor> → <simple expr> *)
      parseSimpleExpr inh stream
  | LPAR ->
      (* <factor> → '(' <expr> ')' *)
      (match stream with parser
      | [< 'LPAR; e = parseExpr inh; 'RPAR >] -> e)
  | NOT | PLUS | MINUS ->
      (* <factor> → '!' <factor> *)
      (* <factor> → '+' <factor> *)
      (* <factor> → '-' <factor> *)
      (match stream with parser
      | [< 'NOT; f = parseFactor inh >] -> UnOp (Not, f)
      | [< 'PLUS; f = parseFactor inh >] -> UnOp (UnaryPlus, f)
      | [< 'MINUS; f = parseFactor inh >] -> UnOp (UnaryMinus, f))
  | _ -> unexpected stream

  (** <term> *)
  and parseTerm' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | PLUS | MINUS
  | ASSIGN | CONCAT | LAZY_OR | LAZY_AND | EQUALS | DIFFERENT
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_EQUALS | STRING_DIFFERENT
  | STRING_GREATER | STRING_LOWER | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS
  | IF | UNLESS ->
      (* <term'> → ε *)
      inh
  | TIMES | DIVIDE ->
      (* <term'> → '*' <factor> <term'> *)
      (* <term'> → '/' <factor> <term'> *)
      (match stream with parser
      | [< 'TIMES; f = parseFactor inh; t' = parseTerm' (BinOp (Times, inh, f)); >] -> t'
      | [< 'DIVIDE; f = parseFactor inh; t' = parseTerm' (BinOp (Divide, inh, f)); >] -> t')
  | _ -> unexpected stream

  (** <term> *)
  and parseTerm inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <term> → <factor> <term'> *)
      (match stream with parser
      | [< f = parseFactor inh; t' = parseTerm' f >] -> t')
  | _ -> unexpected stream

  (** <calc'> *)
  and parseCalc' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | ASSIGN | LAZY_OR | LAZY_AND | EQUALS | DIFFERENT
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_EQUALS | STRING_DIFFERENT
  | STRING_GREATER | STRING_LOWER | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS
  | IF | UNLESS ->
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
  | _ -> unexpected stream

  (** <calc> *)
  and parseCalc inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <calc> → <term> <calc'> *)
        (match stream with parser
	| [< t = parseTerm inh; c' = parseCalc' t >] -> c')
  | _ -> unexpected stream

  (** <comp'> *)
  and parseComp' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | ASSIGN |LAZY_OR |LAZY_AND | EQUALS | DIFFERENT
  | STRING_EQUALS | STRING_DIFFERENT 
  | IF | UNLESS ->
      (* <comp'> → ε *)
      inh
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_GREATER | STRING_LOWER | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS ->
      (* <comp'> -> '>' <calc> <comp'> *)
      (* <comp'> -> '<' <calc> <comp'> *)
      (* <comp'> -> '>=' <calc> <comp'> *)
      (* <comp'> -> '<=' <calc> <comp'> *)
      (* <comp'> -> 'gt' <calc> <comp'> *)
      (* <comp'> -> 'lt' <calc> <comp'> *)
      (* <comp'> -> 'ge' <calc> <comp'> *)
      (* <comp'> -> 'le' <calc> <comp'> *)
      (match stream with parser
      | [< 'GREATER; c = parseCalc inh; c' = parseComp' (BinOp (Greater, inh, c)) >] -> c'
      | [< 'LOWER; c = parseCalc inh; c' = parseComp' (BinOp (Lower, inh, c)) >] -> c'
      | [< 'GREATER_EQUALS; c = parseCalc inh; c' = parseComp' (BinOp (GreaterEquals, inh, c)) >] -> c'
      | [< 'LOWER_EQUALS; c = parseCalc inh; c' = parseComp' (BinOp (LowerEquals, inh, c)) >] -> c'
      | [< 'STRING_GREATER; c = parseCalc inh; c' = parseComp' (BinOp (StrGreater, inh, c)) >] -> c'
      | [< 'STRING_LOWER; c = parseCalc inh; c' = parseComp' (BinOp (StrLower, inh, c)) >] -> c'
      | [< 'STRING_GREATER_EQUALS; c = parseCalc inh; c' = parseComp' (BinOp (StrGreaterEquals, inh, c)) >] -> c'
      | [< 'STRING_LOWER_EQUALS; c = parseCalc inh; c' = parseComp' (BinOp (StrLowerEquals, inh, c)) >] -> c')
  | _ -> unexpected stream

  (** <comp> *)
  and parseComp inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <comp> → <calc> <comp'>  *)
      (match stream with parser
      | [< c = parseCalc inh; c' = parseComp' c >] -> c')
  | _ -> unexpected stream

  (** <expr-eq'> *)
  and parseExprEq' inh stream = match peek stream with
  | LBRACE | RPAR |  SEMICOLON | COMMA | ASSIGN | LAZY_OR | LAZY_AND | IF | UNLESS ->
      (* <expr-eq'> → ε *) 
      inh
  | EQUALS | DIFFERENT | STRING_EQUALS | STRING_DIFFERENT ->
      (*<expr-eq'> → 'eq' <expr-eq'>*)
      (*<expr-eq'> → 'ne' <expr-eq'>*)
      (*<expr-eq'> → '!=' <expr-eq'>*)
      (*<expr-eq'> → '==' <expr-eq'>*)
      (match stream with parser
      | [< 'EQUALS; e = parseExprEq' inh >] -> BinOp (Equals, inh, e)
      | [< 'DIFFERENT; e = parseExprEq' inh >] -> BinOp (Different ,inh, e)
      | [< 'STRING_EQUALS; e = parseExprEq' inh >] -> BinOp (StrEquals ,inh, e)
      | [< 'STRING_DIFFERENT; e = parseExprEq' inh >] -> BinOp (StrDifferent ,inh, e))
  | _ -> unexpected stream

  (** <expr-eq> *)
  and parseExprEq inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
     (* <expr-eq> → <comp> <expr-eq'> *)
     (match stream with parser
	| [< c = parseComp inh; e' = parseExprEq' c >] -> e')
  | _ -> unexpected stream

  (** <expr-and'> *)
  and parseExprAnd' inh stream = match peek stream with
  | LBRACE | RPAR |  SEMICOLON | COMMA | ASSIGN | LAZY_OR | IF | UNLESS ->
      (* <expr-eq'> → ε *) 
      inh
  | LAZY_AND ->
      (* <expr-and'> → '&&' <expr-eq> <expr-and'> *)
      (match stream with parser
      | [< 'LAZY_AND; e = parseExprEq inh; a = parseExprAnd' (And (inh, e)) >] -> a)
  | _ -> unexpected stream

  (** <expr-and> *)
  and parseExprAnd inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
     (* <expr-and> → <expr-eq> <expr-and'> *)
     (match stream with parser
	| [< e = parseExprEq inh; a = parseExprAnd' e >] -> a)
  | _ -> unexpected stream

  (** <expr-or'> *)
  and parseExprOr' inh stream =  match peek stream with
  | LBRACE | RPAR |  SEMICOLON | COMMA | ASSIGN | IF | UNLESS ->
      (* <expr-eq'> → ε *) 
      inh
  | LAZY_OR ->
      (* <expr-or'> → 'or' <expr-and> <expr-or'> *)
      (match stream with parser
      | [< 'LAZY_OR; e = parseExprAnd inh; e' = parseExprOr' (Or (inh, e)) >] -> e')
  | _ -> unexpected stream
  (** <expr-or> *)
  and parseExprOr inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
     (* <expr-or> → <expr-and> <expr-or'> *)
     (match stream with parser
	| [< a = parseExprAnd inh; o' = parseExprOr' a >] -> o')
  | _ -> unexpected stream

  (** <expr> *)
  and parseExpr inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
     (* <expr> → <expr-or> *)
     parseExprOr inh stream
  | NOT_WORD ->
     (* <expr> → 'not' <expr> *)
      (match stream with parser
      | [< 'NOT_WORD; e = parseExpr inh >] -> UnOp (Not, e))
  | _ -> unexpected stream

  (** <simple-expr> *)
  and parseSimpleExpr inh stream = Value Undef (* TODO *)

  (** <cond-end> *)
  and parseCondEnd inh stream = Value Undef (* TODO *)

  (** <cond> *)
  and parseCond inh stream = Value Undef (* TODO *)

  (** <instr'> *)
  and parseInstr' inh stream = Value Undef (* TODO *)

  (** <instr> *)
  and parseInstr inh stream = Value Undef (* TODO *)

  (** <args call list'> *)
  and parseArgsCallList' inh stream = [] (* TODO *)

  (** <args call list> *)
  and parseArgsCallList inh stream = [] (* TODO *)

  (** <funcall args> *)
  and parseFuncallArgs inh stream = [] (* TODO *)

  (** <funcall> *)
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

  (** <instr list'> *)
  and parseInstrList' inh stream = match peek stream with
  | RBRACE ->
      (* <instr list'> → ε *)
      []
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | RETURN  | CALL_MARK | LPAR | IF | UNLESS | NOT_WORD | NOT | PLUS | MINUS ->
    (* <instr list'> → <instr> ';' <instr list'>  *)
      (match stream with parser
        [< i = parseInstr inh; 'SEMICOLON; i' = parseInstrList' inh >] -> i::i')

  (** <instr list> *)
  and parseInstrList inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | RETURN | CALL_MARK | LPAR | IF | UNLESS | NOT_WORD | NOT | PLUS | MINUS->
      (* <instr list> → <instr> ';' <instr list'> *)
      (match stream with parser
      | [< i = parseInstr inh; 'SEMICOLON; i' = parseInstrList' inh >] ->
          i::i')
  | LBRACE ->
    (* <instr list> → '{' <instr list> '}' *)
      (match stream with parser
        [< 'LBRACE; i = parseInstrList inh; 'RBRACE >] -> i)
  | _ -> unexpected stream

  (** <arg list'> *)
  and parseArgList' inh stream = match peek stream with
  | COMMA ->
      (* <arg list'> → ',' var <arg list'> *)
      (match stream with parser
      | [< 'COMMA; '(VAR v); args = parseArgList' inh >] ->
          v::args)
  | RPAR ->
      (* <arg list'> → ε *)
      []
  | _ -> unexpected stream

  (** <arg list> *)
  and parseArgList inh stream = match peek stream with
  | VAR _ ->
    (* <arg list> → var <arg list'> *)
      (match stream with parser
        [< '(VAR v); args = parseArgList' inh >] ->
          v::args)
  | RPAR ->
    (* <arg list> → ε *)
      []
  | _ -> unexpected stream

  (** <function args> *)
  and parseFunctionArgs inh stream = match peek stream with
  | LPAR ->
      (* <function args>  → '(' <arg list> ')' *)
      (match stream with parser
        [< 'LPAR; args = parseArgList inh; 'RPAR >] ->
          args)
  | _ -> unexpected stream

  (** <function> *)
  and parseFunction inh stream = match peek stream with
  | SUB ->
      (* <function> → 'sub' identifier <function args> '{' <instr list> '}' *)
      (match stream with parser
        [< 'SUB; 'IDENTIFIER name; args = parseFunctionArgs inh;
           'LBRACE; body = parseInstrList inh; 'RBRACE >] ->
             Fundef (name, args, body))
  | _ -> unexpected stream

  (** <function list'> *)
  and parseFunctionList' inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | NOT | PLUS | MINUS
  | LBRACE | LPAR | IF | UNLESS | NOT_WORD | EOF ->
      (* <function list'> → ε *)
      []
  | SUB ->
      (* <function list'> → <function> <function list'> *)
      (match stream with parser
      | [< f = parseFunction inh; l = parseFunctionList' inh >] ->
          f::l)
  | _ -> unexpected stream

  (** <function list> *)
  and parseFunctionList inh stream = match peek stream with
  | SUB ->
      (* <function list> → <function> <function list'> *)
      (match stream with parser
      | [< f = parseFunction inh; l = parseFunctionList' inh >] ->
          f::l)
  | _ -> unexpected stream

  (** <program'> *)
  and parseProgram' inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | NOT | PLUS | MINUS
  | LBRACE | LPAR | IF | UNLESS | NOT_WORD ->
      (* <program'> → <instr list> *)
      (match stream with parser
      | [< l = parseInstrList inh >] -> l)
  | EOF ->
      (* <program'> → ε *)
      []
  | _ -> unexpected stream

  (** <program> *)
  and parseProgram inh stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | NOT | PLUS | MINUS
  | LBRACE | LPAR | IF | UNLESS | NOT_WORD ->
      (* <program> → <instr list> *)
      (match stream with parser
      | [< l = parseInstrList inh >] -> ([], l))
  | SUB ->
      (*  <program> →  <function list> <program'>  *)
      (match stream with parser
      | [< l = parseFunctionList inh; p = parseProgram' inh >] ->
          (l, p))
  | _ -> unexpected stream
  in
  (* TODO: the inh argument is not needed everywhere *)
  parseProgram (Value Undef)
