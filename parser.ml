open Tokens
open Expression

let unoption = function
  | Some x -> 
      (* print_string (string_of_token x); print_newline (); *)
      x
  | None -> failwith "Premature end of input"

let peek stream = unoption (Stream.peek stream)

let unexpected stream rule =
  failwith ("unexpected symbol: " ^
            (string_of_token (Stream.next stream)) ^
           " while in rule " ^ rule)

let rec parse stream =
  let symtable = Symtable.create () in
  (** <factor> *)
  let rec parseFactor stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | CALL_MARK ->
      (* <factor> → <simple expr> *)
      parseSimpleExpr stream
  | LPAR ->
      (* <factor> → '(' <expr> ')' *)
      (match stream with parser
      | [< 'LPAR; e = parseExpr; 'RPAR >] -> e)
  | NOT | PLUS | MINUS ->
      (* <factor> → '!' <factor> *)
      (* <factor> → '+' <factor> *)
      (* <factor> → '-' <factor> *)
      (match stream with parser
      | [< 'NOT; f = parseFactor >] -> UnOp (Not, f)
      | [< 'PLUS; f = parseFactor >] -> UnOp (UnaryPlus, f)
      | [< 'MINUS; f = parseFactor >] -> UnOp (UnaryMinus, f))
  | _ -> unexpected stream "factor"

  (** <term'> *)
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
      | [< 'TIMES; f = parseFactor;
           t' = parseTerm' (BinOp (Times, inh, f)); >] -> t'
      | [< 'DIVIDE; f = parseFactor;
           t' = parseTerm' (BinOp (Divide, inh, f)); >] -> t')
  | _ -> unexpected stream "term'"

  (** <term> *)
  and parseTerm stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <term> → <factor> <term'> *)
      (match stream with parser
      | [< f = parseFactor; t' = parseTerm' f >] -> t')
  | _ -> unexpected stream "term'"

  (** <calc'> *)
  and parseCalc' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | ASSIGN | LAZY_OR | LAZY_AND
  | EQUALS | DIFFERENT | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_EQUALS | STRING_DIFFERENT
  | STRING_GREATER | STRING_LOWER
  | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS
  | IF | UNLESS ->
      (* <calc'> → ε *)
      inh
  | CONCAT | PLUS | MINUS ->
      (* <calc'> → '.' <term> <calc'>*)
      (* <calc'> → '+' <term> <calc'>*)
      (* <calc'> → '-' <term> <calc'>*)
      (match stream with parser
      | [< 'CONCAT; t = parseTerm;
           c' = parseCalc' (BinOp (Concat, inh, t)); >] -> c'
      | [< 'PLUS; t = parseTerm;
           c' = parseCalc' (BinOp (Plus, inh, t)); >] -> c'
      | [< 'MINUS; t = parseTerm;
           c' = parseCalc' (BinOp (Minus, inh, t)); >] -> c')
  | _ -> unexpected stream "calc'"

  (** <calc> *)
  and parseCalc stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <calc> → <term> <calc'> *)
      (match stream with parser
      | [< t = parseTerm; c' = parseCalc' t >] -> c')
  | _ -> unexpected stream "calc"

  (** <comp'> *)
  and parseComp' inh stream = match peek stream with
  | LBRACE | RPAR | SEMICOLON | COMMA | ASSIGN | LAZY_OR | LAZY_AND
  | EQUALS | DIFFERENT | STRING_EQUALS | STRING_DIFFERENT 
  | IF | UNLESS ->
      (* <comp'> → ε *)
      inh
  | GREATER | LOWER | GREATER_EQUALS | LOWER_EQUALS
  | STRING_GREATER | STRING_LOWER
  | STRING_GREATER_EQUALS | STRING_LOWER_EQUALS ->
      (* <comp'> -> '>' <calc> <comp'> *)
      (* <comp'> -> '<' <calc> <comp'> *)
      (* <comp'> -> '>=' <calc> <comp'> *)
      (* <comp'> -> '<=' <calc> <comp'> *)
      (* <comp'> -> 'gt' <calc> <comp'> *)
      (* <comp'> -> 'lt' <calc> <comp'> *)
      (* <comp'> -> 'ge' <calc> <comp'> *)
      (* <comp'> -> 'le' <calc> <comp'> *)
      (match stream with parser
      | [< 'GREATER; c = parseCalc;
           c' = parseComp' (BinOp (Greater, inh, c)) >] -> c'
      | [< 'LOWER; c = parseCalc;
           c' = parseComp' (BinOp (Lower, inh, c)) >] -> c'
      | [< 'GREATER_EQUALS; c = parseCalc;
           c' = parseComp' (BinOp (GreaterEquals, inh, c)) >] -> c'
      | [< 'LOWER_EQUALS; c = parseCalc;
           c' = parseComp' (BinOp (LowerEquals, inh, c)) >] -> c'
      | [< 'STRING_GREATER; c = parseCalc;
           c' = parseComp' (BinOp (StrGreater, inh, c)) >] -> c'
      | [< 'STRING_LOWER; c = parseCalc;
           c' = parseComp' (BinOp (StrLower, inh, c)) >] -> c'
      | [< 'STRING_GREATER_EQUALS; c = parseCalc;
           c' = parseComp' (BinOp (StrGreaterEquals, inh, c)) >] -> c'
      | [< 'STRING_LOWER_EQUALS; c = parseCalc;
           c' = parseComp' (BinOp (StrLowerEquals, inh, c)) >] -> c')
  | _ -> unexpected stream "comp'"

  (** <comp> *)
  and parseComp stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <comp> → <calc> <comp'>  *)
      (match stream with parser
      | [< c = parseCalc; c' = parseComp' c >] -> c')
  | _ -> unexpected stream "comp"

  (** <expr-eq'> *)
  and parseExprEq' inh stream = match peek stream with
  | LBRACE | RPAR |  SEMICOLON | COMMA | ASSIGN | LAZY_OR | LAZY_AND
  | IF | UNLESS ->
      (* <expr-eq'> → ε *) 
      inh
  | EQUALS | DIFFERENT | STRING_EQUALS | STRING_DIFFERENT ->
      (*<expr-eq'> → 'eq' <expr-eq'>*)
      (*<expr-eq'> → 'ne' <expr-eq'>*)
      (*<expr-eq'> → '!=' <expr-eq'>*)
      (*<expr-eq'> → '==' <expr-eq'>*)
      (match stream with parser
      | [< 'EQUALS; e = parseExprEq >] ->
          BinOp (Equals, inh, e)
      | [< 'DIFFERENT; e = parseExprEq >] ->
          BinOp (Different, inh, e)
      | [< 'STRING_EQUALS; e = parseExprEq >] ->
          BinOp (StrEquals, inh, e)
      | [< 'STRING_DIFFERENT; e = parseExprEq >] ->
          BinOp (StrDifferent, inh, e))
  | _ -> unexpected stream "expr-eq'"

  (** <expr-eq> *)
  and parseExprEq stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <expr-eq> → <comp> <expr-eq'> *)
      (match stream with parser
      | [< c = parseComp; e' = parseExprEq' c >] -> e')
  | _ -> unexpected stream "expr-eq"

  (** <expr-and'> *)
  and parseExprAnd' inh stream = match peek stream with
  | LBRACE | RPAR |  SEMICOLON | COMMA | ASSIGN | LAZY_OR | IF | UNLESS ->
      (* <expr-eq'> → ε *) 
      inh
  | LAZY_AND ->
      (* <expr-and'> → '&&' <expr-eq> <expr-and'> *)
      (match stream with parser
      | [< 'LAZY_AND; e = parseExprEq;
           a = parseExprAnd' (And (inh, e)) >] -> a)
  | _ -> unexpected stream "expr-and'"

  (** <expr-and> *)
  and parseExprAnd stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <expr-and> → <expr-eq> <expr-and'> *)
      (match stream with parser
      | [< e = parseExprEq; a = parseExprAnd' e >] -> a)
  | _ -> unexpected stream "expr-and"

  (** <expr-or'> *)
  and parseExprOr' inh stream =  match peek stream with
  | LBRACE | RPAR |  SEMICOLON | COMMA | ASSIGN | IF | UNLESS ->
      (* <expr-eq'> → ε *) 
      inh
  | LAZY_OR ->
      (* <expr-or'> → 'or' <expr-and> <expr-or'> *)
      (match stream with parser
      | [< 'LAZY_OR; e = parseExprAnd;
           e' = parseExprOr' (Or (inh, e)) >] -> e')
  | _ -> unexpected stream "expr-or'"

  (** <expr-or> *)
  and parseExprOr stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <expr-or> → <expr-and> <expr-or'> *)
      (match stream with parser
      | [< a = parseExprAnd; o' = parseExprOr' a >] -> o')
  | _ -> unexpected stream "expr-or"

  (** <expr> *)
  and parseExpr stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | CALL_MARK | LPAR | NOT | PLUS | MINUS ->
      (* <expr> → <expr-or> *)
      parseExprOr stream
  | NOT_WORD ->
      (* <expr> → 'not' <expr> *)
      (match stream with parser
      | [< 'NOT_WORD; e = parseExpr >] -> UnOp (Not, e))
  | _ -> unexpected stream "expr"

  (** <simple-expr> *)
  and parseSimpleExpr stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ -> 
      (* <simple expr> → var *)
      (* <simple expr> → integer *)
      (* <simple expr> → string *)
      (match stream with parser 
      | [< 'VAR x >] ->
          Symtable.add_var symtable x;
          Variable x
      | [< 'INTEGER n >] -> Value (Integer n)
      | [< 'STRING s >] -> Value (String s))
  | IDENTIFIER _  | CALL_MARK ->
      (* <simple expr> → <funcall> *)
      parseFuncall stream
  | _ -> unexpected stream "simple-expr"
	 
  (** <cond-end> *)
  and parseCondEnd stream = match peek stream with
  | RPAR | SEMICOLON | COMMA ->
      (* <cond end> → ε *) 
      CondEnd
  | ELSE | ELSEIF ->
      (* <cond end> → 'else' '{' <instr list> '}' <cond end> *)
      (* <cond end> → 'elsif' <expr> '{' <instr list> '}' <cond end> *)
      (match stream with parser
      | [< 'ELSE; 'LBRACE; i = parseInstrList; 
	   'RBRACE; c = parseCondEnd >] ->
	     Cond (Value True, i, c)
      | [< 'ELSEIF; e = parseExpr; 'LBRACE; i = parseInstrList; 
	   'RBRACE; c = parseCondEnd >] -> Cond (e, i, c))
  | _ -> unexpected stream "cond-end"

  (** <cond> *)
  and parseCond stream = match peek stream with
  | IF | UNLESS ->
      (* <cond> → 'if' <expr> '{' <instr list> '}' <cond end> *)
      (* <cond> → 'unless' <expr> '{' <instr list> '}' <cond end> *)
      (match stream with parser
      | [< 'IF; e = parseExpr; 'LBRACE; i = parseInstrList;
           'RBRACE; ce = parseCondEnd >] ->
          Cond (e, i, ce)
      | [< 'UNLESS; e = parseExpr; 'LBRACE; i = parseInstrList;
           'RBRACE; ce = parseCondEnd >] ->
          Cond (UnOp (Not, e), i, ce))
  | _ -> unexpected stream "cond"

  (** <instr'> *)
  and parseInstr' inh stream = match peek stream with
  | RPAR | SEMICOLON | COMMA ->
      (* <instr'> → ε *)
      inh
  | IF | UNLESS | ASSIGN ->
      (* <instr'> → 'if' <expr> *)
      (* <instr'> → 'unless' <expr> *)
      (* <instr'> → '=' <expr> *)
      (match stream with parser
      | [< 'IF; e = parseExpr >] ->
          Cond (e, [inh], CondEnd)
      | [< 'UNLESS; e = parseExpr >] ->
          Cond (UnOp (Not, e), [inh], CondEnd)
      | [< 'ASSIGN; e = parseExpr >] ->
          (match inh with
          | Variable v -> Assign (v, e)
          | nv -> failwith ("Cannot assign a value to a non-variable: " ^
                            (string_of_expression nv))))
  | _ -> unexpected stream "instr'"

  (** <instr> *)
  and parseInstr stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | CALL_MARK
  | LPAR | NOT | NOT_WORD | PLUS | MINUS ->
      (* <instr> → <expr> <instr'> *)
      (match stream with parser
      | [< e = parseExpr; i' = parseInstr' e >] -> i')
  | RETURN ->
      (* <instr> → 'return' <expr> *)
      (match stream with parser
      | [< 'RETURN; e = parseExpr >] -> Return e)
  | IF | UNLESS ->
      (* <instr> → <cond> *)
      parseCond stream
  | _ -> unexpected stream "instr"

  (** <args call list'> *)
  and parseArgsCallList' stream = match peek stream with
  | RPAR ->
      (* <args call list'> → ε *)
      []
  | COMMA ->
      (* <args call list'> → ',' <instr> <args call list'> *)
      (match stream with parser
      | [< 'COMMA; i = parseInstr; a' = parseArgsCallList' >] -> i::a')
  | _ -> unexpected stream "args call list'"

  (** <args call list> *)
  and parseArgsCallList stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | RETURN | CALL_MARK | LPAR | IF | UNLESS | NOT_WORD | NOT
  | PLUS | MINUS ->
      (* <args call list> → <instr> <args call list'> *)
      (match stream with parser
      | [< i = parseInstr; a' = parseArgsCallList' >] -> i::a')
  | RPAR ->
      (* <args call list> → ε *)
      []
  | _ -> unexpected stream "args call list"

  (** <funcall args> *)
  and parseFuncallArgs stream = match peek stream with
  | LPAR ->
      (* <funcall args> → '(' <args call list> ')' *)
      (match stream with parser
      | [< 'LPAR; a = parseArgsCallList; 'RPAR >] -> a)
  | _ -> unexpected stream "funcall args"

  (** <funcall> *)
  and parseFuncall stream = match peek stream with
  | IDENTIFIER _ | CALL_MARK ->
      (* <funcall> → identifier <funcall args> *)
      (* <funcall> → '&' identifier <funcall args> *)
      (match stream with parser
      | [< '(IDENTIFIER name); args = parseFuncallArgs >] ->
	  Funcall (name, args)
      | [< 'CALL_MARK; '(IDENTIFIER name); args = parseFuncallArgs >] ->
	  Funcall (name, args))
  | _ -> unexpected stream "funcall"

  (** <instr list'> *)
  and parseInstrList' stream = match peek stream with
  | RBRACE | EOF ->
      (* <instr list'> → ε *)
      []
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | RETURN  | CALL_MARK | LPAR | IF | UNLESS
  | NOT_WORD | NOT | PLUS | MINUS ->
      (* <instr list'> → <instr> ';' <instr list'>  *)
      (match stream with parser
        [< i = parseInstr; 'SEMICOLON; i' = parseInstrList' >] -> i::i')
  | _ -> unexpected stream "instr list'"

  (** <instr list> *)
  and parseInstrList stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _
  | RETURN | CALL_MARK | LPAR | IF | UNLESS
  | NOT_WORD | NOT | PLUS | MINUS ->
      (* <instr list> → <instr> ';' <instr list'> *)
      (match stream with parser
      | [< i = parseInstr; 'SEMICOLON; i' = parseInstrList' >] ->
          i::i')
  | LBRACE ->
      (* <instr list> → '{' <instr list> '}' *)
      (match stream with parser
      | [< 'LBRACE; i = parseInstrList; 'RBRACE >] -> i)
  | _ -> unexpected stream "instr list"

  (** <arg list'> *)
  and parseArgList' stream = match peek stream with
  | COMMA ->
      (* <arg list'> → ',' var <arg list'> *)
      (match stream with parser
      | [< 'COMMA; '(VAR v); args = parseArgList' >] ->
          v::args)
  | RPAR ->
      (* <arg list'> → ε *)
      []
  | _ -> unexpected stream "arg list'"

  (** <arg list> *)
  and parseArgList stream = match peek stream with
  | VAR _ ->
      (* <arg list> → var <arg list'> *)
      (match stream with parser
        [< '(VAR v); args = parseArgList' >] ->
          v::args)
  | RPAR ->
    (* <arg list> → ε *)
      []
  | _ -> unexpected stream "arg list"

  (** <function args> *)
  and parseFunctionArgs stream = match peek stream with
  | LPAR ->
      (* <function args>  → '(' <arg list> ')' *)
      (match stream with parser
        [< 'LPAR; args = parseArgList; 'RPAR >] ->
          Symtable.set_locals symtable args;
          args)
  | _ -> unexpected stream "function args"

  (** <function> *)
  and parseFunction stream = match peek stream with
  | SUB ->
      (* <function> → 'sub' identifier <function args> '{' <instr list> '}' *)
      (match stream with parser
        [< 'SUB; 'IDENTIFIER name; args = parseFunctionArgs;
           'LBRACE; body = parseInstrList; 'RBRACE >] ->
             (try
               Symtable.add_fun symtable name (List.length args);
               Fundef (name, args, body)
             with
             | Symtable.Already_defined ->
                 failwith ("Function already defined: " ^ name)))
  | _ -> unexpected stream "function"

  (** <function list'> *)
  and parseFunctionList' stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | NOT | PLUS | MINUS
  | LBRACE | LPAR | IF | UNLESS | NOT_WORD | EOF ->
      (* <function list'> → ε *)
      []
  | SUB ->
      (* <function list'> → <function> <function list'> *)
      (match stream with parser
      | [< f = parseFunction; l = parseFunctionList' >] ->
          f::l)
  | _ -> unexpected stream "function list'"

  (** <function list> *)
  and parseFunctionList stream = match peek stream with
  | SUB ->
      (* <function list> → <function> <function list'> *)
      (match stream with parser
      | [< f = parseFunction; l = parseFunctionList' >] ->
          f::l)
  | _ -> unexpected stream "function list"

  (** <program'> *)
  and parseProgram' stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | NOT | PLUS | MINUS
  | LBRACE | LPAR | IF | UNLESS | NOT_WORD ->
      (* <program'> → <instr list> *)
      (match stream with parser
      | [< l = parseInstrList >] -> l)
  | EOF ->
      (* <program'> → ε *)
      []
  | _ -> unexpected stream "program'"

  (** <program> *)
  and parseProgram stream = match peek stream with
  | VAR _ | INTEGER _ | STRING _ | IDENTIFIER _ | RETURN | CALL_MARK
  | NOT | PLUS | MINUS
  | LBRACE | LPAR | IF | UNLESS | NOT_WORD ->
      (* <program> → <instr list> *)
      (match stream with parser
      | [< l = parseInstrList >] -> ([], l))
  | SUB ->
      (* <program> → <function list> <program'> *)
      (match stream with parser
      | [< l = parseFunctionList; p = parseProgram' >] ->
          (l, p))
  | _ -> unexpected stream "program"

  (** <S> *)
  and parseS stream = match stream with parser
  | [< p = parseProgram >] -> p
in
  parseS stream, symtable
