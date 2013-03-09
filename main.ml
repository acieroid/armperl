open Utils
open Expression

let rec drop_errors stream =
  match Stream.peek stream with
  | Some x -> Stream.junk stream;
      (match x with
      | Left err ->
          print_string "error during lexing: ";
          print_string err;
          print_newline ();
          drop_errors stream
      | Right Tokens.EOF -> [< >]
      | Right x ->
          [< 'x; drop_errors stream >])
  | None -> [< >]

let () =
  let module L = Hand_lexer in
  let module P = Parser in
  let (fns, instrs) = P.parse (drop_errors (L.lex stdin)) in
  List.iter (fun e -> print_string (string_of_expression e)) fns;
  List.iter (fun e -> print_string (string_of_expression e)) instrs;
  (* let e = (Or (Value (String "foo"), (BinOp (Plus, Value (Integer 3), Value (Integer 5))))) in *)
  (* let f = (Fundef ("hello", ["name"],
                   [BinOp (Concat,
                           BinOp (Concat, Value (String "Hello, "), Variable "name"),
                           Value (String "\n"))])) and
      e = (Funcall ("hello", [Value (String "world!")])) in
  let res, st' = Eval.eval (Symtable.empty ()) true f in
  let res', _ = Eval.eval st' true e in *)
  (* let e = [Fundef ("name", ["arg"],
                   [Assign ("external_variable",
                            Value (String "side effect\n"));
                    Funcall ("print", [Variable "arg"])]);
           Funcall ("name", [Value (String "foo\n")]);
           Funcall ("print", [Variable "external_variable"])
         ] in
  let _ = Eval.eval_sequence (Symtable.empty ()) true e in
  () *)
    
