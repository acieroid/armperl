open Utils
open Expression

let rec loop s =
  match (Lexer.lexer s) with
  | Left err ->
      print_string "error during lexing: ";
      print_string err;
      print_newline ();
      loop s
  | Right Lexer.EOF -> ()
  | Right x ->
      print_string (Lexer.string_of_token x);
      print_newline ();
      loop s

let () =
  (* loop (Lexer.state_of_channel stdin) *)
  (* let e = (Or (Value (String "foo"), (BinOp (Plus, Value (Integer 3), Value (Integer 5))))) in *)
  let f = (Fundef ("hello", ["name"],
                   [BinOp (Concat,
                           BinOp (Concat, Value (String "Hello, "), Variable "name"),
                           Value (String "\n"))])) and
      e = (Funcall ("hello", [Value (String "world!")])) in
  let res, st' = Eval.eval (Symtable.empty ()) true f in
  let res', _ = Eval.eval st' true e in
  print_string (string_of_value res')
    
