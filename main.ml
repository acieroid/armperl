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
  let res, _ = Eval.eval Symtable.empty (BinOp (Plus, Value (Integer 3), Value (Integer 5))) in
  print_string (string_of_value res);
  print_newline ()
    
