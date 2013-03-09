open Utils
open Expression

let rec loop s =
  match Stream.peek s with
  | Some x -> Stream.junk s;
      (match x with
      | Left err ->
          print_string "error during lexing: ";
          print_string err;
          print_newline ();
          loop s
      | Right Tokens.EOF -> ()
      | Right x ->
          print_string (Lexer.string_of_token x);
          print_newline ();
          loop s)
  | None -> ()

let () =
  let module L = Lexer in
  loop (L.lex stdin)
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
    
