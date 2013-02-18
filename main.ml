open Utils

let rec loop s =
  match (Lexer.lexer s) with
  | Left err ->
      print_newline ();
      print_string "error when lexing character at position ";
      print_int (Stream.count s);
      print_string ": ";
      print_string err;
      print_newline ();
      loop s
  | Right Lexer.EOF -> ()
  | Right x ->
      print_string (Lexer.string_of_token x);
      print_newline ();
      loop s

let () =
  loop (stream_of_input stdin)
