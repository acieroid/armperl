open Utils
open Expression

let lexer_arg = ref "hand"
let parser_arg = ref "hand"
let codegen_arg = ref "hand"
let evaluate_arg = ref false
 
let usage = "usage: " ^ Sys.argv.(0) ^ " [-l lexer] [-p parser] [-c codegenerator] [-e]"
 
let speclist = [
  ("-l", Arg.String (fun s -> lexer_arg := s),   ": lexer to use [hand]");
  ("-p", Arg.String (fun s -> parser_arg := s),  ": parser to use [hand]");
  ("-c", Arg.String (fun s -> codegen_arg := s), ": code generator to use [hand]");
  ("-e", Arg.Set    evaluate_arg,                ": evaluate the program instead of generating code");
]

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
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  let lex = match !lexer_arg with
  | "hand" -> Hand_lexer.lex
  | "ocamllex" -> Ocamllex_lexer.lex
  | _ -> failwith ("Unknown lexer: " ^ !lexer_arg)
  and parse = match !parser_arg with
  | "hand" -> Parser.parse
  | _ -> failwith ("Unknown parser: " ^ !parser_arg) in
  let (fns, instrs) = parse (drop_errors (lex stdin)) in
  let _ = Eval.eval_sequence (Symtable.empty ()) true instrs in
  ()
  (* List.iter (fun e -> print_string (string_of_expression e)) fns;
  List.iter (fun e -> print_string (string_of_expression e)) instrs; *)
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
    
