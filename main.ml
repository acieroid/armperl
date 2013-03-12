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
  | _ -> failwith ("Unknown parser: " ^ !parser_arg)
  and gen = match !codegen_arg with
  | "hand" -> Codegen.gen
  in
  let (fns, instrs) = parse (drop_errors (lex stdin)) in
  gen stdout (fns, instrs)
  (* let _ = Eval.eval_sequence (Eval_symtable.empty ()) true instrs in *)

    
