open Utils
open Expression

let lexer_arg = ref "hand"
let parser_arg = ref "hand"
let codegen_arg = ref "hand"
let in_arg = ref ""
let out_arg = ref ""
let evaluate_arg = ref false
 
let usage = "usage: " ^ Sys.argv.(0) ^ " [-l lexer] [-p parser] [-c codegenerator] [-e] [-i in] [-o out]"
 
let speclist = [
  ("-l", Arg.String (fun s -> lexer_arg := s),   ": lexer to use [hand]");
  ("-p", Arg.String (fun s -> parser_arg := s),  ": parser to use [hand]");
  ("-c", Arg.String (fun s -> codegen_arg := s), ": code generator to use [hand]");
  ("-e", Arg.Set    evaluate_arg,                ": evaluate the program instead of generating code");
  ("-i", Arg.String (fun s -> in_arg := s),      ": input file to read (stdin)");
  ("-o", Arg.String (fun s -> out_arg := s),     ": output file to write (stdout)");
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
      | Right Tokens.EOF -> [< 'Tokens.EOF >]
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
  | _ -> failwith ("Unknown code generator: " ^ !codegen_arg)
  in
  let chan_in = match !in_arg with
  | "" -> stdin
  | f -> open_in f
  and chan_out = match !out_arg with
  | "" -> stdout
  | f -> open_out f in
  let (fns, instrs), symtable = parse (drop_errors (lex chan_in)) in
  if !evaluate_arg then
    let (_, st) = Eval.eval_sequence (Eval_symtable.empty ()) true fns in
    let _ = Eval.eval_sequence st true instrs in
    ()
  else
    gen chan_out (fns, instrs) symtable

    
