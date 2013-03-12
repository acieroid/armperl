open Expression

let box_int n =
  (n lsl 1) + 1

type state = {
    symtable: Symtable.t;
    stringtable: Stringtable.t;
    buffer: Buffer.t;
    mutable args: string list option;
  }

let create_state () =
  {symtable=Symtable.create ();
   stringtable=Stringtable.create ();
   buffer=Buffer.create 16;
   args=None}

let state_add state string =
  Buffer.add_string state.buffer string;
  Buffer.add_char state.buffer '\n'

let state_string_addr state string =
  Stringtable.add state.stringtable string;
  let addr = Stringtable.get_addr state.stringtable string in
  ".Lstrs+" ^ (string_of_int addr)

let state_output_strings state channel =
  Stringtable.iter state.stringtable
    (fun id str ->
      output_string channel ("
    .align 2
.Lstr" ^ (string_of_int id) ^ ":
    .ascii \"" ^ str ^ "\\000\""))

let state_output_globals state channel =
  let convert_value = function
    | Integer n -> string_of_int (box_int n)
    | String s -> state_string_addr state s
    | True -> string_of_int (box_int 1)
    | False -> string_of_int (box_int 0)
    | Undef -> "2"
    | Float _ -> failwith "Floats are unsupported"
  in
  Symtable.iter state.symtable
    (fun id name value ->
      output_string channel ("
    .align 2
    .type " ^ name ^ ", %object
    .size " ^ name ^ ", 4
" ^ name ^ ":
    .word " ^ (convert_value value)))

let state_output_addresses state channel =
  (* output the addresses of the global variables *)
  output_string channel "
    .align 2
.Lglobals:";
  Symtable.iter state.symtable
    (fun id var value ->
      output_string channel ("
    .word " ^ var));
  (* output the addresses of the strings *)
  output_string channel "
    .align 2
.Lstrs:";
  Stringtable.iter state.stringtable
    (fun id str ->
      output_string channel ("
    .word .Lstr" ^ (string_of_int id)))

let output_header channel =
  output_string channel "
    .arch armv5te
    .fpu softvfp
    .eabi_attribute 20, 1
    .eabi_attribute 21, 1
    .eabi_attribute 23, 1
    .eabi_attribute 24, 1
    .eabi_attribute 25, 1
    .eabi_attribute 26, 1
    .eabi_attribute 30, 1
    .eabi_attribute 18, 4
"

(**
  Generate the assembly code to load a value in the register r0.

  Integers are left shifted by one bit, and added to 1, such that the
  least significant bit of an integer is always 1 (thus, integers are
  31 bits).

  True and False are stored respectively as the integers 1 and 0.

  Strings are stored in pointers, so that the two least significant
  bits are always 0.

  Undef is represented by 2, so that it is distinguishable from an
  integer and a string.
*)
let gen_value state = function
  | Integer x -> state_add state ("
    mov r0, #" ^ (string_of_int (box_int x)))
  | True -> state_add state ("
    mov r0, #" ^ (string_of_int (box_int 1)))
  | False -> state_add state ("
    mov r0, #" ^ (string_of_int (box_int 0)))
  | String str ->
      let addr = state_string_addr state str in
      state_add state ("
    ldr r0, .L" ^ addr)
  | Undef -> state_add state "
    mov r0, #2"
  | Float _ -> failwith "Floats are unsupported"

let gen_instr state = function
  | Value v -> gen_value state v
  (* TODO *)
  | Variable v -> failwith "Not implemented"
  | BinOp (op, e1, e2) -> failwith "Not implemented"
  | Assign (var, value) -> failwith "Not implemented"
  | Or (e1, e2) -> failwith "Not implemented"
  | And (e1, e2) -> failwith "Not implemented"
  | UnOp (op, e) -> failwith "Not implemented"
  | Funcall (fname, args) -> failwith "Not implemented"
  | Cond (cond, consequent, alternative) -> failwith "Not implemented"
  | CondEnd -> failwith "Not implemented"
  | Return x -> failwith "Not implemented"
  | Fundef _ -> failwith "Function definition not allowed here"

let gen_fun state = function
  | Fundef (fname, args, body) -> failwith "Not implemented"
  | _ -> failwith "Not a function definition"

let gen channel (funs, instrs) =
  let state = create_state () in
  List.iter (gen_fun state) funs;
  state_add state "
    .align 2
    .global main
    .type main, %function
main:";
  List.iter (gen_instr state) instrs;
  (* Output the processor configuration *)
  output_header channel;
  (* Output the global variables *)
  state_output_globals state channel;
  (* Start the read-only section *)
  output_string channel "
    .section .rodata";
  (* Output the strings definitions *)
  state_output_strings state channel;
  (* Output the code *)
  Buffer.output_buffer channel state.buffer;
  (* Output the addresses definitions *)
  state_output_addresses state channel
