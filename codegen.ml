open Expression

let box_int n =
  (n lsl 1) + 1

type state = {
    symtable: Symtable.t;
    stringtable: Stringtable.t;
    buffer: Buffer.t;
    body_buffer: Buffer.t;
    mutable args: string list option;
  }

let create_state () =
  {symtable=Symtable.create ();
   stringtable=Stringtable.create ();
   buffer=Buffer.create 16;
   body_buffer=Buffer.create 16;
   args=None}

let state_get_arg_addr state arg =
  match state.args with
  | Some args ->
      let index = Utils.index_of arg args in
      if index < 4 then
        "[fp, #-" ^ (string_of_int (8 + (index*4)))
      else
        "[fp, #" ^ (string_of_int ((index-4)*4))
  | None -> failwith ("No such argument: " ^ arg)

let state_is_arg state arg =
  match state.args with
  | Some args -> List.mem arg args
  | None -> false

let state_add state string =
  Buffer.add_string state.body_buffer string

let state_add_directly state string =
  Buffer.add_string state.buffer string

let state_merge state =
  Buffer.add_buffer state.buffer state.body_buffer;
  Buffer.clear state.body_buffer

let state_string_addr state string =
  Stringtable.add state.stringtable string;
  let addr = Stringtable.get_addr state.stringtable string in
  ".Lstrs+" ^ (string_of_int addr)

let state_global_addr state var =
  (* Variables have the value undef by default *)
  Symtable.add state.symtable var Undef;
  let addr = Symtable.get_addr state.symtable var in
  ".Lglobals+" ^ (string_of_int addr)

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
  Generate the assembly code to load a value in the register r3.

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
    mov r3, #" ^ (string_of_int (box_int x)))
  | True -> state_add state ("
    mov r3, #" ^ (string_of_int (box_int 1)))
  | False -> state_add state ("
    mov r3, #" ^ (string_of_int (box_int 0)))
  | String str ->
      let addr = state_string_addr state str in
      state_add state ("
    ldr r3, .L" ^ addr)
  | Undef -> state_add state "
    mov r3, #2"
  | Float _ -> failwith "Floats are unsupported"

(* Load a local variable *)
let gen_local state v =
  failwith "Not implemented"

(* Load a global variable *)
let gen_global state v =
  let addr = state_global_addr state v in
  state_add state ("
    ldr r3, " ^ addr)

(* Generate multiple instructions and return the number of bytes
  needed on the stack for those instructions *)
let rec gen_instrs state instrs =
  List.fold_left max 0 (List.map (gen_instr state) instrs) 

and gen_instr state = function
  | Value v -> gen_value state v; 0
  | Variable v -> (match state.args with
    | Some args ->
        if List.mem v args
        then gen_local state v
        else gen_global state v
    | None -> gen_global state v); 0
  (* TODO *)
  | BinOp (op, e1, e2) -> failwith "Not implemented"
  | Assign (var, value) ->
      if state_is_arg state var then
        gen_assign_local state var value
      else
        gen_assign_global state var value
  | Or (e1, e2) -> failwith "Not implemented"
  | And (e1, e2) -> failwith "Not implemented"
  | UnOp (op, e) -> failwith "Not implemented"
  | Funcall (fname, args) -> failwith "Not implemented"
  | Cond (cond, consequent, alternative) -> failwith "Not implemented"
  | CondEnd -> failwith "Not implemented"
  | Return x -> failwith "Not implemented"
  | Fundef _ -> failwith "Function definition not allowed here"

(* Assign a value to a local variable *)
and gen_assign_local state var value =
  let addr = state_get_arg_addr state var
  (* TODO: gen_expr ? *)
  and stack_needed = gen_instr state value in
  (* copy the value from r3 to the argument *)
  state_add state ("
    str r3, " ^ addr);
  stack_needed

(* Assign a value to a global variable *)
and gen_assign_global state var value =
  let addr = state_global_addr state var
  (* TODO: gen_expr ? *)
  and stack_needed = gen_instr state value in
  (* TODO *)
  failwith "Not implemented"

and gen_fun state = function
  | Fundef (fname, args, body) ->
      let stack_needed =
        (gen_instrs state body) + (min ((List.length args)*4) 16)
      in
      state_add_directly state ("
    .align 2
    .global " ^ fname ^ "
    .type " ^ fname ^ ", %function
" ^ fname ^ ":
    stmfd sp!, {fp, lr}
    add fp, sp, #4
    sub sp, sp, #" ^ (string_of_int stack_needed));
      for i = 0 to min (List.length args) 4 do
        state_add_directly state ("
    str r" ^ (string_of_int i) ^ ", [fp, #-" ^ (string_of_int (8+i*4)) ^ "]")
      done;
      (* TODO: load arguments on the stack *)
      state_merge state;
      (* Return undef by default *)
      state_add_directly state ("
    mov r0, #2
    sub sp, fp, #4
    ldmfd   sp!, {fp, pc}
    .size " ^ fname ^ ", .-" ^ fname)
  | _ -> failwith "Not a function definition"

let gen channel (funs, instrs) =
  let state = create_state () in
  (* Generate the function definitions *)
  List.iter (gen_fun state) funs;
  (* Generate the body of the main function *)
  let stack_needed = gen_instrs state instrs in
  (* Generate the main function 
     TODO: use gen_fun to generate this ? *)
  state_add_directly state ("
    .align 2
    .global main
    .type main, %function
main:
    stmfd sp!, {fp, lr}
    add fp, sp, #4
    sub sp, sp, #" ^ (string_of_int stack_needed));
  state_merge state; (* merge the body of main *)
  state_add_directly state "
    mov r0, #0
    sub sp, fp, #4
    ldmfd   sp!, {fp, pc}
    .size main, .-main";
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
  state_output_addresses state channel;
  (* Add a trailing new line *)
  output_string channel "
    .section .note.GNU-stack,\"\",%progbits
"
