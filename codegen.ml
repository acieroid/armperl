open Expression

let box_int n =
  (n lsl 1) + 1

type state = {
    symtable: Symtable.t;
    stringtable: Stringtable.t;
    buffer: Buffer.t;
    args: mutable string list;
  }

let create_state () =
  {symtable=Symtable.create ();
   stringtable=Stringtable.create ();
   buffer=Buffer.create 16;
   args=[]}

let state_add string state =
  Buffer.add_string state.buffer string;
  Buffer.add_char state.buffer '\n'

(* TODO *)
let state_string_addr state string =
  add_string state.stringtable;
  let addr = get_addr state.stringtable string in
  ".Lstrs+" ^ (string_of_int addr)

let state_output_strings state channel =
  Stringtable.iter st.stringtable
    (fun id str ->
      output_string channel ("
    .align 2
.Lstr" ^ (string_of_int id) ^ ":
    .ascii \"" ^ str ^ "\\000\""))

let state_output_addresses state channel =
  output_string channel "
    .align 2
.Lstrs:";
  Stringtable.iter st.stringtable
    (fun id str ->
      output_string channel "
    .word .Lstr" ^ (string_of_int id) ^ "")

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
  | Integer x -> state_add state ("mov r0, #" ^ (string_of_int (box_int x)))
  | True -> state_add state ("mov r0, #" ^ (string_of_int (box_int 1)))
  | False -> state_add state ("mov r0, #" ^ (string_of_int (box_int 0)))
  | String str ->
      let addr = state_string_addr state str in
      state_add state ("ldr r0, .L" ^ addr)
  | Undef -> state_add state "mov r0, #2"

let gen_expr = function
  | Value v -> gen_value
  (* TODO *)

let gen channel (funs, instrs) =
  let state = create_state () in
  List.iter (fun x -> gen_fun x state) funs;
  List.iter (fun x -> gen_instr x state) instr;
  (* Output the processor configuration *)
  output_header channel;
  (* Output the variables *)
  state_output_variables state channel;
  (* Start the read-only section *)
  output_string channel "
    .section .rodata"
  (* Output the strings definitions *)
  state_output_strings state channel;
  (* Output the code *)
  Buffer.output_buffer channel state.buffer;
  (* Output the addresses definitions *)
  state_output_addresses state channel
