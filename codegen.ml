open Expression

(** Box an integer, to be able to distinguish it from a pointer to a string *)
let box_int n =
  (n lsl 1) + 1

(** The state of the code generator *)
type state = {
    symtable: Symtable.t;             (** Table containing the global variables *)
    stringtable: Stringtable.t;       (** Table containing the strings *)
    buffer: Buffer.t;                 (** Buffer containing the generated code *)
    body_buffer: Buffer.t;            (** Temporary buffer to store function bodies *)
    mutable args: string list option; (** Optional list of arguments *)
    mutable last_label: int;          (** Store the last label generated *)
    mutable return_label: int option; (** Hold the current 'return' label *)
  }

(** Create an empty state *)
let create_state () =
  {symtable=Symtable.create ();
   stringtable=Stringtable.create ();
   buffer=Buffer.create 16;
   body_buffer=Buffer.create 16;
   args=None;
   last_label=0;
   return_label=None;
 }

(** Set the current arguments of the state *)
let state_set_args state args =
  state.args <- Some args

(** Clear the current arguments of the state *)
let state_clear_args state =
  state.args <- None

(** Return a new label (as a ".Ln" string) *)
let state_new_label state =
  let n = state.last_label + 1 in
  state.last_label <- n;
  ".L" ^ (string_of_int n)

(** Return the current 'return' label, or output an error *)
let state_return_label state =
  match state.return_label with
  | Some l -> ".Lreturn" ^ (string_of_int l)
  | None -> failwith "Cannot return outside a subroutine"

(** Return a new 'return' label *)
let state_new_return_label state =
  let _ = state_new_label state in
  state.return_label <- Some state.last_label;
  state_return_label state

(** Return the address of a function argument *)
let state_arg_addr state arg =
  match state.args with
  | Some args ->
      let index = Utils.index_of arg args in
      let addr = if index < 4 then -(8 + (index*4)) else (index-3)*4 in
      "[fp, #" ^ (string_of_int addr) ^ "]"
  | None -> failwith ("No such argument: " ^ arg)

(** Is the variable a function argument or a global variable ? *)
let state_is_arg state arg =
  match state.args with
  | Some args -> List.mem arg args
  | None -> false

(** Add generated code to the temporary buffer *)
let state_add state string =
  Buffer.add_string state.body_buffer string

(** Add generated code to the final buffer *)
let state_add_directly state string =
  Buffer.add_string state.buffer string

(** Merge the temporary buffer into the final buffer *)
let state_merge state =
  Buffer.add_buffer state.buffer state.body_buffer;
  Buffer.clear state.body_buffer

(** Store a new string *)
let state_string_addr state string =
  Stringtable.add state.stringtable string;
  let addr = Stringtable.get_addr state.stringtable string in
  ".Lstrs+" ^ (string_of_int addr)

(** Return the address of a global variable *)
let state_global_addr state var =
  (* Variables have the value undef by default *)
  Symtable.add state.symtable var Undef;
  let addr = Symtable.get_addr state.symtable var in
  ".Lglobals+" ^ (string_of_int addr)

(** Output the strings part of the assembly file *)
let state_output_strings state channel =
  let replace_newline = Str.global_replace (Str.regexp "\n") "\\n" in
  Stringtable.iter state.stringtable
    (fun id str ->
      output_string channel ("
    .align 2
.Lstr" ^ (string_of_int id) ^ ":
    .ascii \"" ^ (replace_newline str) ^ "\\000\""))

(** Output the global variables part of the assembly file *)
let state_output_globals state channel =
  let convert_value = function
    | Integer n -> string_of_int (box_int n)
    | String s -> state_string_addr state s
    | True -> string_of_int (box_int 1)
    | False -> string_of_int (box_int 0)
    | Undef -> "2"
    | Float _ -> failwith "Floats are unsupported"
  in
  output_string channel ("
    .data");
  Symtable.iter state.symtable
    (fun id name value ->
      output_string channel ("
    .global " ^ name ^ "
    .align 2
    .type " ^ name ^ ", %object
    .size " ^ name ^ ", 4
" ^ name ^ ":
    .word " ^ (convert_value value)))

(** Output the addresses part of the assembly file *)
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

let function_name = function
  | Plus -> "perl_plus"
  | Minus -> "perl_minus"
  | Times -> "perl_times"
  | Divide -> "perl_divide"
  | Concat -> "perl_concat"
  | Equals -> "perl_equals"
  | Different -> "perl_different"
  | Greater -> "perl_greater"
  | Lower -> "perl_lower"
  | GreaterEquals -> "perl_greater_equals"
  | LowerEquals -> "perl_lower_equals"
  | StrEquals -> "perl_str_equals"
  | StrDifferent -> "perl_str_different"
  | StrGreater -> "perl_str_greater"
  | StrLower -> "perl_str_lower"
  | StrGreaterEquals -> "perl_str_greater_equals"
  | StrLowerEquals -> "perl_str_lower_equals"

(** Output the header of the assembly file *)
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
  Generate the assembly code to load a value in the register r4.

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
    mov r4, #" ^ (string_of_int (box_int x)))
  | True -> state_add state ("
    mov r4, #" ^ (string_of_int (box_int 1)))
  | False -> state_add state ("
    mov r4, #" ^ (string_of_int (box_int 0)))
  | String str ->
      let addr = state_string_addr state str in
      state_add state ("
    ldr r4, " ^ addr)
  | Undef -> state_add state "
    mov r4, #2"
  | Float _ -> failwith "Floats are unsupported"

(** Load a local variable *)
let gen_local state v =
  let addr = state_arg_addr state v in
  state_add state ("
    ldr r4, " ^ addr)

(** Load a global variable *)
let gen_global state v =
  let addr = state_global_addr state v in
  state_add state ("
    ldr r4, " ^ addr ^ "
    ldr r4, [r4, #0]")

(** Generate multiple instructions and return the number of bytes
    needed on the stack for those instructions *)
let rec gen_instrs state instrs =
  List.fold_left max 0 (List.map (gen_instr state) instrs) 

(** Generate an instruction *)
and gen_instr state = function
  | Value v -> gen_value state v; 0
  | Variable v ->
      if state_is_arg state v
      then gen_local state v
      else gen_global state v;
      0
  | BinOp (op, e1, e2) ->
      gen_binop state op e1 e2
  | Assign (var, value) ->
      if state_is_arg state var then
        gen_assign_local state var value
      else
        gen_assign_global state var value
  | Or (e1, e2) ->
      (* TODO: check if the generated code is correct *)
      gen_instr state (Cond (e1, [Value True],
                             Cond (e2, [Value True], Value False)))
  | And (e1, e2) ->
      (* TODO: check if the generated code is correct *)
      gen_instr state (Cond (e1,
                             [Cond (e2, [Value True], Value False)],
                             Value False))
  (* TODO *)
  | UnOp (op, e) -> failwith "unop implemented"
  | Funcall (fname, args) ->
      (* TODO: check that the number of arguments is correct *)
      (* TODO: merge the strings for print *)
      (* TODO: compute the last argument for substr *)
      gen_funcall state fname args
  | Cond (cond, consequent, alternative) ->
      (* TODO: check if the generated code is correct *)
      gen_cond state cond consequent alternative
  | CondEnd -> 0
  | Return x ->
      gen_return state x
  | Fundef _ -> failwith "Function definition not allowed here"

(** Generate a binary operation *)
and gen_binop state op e1 e2 =
  (* Generate e1 *)
  let stack_needed_e1 = gen_instr state e1 in
  (* Push the result on the stack *)
  state_add state ("
    stmfd sp!, {r4}" );
  (* Generate e2 *)
  let stack_needed_e2 = gen_instr state e2 in
  (* Store the result of e2 in r1, the result of e1 in r0, and call
  the function that does the operation *)
  state_add state ("
    mov r1, r4
    ldmfd sp!, {r0}
    bl " ^ (function_name op) ^ "
    mov r4, r0");
  max stack_needed_e1 stack_needed_e2

(** Generate a conditional jump *)
and gen_cond state cond consequent alternative =
  let alternative_label = state_new_label state
  and end_label = state_new_label state
  (* Generate the code for the condition *)
  and stack_needed_cond = gen_instr state cond in
  (* r4 contains true or false, jump if it is false *)
  state_add state ("
    cmp r4, #" ^ (string_of_int (box_int 1)) ^ "
    bne " ^ alternative_label);
  (* generate consequent *)
  let stack_needed_consequent = gen_instrs state consequent in
  (* jump to the end *)
  state_add state ("
    b " ^ end_label);
  (* add the alternative label *)
  state_add state ("
" ^ alternative_label ^ ":");
  (* generate alternative *)
  let stack_needed_alternative = gen_instr state alternative in
  (* add the end label *)
  state_add state ("
" ^ end_label ^ ":");
  max (max stack_needed_alternative stack_needed_consequent) stack_needed_cond

(** Generate a return statement *)
and gen_return state x =
  let stack_needed = gen_instr state x in
  (* Jump to the current function's return label *)
  state_add state ("
    mov r0, r4
    b " ^ (state_return_label state));
  stack_needed


(** Assign a value to a local variable *)
and gen_assign_local state var value =
  let addr = state_arg_addr state var
  and stack_needed = gen_instr state value in
  (* copy the value from r4 to the argument *)
  state_add state ("
    str r4, " ^ addr);
  stack_needed

(** Assign a value to a global variable *)
and gen_assign_global state var value =
  let addr = state_global_addr state var
  and stack_needed = gen_instr state value in
  (* Load global into r5 and store the value (which is in r4)
     into the global *)
  state_add state ("
    ldr r5, " ^ addr ^ "
    str r4, [r5, #0]");
  stack_needed;

(** Generate a function call *)
and gen_funcall state fname args =
  let stack_needed =
    if List.length args > 4 then
      ((List.length args)-4)*4
    else
      0
  in
  (* Generate the arguments *)
  let stack_needed_l = List.mapi (fun i arg ->
    (* Generate this argument *)
    let stack_needed = gen_instr state arg in
    (* Put it in the correct register or push it on the stack *)
    if i < 4 then
      state_add state ("
    mov r" ^ (string_of_int i) ^ ", r4")
    else
      state_add state ("
    str r4, [sp, #" ^ (string_of_int ((i-4)*4)) ^ "]");
    stack_needed) args in
  (* Call the function *)
  state_add state ("
    bl " ^ fname);
  (* Return the stack size needed *)
  List.fold_left max 0 (stack_needed::stack_needed_l)

(** Generate a function definition *)
and gen_fun state = function
  | Fundef (fname, args, body) ->
      state_set_args state args;
      let return_label = state_new_return_label state in
      (* the number of bytes we need on the stack for this function *)
      let stack_needed =
        (gen_instrs state body) + (min ((List.length args)*4) 16) in
      let stack_increment =
        (* the actual number of bytes that will be allocated (it
        should be a multiple of 8 *)
        if stack_needed mod 8 = 0 then stack_needed else stack_needed+4
      in
      state_add_directly state ("
    .align 2
    .global " ^ fname ^ "
    .type " ^ fname ^ ", %function
" ^ fname ^ ":
    stmfd sp!, {fp, lr}
    add fp, sp, #4
    sub sp, sp, #" ^ (string_of_int stack_increment));
      for i = 0 to (min (List.length args) 4)-1 do
        state_add_directly state ("
    str r" ^ (string_of_int i) ^ ", [fp, #-" ^ (string_of_int (8+i*4)) ^ "]")
      done;
      state_merge state;
      (* Return undef by default *)
      state_add_directly state ("
    mov r0, #2
" ^ return_label ^ ":
    sub sp, fp, #4
    ldmfd   sp!, {fp, pc}
    .size " ^ fname ^ ", .-" ^ fname);
      state_clear_args state
  | _ -> failwith "Not a function definition"

(** Main code generation function *)
let gen channel (funs, instrs) =
  let state = create_state () in
  (* Generate the function definitions *)
  List.iter (gen_fun state) funs;
  (* Generate the body of the main function *)
  let stack_needed = gen_instrs state instrs in
  (* Generate the main function *)
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
    .text";
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
