@ translation of the arith.pl program
  .section .rodata
  .align 2

  .text
  .align 2

  .global main
  .type main, %function
main:
  stmfd sp!, {fp, lr}

  @@ (5 + 1)
  @ saw 5, load 5
  mov r0, #5

  @ saw addition, save 5
  stmfd sp!, {r0}

  @ saw 1
  mov r0, #1

  @ do the addition
  @ first load the 5
  ldmfd sp!, {r1}
  add r0, r0, r1

  @ saw multiplication, save addition result
  ldmfd sp!, {r0}

  @@ ((12 / 2) + 1)
  @ saw 12
  mov r0, #12

  @ saw division, save 12
  stmfd sp!, {r0}

  @ saw 2
  mov r0, #2

  @ do the division
  @ first load the 12
  ldmfd sp!, {r1}
  div r0, r0, r1

  @ saw addition, save division result
  stmfd sp!, {r0}
  
  @ saw 1
  mov r0, #1

  @ do the addition
  @ first load the (12 / 2)
  ldmfd sp!, {r1}
  add r0, r0, r1

  @ do the multiplication
  @ first load the (5 + 1)
  ldmfd sp!, {r1}
  add r0, r0, r1

  @ set the return value as return (TODO: print it)
  mov r3, r0
  mov r0, r3

  @ return
  ldmfd sp!, {fp, lr}
  bx lr

  .section .note.GNU-stack,"",%progbits
