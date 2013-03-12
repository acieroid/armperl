  .section .rodata
  .align 2
  .text

@ foo function
  .align 2
  .global foo
  .type foo, %function
foo:
  @ store the fp and lr registers
  stmfd sp!, {fp, lr}

  @ compute new fp (always sp+4)
  add fp, sp, #4

  @ compute the new sp, which is the old sp minus the number of elements we
  @ need to store on the stack
  @ here, we need to store the 4 arguments: 4*4 = 16 bytes needed
  @ and we need to call a function with 1 argument: 0 byte needed
  @ if we needed to call a function with >4 arguments, it would need
  @   (n-4)*4 more bytes
  @ so, we extend the stack by 16 bytes.
  sub sp, sp, #16

  @ store the first four arguments on the stack
  str r0, [fp, #-8]
  str r1, [fp, #-12]
  str r2, [fp, #-16]
  str r3, [fp, #-20]

  @ note: the four arguments are now in:
  @ first: fp-8
  @ second: fp-12
  @ third: fp-16
  @ fourth: fp-20
  @ fifth: fp+4
  @ nth with n > 4: fp+(n-4)*4

  @ load the fifth argument to pass it to print
  ldr r0, [fp, #4]

  @ call print
  bl print

  @ return undef
  mov r3, #2
  mov r0, r3
  sub sp, fp, #4
  ldmfd sp!, {fp, pc}
  .size foo, .-foo

@ main function
  .align 2
  .global main
  .type main, %function
main:
  stmfd sp!, {fp, lr}
  add fp, sp, #4

  @ load the arguments
  mov r3, #5
  str r3, [sp, #0]
  mov r0, #1
  mov r1, #2
  mov r2, #3
  mov r3, #4

  @ call the function
  bl foo

  @ return
  mov r0, #0
  sub sp, fp, #4
  ldmfd sp!, {fp, pc}

  .section .note.GNU-stack,"",%progbits
