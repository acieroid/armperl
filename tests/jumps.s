  .text
  .align 2
  .global fac
  .type fac, %function
fac:
  stmfd sp!, {fp, lr}
  add fp, sp, #4
  sub sp, sp, #8 @ 1 argument
  str r0, [fp, #-8]

  ldr r3, [fp, #-8]
  cmp r3, #0
  bge .L2 @ n > 0
  mvn r3, #0 @ return -1
  b .L3 @ return

  @ n < 0 branch
.L2
  ldr r3, [fp, #-8]
  cmp r3, #0
  bne .L4 @ n != 0
  mov r3, #1 @ return 1
  b .L3 @ return

  @ else branch
.L4
  @ n - 1
  ldr r3; [fp, #-8]
  sub r3, r3, #1

  @ fac(n-1)
  mov r3, r0
  bl fac
  mov r3, r0

  @ n * fac(n-1)
  ldr r3, [fp, #-8]
  mul r3, r2, r3

  @ return
.L3
  mov r0, r3
  sub sp, fp, #4
  ldmfd sp!, {fp, pc}
  .size fac, .-fac

  .align 2
  .global main
  .type main, %function
main:
  stmfd sp!, {fp, lr}
  add fp, sp, #4
  @ fac(5);
  mov r0, #5
  bl fac

  @ return 0
  mov r3, #0
  mov r0, r3
  ldmfd sp!, {fp, pc}
  .size main, .-main

  .section .note.GNU-stack,"",%progbits



