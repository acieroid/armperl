@ constants
  .section .rodata
  .align 2
.Lmessage:
  .ascii "Hello, world!\n\000"

@ code
  .text
  .align 2

@ builtin print
  .global print
  .type print, %function
print:
  @ setup
  stmfd sp!, {fp, lr}
  add fp, sp, #4
  sub sp, sp, #8

  @ args
  str r0, [fp, #-8]
  ldr r0, [fp, #-8]

  @ body
  bl puts(PLT)

  @ restore and return
  sub sp, fp, #4
  ldmfd sp!, {fp, pc}

  @ declare size
  .size println, .-println

@ script
  .global main
  .type main, %function
main:
  @ setup
  stmfd sp!, {fp, lr}

  @ get constant
  ldr r0, .Laddr_message
  bl print

  @ signal no error
  mov r3, #0
  mov r0, r3

  @ return
  ldmfd sp!, {fp, lr}
  bx lr

@ addresses
.Laddr_message:
  .word .Lmessage

  @ stack note
  .section .note.GNU-stack,"",%progbits
