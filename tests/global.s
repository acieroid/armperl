  @ definition of the global variable
  .global global
  .data
  .align  2
  .type   global, %object
  .size   global, 4
global:
  .word   42

  @ function definition
  .text

  @ definition of the 'fun' function
  .align  2
  .global fun
  .type   fun, %function
fun:
  str     fp, [sp, #-4]!
  add     fp, sp, #0
  sub     sp, sp, #20 
        
  @ two arguments, stored in r0 and r1
  str     r0, [fp, #-16]
  str     r1, [fp, #-20]

  @ load global into r3
  ldr     r3, .L7 
  ldr     r3, [r3, #0] 

  @ store global into arg1
  str     r3, [fp, #-8]

  @ load global into r3
  ldr     r3, .L7 

  @ load 0 into r2
  mov     r2, #0

  @ store 0 into global (in r3)
  str     r2, [r3, #0] 

  @ return arg1
  ldr     r3, [fp, #-8]
  mov     r0, r3

  add     sp, fp, #0
  ldmfd   sp!, {fp}
  bx      lr
  .size   fun, .-fun

  @ definition of the main function
  .align  2
  .global main
  .type   main, %function
main:
  stmfd   sp!, {fp, lr}
  add     fp, sp, #4
  mov     r0, #1
  mov     r1, #2
  bl      fun
  ldr     r3, .L7 
  ldr     r3, [r3, #0] 
  mov     r0, r3
  ldmfd   sp!, {fp, pc}
  .size   main, .-main

.L7:
  .word   global
  .section        .note.GNU-stack,"",%progbits

