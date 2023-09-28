; Program template (Template.asm)

COMMENT&
This is template that can be used as a starting point for any new assembler programs
Multi - line comments are created using the COMMENT directive.The character after the directive
is repeated to identify when the comment ends.
&

; Either include the Irvine libary
;  INCLUDE Irvine32.inc

; or include the .386 and .model directives
.386
.model flat, stdcall

.stack 4096; declare your stack size

; Declare the function prototype for the ExitProcess function
ExitProcess PROTO dwExitCode : DWORD

.data
; declare variables here

.code
main PROC
; write your code here

    NOP
    INVOKE ExitProcess,1
main ENDP
END main