NAME    main
 
;main.asm 
;Description: This is the 'main' function that calls all the other functions 
; including the test functions 
$INCLUDE(main.inc)                      ;include file 
                                        ;with all definitions 
 
extrn InitCS:NEAR 
extrn ClrIRQVectors:NEAR 
extrn InitTimer:NEAR 
extrn InstallHandler:NEAR 
extrn DisplayInit:NEAR 
extrn	KeypadInit:NEAR
extrn SerialInit:NEAR
extrn UserInit:NEAR
extrn GetKey:NEAR
extrn ProcessKey:NEAR
extrn InitMotors:NEAR
DATA    SEGMENT PUBLIC  'DATA' 
 
DATA    ENDS 
 
STACK           SEGMENT STACK  'STACK' 
 
 DB      80 DUP ('Stack ')       ;240 words 
 TopOfStack      LABEL   WORD 
 
STACK           ENDS 
 
 
 
 
CODE SEGMENT PUBLIC 'CODE' 
 
        ASSUME CS:CODE, DS:DATA, SS:STACK 
 
 
start: 
 
        MOV     AX, STACK 
        MOV     SS, AX 
        MOV     SP, OFFSET(TopOfStack)  ;initialize stack segment 
        MOV     AX, DATA 
        MOV     DS, AX                  ;initialize data segment 
 
        CALL    InitCS 
        CALL    ClrIRQVectors           ;clear interrupt request vectors 
        CALL    InstallHandler          ;install event handler 
        CALL    InitTimer               ;initialize internal timer 
        CALL    DisplayInit             ;initialize display
        CALL    KeypadInit              ;initialize keypad
        CALL    SerialInit              ;initialize serial port
        CALL    UserInit                ;initialize user interface
		CALL	InitMotors				;initialize motors
	    STI                             ;enable interrupts 

 	 InfiniteLoop:

        CALL  GetKey          ;get key
        CALL  ProcessKey              ;process the key we just got
     JMP     InfiniteLoop 

CODE ENDS 
 
END start 
