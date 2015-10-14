NAME  TIMER
 
;Description: contains the timer event handler and interrupt event handler 
;functions 
;Cesar Del Solar 
 
$INCLUDE(main.inc) 
 
extrn DisplayMux:NEAR 
extrn KeypadScan:NEAR
 
DATA    SEGMENT PUBLIC  'DATA' 
 
DATA    ENDS 
 
STACK           SEGMENT STACK  'STACK' 
 
                DB      80 DUP ('Stack ')       ;240 words 
 
TopOfStack      LABEL   WORD 
 
STACK           ENDS 
 
 
CODE SEGMENT PUBLIC 'CODE' 
 
        ASSUME  CS:CODE, DS:DATA, SS:STACK 
 
 
 
; TimerEventHandler 
; Description: is the event handler for the timer interrupt. 
; 
                                                   
TimerEventHandler       PROC    NEAR 
                        PUBLIC  TimerEventHandler 
        PUSH    AX                      ;save the registers 
        PUSH    BX                      ;Event Handlers should NEVER change 
        PUSH    DX                      ;   any register values 
 
 
Update:                         	    ;update the display and keypad
        CALL    DisplayMux              ;displaymux does this 
 	  CALL    KeypadScan		    ;scan for keys pressed
EndTimerEventHandler:                   ;done taking care of the timer 
 
        MOV     DX, INTCtrlrEOI         ;send the EOI to the interrupt controller 
        MOV     AX, Timer0EOI 
        OUT     DX, AL 
 
        POP     DX                      ;restore the registers 
        POP     BX 
        POP     AX 
 
 
        IRET                            ;and return (Event Handlers end with IRET not RET) 
 
 
TimerEventHandler       ENDP 
 
; InitCS 
; 
; Description:      Initialize the Peripheral Chip Selects on the 80188. 
 
InitCS  PROC    NEAR 
        PUBLIC  InitCS 
 
        MOV     DX, PACSreg     ;setup to write to PACS register 
        MOV     AX, PACSval 
        OUT     DX, AL          ;write PACSval to PACS (base at 0, 3 wait states) 
 
        MOV     DX, MPCSreg     ;setup to write to MPCS register 
        MOV     AX, MPCSval 
        OUT     DX, AL          ;write MPCSval to MPCS (I/O space, 3 wait states) 
 
        RET                     ;done so return 
 
InitCS   ENDP 
 
 
 
 
; InitTimer 
; 
; Description:      Initialize the 80188 Timers.  The timers are initialized 
;                   to generate interrupts every MS_PER_SEG milliseconds.  The 
;                   interrupt controller is also initialized to allow the 
;                   timer interrupts.  Timer #2 is used to prescale the 
;                   internal clock from 2.304 MHz to 1 KHz.  Timer #0 then 
;                   counts MS_PER_SEG timer #2 intervals to generate the 
;                   interrupts. 
; 
 
InitTimer       PROC    NEAR 
                PUBLIC  InitTimer 
         
                                ;initialize Timer #0 for MS_PER_SEG ms interrupts 
        MOV     DX, Tmr0Count   ;initialize the count register to 0 
        XOR     AX, AX 
        OUT     DX, AL 
 
        MOV     DX, Tmr0MaxCntA  
        MOV     AX, COUNTS_PER_MS  ;interrupt every millisecond 
        OUT     DX, AL 
 
        MOV     DX, Tmr0Ctrl    ;setup the control register, interrupts on 
        MOV     AX, Tmr0CtrlVal 
        OUT     DX, AL 
 
                                ;initialize interrupt controller for timers 
        MOV     DX, INTCtrlrCtrl;setup the interrupt control register 
        MOV     AX, INTCtrlrCVal 
        OUT     DX, AL 
 
        MOV     DX, INTCtrlrEOI ;send a non-specific EOI (to clear out controller) 
        MOV     AX, NonSpecEOI 
        OUT     DX, AL 
 
 
        RET                     ;done so return 
 
 
InitTimer       ENDP 
 
 
 
 
; InstallHandler 
; 
; Description:      Install the event handler for the timer interrupt. 
; 
 
InstallHandler  PROC    NEAR 
                PUBLIC  InstallHandler 
 
        PUSH    DS              ;save DS for now 
 
        XOR     AX, AX          ;clear DS (interrupt vectors are in segment 0) 
        MOV     DS, AX 
                                ;store the vector 
        MOV     DS: WORD PTR (4 * Tmr0Vec), OFFSET(TimerEventHandler) 
        MOV     DS: WORD PTR (4 * Tmr0Vec + 2), SEG(TimerEventHandler) 
 
        POP     DS              ;all done, restore DS and return 
        RET 
 
 
InstallHandler  ENDP 
 
 
; ClrIRQVectors 
; 
; Description:      This functions installs the IllegalEventHandler for all 
;                   interrupt vectors in the interrupt vector table.  Note 
;                   that all 256 vectors are initialized so the code must be 
;                   located above 400H.  The initialization skips the first 
;                   RESERVED_VECS vectors. 
 
ClrIRQVectors   PROC    NEAR 
                PUBLIC  ClrIRQVectors 
 
        PUSH    DS              ;save DS for now 
 
 
InitClrVectorLoop:              ;setup to store the same handler 256 times 
 
        XOR     AX, AX          ;clear DS (interrupt vectors are in segment 0) 
        MOV     DS, AX 
        MOV     SI, 4 * RESERVED_VECS   ;initialize SI to skip RESERVED_VECS (4 bytes each) 
 
        MOV     CX, 256 - RESERVED_VECS ;up to 256 vectors to initialize 
 
 
ClrVectorLoop:                  ;loop clearing each vector 
                                ;store the vector 
        MOV     DS: WORD PTR [SI], OFFSET(IllegalEventHandler) 
        MOV     DS: WORD PTR [SI + 2], SEG(IllegalEventHandler) 
 
        ADD     SI, 4           ;update pointer to next vector 
 
        LOOP    ClrVectorLoop   ;loop until have cleared all vectors 
        ;JMP    EndClrIRQVectors;and all done 
 
 
EndClrIRQVectors:               ;all done, restore DS and return 
        POP     DS 
 
        RET 
 
 
ClrIRQVectors   ENDP 
 
 
 
 
; IllegalEventHandler 
; 
; Description:      This procedure is the event handler for illegal 
;                   (uninitialized) interrupts.  It does nothing - it just 
;                   returns after sending a non-specific EOI. 
; 
 
IllegalEventHandler     PROC    NEAR 
 
        NOP                             ;do nothing (can set breakpoint here) 
 
        PUSH    AX                      ;save the registers 
        PUSH    DX 
 
        MOV     DX, INTCtrlrEOI         ;send a non-sepecific EOI to the 
        MOV     AX, NonSpecEOI          ;   interrupt controller to clear out 
        OUT     DX, AL                  ;   the interrupt that got us here 
 
        POP     DX                      ;restore the registers 
        POP     AX 
 
        IRET                            ;and return 
 
 
IllegalEventHandler     ENDP 
 
 
 
CODE ENDS 
 
 
 
END    
