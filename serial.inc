
;serial.inc
;contains definitions my program uses, especially dealing with interrupts
;for the 188

ACE_BASE_ADD 	EQU	180H			;0180H

INT2Vec	EQU	14		;this is the location of INT2 in the 
					;look up table

; Interrupt Controller Definitions 
 
; Addresses 
INT2CtrlrCtrl    EQU     0FF3CH          ;address of interrupt 2 controller
INT2CtrlrEOI      EQU    0FF22H          ;address of interrupt 2 controller EOI register 
 
; Register Values 
INT2CtrlrCVal    EQU     00012H          ;set priority for INT2 to 2 and enable 
NonSpecEOI      EQU     08000H          ;Non-specific EOI command 
SerialEOI	EQU	14


;error codes for my program

RxQFULL	EQU	00000001B		;receive queue is full, lost character