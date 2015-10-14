 NAME serial
 
;Description: Serial functions for the RoboCar 
;             SerialInRdy(), SerialGetChar(), SerialOutRdy(), SerialPutChar(c)
;              SerialStatus(), SetSerialBaudRate(BX), SerialInit(), 
;              SerialEventHandler(), SetSerialParity(BX)


;Author: Cesar Del Solar 
;Date Finished:     March 1, 2001  
;
;Revisions:    March 12, 2001
;              Added SetSerialParity(BX) function                
; 


$include (serial.inc)
$include (serdef.inc)


extrn     QueueInit:NEAR
extrn TxEnqueue:NEAR
extrn TxDequeue:NEAR
extrn TxQueueFull:NEAR
extrn TxQueueEmpty:NEAR
extrn RxEnqueue:NEAR
extrn RxDequeue:NEAR
extrn RxQueueFull:NEAR
extrn RxQueueEmpty:NEAR
;extrn DisplayHex:NEAR

DATA    SEGMENT PUBLIC  'DATA' 
	ErrorCode DB   ?
	SavedLCR  DB   ?
DATA    ENDS 
 
STACK           SEGMENT STACK  'STACK' 
 
 DB      80 DUP ('Stack ')       ;40 words 
 TopOfStack      LABEL   WORD 
 
STACK           ENDS 

CODE SEGMENT PUBLIC 'CODE' 
 ASSUME CS:CODE, DS:DATA 

;SerialInit:
;Description:  This function initializes the serial chip, the queues, and 
; installs the event handler.
;Input:        None.
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.

SerialInit PROC     NEAR
		PUBLIC    SerialInit
				;to wither in denial
;Install Serial Event Handler
;

 PUSH    DS                    ;save DS for now
	XOR     AX, AX                  ;clear DS (interrupt vectors are in segment 0) 
	MOV     DS, AX 
					;store the vector 
	MOV     DS: WORD PTR (4 * INT2Vec), OFFSET(SerialEventHandler) 
	MOV     DS: WORD PTR (4 * INT2Vec + 2), SEG(SerialEventHandler) 
 
	POP     DS                      ;all done, restore DS 

;Setup 188 to Interrupt on INT2
	MOV     DX, INT2CtrlrCtrl      ;setup the interrupt control register 
	MOV     AX, INT2CtrlrCVal 
	OUT     DX, AL 
 
	MOV     DX, INT2CtrlrEOI ;send a non-specific EOI (to clear out controller) 
	MOV     AX, NonSpecEOI 
	OUT     DX, AL 
 
;Initialize Queues

	CALL QueueInit

;Initialize 16C450 chip
;Setup 16C450 to interrupt

					

;write to LCR
	MOV  AL, LCR_EIGHT_BIT   ;word length 8 bits, 1 stop bits
	
	MOV  BX, 0
	MOV  BL, 00000000B       ;parity none
	CALL SetSerialParity
	
	MOV  SavedLCR, AL                  ;save LCR 
	MOV  DX, LCR_ADD              
	OUT  DX, AL

	MOV  BX, DLL_4800             ;prepare to send as an argument to SetSerialBaudRate
	CALL SetSerialBaudRate             ;Set Baud Rate to BX
;
;write to MCR
;
	MOV  AL, MCR_DATA_TERM_RDY + MCR_REQ_TO_SEND
	MOV  DX, MCR_ADD
	OUT  DX, AL
;
;write to IER

	MOV  AL, IER_ALL_INT_ON  ;turn on all interrupts
	MOV  DX, IER_ADD                        ;move the address of the interrupt enable
													;register into DX
	OUT  DX, AL                                  ;output the interrupt enable 'flags' into DX
;now 16C450 should interrupt  


RET        

SerialInit     ENDP 

;SetSerialBaudRate(b):
;Description:  This function sets the baud rate to a passed value.
;Input:        The register BX
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.

SetSerialBaudRate PROC   NEAR
			   PUBLIC SetSerialBaudRate
	PUSH AX
	PUSH DX
	
	MOV  DX, LCR_ADD
	IN   AL, DX
	MOV  SavedLCR, AL
	
	MOV  DX, LCR_ADD
	MOV  AL, LCR_SET_DLAB         ;turn on divisor latch
	OUT  DX, AL              ;turn it on
	
	MOV  AL, BL              ;move into AL the baud rate divisor
	MOV  DX, DLL_ADD
	OUT DX, AL                    ;send in the divisor value from the baud rate generator
	
	MOV DX, DLM_ADD
	MOV  AL, BH              ;move into AL the high byte 
	OUT DX, AL                    

	MOV DX, LCR_ADD
	MOV  AL, SavedLCR
	OUT  DX, AL              ;set LCR back to original value
						;don't want to mess with original value 
	POP DX
	POP  AX
RET
SetSerialBaudRate ENDP


;SetSerialParity(b):
;Description:  This function sets the parity to a passed value.
;Input:        The register BX
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.

SetSerialParity PROC   NEAR
			 PUBLIC   SetSerialParity
	PUSH AX
	PUSH DX
	
	MOV  DX, LCR_ADD
	IN   AL, DX
	AND  AL, 11000111B  ;clear parity bits 
	
	OR   AL, BL         ;now the parity in BL is added to this
					;value to make a new parity
	
	OUT  DX, AL              ;change parity to new value
	
	POP DX
	POP  AX
RET
SetSerialParity ENDP

;SerialEventHandler:
;Description:  This function is called when there is an interrupt on 
;interrupt 2, generated  by the 16C450.
;    It handles those interrupts appropriately.
;Input:        None.
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.

SerialEventHandler  PROC NEAR
;called when there is an interrupt
InHere:
;what type of interrupt is there?
	
	PUSHF
	PUSH AX
	PUSH DX
	MOV       DX, IIR_ADD              ;read in Identification register
	IN        AL, DX              ;into AL
seeIIR:
	CMP       AL, IIR_R_LINE_STATUS    ;receiver line status
	JE        RecLineStatus
	CMP       AL, IIR_R_DATA_READY     ;data received
	JE        DataReady
	CMP       AL, IIR_T_REG_EMPTY ;transmitter holding register empty
	JE        ThrEmpty
;should not get here
	JMP       ExitCMP
RecLineStatus:
	;there has been an error

	JMP       ExitCMP
;;;;;
DataReady:
		
	MOV       DX, REC_BUF_REG_ADD ;prepare to read from receiver buffer
	IN        AL, DX

	CALL RxQueueFull
	JZ        QueueIsFull
	;queue aint full
	CALL RxEnqueue                ;Enqueue(AL) into the receive queue
	JMP       ExitCMP

QueueIsFull:
;generate lost character error
	MOV       ErrorCode, RxQFULL  ;the receiver queue is full
	JMP  ExitCMP

;;;;

ThrEmpty:
	CALL TxQueueEmpty
	JZ        ExitCMP             ;exit if the queue is empty because there's nothing                                  ;to dequeue
;queue is not empty 
	MOV       DX, TRAN_H_REG_ADD  ;prepare to output dequeued value 
	
	CALL TxDequeue                ;dequeue the transmit queue into AL
;    CALL DisplayHex                    ;output AX to display for debug info
	
output:
	OUT       DX, AL              ;output into Transmitter holding register
afteroutput:   
 ;JMP          ExitCMP
;;;;;;    
ExitCMP:
;send a specific EOI

	MOV   DX, INT2CtrlrEOI             ;send a specific EOI (to clear out controller) 
	MOV   AX, SerialEOI 
	OUT   DX, AL 
 

POP  DX
POP  AX
POPF
IRET                               ;always return with IRET from event handler

SerialEventHandler  ENDP

;SerialInRdy:
;Description:  This function sees if there is a character available from the 
; serial channel 
; and returns with the zero flag reset if there is.
;Input:        None.
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.

SerialInRdy    PROC NEAR
		PUBLIC    SerialInRdy
	
	CALL RxQueueEmpty
					;the zero flag is set if the receiver queue is empty
					;this means that there's no character available
					
RET
SerialInRdy    ENDP

;SerialOutRdy:
;Description:  This function checks to see if the serial channel is ready to receive
; another character. If it is, the zero flag is reset, otherwise it is set.
;Input:        None.
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.

SerialOutRdy   PROC NEAR
			PUBLIC    SerialOutRdy

	CALL TxQueueFull
					;the zero flag is set if the transmit queue is full
					;this means that the serial is not ready to transmit
RET
SerialOutRdy   ENDP

;SerialPutChar( c ):
;Description:  This function outputs the character c in the serial channel
;Input:        A character in AL
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.

SerialPutChar  PROC NEAR
			PUBLIC    SerialPutChar


PUSH BX
PUSH DX
PUSH AX                  ;save AX temporarily

SerialOutNotReady:
	CALL SerialOutRdy
	JZ   SerialOutNotReady
;
;serial ready the zero flag is reset
;enqueue the value onto the transmit queue
SerialReady:
PUSHF
	CLI                                          ;critical code, turn off int
	CALL TxQueueEmpty
	JZ        TickleThre
	
Enqueue:
	CALL TxEnqueue           ;move char into transmit queue
	JMP       ExitProc
	;tickle thre
	TickleThre:
	MOV  DX, IER_ADD              ;move the address of the interrupt enable
											;register into DX
	MOV  AL, IER_NOETBE ;ready to turn off thre interrupts
	OUT  DX, AL                        ;turn off thre interrupts
					;now turn them back on
	MOV  DX, IER_ADD

	MOV  AL, IER_ALL_INT_ON
	OUT  DX, AL                        ;ints are back on
	JMP  Enqueue

ExitProc:
POPF                                         ;restore flags

POP  AX
POP  DX
POP  BX

RET
SerialPutChar  ENDP

;SerialGetChar:
;Description:  This function gets the next character from the serial channel.
;Input:        None.
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.
;set carry flag on error


SerialGetChar  PROC NEAR
			PUBLIC    SerialGetChar
SerialInNotReady:
	CALL SerialInRdy
	JZ        SerialInNotReady
;the serial is ready, the zero flag is reset

PUSHF
CLI
	CALL RxDequeue           ;critical code, then restore flags
POPF

RET
SerialGetChar  ENDP


CODE  ENDS

END
