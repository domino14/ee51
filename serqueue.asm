NAME serqueue
 
;Description: Serial Queue functions for the RoboCar 
;
;		TxDequeue(), TxEnqueue(b), QueueInit(), TxQueueEmpty(), TxQueueFull()
;		RxDequeue(), RxEnqueue(b), QueueInit(), RxQueueEmpty(), RxQueueFull()
;Author: Cesar Del Solar 
;Date Finished: February 25, 2001 
; 
;Revisions: March 01, 2001		Fixed weird bugs
; 
; 
QUEUESIZE	EQU	256
QUEUEMOD	EQU	QUEUESIZE - 1

DATA    SEGMENT PUBLIC  'DATA' 
	TransmitQ	DB	QUEUESIZE	DUP(?)
	ReceiveQ	DB	QUEUESIZE	DUP(?)
	TQHead	DB	?
	TQTail	DB	?
	RQHead	DB	?
	RQTail	DB	?
DATA    ENDS 

CODE SEGMENT PUBLIC 'CODE' 
 ASSUME CS:CODE, DS:DATA 
; QueueInit
; Description: 	Initializes both the transmit and receive queues.
;Input:		None. 
;Output:	None.
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Pointers to head and tail elements of the queues. 
;Known Bugs:	None
;Limitations: None

QueueInit PROC NEAR
		PUBLIC	QueueInit
	PUSH BP
	MOV TQHead, 0                ;HeadPtr and TailPtr
	MOV RQHead, 0
	MOV TQTail, 0                ;point to same element
	MOV RQTail, 0
	MOV BP, QUEUESIZE
	ClearQueues:
	MOV	ReceiveQ[BP], 0
	MOV	TransmitQ[BP], 0
	DEC	BP
	CMP	BP, 0
	JL	ExitInit
	JMP	ClearQueues
	ExitInit:
	POP	BP						;bye
	
RET
QueueInit ENDP

;TxQueueEmpty
;Description:	Returns with zero flag set if the transmit queue is empty and with
;the zero flag reset otherwise.
;Input:		None
;Output:	None.
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Uses the Head and Tail pointers
;Known Bugs:	None
;Limitations: None


TxQueueEmpty PROC NEAR
		PUBLIC	TxQueueEmpty
 PUSH	CX
 PUSH DX
 MOV 	CL, TQTail
 MOV 	DL, TQHead				;moves headptr and tailptr of the
							;queues and set the zero flag 
							;accordingly
 CMP 	CL, DL
                                        ;zero flag set if HeadPtr
                                        ;equals TailPtr meaning Queue empty
 POP	DX
 POP	CX
 RET
TxQueueEmpty ENDP

;TxQueueFull
;Description:	Returns with the zero flag set if the transmit queue is full and with
;the zero flag reset otherwise.
;Input:		A queue in BX 
;Output:	None.
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Uses size of the queue, as well as the head and tail
;pointers.
;Known Bugs:	None
;Limitations: None

TxQueueFull PROC NEAR
		PUBLIC	TxQueueFull
 PUSH	AX
 MOV	  AL, TQTail
 INC	  AL
 AND	AL, QUEUEMOD				;queuesize serves as mod
													;mask

 CMP	  AL, TQHead				;compares result (TAIL + 1 MOD SIZE)
											    ;to HEAD
                          ;If (TAIL + 1) MOD SIZE = HEAD
													;set zero flag
                          ;meaning queue is full
 POP	AX
RET
TxQueueFull ENDP

;TxDequeue
;Description:	Removes an 8-bit value from the head of the transmit queue and returns
; it. If the queue is empty, it waits until the queue has a value to be
; removed.
;Input:		None
;Output:	Returns the value the head pointer is pointing to in AX
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Uses the head and tail pointers, the queue size, and a
;temporary pointer.
;Known Bugs:	None
;Limitations: None

TxDequeue PROC NEAR
		PUBLIC	TxDequeue
 PUSH	CX
 PUSH BX
 PUSH	BP
 
BeginTxWhile:
 CALL   TxQueueEmpty                      ;WHILE QueueEmpty()
                                        ;ENDWHILE
 JZ     BeginTxWhile
 
							
 ExitTxWhile:
 MOV	BX, 0
 MOV    BL, TQHead
 MOV	BP, BX
 MOV	  AL, TransmitQ[BP]		;store for return
 MOV		AH, 0
 INC    BL
 AND	  BL, QUEUEMOD                    ;MOD mask for wraparound
 MOV    TQHead, BL                ;Head = (Head + 1) MOD size                    
   
POP		BP                
POP	  BX
POP 	CX
RET                                     ;returns, value has been stored in AX
TxDequeue ENDP

;TxEnqueue
;Description:	Adds the 8-bit value b to the tail of the transmit queue. If the queue
; is full, it waits until the queue has an open space in which to add the
; value.
;Input:         A BYTE b, stored in register AX 
;Output:	None.
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Uses the head and tail pointers and the queue size.
;Known Bugs:	None
;Limitations: None

TxEnqueue PROC NEAR
		PUBLIC	TxEnqueue
 PUSH	AX
 PUSH BX
 PUSH	BP
 
 BeginTxL:
 CALL TxQueueFull                        ;WHILE QueueFull()
                                        ;ENDWHILE
 JZ     BeginTxL
 
 ExitTxL:
 MOV	BX, 0
 MOV    BL, TQTail
 MOV	BP, BX
 MOV    TransmitQ[BP], AL			;store AL in the element pointed to
							;by the tail ptr
 INC    BL
 AND	  BL, (QUEUEMOD)		;mod mask for wrap-around
 
 MOV    TQTail, BL                    ;Tail = (Tail + 1) MOD size                    
 
 POP BP
 POP BX
 POP AX
  
RET
TxEnqueue	ENDP

;RxQueueEmpty
;Description:	Returns with zero flag set if the receive queue is empty and with
;the zero flag reset otherwise.
;Input:		None
;Output:	None.
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Uses the Head and Tail pointers
;Known Bugs:	None
;Limitations: None


RxQueueEmpty PROC NEAR
		 PUBLIC	RxQueueEmpty
 PUSH	CX
 PUSH DX
 MOV 	CL, RQTail
 MOV 	DL, RQHead				;moves headptr and tailptr of the
							;queues and set the zero flag 
							;accordingly
 CMP 	CL, DL
                                        ;zero flag set if HeadPtr
                                        ;equals TailPtr meaning Queue empty
 POP	DX
 POP	CX
 RET
RxQueueEmpty ENDP

;RxQueueFull
;Description:	Returns with the zero flag set if the receive queue is full and with
;the zero flag reset otherwise.
;Input:		A queue in BX 
;Output:	None.
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Uses size of the queue, as well as the head and tail
;pointers.
;Known Bugs:	None
;Limitations: None

RxQueueFull PROC NEAR
		PUBLIC	RxQueueFull
 PUSH	AX
 MOV	  AL, RQTail
 INC	  AL
 AND	AL, QUEUEMOD				;queuesize serves as mod
							;mask
 
 CMP	  AL, RQHead		;compares result (TAIL + 1 MOD SIZE)
						    ;to HEAD
                                        ;If (TAIL + 1) MOD SIZE = HEAD
							;set zero flag
                                        ;meaning queue is full
 POP	AX
RET
RxQueueFull ENDP

;RxDequeue
;Description:	Removes an 8-bit value from the head of the receive queue and returns
; it. If the queue is empty, it waits until the queue has a value to be
; removed.
;Input:		None
;Output:	Returns the value the head pointer is pointing to in AX
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Uses the head and tail pointers, the queue size, and a
;temporary pointer.
;Known Bugs:	None
;Limitations: None

RxDequeue PROC NEAR
		PUBLIC	RxDequeue
 PUSH	CX
 PUSH BX
 PUSH	BP
BeginRxWhile:
 CALL   RxQueueEmpty                      ;WHILE QueueEmpty()
                                        ;ENDWHILE
 JZ     BeginRxWhile

							
 ExitRxWhile:
 MOV	BX, 0
 MOV    BL, RQHead
 MOV	  BP, BX
 
 MOV	  AL, ReceiveQ[BP]		;store for return
 MOV		AH, 0
 INC    BL
 AND	  BL, QUEUEMOD                  ;MOD mask for wraparound
 MOV    RQHead, BL                ;Head = (Head + 1) MOD size                    
   
POP		BP                
POP	  BX
POP 	CX
RET                                     ;returns, value has been stored in AL
RxDequeue ENDP

;RxEnqueue
;Description:	Adds the 8-bit value b to the tail of the receive queue. If the queue
; is full, it waits until the queue has an open space in which to add the
; value.
;Input:         A BYTE b, stored in register AX 
;Output:	None.
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: Uses the head and tail pointers and the queue size.
;Known Bugs:	None
;Limitations: None

RxEnqueue PROC NEAR
		PUBLIC	RxEnqueue
 PUSH	AX
 PUSH BX
 PUSH	BP
 
 BeginRxL:
 CALL RxQueueFull                        ;WHILE QueueFull()
                                        ;ENDWHILE
 JZ    BeginRxL
 

 ExitRxL:
 MOV	BX, 0
 MOV  BL, RQTail
 MOV	BP, BX
 MOV  ReceiveQ[BP], AL			;store AX in the element pointed to
							;by the tail ptr
 INC    BL
 AND	  BL, QUEUESIZE	- 1		;mod mask for wrap-around
 
 MOV    RQTail, BL                    ;Tail = (Tail + 1) MOD size                    

 POP	BP
 POP BX
 POP AX
  
RET

RxEnqueue ENDP


CODE ENDS

END
