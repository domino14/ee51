name MOTORS

;MOTORS.asm
;
;contains the functions to rotate and steer the wheels
;
;StepSteering, RotateWheels
$include (main.inc)
$include (motors.inc)

DATA    SEGMENT PUBLIC  'DATA' 
	CurrentRotate	DB	?	;the current element of the rotate table
	CurrentSteer	DB	?	;the current element of the steer table

	RotateCounter	DW	?
	SteerCounter	DW	?

	RotateLookUp	DB	?	;rotate table lookup value 
	SteerLookUp		DB	?	;steer table lookup value
DATA    ENDS 
 
STACK           SEGMENT STACK  'STACK' 
 
 DB      80 DUP ('Stack ')       ;240 words 
 TopOfStack      LABEL   WORD 
 
STACK           ENDS 

CODE SEGMENT PUBLIC 'CODE' 
 ASSUME CS:CODE, DS:DATA 


;InitMotors:
;Description:  This function initializes the motor values to 0 and installs
;			event handler
;Input:        None
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.
InitMotors	PROC	NEAR
			PUBLIC	InitMotors

MOV	RotateCounter, 0
MOV	SteerCounter, 0
MOV	RotateLookUp, 0
MOV	SteerLookUp, 0
;now install event handler
PUSH    DS                    ;save DS for now
XOR     AX, AX                  ;clear DS (interrupt vectors are in segment 0) 
MOV     DS, AX 
				;store the vector 
MOV     DS: WORD PTR (4 * Tmr0Vec), OFFSET(MotorEventHandler) 
MOV     DS: WORD PTR (4 * Tmr0Vec + 2), SEG(MotorEventHandler) 

POP     DS                      ;all done, restore DS 


RET
InitMotors	ENDP

;StepSteering(a):
;Description:  This function steps the motors to an angle a
;Input:        The register AX
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.

StepSteering	PROC	NEAR
			PUBLIC	StepSteering
PUSH	AX
PUSH	BX
PUSH	DX
	MOV	DX, 0
	MOV	BX, STEER_CONST
	MUL	BX				;Number of times we will step thru motor is in AX
	MOV	SteerCounter, AX
POP	DX	
POP	BX
POP	AX

RET
StepSteering	ENDP

;RotateWheels(r):
;Description:  This function rotates the wheel r times.
;Input:        The register AX
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None.
RotateWheels	PROC	NEAR
			PUBLIC	RotateWheels
PUSH	AX
PUSH	BX
PUSH	DX
	MOV	DX, 0
	MOV	BX, ROTATE_CONST
	MUL	BX				;Number of times we will step thru motor is in AX
	MOV	RotateCounter, AX
POP		DX
POP		BX
POP		AX


RET
RotateWheels	ENDP

;MotorEventHandler:
;Description:  This function handles the stepping through the steer and rotate
;			tables
;Input:        None
;Output:       None.
;User Interface: None
;Error Handling: None
;Algorithm:    None.
;Data Structures: None
;Known Bugs:   None
;Limitations:  None

MotorEventHandler	PROC	NEAR
PUSH	AX
PUSH	BX
PUSH	DX		

MOV	AX, RotateCounter
MOV	DL, RotateLookUp
CMP	AX, 0				;if it has finished cycling through table
JE	ThenSteer			;handle steering 
DEC	AX
INC	DL
CMP	DL, 4				;if the rotate look up value is  4
JE	RotateWrapAround		;wrap around to 0
JMP	RLookUp
RotateWrapAround:
MOV	DL, 0

RLookUp:
MOV	BX, OFFSET(RotateTable)
MOV	AL, DL
XLAT	CS:RotateTable			;now write this value to the motor
MOV	DX, ROTATE_ADD
OUT	DX, AL
JMP	ThenSteer

ThenSteer:
MOV	AX, SteerCounter
MOV	DL, SteerLookUp
CMP	AX, 0				;if it has finished cycling through table
JE	ExitHandler			;exit
DEC	AX
INC	DL
CMP	DL, 8				;if the rotate look up value is 8
JE	SteerWrapAround		;wrap around to 0
JMP	SLookUp
SteerWrapAround:
MOV	DL, 0

SLookUp:
MOV	BX, OFFSET(StepperTable)
MOV	AL, DL
XLAT	CS:StepperTable			;now write this value to the motor
MOV	DX, STEER_ADD
OUT	DX, AL

ExitHandler:

MOV   DX, INTCtrlrEOI         ;send a non-sepecific EOI to the
MOV   AX, NonSpecEOI          ;   interrupt controller to clear out
OUT   DX, AL                  ;   the interrupt that got us here

POP	DX
POP	BX
POP	AX
IRET			;event handlers always return with IRET
MotorEventHandler	ENDP

;StepperTable
;Description: Contains values to output to the steer register (for stepsteering)

StepperTable	LABEL	BYTE
	DB	1010B
	DB	1000B
	DB	1001B
	DB	0001B
	DB	0101B
	DB	0100B
	DB	0110B
	DB	0010B

;RotateTable
;Description: Contains values to output to the rotate register (for rotatewheels)

RotateTable		LABEL	BYTE
	DB	1010B
	DB	1001B
	DB	0101B
	DB	0110B

CODE ENDS
END
