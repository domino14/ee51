
NAME userint

;Description: The user interface for the Robo-Car
;
;The following functions are included in this file:
;
; UserInit, ProcessKey, DisplayCurOption, ExecCurOption, SetDist,
; DispErrorMessage, SetSteer, SetBaudRate, SetParity, DownloadPath,
; UpdateStep, SaveStep, DisplayStep, RunPath, ResetPath, ClearPath,
; UserIntSetParity, UserIntSetBR, UserIntReadNumber
;
;Author:  Cesar Del Solar
TEN			EQU	10
PI			EQU	3		;pi = 3
STATE_DIST              EQU     0
STATE_ANG               EQU     1
STATE_DIST_NUM          EQU     2
STATE_ANG_NUM           EQU     3
STATE_IDLE              EQU     4

WHEEL_RADIUS	EQU	10		;radius is 10 units

$include (userint.inc)
$include (serial.inc)
$include (serdef.inc)

extrn     Display:NEAR
extrn     SetSerialBaudRate:NEAR
extrn     SetSerialParity:NEAR
extrn     DisplayInit:NEAR
extrn     GetKey:NEAR
extrn     DisplayNum:NEAR
extrn     RotateWheels:NEAR
extrn     StepSteering:NEAR
extrn		IsAKey:NEAR
extrn 	SerialGetChar:NEAR

DATA    SEGMENT PUBLIC  'DATA' 
        CurrMenu        DB   ?
        CurrOption      DB   ?
        KeysPressed     DB   ?
    

        KeyPressed      DB      ?       ;key pressed in enter number function
        LoopCounter     DB      ?       ;loop counter in enter number function
        CurrNumber      DW      ?       ;the current number in enter number func

	;path arrays
        PathDist        DW      PATH_SIZE       DUP (?)
        PathAng         DW      PATH_SIZE       DUP (?)
        CurrStep        DW      ?

        CurrState       DB      ?       ;current state for downloading path

		IsNeg			DB		?		;is the current number negative or not?
DATA    ENDS


STACK           SEGMENT STACK  'STACK' 
 
 DB      80 DUP ('Stack ')       ;40 words 
 TopOfStack      LABEL   WORD 
 
STACK           ENDS 

CODE SEGMENT PUBLIC 'CODE' 
 ASSUME CS:CODE, DS:DATA 

;UserInit:
;Description: 	Initializes all user interface variables to default values
;			and the path to 0
;Input:		None.
;Output:		None. 
;User Interface: None
;Error Handling: None
;Algorithm:	None.
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

UserInit  PROC NEAR
			 PUBLIC  UserInit
PUSH    BP

	MOV  CurrMenu, MAIN_MENU
	MOV  CurrOption, 0       ;the current option is the first one
	MOV     KeysPressed, 0
	MOV	CurrStep, 0
        MOV     CurrState, STATE_IDLE
        ;clear path
        MOV     BP, 0
InitClearPath:
        MOV     PathAng[BP], 0
        MOV     PathDist[BP], 0
        CMP     BP, PATH_SIZE       
        JE      EndClearPath
        INC     BP
        JMP     InitClearPath
EndClearPath:
POP     BP
RET
UserInit  ENDP


;ProcessKey:
;Description: 	Processes the following keys:
;			KEY_ENTER - executes the option on display
;			KEY_PLUS_MINUS	- updates the option on display
;			KEY_CLEAR		- exits back to main menu
;			KEY_SERIAL		-changes menu to serial menu
;			KEY_PATH		- changes menu to path menu
;			KEY_STEP		-changes menu to step menu
;Input:		Nothing
;Output:		None
;User Interface: GetKey is used to get a key from the keypad
;Error Handling: Ignores all other keys
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

ProcessKey     PROC NEAR
			 PUBLIC  ProcessKey

PUSH BX
PUSH AX
PUSH BP
PUSH SI
	;AL has the keycode
	;look up key code
	   INC     KeysPressed
	CMP  AL, KEY_PLUS_MINUS
	JE   PlusMinusPress
	CMP  AL, KEY_ENTER
	JE   EnterPress
	CMP  AL, KEY_CLEAR
	JE   ClearPress
	
	CMP  AL, KEY_PATH
	JE   PathPress
	CMP  AL, KEY_SER
	JE   SerialPress
	CMP  AL, KEY_STEP
	JE   StepPress

	JMP  ExitProcess         ;do nothing

PlusMinusPress:
	INC  CurrOption
	MOV  BX, OFFSET(OptionTables) ;prepare to look up 
                                      ;# of options
	MOV  AL, CurrMenu             ;for the current menu
        XLAT CS:OptionTables          ;put into AL the number of options
	CMP  CurrOption, AL           ;see if the current option equals
                                      ;the value just returned
	JE   SetBackToZero
	JMP  DisplayOption
SetBackToZero:
	MOV  CurrOption, 0            ;wrap around to 0

DisplayOption:
	CALL DisplayCurOption
	JMP  ExitProcess    

EnterPress:
	   CALL    ExecCurOption
	JMP  ExitProcess
	
ClearPress:
	MOV  CurrMenu, MAIN_MENU
	MOV  CurrOption, 0
    
	;display introductory main menu message
	PUSH CS
	POP  ES             ;CS = ES
	MOV  SI, OFFSET(INTRO_Msg)
	CALL Display        ;Display (ES:SI)
	

	JMP  ExitProcess
PathPress:
	MOV  CurrMenu, PATH_MENU
	MOV  CurrOption, 0
	CALL DisplayCurOption
	JMP  ExitProcess
SerialPress:
	MOV  CurrMenu, SER_MENU
	MOV  CurrOption, 0
	CALL DisplayCurOption
	JMP  ExitProcess
StepPress:
	MOV  CurrMenu, STEP_MENU
	MOV  CurrOption, 0
	CALL DisplayCurOption
	JMP  ExitProcess

ExitProcess:
POP  SI
POP  BP
POP  AX
POP  BX

RET
ProcessKey     ENDP

;DisplayCurOption:
;Description: 	Displays the option currently selected
;Input:		The current menu and option
;Output:		Outputs a string  corresponding to the option to the LED . 
;User Interface: None
;Error Handling: None
;Algorithm:	Two table look ups - first it finds the current menu then the 
;			current option
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

DisplayCurOption    PROC NEAR
PUSH AX
PUSH BX
PUSH SI
PUSH BP

	PUSH CS
	POP  ES                  ;CS = ES
	MOV  AX, 0
	MOV  AL, CurrMenu
	SHL  AX, 1          ;multiply Curr_menu by 2 to look up word
	MOV  BX, OFFSET(MenuStrTables)     ;MenuStrTables contains pointers to the
                                           ;tables that point to strings
	ADD  AX, BX
	MOV  BP, AX    
        MOV     AX, CS:[BP]           ;AX now holds offset to correct string table
	CMP  AX, 0                    ;if we're in the main menu don't display
        JE     NothingToDisplay       ;anything
	
	MOV  BX, AX              ;move offset into BX
	MOV  AX, 0
	MOV  AL, CurrOption      ;move current option into AX
        SHL  AX, 1               ;multiply by 2 to look up word instead of byte
	ADD  AX, BX    
	MOV  BP, AX
        MOV  SI, CS:[BP]         ;this should move
                                 ;the address of the string in
                                 ;the table at the current option
                                 ;into SI to display
	CALL Display
	
NothingToDisplay:
POP  BP
POP  SI
POP  BX
POP  AX
RET
DisplayCurOption    ENDP


;ExecCurOption:
;Description: 	Executes the function corresponding to the current option
;Input:		The current menu and option
;Output:		Displays a done message when the function is done 
;User Interface: Depends on the function called
;Error Handling: None
;Algorithm: Looks up menu then option tables
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

ExecCurOption   PROC    NEAR
PUSH    BX
PUSH    AX
PUSH    BP

	MOV  BX, OFFSET(MenuFuncTables)    ;prepare to look up
                                           ;function to call 
	MOV  AX, 0
	MOV  AL, CurrMenu
	SHL  AX, 1                         ;multiply CurrMenu by 2 to
                                           ;look up word

	ADD  AX, BX
	   MOV     BP, AX
	   MOV     AX, CS:[BP]             ;put into AX the offset of the table with the
                                           ;   function to call

	   CMP    AX, 0
	JE   ExitProcess                   ;if in the main menu just exit

	   MOV     BX, 0
	   MOV     BL, CurrOption          ;store current option
	   SHL     BX, 1                   ;multiply by 2 to look up word

	   ADD     AX, BX
	   MOV     BP, AX
	   MOV     AX, CS:[BP]             ;look up function and put in AX

           CALL    AX                      ;call the function


           PUSH    SI

           PUSH CS
	     POP  ES             ;CS = ES
           MOV  SI, OFFSET(DONE_Msg)
           CALL Display        ;Display done message

           POP     SI
           CALL    GetKey                  ;wait until a keypress
           CALL    DisplayCurOption        ;display current option when return
POP     BP
POP     AX
POP     BX
RET
ExecCurOption   ENDP

;SetDist:
;Description: 	Sets the current distance to some value
;Input:		None
;Output:		Distance array is updated with the new value at the most recent step. 
;User Interface:  A distance is input with the number keys (see UserIntReadNumber)
;Error Handling: None
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

SetDist   PROC NEAR
PUSH    BP
PUSH    DX

        CALL    UserIntReadNumber       ;read the number, it's in DX
        MOV     BP, CurrStep
        CMP     BP, PATH_SIZE           ;no more space if this is true!
        JE      NoMoreSpace
        JMP     PlaceDist

NoMoreSpace:
        CALL    DispErrorMessage
        JMP     ExitSetDist
PlaceDist:
        MOV     PathDist[BP], DX        ;This is the current distance in the
                                        ;path
      
ExitSetDist:

POP     DX
POP     BP
RET
SetDist   ENDP


;DispErrorMessage:
;Description: 	This function outputs "Error =(" to the LED's
;Input:		None
;Output:		None 
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

DispErrorMessage PROC   NEAR
      PUSH    SI

      PUSH CS
	POP  ES             ;CS = ES
      MOV  SI, OFFSET(ERROR_Msg)
      CALL Display        ;Display (ES:SI)

      POP     SI
RET
DispErrorMessage        ENDP


;SetSteer:
;Description: 	Sets the current angle to some value
;Input:		None
;Output:		Angle array is updated with the new value at the most recent step. 
;User Interface:  An angle is input with the number keys (see UserIntReadNumber)
;Error Handling: None
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

SetSteer  PROC NEAR
PUSH    BP
PUSH    DX

        CALL    UserIntReadNumber       ;read the number its in DX
        MOV     BP, CurrStep 
        CMP     BP, PATH_SIZE     ;no more space if this is true!
        JE      NoMoreSpaceTwo
        JMP     PlaceAngle

NoMoreSpaceTwo:
        CALL    DispErrorMessage
        JMP     ExitSetSteer
PlaceAngle:
        MOV     PathAng[BP], DX        ;This is the current angle in the
                                        ;path
       
ExitSetSteer:
        
POP     DX
POP     BP

RET
SetSteer  ENDP

SetOrigin PROC NEAR
;???
;I have no idea what this is supposed to do

RET
SetOrigin ENDP

;SetBaudRate:
;Description: 	Changes current menu to the baud rate menu
;Input:		None
;Output:		None
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

SetBaudRate    PROC NEAR
	MOV  CurrMenu, BR_MENU
	MOV  CurrOption, 0
	CALL DisplayCurOption
RET
SetBaudRate    ENDP

;SetParity:
;Description: 	Changes current menu to the parity menu
;Input:		None
;Output:	None
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

SetParity PROC NEAR
	MOV  CurrMenu, PARITY_MENU
	MOV  CurrOption, 0
	CALL DisplayCurOption
RET
SetParity ENDP

;DownloadPath:
;Description: 	Downloads a path from the serial port.
;Input:		Characters from the serial port
;Output:		None 
;User Interface: Pressing KEY_CLEAR halts the download 
;Error Handling: None
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

DownloadPath   PROC NEAR
PUSH	AX
PUSH	DX
PUSH	BX
PUSH	CX

MOV     CurrNumber, 0

BeginDownloading:
CALL    SerialGetChar   ;get character from serial port into AL
CMP     AL, 'D'         ;distance
JE      StartNewDist
CMP     AL, 'A'         ;angle
JE      StartNewAngle
CMP     AL, 'E'         ;exit
JE      ExitDownload

CMP     AL, '-'
JE      NegateNumber

CMP     AL, '0'
JAE     GreaterThanOrEqualto0

JMP     EndDownloadLoop ;if it's neither of these characters it's
                        ; a '+', a 'P', a space, a line break,
                        ; or any invalid character, in which
                        ; case do nothing and read the next char.
GreaterThanOrEqualto0:
CMP     AL, '9'
JBE     ItsANumber      ;it's a number if it's between '0' & '9'

StartNewDist:
MOV     CurrNumber, 0
MOV		IsNeg, 0
CALL    UpdateStep      ;update the step
MOV		CurrState, STATE_DIST
JMP		EndDownloadLoop

StartNewAngle:
MOV		CurrNumber, 0
MOV		IsNeg, 0
CALL	UpdateStep
MOV		CurrState, STATE_ANG
JMP		EndDownloadLoop

NegateNumber:
MOV		IsNeg, 1
JMP		EndDownloadLoop

ItsANumber:
SUB		AL, '0'			;AL is now the actual number
MOV		BX, 0
MOV		BL, AL
MOV		DX, 0
MOV		AX, CurrNumber	;multiply current number by 10
MOV		CX, 10
MUL		CX

ADD		CurrNumber, BX	;and add in the number we just got


EndDownloadLoop:

CALL	IsAKey
JZ		InvokeGetKey
JMP		BeginDownloading

InvokeGetKey:
CALL	GetKey
CMP		AL, KEY_CLEAR
JE		Errorandexit

JMP	BeginDownloading

Errorandexit:
CALL	DispErrorMessage
JMP		ExitDownload

ExitDownload:

POP CX
POP	BX
POP	DX
POP	AX
RET
DownloadPath   ENDP

;UpdateStep:
;Description:   updates the current step based on the current event from
;               DownloadPath
;Input:		None
;Output:		None
;User Interface: None
;Error Handling: None
;Algorithm: None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

UpdateStep      PROC    NEAR
PUSH    BP
PUSH	AX
 MOV    BP, CurrStep
 MOV	AX, CurrNumber
 CMP	IsNeg, 1
 JE		NumberIsNegative
 JMP	KeepGoing	

 NumberIsNegative:
 NEG	AX

 KeepGoing:
 CMP    CurrState, STATE_DIST
 JE     UpdateDistance
 CMP    CurrState, STATE_ANG
 JE     UpdateAngle
 JMP    ExitUpdateStep          ;if it's none of those two just exit
 
 UpdateDistance:
 MOV	PathDist[BP], AX
 JMP	ExitUpdateStep
 UpdateAngle:
 MOV	PathAng[BP], AX
 ;JMP	ExitUpdateStep
ExitUpdateStep:

POP		AX
POP     BP
RET
UpdateStep      ENDP

;SaveStep:
;Description: 	Saves the current step
;Input:		None
;Output:		None
;User Interface: None
;Error Handling: None
;Algorithm:	Increments current step effectively saving the last angle and distance input.
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

SaveStep  PROC NEAR
 INC    CurrStep                ;just increment the current step
                                ;
      

RET
SaveStep  ENDP

;DisplayStep:
;Description: 	Displays the step whose number is input into the keypad
;Input:		Nothing
;Output:		"Angle=", followed by the angle, followed by "Dist=",
;                        followed by the distance corresponding to that step 
;User Interface: Pressing ENTER goes through the outputs
;Error Handling: None. 
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

DisplayStep    PROC NEAR
PUSH    DX
PUSH    BP
PUSH    AX
PUSH    SI

        CALL    UserIntReadNumber       ;read the step number into DX
        MOV     BP, DX
       

        PUSH CS
	POP  ES             ;CS = ES
        MOV  SI, OFFSET(ANGLE_Msg)
        CALL Display        ;Display Angle =

        CALL    GetKey
        CMP     AL, KEY_ENTER    ;if the key is not the enter key
        JNE     ExitDisplayStep  ;just exit procedure

        MOV     AX, PathAng[BP]
        CALL    DisplayNum

        CALL    GetKey
        CMP     AL, KEY_ENTER    ;if the key is not the enter key
        JNE     ExitDisplayStep  ;just exit procedure

        PUSH CS
	POP  ES             ;CS = ES
        MOV  SI, OFFSET(DIST_Msg)
        CALL Display        ;Display Dist =

        CALL    GetKey
        CMP     AL, KEY_ENTER    ;if the key is not the enter key
        JNE     ExitDisplayStep  ;just exit procedure

        MOV     AX, PathDist[BP] ;display distance
        CALL    DisplayNum

        CALL    GetKey          ;exit when any key is pressed
        


ExitDisplayStep:

POP     SI
POP     AX
POP     BP
POP     DX
RET
DisplayStep    ENDP

;RunPath:
;Description: 	Runs the current path, step by step
;Input:		Nothing
;Output:		The current step number as it's executed. 
;User Interface: None
;Error Handling: None
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

RunPath   PROC NEAR
PUSH	BP
PUSH	AX
PUSH	DX
PUSH	BX
PUSH	CX
	MOV	BP, 0
	StartRunningPath:
		CMP	BP, CurrStep	;if BP = the current path size
						;then exit because there's no step
		JE	ExitRunningPath
		MOV	AX, PathAng[BP]
		CALL	StepSteering
		
		MOV	BX, PathDist[BP]
		MOV	AX, 2			;set AX|DX to 2 for multiplication 
		MOV	DX, 0
		MOV	CX, PI
		IMUL	CX
		MOV	CX, WHEEL_RADIUS
		IMUL	CX
		MOV	CX, AX		;move 2 * pi * r into CX for division
		MOV	AX, BX		
		IDIV	CX			;AX = rotations = distance / (2*pi*r)	
						
				
		CALL	RotateWheels
		MOV	AX, BP
		CALL	DisplayNum
		INC	BP			;update path
		JMP	StartRunningPath	
	ExitRunningPath:	
POP	CX
POP	BX
POP	DX
POP	AX
POP	BP

RET
RunPath   ENDP

;ResetPath:
;Description: 	Resets the current path
;Input:		Nothing
;Output:		Nothing. 
;User Interface: None
;Error Handling: None.
;Algorithm:	Puts 0 into the current step
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

ResetPath PROC NEAR
   MOV  CurrStep, 0             ;make current step 0
RET
ResetPath ENDP

;ClearPath:
;Description: 	Clears the current path
;Input:		Nothing
;Output:		None. 
;User Interface: None
;Error Handling: none
;Algorithm:	None
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

ClearPath PROC NEAR
  MOV   CurrStep, 0             ;make current step 0 ??
RET
ClearPath ENDP

;UserIntSetParity:
;Description: 	Sets the parity to the value selected in the menu
;Input:		None
;Output:	None
;User Interface: None
;Error Handling: None
;Algorithm:	Calls SetSerialParity in serial.asm which writes to the 16C450
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

UserIntSetParity    PROC NEAR
	;check for option
PUSH AX
PUSH BX
PUSH SI
	MOV  BX, OFFSET(ParityArgs)
	MOV  AX, 0
	MOV  AL, CurrOption ;prepare to XLAT
	XLAT CS:ParityArgs  ;now AL contains the parity bits
					;now we have to actually set the parity
					;to AL
	MOV  BL, AL         ;move into BL to pass to SetSerialParity
					;in serial.asm

	CALL SetSerialParity     ;calls SetSerialParity(BL)

	MOV  CurrMenu, SER_MENU
	MOV  CurrOption, 0
POP  SI
POP  BX
POP  AX


RET
UserIntSetParity    ENDP

;UserIntSetBR:
;Description: Sets the baud rate to the value selected from the menu
;Input:		Nothing
;Output:		None 
;User Interface: none
;Error Handling: None
;Algorithm:	Calls SetSerialBaudRate in serial.asm which writes to the 16C450
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

UserIntSetBR PROC   NEAR
  ;   ....
PUSH AX
PUSH BX
PUSH SI
	MOV  BX, OFFSET(BRArgs)
	MOV  AX, 0
	MOV  AL, CurrOption
	SHL  AX, 1          ;multiply by 2 to look up WORD

	ADD  BX, AX
	MOV  BP, BX
	MOV  BX, CS:[BP]    ;put into BX the value of the divisor
				    ; to put into the 16C450 latch
	CALL SetSerialBaudRate     ;calls SetSerialBaudRate(BX)

	MOV  CurrMenu, SER_MENU
	MOV  CurrOption, 0
POP  SI
POP  BX
POP  AX


  ;...
RET
UserIntSetBR   ENDP

;UserIntReadNumber:
;Description: 	This function reads in a number from the keypad. It doesn't exit until
;			the enter key has been pressed.
;			It reads in a positive or negative number (toggle with +/- key)
;			Pressing clear if there is a number clears the display, pressing it if
;			there is no number returns to menu.
;Input:		Nothing
;Output:		A byte positive or negative number. 
;User Interface: The number keys are pressed to enter the current digit
;Error Handling: Stops reading numbers after 3 digits.
;Algorithm:	Multiplies temporary number by 10 and adds it to next number.
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

UserIntReadNumber	PROC	NEAR
 ;       MOV     CurrDigit, 0    ;Start from the beginning 
PUSH	AX
PUSH	BX
PUSH	CX
PUSH    SI

MOV	CL, 0		;Exit loop condition false
MOV	CurrNumber, 0

	PUSH CS
	POP  ES             ;CS = ES

        MOV  SI, OFFSET(NO_Msg)
	CALL Display        ;Display (ES:SI)

ReadingLoop:
	
	CALL	GetKey
	CMP	AL, KEY_CLEAR
	JE	ClearNumber
	CMP	AL, KEY_ENTER
	JE	OutputNumber
	CMP	AL, KEY_PLUS_MINUS
	JE	ToggleSign
	
	MOV	BX, OFFSET(NumberKeyTables)
	MOV	KeyPressed, AL
	MOV	LoopCounter, 0		;initialize loop counter
LookUpTableLoop:
	MOV	AL, LoopCounter
	XLAT	CS:NumberKeyTables	;AL now holds the keycode for that digit
	CMP	AL, KeyPressed		;if the key pressed was the same as the one in
						;the table exit the loop
	JE	ExitLoop

	CMP	LoopCounter, 9		;if it can't find the key then the key 
						;pressed is invalid
	JE	DoneProcessingKeys

	INC	LoopCounter
	JMP	LookUpTableLoop
ExitLoop:
	;LoopCounter holds the current digit
      CMP	CurrNumber, 100
        JG      DoneProcessingKeys      ;if the number is greater than 100
						;then ignore it and exit
        MOV     DX, 0
        MOV     AX, CurrNumber
        MOV     BX, 10
        IMUL    BX                              ;CurrNumber = CurrNumber * 10
        ADD     AL, LoopCounter  
        MOV     CurrNumber, AX          ;CurrNumber = CurrNumber + Digit
	JMP	DoneProcessingKeys	;Done, ready for next iteration
	
ToggleSign:
	NEG	CurrNumber			;negate the number
	JMP	DoneProcessingKeys	;iterate loop again
ClearNumber:
	CMP	CurrNumber, 0		;If the number equals 0 exit loop
	JE	NumEqualZero
        MOV     AX, CurrNumber
	MOV	BX, 10
	IDIV	BX
        MOV     CurrNumber, AX          ;else divide by 10
	
NumEqualZero:
	MOV	CL, 1			;Set Exit loop condition true
	JMP	DoneProcessingKeys

OutputNumber:
	MOV	CL, 1				;we're done with the number
	JMP	DoneProcessingKeys

DoneProcessingKeys:	
	;first display the number by calling DisplayNum
        
        MOV     AX, CurrNumber          ;move the number into AX for argument passing
	CALL	DisplayNum			;display the number

	CMP	CL, 1			;If exit loop condition true
	JE	ExitReadingLoop	;exit reading loop
	JMP	ReadingLoop
ExitReadingLoop:
MOV     DX, CurrNumber

                                ;return value is in DX
POP     SI
POP	CX
POP	BX
POP	AX
RET
UserIntReadNumber ENDP

; NumberKeyTables
; Description: Contains keycodes for each number key from 0-9

NumberKeyTables	LABEL	BYTE
	DB	KEY_0	
	DB	KEY_1
	DB	KEY_2
	DB	KEY_3
	DB	KEY_4
	DB	KEY_5
	DB	KEY_6
	DB	KEY_7
	DB	KEY_8
	DB	KEY_9

; OptionTables
; Description: Contains the number of options for each menu
;

OptionTables   LABEL     BYTE
	DB   0    ;MAIN_MENU
	DB   3    ;SER_MENU
	DB   5    ;PATH_MENU
	DB   3    ;STEP_MENU
	DB   5    ;PARITY_MENU
	DB   7    ;BR_MENU

; MenuStrTables
; Description: Contains OFFSETs to the string tables of each menu
;              for display
;

MenuStrTables       LABEL     WORD
	DW   0                   ;MAIN_MENU
	DW   OFFSET(SerialStrings)    ;SER_MENU
	DW   OFFSET(PathStrings) ;PATH_MENU
	DW   OFFSET(StepStrings) ;STEP_MENU
	DW   OFFSET(ParityStrings)
	DW   OFFSET(BRStrings)
;MenuFuncTables
; Description: Contains OFFSETs to the function tables of each menu
;              for calling

MenuFuncTables LABEL     WORD
	DW   0                   ;no function tables for MAIN_MENU
	DW   OFFSET(SerialFuncs) ;SER_MENU
	DW   OFFSET(PathFuncs)        ;PATH_MENU
	DW   OFFSET(StepFuncs)        ;STEP_MENU
	DW   OFFSET(ParityFuncs)
	DW   OFFSET(BRFuncs)

; SerialTables
;
; Description:      These are the tables for the serial functions.
;                   They are accessed when pressing the 'SER' Key.
;
;
; Author:           Cesar Del Solar
; Last Modified:    March 11, 2001



%*DEFINE(TABLE)  (
	   %TABENT(BaudRateMsg, SetBaudRate)
	   %TABENT(ParityMsg, SetParity)
	   %TABENT(DownloadMsg, DownloadPath)
)


; strings to display table
%*DEFINE(TABENT(string, functocall))  (
	   DW     OFFSET(%string)
)

SerialStrings   LABEL       WORD
	   %TABLE

;functions to call table

%*DEFINE(TABENT(string, functocall))  (
	   DW     OFFSET(%functocall)
)

SerialFuncs   LABEL       WORD
	   %TABLE

; StepTables
;
; Description:      These are the tables for the step management functions.
;                   They are accessed when pressing the 'STP' Key.
;
;
; Author:           Cesar Del Solar
; Last Modified:    March 11, 2001

%*DEFINE(TABLE)  (
	   %TABENT(SteerMsg, SetSteer)               ;set steering angle
	   %TABENT(DistMsg, SetDist)            ;set distance
	   %TABENT(OriginMsg, SetOrigin)        ;set steering origin
)


; strings to display table
%*DEFINE(TABENT(string, functocall))  (
	   DW     OFFSET(%string)
)

StepStrings   LABEL       WORD
	   %TABLE

%*DEFINE(TABENT(string, functocall))  (
	   DW     OFFSET(%functocall)
)

StepFuncs   LABEL       WORD
	   %TABLE

; PathTables
;
; Description:      These are the tables for the path management functions.
;                   They are accessed when pressing the 'P' Key.
;
;
; Author:           Cesar Del Solar
; Last Modified:    March 11, 2001

%*DEFINE(TABLE)  (
	   %TABENT(SaveStepMsg, SaveStep)       ;save step into path
	   %TABENT(DisplayStepMsg, DisplayStep) ;display current step
	   %TABENT(RunPathMsg, RunPath)         ;run path
	  %TABENT(ClearPathMsg, ClearPath)      ;clear path
	  %TABENT(ResetPathMsg, ResetPath)      ;reset path
)


; strings to display table
%*DEFINE(TABENT(string, functocall))  (
	   DW     OFFSET(%string)
)

PathStrings   LABEL       WORD
	   %TABLE

%*DEFINE(TABENT(string, functocall))  (
	   DW     OFFSET(%functocall)
)

PathFuncs   LABEL       WORD
	   %TABLE


; ParityTables
;
; Description:      These are the tables for the parity settings.
;
;
; Author:           Cesar Del Solar
; Last Modified:    March 11, 2001

%*DEFINE(TABLE)  (
	   %TABENT(ParEvenMsg, UserIntSetParity, LCR_EVEN_PAR) ;UserIntSetParity called
	   %TABENT(ParOddMsg, UserIntSetParity, LCR_ODD_PAR)   ;for all of these
	   %TABENT(ParMarkMsg, UserIntSetParity, LCR_STICK_PAR)          
	  %TABENT(ParSpcMsg, UserIntSetParity, LCR_SPACE_PAR)       
	  %TABENT(ParNoneMsg, UserIntSetParity, 00000000B)               
)


; strings to display table
%*DEFINE(TABENT(string, functocall, arg))  (
	   DW     OFFSET(%string)
)

ParityStrings   LABEL       WORD
	   %TABLE

%*DEFINE(TABENT(string, functocall, arg))  (
	   DW     OFFSET(%functocall)
)

ParityFuncs   LABEL       WORD
	   %TABLE

%*DEFINE(TABENT(string, functocall, arg))    (
	DB   %arg
)

ParityArgs     LABEL     BYTE
	%TABLE


; BaudRateTables
;
; Description:      These are the tables for the baud rate settings.
;
;
; Author:           Cesar Del Solar
; Last Modified:    March 11, 2001

%*DEFINE(TABLE)  (
	   %TABENT(BR_110_Msg, UserIntSetBR, DLL_110)          ;UserIntSetBR called
	   %TABENT(BR_300_Msg, UserIntSetBR, DLL_300)          ;for all of these
	   %TABENT(BR_1200_Msg, UserIntSetBR, DLL_1200)        
	  %TABENT(BR_4800_Msg, UserIntSetBR, DLL_4800)         
	  %TABENT(BR_9600_Msg, UserIntSetBR, DLL_9600)         
	  %TABENT(BR_19200_Msg, UserIntSetBR, DLL_19200)
	  %TABENT(BR_56000_Msg, UserIntSetBR, DLL_56000)

)


; strings to display table
%*DEFINE(TABENT(string, functocall, arg))  (
	   DW     OFFSET(%string)
)

BRStrings   LABEL       WORD
	   %TABLE

%*DEFINE(TABENT(string, functocall, arg))  (
	   DW     OFFSET(%functocall)
)

BRFuncs   LABEL       WORD
	   %TABLE

%*DEFINE(TABENT(string, functocall, arg))  (
	   DW %arg
)

BRArgs   LABEL       WORD
	   %TABLE


; MenuStrings
;
; Description:      These are the strings to be displayed on the LEDs
;                   while cycling through different menus.
;
; Author:           Cesar Del Solar
; Last Modified:    March 11, 2001

BaudRateMsg         DB   'SEt br', 0              ;set baud rate
ParityMsg      DB   'SEt PAr', 0        ;set parity
DownloadMsg         DB   'dL PAtH', 0        ;download path
SteerMsg       DB   'SEt AnG', 0        ;set steering angle
DistMsg        DB   'SEt diSt', 0       ;set distance
OriginMsg      DB   'SEt orG', 0        ;set origin
SaveStepMsg         DB   'StoStEP', 0        ;save step
DisplayStepMsg DB   'dSPStEP', 0        ;display step
RunPathMsg          DB   'runPAtH', 0        ;run path
ClearPathMsg   DB   'CLrPAtH', 0        ;clear path
ResetPathMsg   DB   'rStPAtH', 0        ;reset path


; ParityStrings
;
; Description:      These are the strings to be displayed on the LEDs when
;                   setting parity.
;
; Author:           Glen George
; Last Modified:    Nov. 15, 1993

ParEvenMsg      DB      'PArity E', 0           ;parity even
ParOddMsg       DB      'PArity o', 0           ;parity odd
ParMarkMsg      DB      'PArity 1', 0           ;parity mark (1)
ParSpcMsg       DB      'PArity 0', 0           ;parity space (0)
ParNoneMsg      DB      'PArity n', 0           ;parity none


; BRStrings
;
; Description:      These are the strings to be displayed on the LEDs when
;                   setting baud rate.
;
; Author:           Cesar Del Solar
; Last Modified:    March 12, 2001

BR_110_Msg          DB   'br=110', 0         ;110
BR_300_Msg          DB   'br=300', 0         ;300
BR_1200_Msg         DB   'br=1200', 0
BR_4800_Msg         DB   'br=4800', 0
BR_9600_Msg         DB   'br=9600', 0
BR_19200_Msg   DB   'br=19200', 0
BR_56000_Msg   DB   'br=56000', 0

NO_Msg    DB   '        ', 0            ;no message
DONE_Msg  DB   'donE  =]' , 0            ;done =)
INTRO_Msg DB   'SELECt', 0         
ERROR_Msg       DB      'Error =[', 0       ;error =(
ANGLE_Msg       DB      'AnGlE =', 0        ;angle
DIST_Msg        DB      'diSt =', 0         ;distance

CODE ENDS

END
