NAME	keypad

;keypad.asm
;this file implements the four keypad routines
;KeypadInit, GetKey, IsAKey, and ScanAndDebounceKeys
;Author: Cesar Del Solar
;
;Date Finished: February 19, 2001
;
; Revisions:    March 12, 2001
;                -Added auto-repeat
;


$include (keypad.inc)

FALSE	EQU 0
TRUE	EQU 1

DATA    SEGMENT PUBLIC  'DATA' 
 HaveKey	DB	?		;flag set if we have a key
 KeyCode	DB	?		;my defined key code
 DBCounter	DW	?		;debounce counter, updated every millisecond
 CurrRow	DB	?		;current row I'm scanning
 DBKeyCode      DB      ?
DATA    ENDS 
 
STACK           SEGMENT STACK  'STACK' 
 
 DB      80 DUP ('Stack ')       ;240 words 
 TopOfStack      LABEL   WORD 
 
STACK           ENDS 

CODE SEGMENT PUBLIC 'CODE' 
 ASSUME CS:CODE, DS:DATA 

;KeyPadInit:
;Description: 	This function initializes local variables used for the keypad.
;Input:		None.
;Output:		None.
;User Interface: None
;Error Handling: None
;Algorithm:	None.
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.
 
KeypadInit	PROC	NEAR
		PUBLIC	KeypadInit

MOV	HaveKey, FALSE		;we don't have a key at the beginning
MOV	KeyCode, NO_KEY_CODE	;There is no key at the beginning
MOV	DBKeyCode, NO_KEY_CODE	;no key being debounced at the beginning
MOV	CurrRow, 0			;start with row 0
MOV	DBCounter, DB_TIME	;start with the time to debounce

RET
KeypadInit	ENDP


;IsAKey:
;Description: 	This function returns with zero flag set if there is a key available and reset otherwise.
;Input:		None.
;Output:		None.
;User Interface: None
;Error Handling: None
;Algorithm:	None.
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

IsAKey	PROC	NEAR
        PUBLIC  IsAKey       
CMP     HaveKey, FALSE           ;sets zero flag if HaveKey = FALSE

RET
IsAKey 	ENDP

;GetKey:
;Description: 	This function returns with the code for the current debounced key.
;Input:		None.
;Output:		A hex key code (COL *1 + ROW * 16), returned into AL
;User Interface: None
;Error Handling: None
;Algorithm:	None.
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.

GetKey	PROC	NEAR
		PUBLIC	GetKey

IsAKeyFalse:
	CALL	IsAKey
        JNZ      WeHaveAKey              ;if the zero flag is set exit loop
	JMP	IsAKeyFalse		;else keep looping
WeHaveAKey:
	MOV	HaveKey, FALSE	;we don't have a key anymore
	MOV	AL, KeyCode		;RETURN KeyCode, defined in KeypadScan
	
	RET

GetKey	ENDP

;KeypadScan:
;Description: 	This function is called every millisecond by the 
;timer event handler, and scans and debounces keys.
;Input:		None.
;Output:		None.
;User Interface: None
;Error Handling: None
;Algorithm:	None.
;Data Structures: None
;Known Bugs:	None
;Limitations:  None.


KeypadScan	PROC	NEAR
		PUBLIC	KeypadScan

PUSH	AX				;save AX
PUSH	DX
PUSH	BX

MOV     DX, 0
MOV     DL, KEYBASE              
ADD     DL, CurrRow             ;DX = KeyBase + CurrRow
IN      AL, DX                  ;read key into AL
NOT     AL                      ;invert AL
AND     AL, KeyMask             ;filter out top 4 bits, they're garbage

CMP	AL, NO_KEY_CODE		;if the key scanned is invalid then 
                                ;increment row and leave, else deal with
                                ;the key
JE	IncrementAndLeave
JMP	DealWithKey

IncrementAndLeave:
INC	CurrRow
AND	CurrRow, KEYMODMASK	;CurrRow = (CurrRow + 1) MOD 4
JMP	EndOfProc

DealWithKey:			;a key was pressed, debounce it now
MOV	BL, CurrRow
SHL     BL, 4                   ;multiply CurrRow by 16
ADD	AL, BL			;AL = ScannedKey + CurrRow * 16, my keycode
                                ;AL is now a unique keycode
CMP	AL, DBKeyCode		;see if our keycode is equal to that of the
                                ;key currently being debounced
JE      DebounceKey             ;if it is, then decrement debounce counter
JMP	CodesNotEqual		;if it is not, then reset counter and DBKeyCode

DebounceKey:			;debounce the key
DEC     DBCounter               ;DBCounter = DBCounter - 1
CMP     DBCounter, 0            ;see if DBCounter has reached 0 yet
JE	UpdateNewKey		;key has been debounced
JMP	EndOfProc			;exit procedure

UpdateNewKey:
MOV     BL, DBKeyCode
MOV     KeyCode, BL             ;tell GetKey the key code it has to return
;MOV     DBKeyCode, NO_KEY_CODE
MOV	HaveKey, TRUE		;tell IsAKey that we have a key
MOV     DBCounter, REPEAT_TIME  ;reset DBCounter back to auto repeat value
JMP	EndOfProc			;exit procedure

CodesNotEqual:
MOV	DBKeyCode, AL		;DBKeyCode = key that was just scanned
MOV	DBCounter, DB_TIME	;reset debounce counter back to starting value
;JMP 	EndOfProc			;exit procedure 
EndOfProc:

POP	BX
POP	DX
POP	AX

RET
KeypadScan	ENDP

CODE ENDS
END
