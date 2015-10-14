 NAME display
 
;Description: Display functions for the RoboCar 
;             Display(str), DisplayNum(n), DisplayHex(n), DisplayInit, and 
;               DisplayMux 
;Author: Cesar Del Solar 
;Date Finished: February 11, 2001 
; 
;Revisions: 
; 
; February 20, 2001		Updated comments
; March 15, 2001                Fixed bug that effectively
;                              undoes whatever DisplayNum and
;                               DisplayHex are supposed to do
;
 
BPATLENGTH EQU 7 
HEXMASK1 EQU 000FH 
LEDBASE EQU 080H 
 
extrn ASCIISegTable:BYTE 
 
DATA SEGMENT WORD PUBLIC 'DATA' 
 Bpattern       DB      8 DUP(?) 
 curr_digit     DW      ?       ;current digit 
 counter        DW      ?       ;counter for loops 
 curr_patt      DB      ?       ;current bit pattern 
 TempNumber     DW      ?       ;a temporary number          
 NumString      DB      8 DUP(?) 
 HexNum         DW      ?       ;hex number 
 port           DW      ?       ;port to write to 
DATA ENDS 
 

CODE SEGMENT PUBLIC 'CODE' 
 ASSUME CS:CODE, DS:DATA 
 

;DisplayInit
;Description: Initializes all display variables

DisplayInit PROC    NEAR 
            PUBLIC      DisplayInit 
 ;clear buffer 
 push   si
 PUSH   BP
 push   ax

 MOV    BP, 0                 ;counter = 0
StartClear:                        ;REPEAT 

 MOV    Bpattern[BP], 0            ;Bpattern[counter]= 0 
 MOV    NumString[BP],1
 CMP    BP, BPATLENGTH        ;UNTIL counter = 7

 JE     ExitClear
 
 INC    BP                    ;counter++
 JMP    StartClear
 
ExitClear: 
 MOV    curr_digit, 0              ;curr_digit = 0

 pop    ax
 POP    BP
 pop    si
 RET
 
DisplayInit     ENDP 
 
;Display:
;\Description:   This function turns a string into a bit pattern.
;Input:		A string in ES:SI that is null terminated.
;Output:		An array BPattern read by DisplayMux.
;User Interface: None
;Error Handling: None
;Algorithm:	Turns the character into an ascii number, which is looked up in the codetable.
;Data Structures: None
;Known Bugs:	None
;Limitations:  Eight letters at most.

 
Display PROC    NEAR 
        PUBLIC  Display 

;CALL   DisplayInit


 PUSH   BP
 PUSH   AX
 PUSH   BX

 MOV    BP, 0                 ;counter = 0
PatternClear:                        ;REPEAT 

 MOV    Bpattern[BP], 0            ;Bpattern[counter]= 0 
 CMP    BP, BPATLENGTH        ;UNTIL counter = 7

 JE     EndPatternClear
 
 INC    BP                    ;counter++
 JMP    PatternClear
     
EndPatternClear:
 MOV    BP, 0                           ;counter used for current index in
                                        ;string 
 StartPattern: 
                                        ;everything's cool 
 MOV    AL, ES:[SI + BP] 
 CMP    BP, BPATLENGTH + 1                  ;check if counter = 8 
 JE     EndPattern 
 CMP    AL, 0                           ;check if 0 terminated
 JE     EndPattern 
 
 MOV    BX, OFFSET(ASCIISegTable) 
 XLAT   CS:ASCIISegTable                 ;AL<- BX[AL] 
 MOV    BPattern[BP], AL                 ;put pattern into buffer 
 INC    BP                               ;increment counter

 JMP    StartPattern 
 EndPattern: 
 
 POP    BX
 POP    AX
 POP    BP

 RET                                    ;returns. DisplayMUX will display 
                                        ;the actual string in BPattern 
                                        ;a letter at a time, each millisecond 
 
Display ENDP 
 
 
;DisplayNum:
;Description: 	This function turns a 16-bit number into a string.
;Input:		A 16-bit number 
;Output:		None.
;User Interface: None
;Error Handling: None
;Algorithm:	Turns the number into a string by division
;Data Structures: None
;Known Bugs:	None
;Limitations:  Five digits (16 bits) at most.
 
DisplayNum PROC NEAR 
           PUBLIC       DisplayNum 

 CALL   DisplayInit                     ;clear numstring

 PUSH   BX
 PUSH   DX
 PUSH   BP
 PUSH   AX
 MOV    NumString[6], 0                 ;terminates NumberString with a 0
 MOV    BP, 6                           ;start counter at 5, the most
                                        ;significant digit 
 
 MOV    TempNumber, AX 
 CMP    TempNumber, 0 
 JL     NegativeNum                     ;it's negative 
                                        ;else
 JMP    StartConversion                 ;start conversion
  
 NegativeNum: 
 ;Turn to positive and add a sign
 
 MOV    BYTE PTR NumString, '-' 
 NEG    TempNumber
 
 ;JMP   StartConversion 
 StartConversion:                       ;start conversion 
 DEC    BP                              ;counter = counter - 1 
 CMP    BP, 0 
 JE     ExitConversion 
 MOV    AX, TempNumber
 MOV    DX, 0                           ;turns TempNumber into a double word 
 MOV    BX, 10 
 DIV    BX 
 ADD    DX, '0'                         ;dx = number mod 10
 MOV    NumString[BP], DL               ;puts Number mod 10 into the string 
                                        ;array 
 MOV    TempNumber, AX                  ;puts Number/10 into TempNumber 
 
 JMP    StartConversion 
 
 ExitConversion: 
 ;put number string in ES:SI 
 MOV    AX, DS 
 MOV    ES, AX                          ;move data segment into ES 
 MOV    SI, OFFSET(NumString)           ;put NumString into ES:SI 
 CALL   Display                         ;deals with Bpattern 

 POP		AX
 POP    BP
 POP    DX
 POP    BX

 RET                                    ;number is in ES:SI
                                        ;DisplayMux will display it 
 
 
DisplayNum ENDP 
 
;DisplayHex:
;Description: 	This function turns a 16-bit hex number into a string.
;Input:		A 16-bit hex number 
;Output:		None.
;User Interface: None
;Error Handling: None
;Algorithm:	Turns the number into a string.
;Data Structures: None
;Known Bugs:	None
;Limitations: Four digits (16 bits) at most.

 
 
DisplayHex PROC NEAR 
           PUBLIC DisplayHex 


 CALL   DisplayInit                     ;clear
 PUSH		AX
 PUSH   BP
             
 MOV    NumString[4], 0                 ;terminates NumberString with a 0
 MOV    BP, 3                           ;start counter at 3, the most 
                                        ;significant digit 
 
 StartHexConversion:                       ;start conversion

 MOV    TempNumber, AX 

 AND    AX, HEXMASK1                    ;'filters' out the rightmost 4 bits 
                                        ;now there's a 4 bit number in AX 
 
 CMP    AX, 9 
 JG     NumberAbove9                    ;turns it into a character 
                                        ;else
 ADD    AX, '0' 
 JMP    NumberBelow10 
 NumberAbove9: 
                                        ;the number is above 9, meaning it 
                                        ;must be turned into a letter 
 SUB    AX, 10 
 ADD    AX, 'A'                         ;adds 'A' to number - 10 
 
 NumberBelow10: 
 MOV    NumString[BP], AL               ;puts it into the string   

 MOV    AX, TempNumber                  ; stores new temp number for next 
                                        ; iteration 
 SHR    AX, 4                           ;shift right by 4 bits

 DEC    BP                              ;counter = counter - 1
 CMP    BP, 0
 JL     ExitHexConversion
 JMP    StartHexConversion 
 
 ExitHexConversion: 
 ;put number in ES:SI 
 MOV    AX, DS 
 MOV    ES, AX                          ;move data segment into ES 
 MOV    SI, OFFSET(NumString)           ;put NumString into ES:SI 
 CALL   Display                         ;deals with Bpattern 

 POP    BP
 POP		AX
 RET                                    ;number is in ES:SI
                                        ;DisplayMux will display it 
 
  
 
DisplayHex ENDP 
 
;DisplayMux 
;Description: Displays whatever is in the Bpattern buffer, one digit at a 
;       time, each time it's called 
;Input: An array of bytes Bpattern 
;Output: Outputs to display! 
;Algorithm: straightforward, must exit ASAP  
;Limitations: None 
 
DisplayMux PROC NEAR 
           PUBLIC DisplayMux 
 StartDMux: 

 PUSH   DX
 PUSH   BP
 push   si
 PUSH   AX

 MOV    DX, LEDBASE                             ;DX = 080H
 ADD    DX, curr_digit                          ;DX = 080H + curr_digit 
 MOV    BP, curr_digit 
 MOV    AL, Bpattern[BP]                        ;puts current digit pattern 
                                                ; into AL 
 OUT    DX, AL                                  ;outputs to display 
 
 INC    curr_digit                              ;updates curr_digit 
 CMP    curr_digit, 8                           ;if equals 8 
 JE     ChangeTo0 
 JMP    ExitDisplayMux 
 Changeto0:                                     ;make it 0 
 MOV    curr_digit, 0 
 
 ExitDisplayMux: 
 
 POP    AX
 pop    si
 POP    BP
 POP    DX

 RET
DisplayMux ENDP 
 
CODE ENDS 
 
END 
