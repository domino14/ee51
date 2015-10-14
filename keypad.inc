;keypad.inc
;includes keypad definitions

DB_TIME         EQU     100             ;debounce time in milliseconds
REPEAT_TIME     EQU     500             ;auto-repeat rate in milliseconds
NO_KEY_CODE     EQU     00H             ;some invalid key code
KEYMASK         EQU     0FH             ;mask for high garbage bits
KEYBASE         EQU     00H             ;port to start scanning at
KEYMODMASK	EQU	03H		;mod mask for key wrap-around

