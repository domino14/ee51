;These are the values for important constants needed by main.asm
; 
 
; Addresses 
Tmr0Ctrl        EQU     0FF56H          ;address of Timer 0 Control Register 
Tmr0MaxCntA     EQU     0FF52H          ;address of Timer 0 Max Count A Register 
Tmr0Count       EQU     0FF50H          ;address of Timer 0 Count Register 
 
; Control Register Values 
Tmr0CtrlVal     EQU     0E001H          ;value to write to Timer 0 Control Register 
                                        ;1---------------  enable timer 
                                        ;-1--------------  write to control 
                                        ;--1-------------  enable interrupts 
                                         
                                        ;--------------0-  single counter mode 
                                        ;---------------1  continuous mode 
 
; Interrupt Vectors 
Tmr0Vec         EQU     8               ;interrupt vector for Timer 0 
 
 
; Interrupt Controller Definitions 
 
; Addresses 
INTCtrlrCtrl    EQU     0FF32H          ;address of interrupt controller for timer 
INTCtrlrEOI     EQU     0FF22H          ;address of interrupt controller EOI register 
 
; Register Values 
INTCtrlrCVal    EQU     00001H          ;set priority for timers to 1 and enable 
Timer0EOI       EQU     00008H          ;Timer EOI command (same for all timers) 
NonSpecEOI      EQU     08000H          ;Non-specific EOI command 
 
 
; Chip Select Unit Definitions 
 
; Addresses 
PACSreg         EQU     0FFA4H          ;address of PACS register 
MPCSreg         EQU     0FFA8H          ;address of MPCS register 
 
; Control Register Values 
PACSval         EQU     00003H          ;PCS base at 0, 3 wait states 
MPCSval         EQU     00183H          ;PCS in I/O space, use PCS5/6, 3 wait states 
 
 
 
; Timing Definitions 
 
COUNTS_PER_MS   EQU     2304            ;number of timer counts per 1 ms 
 
 
 
; General Definitions 
 
RESERVED_VECS   EQU     4               ;vectors reserved for debugger 
 
LEDDisplay      EQU     0080H           ;display address 
 
NO_DIGITS       EQU     8               ;number of digits in the display 
 
 
