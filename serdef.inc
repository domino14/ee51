;Cesar Del Solar
;Homework 9 - 16C450 Include file
;TA: Tiago Wright
;ACE = Asynchronous Communications Element
;ADD = ADDRESS

;register addresses
;

REC_BUF_REG_ADD	EQU	ACE_BASE_ADD	;Receiver Buffer register
TRAN_H_REG_ADD  	EQU     ACE_BASE_ADD    ;Transmitter holding register
IER_ADD		EQU	ACE_BASE_ADD + 1	;Interrupt enable register
IIR_ADD		EQU	ACE_BASE_ADD + 2	;Interrupt ident. register
LCR_ADD		EQU	ACE_BASE_ADD + 3 	;Line control register
MCR_ADD		EQU	ACE_BASE_ADD + 4	;Modem control register
LSR_ADD		EQU	ACE_BASE_ADD + 5	;Line status register
MSR_ADD		EQU	ACE_BASE_ADD + 6	;Modem status register
SCR_ADD		EQU	ACE_BASE_ADD + 7	;Scratch register
DLL_ADD		EQU	ACE_BASE_ADD 	;Divisor latch
DLM_ADD		EQU	ACE_BASE_ADD + 1	;Latch
;
;
;Interrupt Enable Register Bits
;
IER_ERBF	EQU	00000001B	;Enable received data available int.
IER_ETBE	EQU	00000010B	;Enable transmitter holding reg empty int.
IER_NOETBE	EQU	00001101B	;everything but ETBE
IER_ELSI	EQU	00000100B	;Enable receiver line status int.
IER_EDSSI	EQU	00001000B	;Enable modem status interrupt
IER_ALL_INT_ON 	EQU	00001111B	;all four of the above are on
;
;Interrupt Ident. Register Bits
;
IIR_NO_INT_MASK	EQU	00000001B	;No interrupt mask (to check status)
IIR_R_LINE_STATUS	EQU	00000110B	;receiver line status
IIR_R_DATA_READY	EQU	00000100B	;receiver data ready
IIR_T_REG_EMPTY	EQU	00000010B	;transmitter holding reg. empty
IIR_M_STATUS	EQU	00000000B	;modem status
;
;Line Control Register Bits
;
LCR_FIVE_BIT	EQU	00000000B	;word length 5 bits
LCR_SIX_BIT		EQU	00000001B	;word length 6 bits
LCR_SEVEN_BIT	EQU	00000010B	;word length 7 bits
LCR_EIGHT_BIT	EQU	00000011B	;word length 8 bits
LCR_ONE_STOPBIT	EQU	00000000B	;one stop bit
LCR_MORE_STOPBIT	EQU	00000100B	;more than one stop bit
LCR_PAR_ENABLE	EQU	00001000B	;parity enable bit
LCR_ODD_PAR		EQU	00001000B	;odd parity
LCR_EVEN_PAR	EQU	00011000B	;even parity
LCR_STICK_PAR	EQU	00101000B	;stick parity (mark)
LCR_SPACE_PAR	EQU	00111000B	;space parity
LCR_BRK_CTRL	EQU	01000000B	;break control bit
LCR_SET_DLAB	EQU	10000000B	;divisor latch access bit on
;
;Line status register bits 
;
LSR_DATA_READY	EQU	00000001B	;set when a bit has been transferred
LSR_OVERRUN_IND	EQU	00000010B	;overrun error indicator
LSR_PARERR_IND	EQU	00000100B	;there is a parity error
LSR_FRAMERR_IND	EQU	00001000B	;framing error - invalid stop bit
LSR_BRKINT_IND	EQU	00010000B	;Break interrupt indicator
LSR_THR_EMPTY	EQU	00100000B	;ACE is ready to accept new char
LSR_TEMT_EMPTY	EQU	01000000B	;Transmitter empty indicator
;
;Modem control register bits
;
MCR_DATA_TERM_RDY	EQU	00000001B	;data terminal ready active
MCR_REQ_TO_SEND	EQU	00000010B	;request to send active
MCR_OUT1		EQU	00000100B	;output 1 signal active
MCR_OUT2		EQU	00001000B	;output 2 signal active
MCR_ACE_DIAGMODE	EQU	00010000B	;sets ACE to diagnostic mode
;
;Modem status register bits
;
MSR_DCLR_TO_SEND	EQU	00000001B	;delta clear to send indicator
MSR_DDATASET_RDY	EQU	00000010B	;delta data set ready indic.
MSR_TERI		EQU	00000100B	;trailing edge of ring indic.
MSR_DDATACARR_DET	EQU	00001000B	;delta data carrier detect indic.
MSR_CTS_COMP	EQU	00010000B	;complement of clear to send
MSR_DSR_COMP	EQU	00100000B	;complement of data set ready
MSR_RI_COMP		EQU	01000000B	;complement of ring indicator
MSR_DCD_COMP	EQU	10000000B	;complement of data carrier detect
;

;Divisor Latches for different baud rates = XTAL1 / 2(baud_rate * 16)
;	
DLL_50		EQU		11520	
DLL_75		EQU		7680
DLL_110		EQU		5235
DLL_134_5	EQU		4285
DLL_150		EQU		3840
DLL_300		EQU		1920
DLL_600		EQU		960
DLL_1200	EQU		480
DLL_1800	EQU		320
DLL_2000	EQU		290
DLL_2400	EQU		240
DLL_3600	EQU		160
DLL_4800	EQU		120
DLL_7200	EQU		80
DLL_9600	EQU		60			
DLL_19200	EQU		30
DLL_38400	EQU		15
DLL_56000	EQU		10