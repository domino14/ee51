;motors.inc
;contain definitions for the motors

PARALLEL_BASE	EQU	180H

STEER_ADD		EQU	PARALLEL_BASE + 1		;step steering (angle)
ROTATE_ADD		EQU	PARALLEL_BASE + 2		;wheel rotating

STEER_CONST		EQU		8				;halfsteps
ROTATE_CONST	EQU		4				;halfsteps