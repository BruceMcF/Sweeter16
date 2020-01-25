; Test Sweet16 VM, to be executed from Monitor so end with BK

	; Test Arithmatic
	JSR SWEETER16
	!byte  $10,$00,$00	; SET R0 $00 $00
	!byte  $11,$01,$00	; SET R1 $01 $00
	!byte  $12,$02,$00	; SET R2 $02 $00
	!byte  $A1			; ADD R1
	!byte  $33			; ST R3
	!byte  $B2			; SUB R2 (should be -1)
	!byte  $34			; ST R4
	!byte  $08,(L2-L1)	; BM1 (L2-L1)
L1:	!byte  $15,$00,$00	; SET R5 $00 $00 (shouldn't)
	!byte  $0A			; BK
L2:	!byte  $15,$10,$04	; SET R5 $10 $04 (should)
	!byte  $00			; RTN
	BRK

	; Test Stack
	JSR SWEETER16
	!byte  $10,$00,$00	; SET R0 $0000
	!byte  $11,$08,$00	; SET R1 $0008 (Count)
	!byte  $12,$00,$C0	; SET R2 $C000 (A stack)
L3:	!byte  $E0			; ICR R0
	!byte  $72			; STD @R2
	!byte  $D1			; CPR R1
	!byte  $07,(L3-L4)	; BNZ (L3-L4)
L4:	!byte  $00			; RTN
	BRK

	; Copy (do Test Stack first, don't clear RAM)
	JSR SWEETER16
	!byte  $13,$00,$C0	; Source of copy, target in R2, count in R1
L5:	!byte  $63			; LDD @R3
	!byte  $72			; STD @R2
	!byte  $F1			; DCR R1
	!byte  $07,(L5-L6)	; BNZ (L5-L6)
L6:	!byte  $00			; RTN
	BRK

	; Test Byte ops (do Test Stack and Test Copy first)
	JSR SWEETER16
	;				; R2 points past last write		
	!byte  $11,$08,$00	; SET R1 $0008  (Count)
	!byte  $1A,$00,$00	; SET R10 $0000
L7:	!byte  $2A			; LD R10 # Zero accumulator
	!byte  $B1			; SUB R1 # Subtract count
	!byte  $52			; ST @R2
	!byte  $F1			; DCR R1
	!byte  $07,(L7-L8)	; BNZ (L7-L8)
L8:	!byte  $22			; LD R2
	!byte  $34			; ST R4
	!byte  $11,$08,$00	; SET R1 $08 $00 (Count)
L9:	!byte  $84			; POP @R4
	!byte  $A0			; ADD R0 # R0+R0 => R0*2
	!byte  $52			; ST @R2
	!byte  $F1			; DCR R1
	!byte  $07,(L9-L10)	; BNZ (L9-L10)
L10:	!byte  $00			; RTN
	BRK



