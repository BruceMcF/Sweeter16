!to "sw816.bin", cbm
!cpu 65816

;	Leaves registers in 8bit mode
!macro BigA{
	REP #%00100000
	!al
}

!macro TinyA{
	SEP #%00100000
	!as
} 

;--------1--------2--------3--------4--------5--------6---
; Sweet816: A 65c816 Implmentation of a Virtual Machine
; for Executing Code written for Steve Wozniak's SWEET16
; Virtual Machine for the 6502.

; Written in 2020 by Bruce Richard McFarling
;	BruceRMcF@aol.com

; To the extent possible under law, the author has
; dedicated all copyright and related and neighboring
; rights to this software to the public domain worldwide.
; This software is distributed without any warranty.

; Only this Virtual Machine Implementation is covered
; by this grant. This is an independent implementation
; of the SWEET16 VM as described by Steve Wozniak. No
; assertion or claim is made regarding the status of
; the original VM. 

;    You should have received a copy of the CC0 Public
; Domain Dedication along with this software. If not, see
; <http://creativecommons.org/publicdomain/zero/1.0/

; You should have received a copy of the CC0 Public
; Domain Dedication along with this software. If not, see
; <http://creativecommons.org/publicdomain/zero/1.0/

; NOTE that unlike the original Sweet16, the Status register
; is the low byte of R14. This is to allow better cross 
; compatibility between Sweet16C, Sweet64, and Sweet816.

; NOTE TWO INTENTIONAL EXTENSIONS.

; Since Sweet816 is not restricted to op codes residing in
; a single 256 byte page, two of the three NUL op codes in
; SWEET16 are implemented as relative offset adds for the
; stack register and the main accumtlator (R0). If the
; operand is negative, it is sign extended, so this is a
; direct add of up to +127/-128.

; The third NUL op code is implemented as a CALL operation,
; which uses the address pointed to by register 11 as 
; the call vector. The operand is the offset for a branch
; on returning from the routine, to support embedded data
; for the called routine.

; You may report all other discrepencies between the
; execution semantics of this Virtual Machine and Steve
; Wozniak's VM as bugs.

; Version 0.0.1 Untested Code

REG	= $2		; R0
CALLV	= REG+22	; R11
STACK	= REG+24	; R12
STATUS = REG+28 ; REG = LO byte, CARRY = High Bit of HIGH Byte of R14
IP	= REG+30	; R15

REGA = REG+32
REGX = REGA+1
REGY = REGX+1
REGP = REGY+1


* = $CC00	; assemble to C64 Golden RAM for testing
		; This would be a VICE emulation of a 65C816
		; equipped C64 in 65C02 emulation mode.

; * = $04000	; assemble to CX16 Golden RAM for testing
; Only if the CX16 has a 65816, of course

SWEET816:
	JSR PUTSTATE
	CLC
	XCE
	+BigA
	PLA			;(IP) uses 6502 Return Address
	STA IP		;6502 RtnAdd = Actual-1
	JMP NEXTOP
	!byte		0,0,0
;	TO_OPs table.
;	Y index of $00-$0F is not a main op, so this
;	table can omit the first two rows
	!byte		$02,$02,$02,$02,$02,$02,$02,$02
	!byte		$02,$02,$02,$02,$02,$02,$02,$02
	!byte		$04,$04,$04,$04,$04,$04,$04,$04
	!byte		$04,$04,$04,$04,$04,$04,$04,$04
	!byte		$06,$06,$06,$06,$06,$06,$06,$06
	!byte		$06,$06,$06,$06,$06,$06,$06,$06
	!byte		$08,$08,$08,$08,$08,$08,$08,$08
	!byte		$08,$08,$08,$08,$08,$08,$08,$08
	!byte		$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A
	!byte		$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A
	!byte		$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C
	!byte		$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C
	!byte		$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
	!byte		$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
	!byte		$10,$10,$10,$10,$10,$10,$10,$10
	!byte		$10,$10,$10,$10,$10,$10,$10,$10
	!byte		$12,$12,$12,$12,$12,$12,$12,$12
	!byte		$12,$12,$12,$12,$12,$12,$12,$12
	!byte		$14,$14,$14,$14,$14,$14,$14,$14
	!byte		$14,$14,$14,$14,$14,$14,$14,$14
	!byte		$16,$16,$16,$16,$16,$16,$16,$16
	!byte		$16,$16,$16,$16,$16,$16,$16,$16
	!byte		$18,$18,$18,$18,$18,$18,$18,$18
	!byte		$18,$18,$18,$18,$18,$18,$18,$18
	!byte		$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A
	!byte		$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A
	!byte		$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C
	!byte		$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C
	!byte		$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
	!byte		$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E

BROPS:
	!word  RTN		;0	Return to 65C02 code
	!word  BR		;1	Branch to Sweet16 location
	!word  BNC		;2	Branch if no carry
	!word  BC		;3	Branch if carry
	!word  BP		;4	Branch if last register >=0
	!word  BM		;5	Branch if last register <0
	!word  BZ		;6	Branch if last register =0
	!word  BNZ		;7	Branch if last register <>0
	!word  BM1		;8	Branch if last register =(-1)
	!word  BNM1	;9	Branch if last register <>(-1)
	!word  BK		;A	Break (hopefully to Monitor)
	!word  RS		;B	Return from Sweet16 subroutine
	!word  BS		;C	Branch Sweet16 subroutine
	!word  CALL	;D	Call ML routine offset from R11
	!word  ADJ0	;E	Adjust Accumulator by offset
	!word  ADJS	;F	Adjust Stack by offset

; Register OPS
OPS:	; I use Wozniak's pseudo-ops
	; add "I" for "@" indirect ops.
	!word  SET		;$1r	SET Rn $[lo] $[hi]
	!word  LD		;$2r	LD Rn
	!word  ST		;$3r	ST Rn
	!word  LDI		;$4r	LD @Rn
	!word  STI		;$5r	ST @Rn
	!word  LDDI	;$6r	LDD @Rn
	!word  STDI	;$7r	STD @Rn
	!word  POPI	;$8r	POP @Rn
	!word  STPI	;$9r	STP @Rn
	!word  ADD		;$Ar	ADD Rn
	!word  SUB		;$Br	SUB Rn
	!word  POPDI	;$Cr POPD @Rn
	!word  CPR		;$Dr	CPR Rn
	!word  INR		;$Er	INR Rn
	!word  DCR		;$Fr	DCR Rn

PUTSTATE:
	PHP			;Preserve Register State
	STA REGA		;Not yet in Native Mode
	STX REGX
	STY REGY
	PLA
	STA REGP
	CLD
	RTS

GETSTATE:			;Restore State
	LDA REGP		;Already in Emulation Mode
	PHA
	LDA REGA
	LDX REGX
	LDY REGY
	PLP
	RTS

BK:	BRK

RTN:	SEC
	XCE
	JSR GETSTATE
	JMP  (IP)		;Go Back to 65C02 code

CALL:	; Pointer to 65C02 machine code is
	; in R11. OP is value loaded into A
	JSR +
	CLC
	XCE
	+BigA
	BRA BR
+	SEC
	XCE
	JMP (CALLV)

; Adjust Stack and R0 uses same offset logic
; As Branch, just different registers

ADJ0:	LDX #0
	BRA OFFSET

ADJS:	LDX #(STACK-REG)
	BRA OFFSET

BRANCH:
	ASL			;A is actually the OP index
	TAX			;X is 8bit mode, this clips top 8bits		
	INC IP		;Now increment IP
	LDA  STATUS	;Holds Register#+(Carry*$8000)
	LSR			;Carry flag -> Carry, Prior Reg Number in A
				;                     (Not index!)
	JMP (BROPS,X)	;Indexed jump to Branch OPS

BNC:	BCC BR	  	;Branch on Carry Clear
	BRA NEXTOP

BC:	BCS BR		;Branch on Carry Set
	BRA NEXTOP

;The previous register number is in A, it must be
;doubled for all users of the previous register
;index, so the X register indexed by word values.

BP:	ASL
	TAX
	LDA REG,X		;Check sign
	BPL BR		;Branch on positive
	BRA NEXTOP

BM:	ASL
	TAX
	LDA REG,X		;Check sign
	BMI BR		;Branch on negative
	BRA NEXTOP

BZ:	ASL
	TAX	
	LDA REG,X		;Check zero
	BEQ BR		;Branch if zero
	BRA NEXTOP

BNZ:	ASL
	TAX	
	LDA REG,X		;Check zero
	BNE BR		;Branch if zero
	BRA NEXTOP

BM1:	ASL
	TAX
	LDA REG,X		;Check $FFFF
	CMP #$FFFF
	BEQ BR		;Branch if -1
	BRA NEXTOP

BNM1:	ASL
	TAX
	LDA REG,X		;Check $FFFF
	CMP #$FFFF
	BNE BR		;Branch if -1
	BRA NEXTOP

RS:				;Pop the IP from the stack
	DEC STACK
	DEC STACK
	LDA (STACK)
	STA IP
	BRA NEXTOP

BS:	LDA IP
	STA (STACK)
	INC STACK
	INC STACK
BR:	LDX #(IP-REG)	; Index to IP
OFFSET:
	LDA  (IP)		;Branch Offset
	BIT #$0080		;Sign Extend?
	BPL  +
	ORA #$FF80		;Sign Extend!
+	CLC
	ADC  REG,X	  	;ADD TO IP
	STA  REG,X
NEXTOP:
	INC IP
	LDA (IP)		; if([(++IP)]&&F0h)
	BIT #$00F0
	BEQ BRANCH
	TAY			;	{[(++IP)]&&F0h/8 -> OP index}
	LDX SWEET816,Y	; Tabled [AND #$F0, /8]
	AND #$000F		; *2 = Reg if OP, BROP if BROP
	ASL
	STA STATUS		; This is the register index operand
				; Carry clear for each main OP
	JMP (OPS-2,X)	; Minimum X=2, since X=0 => BRANCH


DCR:	TAX			; --Rn
	DEC REG,X
	BRA NEXTOP

ADD:	STZ STATUS
	TAX			; ADD R0,Rn
	CLC
	LDA REG
	ADC REG,X
	STA REG
	BCC NEXTOP
	INC STATUS
	BRA NEXTOP

SUB:	LDX #0	; LDD R0,(R0-Rn)
CPR:			; LDD R13,(R0-Rn)
	STZ STATUS	; Zero high byte
	STX STATUS	; JMP,X->CPR => X=R13 index
			; Note X in 8bit mode
	TAY		
	SEC
	LDA REG
	SBC REG,Y
	STA REG,X
	BCC NEXTOP
	INC STATUS
	BRA NEXTOP

SET:
	TAX
	INC IP
	LDA (IP)
	STA REG,X
	INC IP
	BRA NEXTOP

LD:	TAX
	LDY #0
LD1:	+TinyA
	LDA REG,X		; LD R0,Rn
	STA REG,Y
	STZ REG+1
	+BigA
	BRA NEXTOP

ST:	TAY
	LDX #0
	BRA LD1

POPI:
	TAX
	DEC REG,X		; LD R0,(--Rn)
	LDA (REG,X)
	AND #$00FF
	BRA NEXTOP

LDI:		; 
	TAX
	LDA (REG,X)	; LD R0,(Rn++) # Byte
	AND #$00FF
	STA REG
	STZ STATUS 	; R0 is actual target
INR:
	INC  REG,X	; The Rn++ part
	BRA NEXTOP

STI:		; 
	TAX
	+TinyA
	LDA REG		; LD (Rn++), R0	# Byte
	STA (REG,X)
	+BigA
	STX STATUS
	INC REG,X
	JMP NEXTOP

LDDI:				; LDD R0,(Rn++)
	TAX
	LDA (REG,X)
	STA REG
	INC REG,X
	INC REG,X
	JMP NEXTOP

STDI:				; STD (Rn++),R0
	TAX
	LDA REG
	STA (REG,X)
	INC REG,X
	INC REG,X
	INC REG,X
	JMP NEXTOP

STPI:				; LD (--Rn),R0
	TAX
	DEC REG,X
	+TinyA
	LDA REG
	STA (REG,X)
	STZ STATUS		; Branch conditions reflects R0
	+BigA
	JMP NEXTOP

POPDI:			; LDD R0,(--Rn)
	TAX
	DEC REG,X
	DEC REG,X
	LDA (REG,X)
	STA REG
	STZ STATUS
	JMP NEXTOP

SWEETVM = SWEET816

!source "sweet16vm_code.asm"
!eof
: