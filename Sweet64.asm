!to "sweet64.bin", cbm
!cpu 6502
;--------1--------2--------3--------4--------5--------6---
; Sweet64: A C64 Implmentation of a Virtual Machine
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

; NOTE TWO INTENTIONAL EXTENSIONS.

; Since Sweet64 is not restricted to op codes residing in
; a single 256 byte page, two of the three NUL op codes in
; SWEET16 are implemented as relative offset adds for the
; stack register and the main accumtlator (R0). If the
; operand is negative, it is sign extended, so this is a
; direct add of up to +127/-128.

; The third NUL op code is implemented as a CALL operation,
; which uses the address pointed to by register 11 as 
; the call vector. The operand is the offset for a branch
; on returning from the routine. 

; You may report all other discrepencies between the
; execution semantics of this Virtual Machine and Steve
; Wozniak's VM as bugs.

; Do NOT report as a bug that this is a memory hog
; compared to Steve Wozniak's implementation. The effort 
; is to run faster on a 65C02 system. This particular 
; version implements one parsing functions using a page 
; table, which on its own is over half the size of 
; Steve Wozniak's entire implementation.

; Version 0.0.1 Untested Code

REG		= $2		; R0
CALLV		= $18		; R11
STACK		= $1A		; R12
STATUS	= $1E		; LOW Byte of R14
IP		= $20		; R15

; So that the CALL mechanism can use the SAVE/RESTORE
; subroutines within portable Sweeter16 code, the
; registers storage locations are locked to the four
; bytes after the R register.

REGA	= REG+2
REGX = REGA+1
REGY = REGX+1
REGP = REGY+1

VBR = REGP+1
VOP = VBR+2

* = $CC00	; assemble to C64 Golden RAM for testing
		; This would be a VICE emulation of a 65C816
		; equipped C64 in 65C02 emulation mode.

SWEETER16:
	JSR PUTSTATE
	PLA
	STA IP		;(IP) uses 6502 Return Address
	PLA 			;6502 RtnAdd = Actual-1
	STA IP+1
	STA VBR+1
	STA VOP+1
	INC VOP
	JMP NEXTOP


BROPS:
	; I use Wozniak's pseudo-ops
	!byte <RTN		;0	Return to 65C02 code
	!byte <BR		;1	Branch to Sweet16 location
	!byte <BNC		;2	Branch if no carry
	!byte <BC		;3	Branch if carry
	!byte <BP		;4	Branch if last register >=0
	!byte <BM		;5	Branch if last register <0
	!byte <BZ		;6	Branch if last register =0
	!byte <BNZ		;7	Branch if last register <>0
	!byte <BM1		;8	Branch if last register =(-1)
	!byte <BNM1	;9	Branch if last register <>(-1)
	!byte <BK		;A	Break (hopefully to Monitor)
	!byte <RS		;B	Return from Sweet16 subroutine
	!byte <BS		;C	Branch Sweet16 subroutine
	!byte <CALL	;D	Call ML routine offset from R11
	!byte <ADJ0	;E	Adjust Accumulator by offset
	!byte <ADJS	;F	Adjust Stack by offset

; Register OPS
OPS:	; I use Wozniak's pseudo-ops
	; add "I" for "@" indirect ops.
	!byte <SET		;$1r	SET Rn $[lo] $[hi]
	!byte <LD		;$2r	LD Rn
	!byte <ST		;$3r	ST Rn
	!byte <LDI		;$4r	LD @Rn
	!byte <STI		;$5r	ST @Rn
	!byte <LDDI	;$6r	LDD @Rn
	!byte <STDI	;$7r	STD @Rn
	!byte <POPI	;$8r	POP @Rn
	!byte <STPI	;$9r	STP @Rn
	!byte <ADD		;$Ar	ADD Rn
	!byte <SUB		;$Br	SUB Rn
	!byte <POPDI	;$Cr POPD @Rn
	!byte <CPR		;$Dr	CPR Rn
	!byte <INR		;$Er	INR Rn
	!byte <DCR		;$Fr	DCR Rn

PUTSTATE:
	; Implemented as a routine so may be
	; called as a CALL op.
	; eg, when returning from Kernal call
	PHP			;Preserve Register State
	STA REGA
	STX REGX
	STY REGY
	PLA
	STA REGP
	CLD
	RTS

GETSTATE:
	; Implemented as a routine so may be
	; called as a CALL op, eg, with
	; pseudo-regs set up for Kernal call
	LDA REGP
	PHA
	LDA REGA
	LDX REGX
	LDY REGY
	PLP
	RTS

CALL:	; Pointer to 65C02 machine code is
	; in R11. Branch via OP to support
	; embedded data
	JSR +
	JMP BR
+	JMP (CALLV)

; These next Branch four ops inherit their branch 
; to NEXTOP from the branch op that they jump into
; putting them here allows BRANCH to be close enough
; to NEXTOP for the BEQ BRANCH to work.

; BM1 and BNM1 uses the BZ and BNZ logic
; compared to #$FF rather than #0
BM1	ASL
	TAX
	LDA #$FF
	BNE BAX

BNM1	ASL
	TAX
	LDA #$FF
	BNE BNAX

; Adjust Stack and R0 uses same offset logic
; As Branch, just different registers

ADJ0:	LDX #0
	BEQ OFFSET

ADJS:	LDX #(STACK-REG)
	JMP OFFSET

BRANCH:
	TAX
	LDA BROPS,X
	STA VBR			
	INC IP		;Now increment IP, since
	BNE  +		;actual operand is in following byte
	INC  IP+1
+	LDA  STATUS	;Holds Register#+Carry*$80
	ASL			;Carry flag -> Carry, Prior Reg in A
	TAX
	JMP (VBR)	;Indexed jump to Branch OPS

; Branch on condition codes in front of NEXTOP and BR so they
; can branch either way

BNC:	BCC BR	  	;Branch on Carry Clear
	BCS NEXTOP

BC:	BCS BR		;Branch on Carry Set
	BCC NEXTOP

;The previous register number is in A, it must be
;doubled for all users of the previous register
;index, so the X register indexed by word values.

BP:	LDA REG+1,X	;Check sign
	BPL BR		;Branch on positive
	BMI NEXTOP

BM:	LDA REG+1,X	;Check sign
	BMI BR		;Branch on negative
	BPL NEXTOP

BZ:	LDA #0
BAX:
	CMP REG,X		;Check zero
	BNE NEXTOP
	CMP REG+1,X
	BEQ BR
	BNE NEXTOP

BNZ:	LDA #0
BNAX:
	CMP REG,X		;Check zero
	BNE BR
	CMP REG+1,X
	BEQ NEXTOP
	BNE BR

BS:	LDY #0
	LDA IP
	STA (STACK),Y
	LDA IP+1
	INY
	STA (STACK),Y
	TYA
	SEC
	ADC STACK
	STA STACK
	BCC BR
	INC STACK+1
BR:
	LDX #(IP-REG)	; Index to IP
OFFSET:
	LDY #0		;16bit sign extension
	LDA  (IP),Y	;Branch Offset
	BPL  +
	DEY
+	CLC
	ADC  REG,X	  	;ADD TO IP
	STA  REG,X
	TYA
	ADC  REG+1,X
	STA  REG+1,X
NEXTOP:
	INC IP
	BNE  +		; ++IP
	INC IP+1
NEXTOP1:
+	LDY #0
	LDA (IP),Y		; if([(++IP)]&&F0h)
	CMP #$10
	BMI BRANCH
	LSR
	LSR
	LSR
	LSR
	TAX
	LDA OPS-1,X
	STA VOP
	LDA (IP),Y
	AND #$0F
	STA STATUS		; Note Status Register is Offset/2
				; Carry is in High Bit
	ASL
	TAX
	JMP (VOP)

BK:	BRK

RTN:	JSR GETSTATE
	JMP  (IP)	;Go Back to 65C02 code

RS:				;Pop the IP from the stack
	SEC
	LDA STACK
	SBC #2
	STA STACK
	BCS +
	DEC STACK+1
+	LDY #1
	LDA (STACK),Y
	STA IP+1
	DEY
	LDA (STACK),Y
	STA IP
	JMP NEXTOP

ADD:	STY STATUS		; ADD R0,Rn
	CLC
	LDA REG
	ADC REG,X
	STA REG
	LDA REG+1
	ADC REG+1,X
	STA REG+1
	BCC NEXTOP
ADD1:
	LDA STATUS
	ORA #$80
	STA STATUS
	BCS NEXTOP

SUB:	TXA
	TAY
	LDX #0		; SUB R0,Rn
SUB1:
	STX STATUS		; Carry will be ORed in
	SEC
	LDA REG
	SBC REG,Y
	STA REG,X
	LDA REG+1
	SBC REG+1,Y
	STA REG+1,X
	BCC NEXTOP
	BCS ADD1		; Share Carry Set

CPR:	TXA
	TAY
	LDX #13
	BNE SUB1

LDI:
	STY REG+1
	LDA (REG,X)
	STA REG
LDI2:
	STY STATUS 	; R0 is actual target
INR:
	INC REG,X
	BNE +
	INC REG+1,X
+	JMP NEXTOP

LD:	TXA
	TAY
	LDX #0
LD1:	LDA REG,Y		; LD R0,Rn
	STA REG,X
	LDA REG+1,Y
	STA REG+1,X
	JMP NEXTOP

ST:	LDY #0
	BEQ LD1

STPI:				; LD (--Rn),R0
	LDA REG,X
	BNE +
	DEC REG+1,X
+	DEC REG,X
	LDA REG
	STA (REG,X)
STPI1:
	STY STATUS		; Branch conditions reflects R0
	JMP NEXTOP

DCR:	LDA REG,X		; --Rn
	BNE +
	DEC REG+1,X
+	DEC REG,X
	JMP NEXTOP

POPI:
	LDA REG,X		; LD R0,(--Rn)
	BNE +
	DEC REG+1,X
+	DEC REG,X
	LDA (REG,X)
	STA REG
	STY REG+1
	JMP NEXTOP

POPDI:			; LDD R0,(--Rn)
	LDA REG,X
	BNE +
	DEC REG+1,X
+	DEC REG,X
	LDA (REG,X)
	PHA
	LDA REG,X
	BNE +
	DEC REG+1,X
+	DEC REG,X
	LDA (REG,X)
	STA REG
	PLA
	STA REG+1
	JMP STPI1		; Share STP @Rn exit

STI:		; 
	LDA REG		; LD (Rn++), R0	# Byte
	STA (REG,X)
	JMP LDI2

LDDI:				; LDD R0,(Rn++)
	LDA (REG,X)
	STA REG
	INC REG,X
	BNE +
	INC REG+1,X
+	LDA (REG,X)
	STA REG+1
	JMP LDI2

STDI:				; LDD (Rn++),R0
	LDA REG
	STA (REG,X)
	INC REG,X
	BNE +
	INC REG+1,X
+	LDA REG+1
	STA (REG,X)
	JMP INR

SET:				; LDD Rn,#OP1:OP2
	LDY #1
	LDA (IP),Y
	STA REG,X
	INY
	LDA (IP),Y
	STA REG+1,X
	TYA
	SEC
	ADC IP
	STA IP
	BCC +
	INC IP+1
+	JMP NEXTOP1

!source "sweeter16_code.asm"
!eof
: