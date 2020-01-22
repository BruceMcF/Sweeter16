!to "swtr16p2.bin", cbm
!cpu 65c02
;--------1--------2--------3--------4--------5--------6---
; Sweeter16: A 65c02 Implmentation of a Virtual Machine
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

; Since Swift16 is not restricted to op codes residing in
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
; is to run faster on a 65C02 system. While some 
; crunching via "BRA" is done, heavy crunching via 
; subroutines of 8byte or shorter operations is avoided.

; This "2P" version, for "Two Page", implements its parsing
; with shifts, to be smaller, though slower, than the 
; version that has one parsing function using a page 
; table (which on its own is over half the size of 
; Steve Wozniak's entire implementation).

; Version 0.0.2 Partly Untested Code

REG		= $2		; R0
CALLV		= REG+22	; R11
STACK		= REG+24	; R12
STATUS	= REG+29	; HIGH Byte of R14
IP		= REG+30	; R15

; Save/Restore A/X/Y/P
REGA = IP+2
REGX = REGA+1
REGY = REGX+1
REGP = REGY+1


* = $CC00	; assemble to C64 Golden RAM for testing
		; This is VICE emulation of a 65C816
		; (SuperCPU) C64 in 65C02 emulation mode.

; * = $04000	; assemble to CX16 Golden RAM for testing

SWEETER16:
	JSR PUTSTATE
	PLA
	STA IP		;(IP) uses 6502 Return Address
	PLA 			;6502 RtnAdd = Actual-1
	STA IP+1
	JMP NEXTOP

BROPS:
	; I use Wozniak's pseudo-ops
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
	BRA BR
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
	BRA BAX

BNM1	ASL
	TAX
	LDA #$FF
	BRA BNAX

; Adjust Stack and R0 uses same offset logic
; As Branch, just different registers

ADJ0:	LDX #0
	BRA OFFSET

ADJS:	LDX #(STACK-REG)
	BRA OFFSET

BRANCH:
	LDA (IP)
	ASL			;A is actually the OP index
	TAX			
	INC IP		;Now increment IP, since
	BNE  +		;actual operand is in following byte
	INC  IP+1
+	LDA  STATUS	;Holds Register*2+Carry
	LSR			;Carry flag -> Carry, Prior Reg in A
	JMP (BROPS,X)	;Indexed jump to Branch OPS

; Branch on condition codes in front of NEXTOP and BR so they
; can branch either way

BNC:	BCC BR	  	;Branch on Carry Clear
	BCS NEXTOP

BC:	BCS BR		;Branch on Carry Set
	BCC NEXTOP

;The previous register number is in A, it must be
;doubled for all users of the previous register
;index, so the X register indexed by word values.

BP:	ASL
	TAX
	LDA REG+1,X	;Check sign
	BPL BR		;Branch on positive
	BMI NEXTOP

BM:	ASL
	TAX
	LDA REG+1,X	;Check sign
	BMI BR		;Branch on negative
	BPL NEXTOP

BZ:	ASL
	TAX
	LDA #0
BAX:
	CMP REG,X		;Check zero
	BNE NEXTOP
	CMP REG+1,X
	BEQ BR
	BNE NEXTOP

BNZ:	ASL
	TAX
	LDA #0
BNAX:
	CMP REG,X		;Check zero
	BNE BR
	CMP REG+1,X
	BEQ NEXTOP
	BNE BR

; Branch to Subroutine falls through to BR
; BR falls through to NEXTOP

BS:	LDA IP
	STA (STACK)
	INC STACK
	BNE +
	INC STACK+1
	LDA IP+1
	STA (STACK)
	INC STACK
	BNE BR
	INC STACK+1
BR:
	LDX #(IP-REG)	; Index to IP
OFFSET:
	LDY #0		;16bit sign extension
	LDA  (IP)		;Branch Offset
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
+	LDA (IP)		; if([(++IP)]&&F0h)
	AND #$F0
	BEQ BRANCH
	LSR
	LSR
	LSR
	TAX
	LDA (IP)
	AND #$0F		; *2 = Reg if OP, BROP if BROP
	ASL
	STA STATUS		; This is the register index operand
				; Carry clear for each main OP
	JMP (OPS-2,X)	; Minimum X=2, since X=0 => BRANCH

ADD:	STZ STATUS
	TAX			; ADD R0,Rn
	CLC
	LDA REG
	ADC REG,X
	STA REG
	LDA REG+1
	ADC REG+1,X
	STA REG+1
ADD1:	ROL STATUS
	BRA NEXTOP

LD:	TAY
	LDX #0
LD1:	LDA REG,Y		; LD R0,Rn
	STA REG,X
	LDA REG+1,Y
	STA REG+1,X
	BRA NEXTOP

DCR:	
	TAX			; Rn<-Rn-1
	LDA REG,X
	BNE +
	DEC REG+1,X
+	BRA NEXTOP		; For DCR function

STPI:				; LD (--Rn),R0
	TAX
	LDA REG,X
	BNE +
	DEC REG+1,X
+	DEC REG,X
	LDA REG
	STA (REG,X)
STPI1:
	STZ STATUS		; Branch conditions reflects R0
	BRA NEXTOP

LDI:				; LD R0,(Rn++) # Bytey
	TAX
LDI1:
	STZ REG+1
	LDA (REG,X)
	STA REG
LDI2:
	STZ STATUS 	; R0 is actual target
INRX:
	INC REG,X		; The Rn++ part
	BNE +
	INC REG+1,X
+	BRA NEXTOP
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; ~~ * NEXTOP must be no more than ~~~
; ~~ 127 bytes from here ~~~~~~~~~~~~~
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; ~~ These piggbyback their branch ~~~
; ~~ back from the routines they ~~~~~
; ~~ borrow their finish from. ~~~~~~~
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUB:	LDX #0		; SUB R0,Rn
CPR:	TAY			; CPR opcode = R13 index
	TXA
	LSR
	STA STATUS		; Carry will be shifted in
	SEC
	LDA REG
	SBC REG,Y
	STA REG,X
	LDA REG+1
	SBC REG+1,Y
	STA REG+1,X
	BRA ADD1			; Share ADD exit

ST:	TAX
	LDY #0
	BRA LD1

POPDI:			; LDD R0,(--Rn)
	TAX
	LDA REG,X
	BNE +
	DEC REG+1,X
+	DEC REG,X
	LDA (REG,X)
	TAY
	LDA REG,X
	BNE +
	DEC REG+1,X
+	DEC REG,X
	LDA (REG,X)
	STA REG
	STY REG+1
	BRA STPI1		; Share STP @Rn exit

POPI:
	TAX
	LDA REG,X		; LD R0,(--Rn)
	BNE +
	DEC REG+1,X
+	DEC REG,X
	BRA LDI1

STI:		; 
	TAX
	LDA REG		; LD (Rn++), R0	# Byte
	STA (REG,X)
	BRA LDI2

LDDI:				; LDD R0,(Rn++)
	TAX
	LDA (REG,X)
	STA REG
	INC REG,X
	BNE +
	INC REG+1,X
+	LDA (REG,X)
	STA REG+1
	BRA LDI2

STDI:				; LDD (Rn++),R0
	TAX
	LDA REG
	STA (REG,X)
	INC REG,X
	BNE +
	INC REG+1,X
+	LDA REG+1
	STA (REG,X)
	BRA INRX

INR:	TAX
	BRA INRX

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; SET, RS, BK and CALL end in jumps ~~
; not bound by -125/+128 BRA limits ~~
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SET:				; LDD Rn,#OP1:OP2
	TAX
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
	LDA (STACK)
	STA IP
	JMP NEXTOP

BK:	BRK

RTN:	JSR GETSTATE
	JMP  (IP)	;Go Back to 65C02 code

!source "sweeter16_code.asm"
!eof
: