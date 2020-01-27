# Sweeter16
A New implementation of Steve Wozniak's Sweet16 Virtual Machine for the 65C02/65C816.

"Sweet16" is Steve Wozniak's compact virtual machine for concisely handling 16bit data on the 6502 processor. It is embedded in 6502 code by making a call to the Sweet16 subroutine and then executing the following embedded Sweet16 code. When the Sweet16 RTN instruction is executed, execution returns to the following 6502 code.

David Murray (aka "The Eight Bit Guy") has mentioned an interest in including the Sweet16 VM on his CX16 "Dream Computer".

However, while the original code for the Sweet16 is widely available, it is copyrighted Apple code. Further, it is written for the original NMOS 6502 processor, while the CX16 will feature either the 65C02 or 65C816. And Wozniak's Sweet16 makes a number of sacrifices of operating speed in order to achieve extremely (and extremely clever) compact code.

In Sweet16, 16 2-byte locations in  Zero Page space are treated as two byte registers, R0-R15, with R0 serving as an accumulator and most register operations acting between R0 and the specified register. R15 is the instruction pointer, and the high byte of R14 serving as the status register holding an index to the value of the last register operation and a carry bit. There are 15 instructions which have the instruction code in the high four bits and the operand register in the low four bits, making for very compact code. Register instructions include add and subtract, compare, immediate load, and a set of load and store instructions, and while ADD, SUB and CMP set the carry based on the result of the operation, all other register operations clear the carry:
+ SET Rn, word is an immediate load to a register of the following two bytes, in standard 6502 low byte first order
+ ADD Rn and SUB Rn are two's complement add and subtract. SUB Rn has an inverted carry, like the 6502. Both set a carry in the  status register.
+ ICR Rn and DCR Rn increment or decrement the register by one.
+ CMP Rn is a two's complement subtract with 6502 inverted carry, which stores the result, and sets the result register status, to register 13.
+ LD Rn and ST Rn are 16bit load and store between accumulator and the target register.
+ LD @Rn and ST @Rn are 8bit post-increment load from and store to the address pointed to by the register
+ LDD @Rn and STD @Rn are 16bit post-increment load from and store to the address pointed to by the register
+ POP @Rn and STP @Rn ("STore Pop") are 8bit pre-decrement load from and store to the address pointed to by the register.
+ POPD @Rn is a 16bit pre-decrement load from the address pointed to by the register.

If the high four bits of the Sweet16 code is 0, then the lower four bits indicate one of the 16 branch operations. Most of these are followed by a signed 8bit operand which is treated as a sign-extended 16bit operand, and branch instructions work as in the 6502, with the branch relative to the location following the two byte branch code for a range of +129/-128 from the location of the branch instruction. Sweet16 implements 13 of these operations, leaving three operations as NUL, while Sweet16c extends this to include 16 "op 0" operations:
+ BR rel is an absolute branch to the branch target
+ BC rel and BNC rel are branches to the branch target if the carry is set or clear
+ BZ rel and BNZ rel are branches to the branch target if the last register operation value is zero or nonzero
+ BP rel and BM rel are branches to the branch target if the last register operation value is positive or negative
+ BM1 rel and BNM1 rel are branches to the branch target if the last register operation value is -1 or is not -1
+ BS rel is a branch to a subroutine, with the IP stack at r12 and growing up. To make a subroutine call past the limit of the relative branch, branch to a subroutine that does an absolute jump with a "SET R15, target-1".
+ RS is a return from the subroutine
+ BK executes a 6502 machine language BRK instruction. What this does depends on how the BRK vector is set-up -- in the original setting in the early Apple II, it launched a Monitor.
+ RTN ends the Sweet16 virtual code and should be followed by 6502 code.
+ CALL rel is the new operation that calls a 65c02 subroutine at the address in R11, then executes a branch to the target on return, allowing the call to be followed by embedded data accessed via "LDA(IP),Y".
+ ADJ0 rel is the new operation that adds the sign extended offset to the Accumultor register R0
+ ADJS rel is the new operation that adds the sign extended offset to the Stack register R12 

The code values for hand assembly of Sweet16 code is given in Sweeter16_opcodes.txt.

The intention of this implementation is to provide a fresh, open-source, implementation of a 65C02 VM to execute Sweet16 code. Rather than executing an operation by building a subroutine return vector on the stack and returning to it, this VM uses the JMP(abs,X) X-indexed jump operation.

To see the appeal of this approach, consider a generic VM written along the lines of Wozniak's Sweet16: before parsing, the high byte of the operation address is pushed to the stack (+2 clocks to load immediate, +3 clocks to push), and after parsing, having an index in X, the low byte is retrieved (+5), pushed to the stack (+3), a return from subroutine is executed (+6), and then when the operation is completed, it executes a return (+6) to the main loop which jumps (+3) to the subroutine call to the parsing routine (+5), for something on the order of 33 clocks.

By contrast, with the X indexed jump, having an index in X, the X-indexed JUMP is executed (+6), and then when the operation completes, it Branches or Jumps back to the parsing routine (+3), for on the order of 9 clocks.

As a further optimization, one version of the Sweeter16 VM implements the parsing of one type of code using a page table. The processing speed improvement is modest compared to the gain in the main VM loop, so a two-page (<=512 bytes) version that does not use a page table is provided along with a three-page (<=768 byte) version that includes a page table.
