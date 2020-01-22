# Sweeter16
A New implementation of Steve Wozniak's Sweet16 Virtual Machine for the 65C02/65C816.

"Sweet16" is Steve Wozniak's compact virtual machine for concisely handling 16bit data on the 6502 processor. It is embedded in 6502 code by making a call to the Sweet16 subroutine and then executing the following embedded Sweet16 code. When the Sweet16 RTN instruction is executed, execution returns to the following 6502 code.

32 bytes of Zero Page space are treated as 16 two byte register, R0-R15, with R0 serving as an accumulator, R15 as an instruction pointer, R14 as a status register, R13 as a comparison result register, and R12 as a stack register. 15 Register operations include direct byte from and load to R0, autommatic post-incrementing indrect byte and word store, pre-decrement word load and store (to allow building both upward and downward facing stacks), addition, subtraction, and comparison ... which is just subtraction with the result stored in the comparison register R13. Branching operations take a signed byte operand, for branch, branch on previous operation carry clear or set, branch on previous operation positive, negative, zero, not zero, negative one, or not negative one. Branching operations also include branch to subroutine, return from subroutine, and return from Sweet16 back to 6502 code.  

David Murray (aka "The Eight Bit Guy") has mentioned an interest in including the Sweet16 VM on his CX16 "Dream Computer".

However, while the original code for the Sweet16 is widely available, it is copyrighted Apple code. Further, it is written for the original NMOS 6502 processor, while the CX16 will feature either the 65C02 or 65C816. And Wozniak's Sweet16 makes a number of sacrifices of operating speed in order to achieve extremely (and extremely clever) compact code.

The intention of this implementation is to provide a fresh, open-source, implementation of a 65C02 VM to execute Sweet16 code. Rather than executing an operation by building a subroutine return vector on the stack and returning to it, this VM uses the JMP(abs,X) X-indexed jump operation.

To see the appeal of this approach, consider a generic VM written along the lines of Wozniak's Sweet16: before parsing, the high byte of the operation address is pushed to the stack (+2 clocks to load immediate, +3 clocks to push), and after parsing, having an index in X, the low byte is retrieved (+5), pushed to the stack (+3), a return from subroutine is executed (+6), and then when the operation is completed, it executes a return (+6) to the main loop which jumps (+3) to the subroutine call to the parsing routine (+5), for something on the order of 33 clocks.

By contrast, with the X indexed jump, having an index in X, the X-indexed JUMP is executed (+6), and then when the operation completes, it Branches or Jumps back to the parsing routine (+3), for on the order of 9 clocks.

As a further optimization, one version of the Sweeter16 VM implements the parsing of one type of code using a page table. The processing speed improvement is modest compared to the gain in the main VM loop, so a two-page (<=512 bytes) version that does not use a page table is provided along with a three-page (<=768 byte) version that includes a page table.
