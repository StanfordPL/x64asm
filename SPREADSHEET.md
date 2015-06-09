Spreadhsset
=====

### What?

*The* spreadsheet, `x86.csv` is a list of all x86-64 opcodes that we support.
Every time you build a fresh copy of x64asm, this data is used by "Codegen" (a
haskell program) to generate C++ source for the real assembler.  The
intention is that the spreadsheet is very similar to the contents of the intel
x86-64 manuals.

### Spreadsheet Columns

The "Opcode" column has the hex opcode as specified in the Intel manual, along
with the optional prefix bytes like REX, REX.W+, etc.  There are also other
annotations from the manual; Codegen uses these but BRC doesn't know what all
of them do.

The "Instruction" field specifies a textual opcode and the types of each
argument.  Some of the argument types are listed below.  There are others that
are obscure and rarely come up when using STOKE, and they aren't listed here.
 * r8, r16, r32, r64: a register operand
 * m8, m16, m32, m64, m128, m256: a memory operand
 * r/m8, r/m16, r/m32, r/m64: a register or memory operand
 * imm8, imm16, imm32, imm64: an immediate
 * xmm1, xmm2, xmm3,...: *any* XMM register
 * ymm1, ymm2, ymm3,...: *any* YMM register
 * rel8, rel16, rel32: a relative offset for a jump target
 * AL, AX, EAX, RAX, CL, DX, XMM0: These are specific registers

The "Op/En" column specifies how the operands will be encoded.

"Properties" lists, for each operand, if it's read to, written to, or both.  Each operand gets a 'tag', and the tags are comma-separated.  Possible tags include,
 * R: this operand must be read
 * W: this operand must be written
 * RW: this operand must be read and written
 * Z: the bits extending this operand are cleared, and the rest are written to.  Comes up with xmm/ymm registers.
 * Lowercase letters represent *may* instead of *must*.  For example, 'r' means 'this operand *may* be read'.


"Implicit Read/Write/Undef" lists registers and flags that are read/written/or
undefined in excess of the operands themselves.  Entries in these colums are space-separated, and valid entries include,
 * The eflags: E.OF, E.SF, E.ZF, E.AF, E.CF, E.PF
 * Registers: RAX, RDX, RCX, RBX, RDI, RSI, RBP, RSP, R8-R15
 * XMM/YMM registers: XMM0...XMM15, YMM0...YMM15
 * Lowercase means "may", uppercase means "must"

The "Useful", "Protected", "64-bit Mode", "Compact/32-bit Legacy Mode" columns are used to filter which instructions are actually used.

The "CPUID Feature Flags" tell us what flags we need on the CPU to run the instruction.

The "AT&T Mnemonic" gives the ATT name for the instruction.

The "Preferred" column is now legacy.  With small changes to codegen it would be possible to use this column to prioritize some instruction encodings over others.

"Description" describes what the instruction does.

### Opening in LibreOffice

If you open the spreadsheet, you should get a dialog asking how to read the
.csv file.  You want to use the defaults for the character set (Unicode /
    UTF-8) and language (English).  Then, select the "Separated by" option, and
check only the 'tab' box.  There's also a "text delimiter" field; this should
be left blank.

