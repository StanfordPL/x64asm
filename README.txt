Copyright 2103 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-------------------------------------------------------------------------------

x64asm


-------------------------------------------------------------------------------

Supported Platforms:

Ubuntu 12.04 LTS
Mac OSX 10.8.x

-------------------------------------------------------------------------------

Build Instructions:

1. Dependency hell!

Satisfy the following package dependencies.
Equivalents are all available using MacPorts.

$ sudo apt-get install ccache
$ sudo apt-get install g++
$ sudo apt-get install g++-multilib
$ sudo apt-get install ghc
$ sudo apt-get install cabal
$ sudo apt-get install libghc-split-dev

2. Type make (specify a build type to override the default 'release')

$ make (release|profile|debug)

3. Browse the documentation.

$ <browser> doc/html/index.html

4a. Play around with the command line app.

$ x64asm -i att -o hex test.s

4b. Write and compile some code.

// code.cc
#include "<path/to/here>/include/x64asm.h"

int main() {
	...
}

$ g++ code.cc -I<path/to/here> <path/to/here>/lib/x64.a

-------------------------------------------------------------------------------

Undefined Assembler Behavior:

Jumps to undefined labels are handled per g++ by emitting a 32-bit relative
displacement of 0x00000000.

-------------------------------------------------------------------------------

Assembler Simplifications:

Deciding between the 8- and 32-bit relative displacement forms of jump 
instructions is known as the (NP hard) branch displacement problem. The primary
consequence of the decision is code size. Most compilers solve this problem 
using an iterative algorithm which initially assumes 8-bit displacements and
then adjusts as necessary. As our design criteria is speed, we simply emit all 
jumps to labels using the 32-bit form.

For certain operand combinations, it is possible to emit VEX instructions
using a short 2-byte prefix.  Any 2-byte prefix can also be emitted using an
equivalent version of the standard 3-byte prefix form.  As our design criteria
is speed, we always use the 3-byte form.

See (Codegen Notes -- Ambiguity) for notes on instructions with multiple
encodings.

-------------------------------------------------------------------------------

Intel Specification Modifications:

Most of the source code in this project is automatically generated using the
the x64.ods spreadsheet.  Unless otherwise noted below, the contents of the 
spreadsheet are transcribed directly from the Intel Manual.

LAHS/SAHF:

These instructions are conditionally invalid in 64-bit mode based on processor
model. We list these as valid.

CMPS/CMPSB/CMPSW/CMPSD/CMPSQ
INS/INSB/INSW/INSD
LODS/LODSB/LODSW/LODSD/LODSQ
MOVS/MOVSB/MOVSW/MOVSD/MOVSQ
OUTS/OUTSB/OUTSW/OUTSD
SCAS/SCASB/SCASW/SCASD/SCASQ
STOS/STOSB/STOSW/STOSD/STOSQ

Each instruction in this class takes an optional memory argument(s) which is 
(are) ignored except for the purpose of distinguishing operand width, ie: 
CMPS M8 = CMPSB, and for specifying a memory address prefix (67), which affects 
the implicit memory operand. Note that for the short form of these 
instructions, we add a PREF.66+ annotation, as there is no operand to induce 
this value.

Although in principle, the short form versions of these instructions can
be modified by the addition of a memory address	prefix (67), we ignore this 
possibility and require the user to use the long form.

The Intel manual omits any mention of the memory address prefix (67) for the 
LODS class of instructions.  This is likely a typo, and we support the behavior 
per the other instructions in this class.	

CWD/CDQ/CQO
POPF
PUSHF

Each of the instructions in this class require a word prefix (66) to 
distinguish the word version of the instruction from the otherwise identical 
long version. Because there are no operands to infer this prefix, we add it
explicitly.

POPA
PUSHA

Each of the instructions in this class would have a similar problem to the 
above. However, as these are unsupported in 64-bit mode, we ignore these.

IRET/IRETD/IRETQ

Each of the instructions in this class APPEAR as though they would have a 
similar problem to the above. But in fact, they don't. We've left them 
unmodified.

LEAVE
MOV ([mem8] AL/AL [mem8])
POP
RET
SYSEXT
SYSRET

Each of the instructions in this class suffer from ambiguity due to lack of 
mnemonic variation. Furthermore, some of the instructions in this class, even 
if they could be disambiguated by mnemonic, require prefix bytes which cannot 
be infered due to lack of operand. We add both an explicit annotation and 
operand to distinguish these cases.

The Intel manual is unclear on what adding the REX.w+ prefix to
MOV [mem8] AL accomplishes, but we support this behavior nonetheless.

XLATB

The rex.w prefix is a meaningless distinction in 64-bit mode.  As a result, we 
do not support a distinction between the latter two variants of this
instruction.

REP/REPE/REPZ/REPNE/REPNZ

The REX.w prefix has no control over count register for these instructions. It 
functions simply to disambiguate the 64-bit operand version of some versions of 
the instruction. Count register width is controled by the address width prefix 
which follows from the long form version of each instruction. Note that in 
contrast to the non-repeated versions of these instructions, no short form is 
given.

It seems likely that there is a typo in the Intel manual, and that the first 
five rows (REP INS) are missing REX.w+ prefix annotations. We have added these.
Furthermore, we note that the fifth entry for REP INS and REP OUTS  are the 
only REP instruction which the Intel manual allows register operands for. This 
is asymmetric with the INS / OUTS documentation and likely a typo. we have 
removed these register operands.

In either case, where the REX.w prefix isn't used to disambiguate a 64 bit 
operand form, it's use is meaningless. For these rows, any arbitrary choice of 
encoding will suffice.

Note that we have also added underscores to these mnemonics to maintain
the one-word-per-mnemonic invariant.

SETxx

The SETxx class of instructions is missing the /0 register code annotation.
This has been added.

AND

The op/en values for this instruction are likely incorrect and have been 
modified to match those from ADC,SBB,etc.

MOV

The %cr8 variants of this instruction are annotated with a /0
register code, which is technically correct, those inconsistent with the 
rest of the manual with respect to codegen.  We have promoted this annotation
to the more general, though still correct, /r.

Misc:

Various small typos, such as missing spaces in the opcode column have been
silently fixed.

-------------------------------------------------------------------------------

Codegen Notes:

Memory Types:
	
In many cases, the only thing distinguishing two otherwise identical 
instructions is operand type. Furthermore, certain operand types (ie. M16) are 
required for infering prefix bytes. We account for this by introducing a 
distinct memory type for each operand type appearing in the Intel manual. 
Barring these requirements, a single memory type would certainly simplify 
things.

REX+:

Any instruction prefixed with REX+ has at least one 8-bit operand.
We identify these instructions and replace them with as many variants as are
necessary to represent all possible combinations of replacements by elements
in the sets {rl,rb} and {rl,rh}. This follows from the x86 hardware, which
prevents the user from specifying the simultaneous use of rh registers at the
same time as operand or mnemonic which requires a REX prefix. As a result, 
opting to use any of these high registers for any operand implies that any 
other r8 registers must be drawn from {rl,rh}.

Ambiguity:

Even after we have (1) added the annotations described above, (2) ignored 
the meaningless rows described above, and (3) rewritten rows that use the REX
+ prefix, we are still left with rows which are indistinguishable up to 
mnemonic and operand. These ambiguities represent a distinction without a 
difference. They are alternate hardware methods for performing the same 
operation, which as far as we know, have no noticable performance tradeoffs.
	
We remove this redudancy by eliminating all but one row from each such 
equivalence class. In the interest of encoding efficiency, preference is given 
to variants with shorter encodings.

-------------------------------------------------------------------------------

TODO:

assembler.h

- write_elf()
- write_intel()

instruction.h

- read_att()
- read_intel()
- write_intel()
- implicit sets()

test/

- Actually write some tests
