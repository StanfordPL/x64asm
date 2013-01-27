Build Instructions:

0. Upgrade to ubuntu 12.04 LTS.

1. Dependency hell!

$ sudo apt-get install ccache
$ sudo apt-get install g++
$ sudo apt-get install g++-multilib
$ sudo apt-get install doxygen
$ sudo apt-get install ghc
$ sudo apt-get install cabal
$ sudo apt-get install libghc-split-dev
$ sudo apt-get install graphviz

TODO: Add more dependencies here as we discover them.

2. Type make.
   You can optionally specify a build type.

$ make (release|profile|debug)

3. Browse the (no longer totally) woefully incomplete documentation.

$ <browser> doc/html/index.html

4. Write and compile some code.

// code.cc
#include "<path/to/here>/include/x64.h"

int main() {
	...
}

$ g++ code.cc -I<path/to/here> <path/to/here>/lib/x64.a

-------------------------------------------------------------------------------

x86-64 quirks:

 * esp/rsp cannot be used as an index for looking up stuff from memory.

 * g++ allows jumps to undefined labels.  
   These are emitted as a 32-bit relative displacement of 0.

	 .foo:      <nothing>
	 jns .bar   e9 00 00 00 00
	 retq       c3

 * Deciding between the 8- and 32-bit relative displacement forms is
   known as the branch displacement problem.
	 The primary consequence of algorithm choice seems to be code size.
	 I *think* that gcc uses an iterative algorithm which initially assumes
	 8 bit offsets and then adjusts as necessary.
	 For this go around, I think we'll always emit 32-bit in the interest of speed.

-------------------------------------------------------------------------------

Spreadsheet modifications:

Unless otherwise mentioned below, the contents of x64.ods are copied directly
	from the Intel Manual.

LAHS/SAHF:

	These instructions are conditionally invalid in 64-bit mode based on processor
		model.
	We list these as valid.

CMPS/CMPSB/CMPSW/CMPSD/CMPSQ
INS/INSB/INSW/INSD
LODS/LODSB/LODSW/LODSD/LODSQ
MOVS/MOVSB/MOVSW/MOVSD/MOVSQ
OUTS/OUTSB/OUTSW/OUTSD
SCAS/SCASB/SCASW/SCASD/SCASQ
STOS/STOSB/STOSW/STOSD/STOSQ

	Each instruction in this group takes an optional memory argument(s) 
		which are ignored except for the purpose of distinguishing operand width,
		ie: CMPS M8 = CMPSB, and for specifying a memory address prefix (67),
		which affects the implicit memory operand.

	Note that for the short form of these instructions, we add a PREF.66+
		annotation, as there is no operand to induce this value.

	Although in principle, the short form versions of these instructions can
		be modified by the addition of a memory address	prefix (67), we
		ignore this possibility and require the user to use the long form.

	The Intel manual omits any mention of the memory address prefix (67) for
		the LODS family of instructions.  This is likely a typo, and we support 
		this behavior as per the other instructions in this family.	

CWD/CDQ/CQO
POPF
PUSHF

	Each of the instructions in this group require a word prefix (66) to
		distinguish the word version of the instruction from the otherwise
		identical long version.
	Because there are no operands to infer this prefix, we add it
		explicitly.

POPA
PUSHA

	Each of the instructions in this group would have a similar problem
		to the above.
	However, as these are unsupported in 64-bit mode, we ignore these.

IRET/IRETD/IRETQ

	Each of the instructions in this group APPEAR as though they would
		have a similar problem to the above.
	But in fact, they don't.
	We've left them unmodified.

LEAVE
MOV ([mem8] AL/AL [mem8])
POP
RET
SYSEXT
SYSRET

	Each of the instructions in this group suffer from ambiguity
		due to lack of mnemonic variation.
	Furthermore, some of the instructions in this group, even if they
		could be disambiguated by mnemonic, require prefix bytes which
		cannot be infered due to lack of operand.
	We add both an explicit annotation and operand to distinguish these cases.

	The Intel manual is unclear on what adding the REX.w+ prefix to
		MOV [mem8] AL accomplishes, but we support this behavior nonetheless.

XLATB

	The rex.w prefix is a meaningless distinction in 64-bit mode.
	Any arbitrary choice of encoding will suffice (see below).

REP/REPE/REPZ/REPNE/REPNZ

	The REX.w prefix has no control over count register for these instructions.
	It functions simply to disambiguate the 64-bit operand version of some
		versions of the instruction.
	Count register width is controled by the address width prefix which follows
		from the long form version of each instruction.
	Note that in contrast to the non-repeated versions of these instructions,
		no short form is given.

	It seems likely that there is a typo in the Intel manual, and that the first
		five rows (REP INS) are missing REX.w+ prefix annotations.
	We have added these.
	Furthermore, we note that the fifth entry for REP INS and REP OUTS 
	  are the only REP instruction which the Intel manual allows 
		register operands for.
  This is asymmetric with the INS / OUTS documentation and likely a bug.
	  we have removed these register operands.

	In either case, where the REX.w prefix isn't used to disambiguate a 64 bit
		operand form, it's use is meaningless.
	For these rows, any arbitrary choice of encoding will suffice (see below).

	Note that we have also added underscores to these mnemonics to maintain
		the one-word-per-mnemonic invariant.

Codegen Notes:

Memory Types:
	
	In many cases, the only thing distinguishing two otherwise identical
		instructions is operand type.
	Furthermore, certain operand types (ie. M16) are required for infering
		prefix bytes.
	We account for this by introducing a distinct memory type for each 
		operand type appearing in the Intel manual.
	Barring these requirements, a single memory type would certainly
		simplify things.

REX+

	Any instruction prefixed with REX+ has at least one 8-bit operand.
	We identify these instructions and replace them with a variant
		in which all instances of r8 are replaced by [PUT A TYPENAME HERE]
		which indicates the use of AH/BH/CH/DH or AL/BL/CL/DL.
	This is a function of the x86 hardware, which prevents the user from
		specifying the use of AH/BH/CH/DH at the same time as an operand
		or mnemonic which requires the use of a REX prefix.
	As a result, opting to use any of these high registers for any
		operand implies that any other r8 registers must be drawn from
		AH/BH/CH/DH/AL/BL/CL/DL.	

	Technically, this behavior is associated with the non-prefixed versions
		of the instructions.
	However these versions always come in pairs, and it's easier to identify
		and modify the versions that use the prefix.
	Either way, the result is the same.

Ambiguity:

	Even after we have (1) added the annotations described above, (2) ignored
		the meaningless rows described above, and (3) rewritten rows
		that use the REX+ prefix, we are still left with rows which are 
		indistinguishable up to mnemonic and operand.
	These ambiguities represent a distinction without a difference.
	They are alternate hardware methods for performing the same operation,
		which as far as we know, have no noticable performance tradeoffs.
	
	We remove this redudancy by eliminating all but one row from each
		such equivalence class.
	In the interest of encoding efficiency, preference is given to 
		variants with shorter encodings.

TODO:
	REMEMBER: You will need 66 prefixes for ALL 16-bit operand instructions.
	          r16/m16/AX/imm16 ... more?

-------------------------------------------------------------------------------

TODO items:

assembler

- Vex Instructions

cfg

- live in/out for locs
- def in/out for locs

io

- ElfWriter
- IntelWriter
- AttReader
- IntelReader

Testbench

- ...

