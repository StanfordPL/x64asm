x64asm
=====

x64asm is a c++11 library for working with x86_64 assembly. It provides a parser, in-memory assembler and linker, and primitives for building data flow analyses. x64asm was built with the following design goals in mind:

- __Simplicity:__ x64 asm does NOT include a register allocator, instruction scheduler, control flow graph builder, or any of the features you would expect of a full compiler. It is a low-level library for building YOUR optimizing compiler.

- __Completeness:__ x64asm supports the entire ring 3 application level subset of the x86_64 instruction set, including the most recent AVX2/BMI1/BMI2/FMA extensions.

- __Correctness:__ The majority of the source in the x64asm repository is auto-generated using a declarative specification.  This means that bugs can be fixed quickly without major modifications to its codebase.

### Supported Platforms:

- Ubuntu 12.04 LTS
- Ubuntu 13.10
- Ubuntu 14.04 LTS

### Getting started:

Dependencies are available through `apt-get`.
   
    $ sudo apt-get install bison ccache doxygen flex g++ g++-multilib ghc libghc-regex-tdfa-dev libghc-regex-compat-dev libghc-split-dev

To build x64asm, type:

    $ make (release|profile|debug)

For examples of how to use the library, browse the `examples/` folder:

- `abi.cc` How the assembler API interacts with the linux ABI.
- `constants.cc` Shows off the set of built-in assembler constants.
- `context.cc` How to assemble functions that modify program state.
- `dataflow.cc` How to use the dataflow API.
- `functions.cc` How to assemble functions that call other functions.
- `hello.cc` How to use the assembler API; look here first.
- `linker.cc` How to assemble functions with external linkage.

And to use x64asm as an assembler from the command line, type:
    
    $ cat test.s | <path/to/here>/bin/asm 

To use x64asm as part of a larger project, include the header:

```c++
#include "<path/to/here>/include/x64asm.h"

int main() {
  // ...
  return 0;
}
```

and link against the library:

    $ g++ code.cc -I<path/to/here> <path/to/here>/lib/x64.a

#### Undefined Assembler Behavior

Jumps to undefined labels are handled by emitting a 32-bit relative 
displacement of 0x00000000.

#### Assembler Simplifications

Deciding between the 8- and 32-bit relative displacement forms of jump instructions is known as the (NP hard) branch displacement problem. The primary consequence of this decision is code size. Most compilers solve this problem using an iterative algorithm which initially assumes 8-bit displacements and then adjusts as necessary. We emit all jumps to labels using the 32-bit form.

#### Memory Types
	
In many cases, the only thing distinguishing two otherwise identical instructions is operand type. Furthermore, certain operand types (ie. M16) are required for infering prefix bytes. We account for this by introducing a distinct memory type for each operand type appearing in the Intel manual. Barring these requirements, a single memory type would simplify our implementation.

#### Ambiguity

The x86_64 instruction set contains many instructions that are indistinguishable up to mnemonic and operand. These ambiguities represent a distinction without a difference. They are alternate hardware methods for performing the same operation, which as far as we know, have no noticable performance tradeoffs. We remove this redudancy by choosing the encoding preferred by g++.

#### Intel Eratta

Most of the source code in this project is automatically generated using the the x64.csv spreadsheet.  Unless otherwise noted below, the contents of the spreadsheet are transcribed directly from the Intel manuals. If you discover an error, or an edit which has not been documented below, please submit an error report.

#### String Instructions

Each instruction in this class takes an optional memory argument(s) which is (are) ignored except for the purpose of distinguishing operand width, ie: CMPS M8 = CMPSB, and for specifying a memory address prefix (67), which affects the implicit memory operand. Note that for the short form of these instructions, we add a PREF.66+ annotation, as there is no operand to induce this value.

Although in principle, the short form versions of these instructions can be modified by the addition of a memory address	prefix (67), we ignore this possibility and require the user to use the long form.

The Intel manual omits any mention of the memory address prefix (67) for the LODS class of instructions.  This is likely a typo, and we support the behavior per the other instructions in this class.	

#### CWD/CDQ/CQO/POPF/PUSHF

Each of the instructions in this class require a word prefix (66) to distinguish the word version of the instruction from the otherwise identical long version. Because there are no operands to infer this prefix, we add it explicitly.

#### POPA/PUSHA

Each of the instructions in this class would have a similar problem to the above. However, as these are unsupported in 64-bit mode, we ignore these.

#### IRET/IRETD/IRETQ

Each of the instructions in this class APPEAR as though they would have a similar problem to the above, but don't. We've left them unmodified.

#### LEAVE/MOV/POP/RET/SYSEXT/SYSRET

Each of the instructions in this class suffer from ambiguity due to lack of mnemonic variation. Furthermore, some of the instructions in this class, even if they could be disambiguated by mnemonic, require prefix bytes which cannot be infered due to lack of operand. We add both an explicit annotation and operand to distinguish these cases. The Intel manual is unclear on what adding the REX.w+ prefix to MOV [mem8] AL accomplishes, but we support this behavior nonetheless.

#### XLATB

The rex.w prefix is a meaningless distinction in 64-bit mode.  As a result, we do not support a distinction between the latter two variants of this instruction.

#### REP/REPE/REPZ/REPNE/REPNZ

The REX.w prefix has no control over count register for these instructions. It functions simply to disambiguate the 64-bit operand version of some versions of the instruction. Count register width is controled by the address width prefix which follows from the long form version of each instruction. Note that in contrast to the non-repeated versions of these instructions, no short form is given.

In either case, where the REX.w prefix isn't used to disambiguate a 64 bit operand form, it's use is meaningless. For these rows, any arbitrary choice of encoding will suffice.

Note that we have also added underscores to these mnemonics to maintain the one-word-per-mnemonic invariant.

#### Dataflow Values

Some dataflow information is missing and/or incomplete. If you discover an error, please submit a bug report.

#### Further documenation

See SPREADSHEET.md for the tiny bit of documentation on the internals that we
have, which is focused on the x86.csv file.
