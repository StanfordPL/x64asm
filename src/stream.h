#ifndef X64ASM_SRC_STREAM_H
#define X64ASM_SRC_STREAM_H

#include <iostream>
#include <stdint.h>

#include "src/code.h"
#include "src/instruction.h"
#include "src/op_set.h"
#include "src/operand.h"

namespace x64asm {

/** I/O syntax for iostreams. */
enum class Syntax : uint32_t {
	ATT = 0,
	INTEL
};

} // namespace x64asm

std::istream& operator>>(std::istream& is, const x64asm::Syntax s);
std::ostream& operator<<(std::ostream& os, const x64asm::Syntax s);

std::istream& operator>>(std::istream& is, x64asm::Code& c);

std::ostream& operator<<(std::ostream& os, const x64asm::Code& c);
std::ostream& operator<<(std::ostream& os, const x64asm::Instruction& i);
std::ostream& operator<<(std::ostream& os, const x64asm::Operand& o);
std::ostream& operator<<(std::ostream& os, const x64asm::OpSet& o);

#endif
