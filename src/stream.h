#ifndef X64_SRC_STREAM_H
#define X64_SRC_STREAM_H

#include <iostream>
#include <stdint.h>

#include "src/code.h"
#include "src/instruction.h"
#include "src/op_set.h"
#include "src/operand.h"

namespace x64 {

/** I/O syntax for iostreams. */
enum class Syntax : uint32_t {
	ATT = 0,
	INTEL
};

} // namespace x64

std::istream& operator>>(std::istream& is, const x64::Syntax s);
std::ostream& operator<<(std::ostream& os, const x64::Syntax s);

std::istream& operator>>(std::istream& is, x64::Code& c);

std::ostream& operator<<(std::ostream& os, const x64::Code& c);
std::ostream& operator<<(std::ostream& os, const x64::Instruction& i);
std::ostream& operator<<(std::ostream& os, const x64::Operand& o);
std::ostream& operator<<(std::ostream& os, const x64::OpSet& o);

#endif
