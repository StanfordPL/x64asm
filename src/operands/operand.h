#ifndef X64_SRC_OPERANDS_OPERAND_H
#define X64_SRC_OPERANDS_OPERAND_H

#include <stdint.h>

namespace x64 {

/** Base operand type. */
class Operand {
	friend class Assembler;
	friend class AttWriter;
	friend class Checker;
	friend class Instruction;
	friend class M;
	protected:
		inline Operand(uint64_t val) : val_{val} { }
		uint64_t val_;	
};

} // namespace x64

#endif
