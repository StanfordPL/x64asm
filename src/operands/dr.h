#ifndef X64_SRC_OPERANDS_DR_H
#define X64_SRC_OPERANDS_DR_H

#include "src/operands/operand.h"

namespace x64 {

/** A debug register. */
class Dr : public Operand {
	friend class Constants;
	private:
		inline Dr(uint64_t val) : Operand{val} { }
};

} // namespace x64

#endif
