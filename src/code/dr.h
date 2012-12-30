#ifndef X64_SRC_CODE_DR_H
#define X64_SRC_CODE_DR_H

#include "src/code/operand.h"

namespace x64 {

/** A debug register. */
class Dr : public Operand {
	public:
		inline Dr(uint64_t val) : Operand{val} { }
};

} // namespace x64

#endif
