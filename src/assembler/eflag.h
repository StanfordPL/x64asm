#ifndef X64_SRC_CODE_EFLAG_H
#define X64_SRC_CODE_EFLAG_H

#include "src/code/operand.h"

namespace x64 {

/** An EFLAGS register bit. */
class Eflag : public Operand {
	friend class Constants;
	private:
		inline Eflag(uint64_t val) : Operand{val} { }
};

} // namespace x64

#endif
