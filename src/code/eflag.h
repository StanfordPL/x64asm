#ifndef X64_SRC_CODE_EFLAG_H
#define X64_SRC_CODE_EFLAG_H

#include "src/code/operand.h"

namespace x64 {

/** An EFLAGS register bit. */
class Eflag : public Operand {
	public:
		inline Eflag(uint64_t val) : Operand{val} { }

		inline virtual bool check() const {
			return val_ < 22 && val_ != 1 && val_ != 3 && val_ != 5;
		}
};

} // namespace x64

#endif
