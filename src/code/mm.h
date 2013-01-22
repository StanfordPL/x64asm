#ifndef X64_SRC_CODE_MM_REG_H
#define X64_SRC_CODE_MM_REG_H

#include <iostream>

#include "src/code/operand.h"

namespace x64 {

/** An MMX register. The 64-bit MMX registers are: MM0 through MM7. */
class Mm : public AtomicOperand {
	friend class Constants;
	private:
		inline Mm(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

} // namespace x64

#endif

