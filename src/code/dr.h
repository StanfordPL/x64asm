#ifndef X64_SRC_CODE_DR_H
#define X64_SRC_CODE_DR_H

#include <iostream>

#include "src/code/operand.h"

namespace x64 {

/** A debug register. */
class Dr : public AtomicOperand {
	friend class Constants;
	private:
		inline Dr(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

} // namespace x64

#endif
