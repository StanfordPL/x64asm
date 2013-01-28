#ifndef X64ASM_SRC_DR_H
#define X64ASM_SRC_DR_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A debug register. */
class Dr : public AtomicOperand {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline Dr(uint64_t val) : AtomicOperand{val} { }
		virtual OpType type() const;
};

} // namespace x64asm

#endif
