#ifndef X64_SRC_CODE_EFLAG_H
#define X64_SRC_CODE_EFLAG_H

#include <iostream>

#include "src/code/op_type.h"
#include "src/code/operand.h"

namespace x64 {

/** An EFLAGS register bit. */
class Eflag : public AtomicOperand {
	friend class Constants;
	private:
		inline Eflag(uint64_t val) : AtomicOperand{val} { }	

	public:
		virtual OpType type() const;
		virtual bool check() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

} // namespace x64

#endif
