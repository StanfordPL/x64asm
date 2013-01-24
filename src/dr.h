#ifndef X64_SRC_DR_H
#define X64_SRC_DR_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64 {

/** A debug register. */
class Dr : public AtomicOperand {
	friend class Constants;
	private:
		inline Dr(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual OpType type() const;
		virtual bool check() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

} // namespace x64

#endif
