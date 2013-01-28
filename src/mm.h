#ifndef X64ASM_SRC_MM_H
#define X64ASM_SRC_MM_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** An MMX register. The 64-bit MMX registers are: MM0 through MM7. */
class Mm : public AtomicOperand {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline Mm(uint64_t val) : AtomicOperand{val} { }
		virtual OpType type() const;
		virtual void insert_in(OpSet& os, bool promote = false) const;
};

} // namespace x64asm

#endif

