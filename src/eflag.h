#ifndef X64ASM_SRC_EFLAG_H
#define X64ASM_SRC_EFLAG_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** An EFLAGS register bit. */
class Eflag : public AtomicOperand {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline Eflag(uint64_t val) : AtomicOperand{val} { }	
		virtual OpType type() const;
		virtual void insert_in(OpSet& os, bool promote = false) const;
};

} // namespace x64asm

#endif
