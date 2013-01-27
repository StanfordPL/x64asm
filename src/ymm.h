#ifndef X64ASM_SRC_YMM_H
#define X64ASM_SRC_YMM_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8 
	  through YMM15 are available in 64-bit mode.
*/
class Ymm : public AtomicOperand {
	friend class Constants;
	friend class Xmm;
	private:
		inline Ymm(uint64_t val) : AtomicOperand{val} { } 

	public:
		virtual OpType type() const;
		virtual bool check() const;
		virtual void insert_in(OpSet& os, bool promote = false) const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

} // namespace x64asm

#endif

