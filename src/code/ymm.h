#ifndef X64_SRC_CODE_YMM_H
#define X64_SRC_CODE_YMM_H

#include <iostream>

#include "src/code/op_type.h"
#include "src/code/operand.h"

namespace x64 {

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8 
	  through YMM15 are available in 64-bit mode.
*/
class Ymm : public AtomicOperand {
	friend class Constants;
	private:
		inline Ymm(uint64_t val) : AtomicOperand{val} { } 

	public:
		virtual OpType type() const;
		virtual bool check() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

} // namespace x64

#endif

