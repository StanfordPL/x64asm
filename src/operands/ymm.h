#ifndef X64_SRC_OPERANDS_YMM_H
#define X64_SRC_OPERANDS_YMM_H

#include "src/operands/operand.h"

namespace x64 {

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8 
	  through YMM15 are available in 64-bit mode.
*/
class Ymm : public Operand {
	friend class Constants;
	private:
		inline Ymm(uint64_t val) : Operand{val} { } 
};

} // namespace x64

#endif

