#ifndef X64_SRC_CODE_YMM_H
#define X64_SRC_CODE_YMM_H

#include "src/code/operand.h"

namespace x64 {

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8 
	  through YMM15 are available in 64-bit mode.
*/
class Ymm : public Operand {
	public:
		inline Ymm(uint64_t val) : Operand{val} { } 

		inline virtual bool check() const {
			return val_ < 16;
		}
};

} // namespace x64

#endif

