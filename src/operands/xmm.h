#ifndef X64_SRC_OPERANDS_XMM_H
#define X64_SRC_OPERANDS_XMM_H

#include "src/operands/operand.h"

namespace x64 {

/** An XMM register. The 128-bit XMM registers are: XMM0 through XMM7; XMM8 
	  through XMM15 are available using REX.R in 64-bit mode.
*/
class Xmm : public Operand {
	friend class Constants;
	protected:
		inline Xmm(uint64_t val) : Operand{val} { } 
};

/** The XMM register XMM0. */
class Xmm0 : public Xmm {
	friend class Constants;
	private:
		inline Xmm0() : Xmm{0} { }
};

} // namespace x64

#endif
