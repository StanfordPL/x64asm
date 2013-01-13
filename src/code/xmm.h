#ifndef X64_SRC_CODE_XMM_H
#define X64_SRC_CODE_XMM_H

#include "src/code/operand.h"

namespace x64 {

/** An XMM register. The 128-bit XMM registers are: XMM0 through XMM7; XMM8 
	  through XMM15 are available using REX.R in 64-bit mode.
*/
class Xmm : public Operand {
	public:
		inline Xmm(uint64_t val) : Operand{val} { } 

		inline bool check() const {
			return val_ < 16;
		}
};

/** The XMM register XMM0. */
class Xmm0 : public Xmm {
	public:
		inline Xmm0() : Xmm{0} { }
		inline Xmm0(uint64_t ignore) : Xmm{0} { }

		inline bool check() const {
			return val_ == 0;
		}
};

} // namespace x64

#endif
