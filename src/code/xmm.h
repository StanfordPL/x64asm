#ifndef X64_SRC_CODE_XMM_H
#define X64_SRC_CODE_XMM_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** An XMM register. The 128-bit XMM registers are: XMM0 through XMM7; XMM8 
	  through XMM15 are available using REX.R in 64-bit mode.
*/
class Xmm {
	public:
		inline Xmm() 
				: x_{0} { 
		}

		inline Xmm(Operand x) 
				: x_{x} { 
		}

		inline operator Operand() const {
			return x_;
		}

	private:
		Operand x_;
};

/** The XMM register XMM0. */
struct Xmm0 : public Xmm {
	inline Xmm0() : Xmm{0} { }
};

extern const Xmm0 xmm0;
extern const Xmm xmm1;
extern const Xmm xmm2;
extern const Xmm xmm3;
extern const Xmm xmm4;
extern const Xmm xmm5;
extern const Xmm xmm6;
extern const Xmm xmm7;
extern const Xmm xmm8;
extern const Xmm xmm9;
extern const Xmm xmm10;
extern const Xmm xmm11;
extern const Xmm xmm12;
extern const Xmm xmm13;
extern const Xmm xmm14;
extern const Xmm xmm15;

typedef std::vector<Xmm> Xmms;
extern const Xmms xmms;

} // namespace x64

#endif
