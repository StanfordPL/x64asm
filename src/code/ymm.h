#ifndef X64_SRC_CODE_YMM_H
#define X64_SRC_CODE_YMM_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8 
	  through YMM15 are available in 64-bit mode.
*/
class Ymm {
	public:
		inline Ymm() 
				: x_{0} { 
		}

		inline Ymm(Operand x) 
				: x_{x} { 
		}

		inline operator Operand() const {
			return x_;
		}

	private:
		Operand x_;
};

extern const Ymm ymm0;
extern const Ymm ymm1;
extern const Ymm ymm2;
extern const Ymm ymm3;
extern const Ymm ymm4;
extern const Ymm ymm5;
extern const Ymm ymm6;
extern const Ymm ymm7;
extern const Ymm ymm8;
extern const Ymm ymm9;
extern const Ymm ymm10;
extern const Ymm ymm11;
extern const Ymm ymm12;
extern const Ymm ymm13;
extern const Ymm ymm14;
extern const Ymm ymm15;

typedef std::vector<Ymm> Ymms;
extern const Ymms ymms;

} // namespace x64

#endif

