#ifndef X64_SRC_CODE_XMM_REG_H
#define X64_SRC_CODE_XMM_REG_H

#include "src/code/operand.h"

namespace x64 {

/** An SSE register: xmm0, ..., xmm15.
*/
class XmmReg {
	public:
		inline XmmReg() 
				: x_(16) { 
		}

		inline XmmReg(Operand x) 
				: x_(x) { 
		}

		inline operator Operand() const {
			return x_;
		}

		inline bool is_null() const {
			return x_ >= 16;	
		}

	private:
		Operand x_;
};

extern const XmmReg xmm0;
extern const XmmReg xmm1;
extern const XmmReg xmm2;
extern const XmmReg xmm3;
extern const XmmReg xmm4;
extern const XmmReg xmm5;
extern const XmmReg xmm6;
extern const XmmReg xmm7;
extern const XmmReg xmm8;
extern const XmmReg xmm9;
extern const XmmReg xmm10;
extern const XmmReg xmm11;
extern const XmmReg xmm12;
extern const XmmReg xmm13;
extern const XmmReg xmm14;
extern const XmmReg xmm15;
extern const XmmReg xmm_null;

} // namespace x64

#endif
