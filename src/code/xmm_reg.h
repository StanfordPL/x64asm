#ifndef X64_SRC_CODE_XMM_REG_H
#define X64_SRC_CODE_XMM_REG_H

#include <cassert>
#include <iostream>

#include "src/code/operand.h"

namespace x64 {

/** An SSE register: XMM0, ..., XMM15.
*/
class XmmReg {
	public:
		inline XmmReg() 
				: x_(16) { 
		}

		inline XmmReg(Operand x) 
				: x_(x) { 
			assert(is_valid());
		}

		inline operator Operand() const {
			return x_;
		}

		inline bool is_null() const {
			return x_ == 16;	
		}

		inline bool is_valid() const { 
			return x_ <= 16; 
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		void write_att(std::ostream& os) const;

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
