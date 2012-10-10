#ifndef X64_SRC_CODE_FP_REG_H
#define X64_SRC_CODE_FP_REG_H

#include "src/code/operand.h"

namespace x64 {

/** A floating point stack register: st(0), ..., st(7).
*/
class FpReg {
	public:
		inline FpReg() 
				: f_(8) { 
		}

		inline FpReg(Operand f) 
				: f_(f) { 
		}

		inline operator Operand() const {
			return f_;
		}

		inline bool is_null() const {
			return f_ >= 8;	
		}

	private:
		Operand f_;
};

extern const FpReg st0;
extern const FpReg st1;
extern const FpReg st2;
extern const FpReg st3;
extern const FpReg st4;
extern const FpReg st5;
extern const FpReg st6;
extern const FpReg st7;

} // namespace x64

#endif


