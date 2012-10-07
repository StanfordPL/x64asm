#ifndef X64_SRC_CODE_FP_REG_H
#define X64_SRC_CODE_FP_REG_H

#include <cassert>
#include <iostream>

#include "src/code/operand.h"

namespace x64 {

/** A floating point stack register: ST0, ..., ST7.
*/
class FpReg {
	public:
		inline FpReg() 
				: f_(8) { 
		}

		inline FpReg(Operand f) 
				: f_(f) { 
			assert(is_valid());
		}

		inline operator Operand() const {
			return f_;
		}

		inline bool is_null() const {
			return f_ == 8;	
		}

		inline bool is_valid() const { 
			return f_ <= 8; 
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		void write_att(std::ostream& os) const;

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


