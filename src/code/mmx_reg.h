#ifndef X64_SRC_CODE_MMX_REG_H
#define X64_SRC_CODE_MMX_REG_H

#include <cassert>
#include <iostream>

#include "src/code/operand.h"

namespace x64 {

/** An MMX register: MM0, ..., MM7.
*/
class MmxReg {
	public:
		inline MmxReg() 
				: m_(8) { 
		}

		inline MmxReg(Operand m) 
				: m_(m) { 
			assert(is_valid());
		}

		inline operator Operand() const {
			return m_;
		}

		inline bool is_null() const {
			return m_ == 8;	
		}

		inline bool is_valid() const { 
			return m_ <= 8; 
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		void write_att(std::ostream& os) const;

	private:
		Operand m_;
};

extern const MmxReg mm0;
extern const MmxReg mm1;
extern const MmxReg mm2;
extern const MmxReg mm3;
extern const MmxReg mm4;
extern const MmxReg mm5;
extern const MmxReg mm6;
extern const MmxReg mm7;
extern const MmxReg mm_null;

} // namespace x64

#endif

