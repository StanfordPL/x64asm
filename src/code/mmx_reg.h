#ifndef X64_SRC_CODE_MMX_REG_H
#define X64_SRC_CODE_MMX_REG_H

#include "src/code/operand.h"

namespace x64 {

/** An MMX register: mm0, ..., mm7.
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
			return m_ >= 8;	
		}

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

