#ifndef X64_SRC_CODE_MM_REG_H
#define X64_SRC_CODE_MM_REG_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** An MMX register. The 64-bit MMX registers are: MM0 through MM7. */
class Mm {
	public:
		inline Mm() 
				: m_{0} { 
		}

		inline Mm(Operand m) 
				: m_{m} { 
		}

		inline operator Operand() const {
			return m_;
		}

	private:
		Operand m_;
};

extern const Mm mm0;
extern const Mm mm1;
extern const Mm mm2;
extern const Mm mm3;
extern const Mm mm4;
extern const Mm mm5;
extern const Mm mm6;
extern const Mm mm7;

typedef std::vector<Mm> Mms;
extern const Mms mms;

} // namespace x64

#endif

