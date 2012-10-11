#ifndef X64_SRC_CODE_IMM_H
#define X64_SRC_CODE_IMM_H

#include "src/code/operand.h"

namespace x64 {

/** A 64-bit immediate.
*/
class Imm {
	public:
		inline Imm() 
				: i_(0) { 
		}

		inline Imm(Operand o) 
				: i_(o) {
		}
		
		inline operator Operand() const { 
			return i_;
		}

		inline bool is_null() const {
			return false;
		}

	private:
		Operand i_;
};

} // namespace x64

#endif
