#ifndef X64_SRC_OPERANDS_ST_H
#define X64_SRC_OPERANDS_ST_H

#include "src/operands/operand.h"

namespace x64 {

/** The ith element from the top of the FPU register stack 
	  (i = 0 through 7). 
*/
class St : public Operand {
	friend class Constants;
	protected:
		inline St(uint64_t val) : Operand{val} { } 
};

/** The top element of the FPU register stack. */
class St0 : public St {
	friend class Constants;
	private:
		inline St0() : St{0} { }
};

} // namespace x64

#endif
