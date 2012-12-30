#ifndef X64_SRC_OPERANDS_ST_H
#define X64_SRC_OPERANDS_ST_H

#include "src/operands/operand.h"

namespace x64 {

/** The ith element from the top of the FPU register stack 
	  (i = 0 through 7). 
*/
class St : public Operand {
	public:
		inline St(uint64_t val) : Operand{val} { } 
};

/** The top element of the FPU register stack. */
class St0 : public St {
	public:
		inline St0() : St{0} { }
		inline St0(uint64_t ignore) : St{0} { }
};

} // namespace x64

#endif
