#ifndef X64_SRC_CODE_ST_H
#define X64_SRC_CODE_ST_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** The ith element from the top of the FPU register stack 
	  (i = 0 through 7). 
*/
class St {
	public:
		inline St() 
				: f_(0) { 
		}

		inline St(Operand f) 
				: f_(f) { 
		}

		inline operator Operand() const {
			return f_;
		}

	private:
		Operand f_;
};

/** The top element of the FPU register stack. */
struct St0 : public St {
	inline St0() : St{0} { }
};

extern const St0 st0;
extern const St st1;
extern const St st2;
extern const St st3;
extern const St st4;
extern const St st5;
extern const St st6;
extern const St st7;

typedef std::vector<St> Sts;
extern const Sts sts;

} // namespace x64

#endif
