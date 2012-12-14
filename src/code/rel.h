#ifndef X64_SRC_CODE_REL_H
#define X64_SRC_CODE_REL_H

#include "src/code/operand.h"

namespace x64 {

/** A relative address. */
class Rel {
	public:
		inline Rel()
				: r_{0} {
		}

		inline Rel(Operand r)
				: r_{r} {
		}

		inline operator Operand() const {
			return r_;
		}

	private:
		Operand r_;
};

/** A relative address in the range from 128 bytes before the end of the 
	  instruction to 127 bytes after the end of the instruction.
*/
struct Rel8 : public Rel {
	inline Rel8() : Rel{} { }
	inline Rel8(Operand r) : Rel{r} { }
};

/** A relative address within the same code segment as the instruction 
	  assembled. The rel32 symbol applies to instructions with an 
		operand-size attribute of 32 bits.
*/
struct Rel32 : public Rel {
	inline Rel32() : Rel{} { }
	inline Rel32(Operand r) : Rel{r} { }
};

} // namespace x64

#endif
