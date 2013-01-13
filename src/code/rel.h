#ifndef X64_SRC_CODE_REL_H
#define X64_SRC_CODE_REL_H

#include "src/code/operand.h"

namespace x64 {

/** A relative address in the range from 128 bytes before the end of the 
	  instruction to 127 bytes after the end of the instruction.
*/
class Rel8 : public Operand {
	public:
		inline Rel8(uint64_t val) : Operand{val} { }

		inline virtual bool check() const {
			return true;
		}
};

/** A relative address within the same code segment as the instruction 
	  assembled. The rel32 symbol applies to instructions with an 
		operand-size attribute of 32 bits.
*/
class Rel32 : public Operand {
	public:
		inline Rel32(uint64_t val) : Operand{val} { }

		inline virtual bool check() const {
			return true;
		}
};

} // namespace x64

#endif
