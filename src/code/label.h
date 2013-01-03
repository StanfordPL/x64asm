#ifndef X64_SRC_CODE_LABEL_H
#define X64_SRC_CODE_LABEL_H

#include "src/code/operand.h"

namespace x64 {

/** A symbolic representation of a Rel32.
	  For simplicity, we do not provide a Rel8 equivalent.
*/
class Label : public Operand {
	public:
		inline Label(uint64_t val) : Operand{val} { } 
};

} // namespace x64

#endif
