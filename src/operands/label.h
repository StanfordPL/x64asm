#ifndef X64_SRC_OPERANDS_LABEL_H
#define X64_SRC_OPERANDS_LABEL_H

#include "src/operands/operand.h"

namespace x64 {

/** A symbolic representation of a Rel.
*/
class Label : public Operand {
	protected:
		inline Label(uint64_t val) : Operand{val} { } 
};

/** A symbolic representation of a Rel8. */
class Label8 : public Label {
	public:
		inline Label8(uint64_t l) : Label{l} { }
};

/** A symbolic representation of a Rel32. */
class Label32 : public Label {
	public:
		inline Label32(uint64_t l) : Label{l} { }
};

} // namespace x64

#endif
