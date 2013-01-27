#ifndef X64ASM_SRC_LABEL_H
#define X64ASM_SRC_LABEL_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A symbolic representation of a Rel32.
	  For simplicity, we do not provide a Rel8 equivalent.
*/
class Label : public AtomicOperand {
	public:
		inline Label(uint64_t val) : AtomicOperand{val} { } 

		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

} // namespace x64asm

#endif
