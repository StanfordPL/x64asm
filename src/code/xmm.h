#ifndef X64_SRC_CODE_XMM_H
#define X64_SRC_CODE_XMM_H

#include <iostream>

#include "src/code/op_type.h"
#include "src/code/operand.h"

namespace x64 {

/** An XMM register. The 128-bit XMM registers are: XMM0 through XMM7; XMM8 
	  through XMM15 are available using REX.R in 64-bit mode.
*/
class Xmm : public AtomicOperand {
	friend class Constants;
	protected:
		inline Xmm(uint64_t val) : AtomicOperand{val} { } 

	public:
		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** The XMM register XMM0. */
class Xmm0 : public Xmm {
	friend class Constants;
	private:
		inline Xmm0() : Xmm{0} { }

	public:
		virtual OpType type() const;
};

} // namespace x64

#endif
