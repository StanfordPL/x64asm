#ifndef X64_SRC_XMM_H
#define X64_SRC_XMM_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"
#include "src/ymm.h"

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
		virtual bool check() const;
		virtual void insert_in(OpSet& os, bool promote = false) const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;

		Ymm parent() const;
};

/** The XMM register XMM0. */
class Xmm0 : public Xmm {
	friend class Constants;
	private:
		inline Xmm0() : Xmm{0} { }

	public:
		virtual OpType type() const;
		virtual bool check() const;
};

} // namespace x64

#endif
