#ifndef X64_SRC_CODE_SREG_H
#define X64_SRC_CODE_SREG_H

#include <iostream>

#include "src/code/op_type.h"
#include "src/code/operand.h"

namespace x64 {

/** A segment register. The segment register bit assignments are ES = 0, 
	  CS = 1, SS = 2, DS = 3, FS = 4, and GS = 5.
*/
class Sreg : public AtomicOperand {
	friend class Constants;
	protected:
		inline Sreg(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** The segment register FS. */
class Fs : public Sreg {
	friend class Constants;
	private:
		inline Fs() : Sreg{4} { }

	public:
		virtual OpType type() const;
};

/** The segment register GS. */
class Gs : public Sreg {
	friend class Constants;
	private:
		inline Gs() : Sreg{5} { }

	public:
		virtual OpType type() const;
};

} // namespace x64

#endif
