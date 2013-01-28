#ifndef X64ASM_SRC_SREG_H
#define X64ASM_SRC_SREG_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A segment register. The segment register bit assignments are ES = 0, 
	  CS = 1, SS = 2, DS = 3, FS = 4, and GS = 5.
*/
class Sreg : public AtomicOperand {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		inline Sreg(uint64_t val) : AtomicOperand{val} { }
	private:
		virtual OpType type() const;
};

/** The segment register FS. */
class Fs : public Sreg {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Fs() : Sreg{4} { }
		virtual OpType type() const;
};

/** The segment register GS. */
class Gs : public Sreg {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Gs() : Sreg{5} { }
		virtual OpType type() const;
};

} // namespace x64asm

#endif
