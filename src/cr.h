#ifndef X64ASM_SRC_CR_H
#define X64ASM_SRC_CR_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A control register. */
class Cr : public AtomicOperand {
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		inline Cr(uint64_t val) : AtomicOperand{val} { }
	private:
		virtual OpType type() const;
};

/** One of the control reigsters: CR0, CR2, CR3, CR4. */
class Cr0234 : public Cr {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Cr0234(uint64_t val) : Cr{val} { }
		virtual OpType type() const;
};

/** The control register CR8 */
class Cr8 : public Cr {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Cr8() : Cr{8} { }
		virtual OpType type() const;
};

} // namespace x64asm

#endif
