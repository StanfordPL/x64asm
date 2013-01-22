#ifndef X64_SRC_CODE_CR_H
#define X64_SRC_CODE_CR_H

#include <iostream>

#include "src/code/op_type.h"
#include "src/code/operand.h"

namespace x64 {

/** A control register. */
class Cr : public AtomicOperand {
	protected:
		inline Cr(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual OpType type() const;
		virtual bool check() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** One of the control reigsters: CR0, CR2, CR3, CR4. */
class Cr0234 : public Cr {
	friend class Constants;
	private:
		inline Cr0234(uint64_t val) : Cr{val} { }

	public:	
		virtual OpType type() const;
		virtual bool check() const;
};

/** The control register CR8 */
class Cr8 : public Cr {
	friend class Constants;
	private:
		inline Cr8() : Cr{8} { }

	public:	
		virtual OpType type() const;
		virtual bool check() const;
};

} // namespace x64

#endif
