#ifndef X64_SRC_CODE_MODIFIER_H
#define X64_SRC_CODE_MODIFIER_H

#include <iostream>

#include "src/code/op_type.h"
#include "src/code/operand.h"

namespace x64 {

/** A modifier. */
class Modifier : public AtomicOperand {
	public:
		inline Modifier(uint64_t val) : AtomicOperand{val} { }
		virtual ~Modifier() = 0;

		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** The 32-bit memory address override prefix: 0x66. */
class Pref66 : public Modifier {
	friend class Constants;
	private:
		inline Pref66() : Modifier{0} { }

	public:
		virtual OpType type() const;	
};

/** The REX.w prefix: 0x48. */
class PrefRexW : public Modifier {
	friend class Constants;
	private:
		inline PrefRexW() : Modifier{0} { }

	public:
		virtual OpType type() const;	
};

/** Far instruction variant. */
class Far : public Modifier {
	friend class Constants;
	private:
		inline Far() : Modifier{0} { }

	public:
		virtual OpType type() const;	
};

} // namespace x64

#endif
