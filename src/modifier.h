#ifndef X64ASM_SRC_MODIFIER_H
#define X64ASM_SRC_MODIFIER_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A modifier. */
class Modifier : public AtomicOperand {
	public:
		inline Modifier(uint64_t val) : AtomicOperand{val} { }
		virtual ~Modifier() = 0;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		virtual OpType type() const;
};

/** The 32-bit memory address override prefix: 0x66. */
class Pref66 : public Modifier {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Pref66() : Modifier{0} { }
		virtual OpType type() const;	
};

/** The REX.w prefix: 0x48. */
class PrefRexW : public Modifier {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline PrefRexW() : Modifier{0} { }
		virtual OpType type() const;	
};

/** Far instruction variant. */
class Far : public Modifier {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Far() : Modifier{0} { }
		virtual OpType type() const;	
};

} // namespace x64asm

#endif
