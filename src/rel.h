#ifndef X64ASM_SRC_REL_H
#define X64ASM_SRC_REL_H

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A relative address. */
class Rel : public AtomicOperand {
	public:
		inline Rel(uint64_t val) : AtomicOperand{val} { }
		virtual ~Rel() = 0;

		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** A relative address in the range from 128 bytes before the end of the 
	  instruction to 127 bytes after the end of the instruction.
*/
class Rel8 : public Rel {
	public:
		inline Rel8(int8_t val) : Rel{(uint64_t)val} { }

		virtual OpType type() const;
		virtual bool check() const;
};

/** A relative address within the same code segment as the instruction 
	  assembled. The rel32 symbol applies to instructions with an 
		operand-size attribute of 32 bits.
*/
class Rel32 : public Rel {
	public:
		inline Rel32(int64_t val) : Rel{(uint64_t)val} { }

		virtual OpType type() const;
		virtual bool check() const;
};

} // namespace x64asm

#endif
