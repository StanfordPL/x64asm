#ifndef X64ASM_SRC_OPERAND_H
#define X64ASM_SRC_OPERAND_H

#include <iostream>
#include <stdint.h>

#include "src/op_type.h"

namespace x64asm {

class OpSet;

/** Base operand type. */
class Operand {
	public:
		virtual ~Operand() = 0; 

		virtual OpType type() const;
		virtual bool check() const;
		virtual void insert_in(OpSet& os, bool promote = false) const;

		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
};

/** Atomic Operand Type. */
class AtomicOperand : public Operand {
	public:
		inline AtomicOperand(uint64_t val) : val_{val} { }
		virtual ~AtomicOperand() = 0;

		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;

		inline uint64_t val() const {
			return val_;
		}
			
	private:
		uint64_t val_;	
};

/** Aggregate Operand Type. */
class CompoundOperand : public Operand {
	public:
		virtual ~CompoundOperand() = 0;

		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
};

} // namespace x64asm

#endif
