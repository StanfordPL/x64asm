#ifndef X64ASM_SRC_OPERAND_H
#define X64ASM_SRC_OPERAND_H

#include <iostream>
#include <stdint.h>

#include "src/op_type.h"

namespace x64asm {

class OpSet;

/** Base operand type. */
class Operand {
	friend class Instruction;
	public:
		virtual ~Operand() = 0; 
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
	private:
		virtual OpType type() const;
		virtual void insert_in(OpSet& os, bool promote = false) const;
};

/** Atomic Operand Type. */
class AtomicOperand : public Operand {
	friend class Assembler;
	friend class M;
	friend class OpSet;
	public:
		inline AtomicOperand(uint64_t val) : val_{val} { }
		virtual ~AtomicOperand() = 0;
		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
	protected:
		inline uint64_t val() const { return val_; }
	private:
		virtual OpType type() const;
		uint64_t val_;	
};

/** Aggregate Operand Type. */
class CompoundOperand : public Operand {
	public:
		virtual ~CompoundOperand() = 0;
		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
	private:
		virtual OpType type() const;
};

} // namespace x64asm

#endif
