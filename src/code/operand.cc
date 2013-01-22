#include "src/code/operand.h"

namespace x64 {

Operand::~Operand() {
	// Does nothing.
}	

OpType Operand::type() const {
	return OpType::OPERAND;
}

bool Operand::check() const {
	return true;
}

AtomicOperand::~AtomicOperand() {
	// Does nothing.
}	

OpType AtomicOperand::type() const {
	return OpType::ATOMIC_OPERAND;
}

CompoundOperand::~CompoundOperand() {
	// Does nothing.
}	

OpType CompoundOperand::type() const {
	return OpType::COMPOUND_OPERAND;
}

} // namespace x64
