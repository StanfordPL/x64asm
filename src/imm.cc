#include "src/imm.h"

using namespace std;

namespace x64 {

Imm::~Imm() {
	// Does nothing.
}

OpType Imm::type() const {
	return OpType::IMM;
}

void Imm::write_att(ostream& os) const {
	os << "$" << hex << val();
}

void Imm::write_intel(ostream& os) const {
}

OpType Imm8::type() const {
	return OpType::IMM_8;
}

bool Imm8::check() const {
	return (int64_t)val() >= -128 && (int64_t)val() < 128;
}

OpType Imm16::type() const {
	return OpType::IMM_16;
}

bool Imm16::check() const {
	return (int64_t)val() >= -32768 && (int64_t)val() < 32768;
}

OpType Imm32::type() const {
	return OpType::IMM_32;
}

bool Imm32::check() const {
	return (int64_t)val() >= -2147483648 && (int64_t)val() < 2147483648;
}

OpType Imm64::type() const {
	return OpType::IMM_64;
}

OpType Zero::type() const {
	return OpType::ZERO;
}

bool Zero::check() const {
	return val() == 0;
}

OpType One::type() const {
	return OpType::ONE;
}

bool One::check() const {
	return val() == 1;
}

OpType Three::type() const {
	return OpType::THREE;
}

bool Three::check() const {
	return val() == 3;
}

} // namespace x64
