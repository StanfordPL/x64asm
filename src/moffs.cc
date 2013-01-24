#include "src/moffs.h"

using namespace std;

namespace x64 {

Moffs::~Moffs() {
	// Does nothing.
}

OpType Moffs::type() const {
	return OpType::MOFFS;
}

void Moffs::write_att(ostream& os) const {
	os << hex << showbase << val();
}

void Moffs::write_intel(ostream& os) const {
}

OpType Moffs8::type() const {
	return OpType::MOFFS_8;
}

OpType Moffs16::type() const {
	return OpType::MOFFS_16;
}

OpType Moffs32::type() const {
	return OpType::MOFFS_32;
}

OpType Moffs64::type() const {
	return OpType::MOFFS_64;
}

} // namespace x64
