#include "src/code/rel.h"

using namespace std;

namespace x64 {

Rel::~Rel() {
	// Does nothing.
}

OpType Rel::type() const {
	return OpType::REL;
}

void Rel::write_att(ostream& os) const {
	os << hex << showbase << val();
}

void Rel::write_intel(ostream& os) const {
}

OpType Rel8::type() const {
	return OpType::REL_8;
}

bool Rel8::check() const {
	return (int64_t)val() >= -128 && (int64_t)val() < 128;
}

OpType Rel32::type() const {
	return OpType::REL_32;
}

bool Rel32::check() const {
	return (int64_t)val() >= -2147483648 && (int64_t)val() < 2147483648;
}

} // namespace x64
