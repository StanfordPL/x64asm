#include "src/dr.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType Dr::type() const {
	return OpType::DR;
}

bool Dr::check() const {
	return val() < 8;
}

void Dr::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Dr::write_intel(ostream& os) const {
	assert(check());
	os << "db" << dec << val();
}

} // namespace x64asm
