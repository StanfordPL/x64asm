#include "src/mm.h"

#include "src/op_set.h"

#include <cassert>

using namespace std;

namespace x64 {

OpType Mm::type() const {
	return OpType::MM;
}

bool Mm::check() const {
	return val() < 8;
}

void Mm::insert_in(OpSet& os, bool promote) const {
	os += *this;
}

void Mm::write_att(ostream& os) const {
	assert(val() < 8);
	os << "%mm" << dec << val();
}

void Mm::write_intel(ostream& os) const {
}

} // namespace x64
