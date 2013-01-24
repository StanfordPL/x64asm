#include "src/ymm.h"

#include "src/op_set.h"

#include <cassert>

using namespace std;

namespace x64 {

OpType Ymm::type() const {
	return OpType::YMM;
}

bool Ymm::check() const {
	return val() < 16;
}

void Ymm::insert_in(OpSet& os, bool promote) const {
	os += *this;
}

void Ymm::write_att(ostream& os) const {
	assert(val() < 16);
	os << "%ymm" << dec << val();
}

void Ymm::write_intel(ostream& os) const {
}

} // namespace x64

