#include "src/code/ymm.h"

#include <cassert>

using namespace std;

namespace x64 {

OpType Ymm::type() const {
	return OpType::YMM;
}

bool Ymm::check() const {
	return val() < 16;
}

void Ymm::write_att(ostream& os) const {
	assert(val() < 16);
	os << "%ymm" << dec << val();
}

void Ymm::write_intel(ostream& os) const {
}

} // namespace x64

