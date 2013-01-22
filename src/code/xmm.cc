#include "src/code/xmm.h"

#include <cassert>

using namespace std;

namespace x64 {

OpType Xmm::type() const {
	return OpType::XMM;
}

bool Xmm::check() const {
	return val() < 16;
}

void Xmm::write_att(ostream& os) const {
	assert(val() < 16);
	os << "%xmm" << dec << val();
}

void Xmm::write_intel(ostream& os) const {
}

OpType Xmm0::type() const {
	return OpType::XMM_0;
}

bool Xmm0::check() const {
	return val() == 0;
}

} // namespace x64
