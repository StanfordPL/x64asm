#include "src/xmm.h"

#include "src/op_set.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType Xmm::type() const {
	return OpType::XMM;
}

bool Xmm::check() const {
	return val() < 16;
}

void Xmm::insert_in(OpSet& os, bool promote) const {
	if ( promote )
		os += parent();
	else
		os += *this;
}

void Xmm::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Xmm::write_intel(ostream& os) const {
	assert(check());
	os << "xmm" << dec << val();
}

Ymm Xmm::parent() const {
	return Ymm{val()};
}

OpType Xmm0::type() const {
	return OpType::XMM_0;
}

bool Xmm0::check() const {
	return val() == 0;
}

} // namespace x64asm
