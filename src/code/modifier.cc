#include "src/code/modifier.h"

using namespace std;

namespace x64 {

Modifier::~Modifier() {
	// Does nothing.
}

OpType Modifier::type() const {
	return OpType::MODIFIER;
}

void Modifier::write_att(ostream& os) const {
	// Does nothing.
}

void Modifier::write_intel(ostream& os) const {
	// Does nothing.
}

OpType Pref66::type() const {
	return OpType::PREF_66;
}

bool Pref66::check() const {
	return val() == 0;
}

OpType PrefRexW::type() const {
	return OpType::PREF_REX_W;
}

bool PrefRexW::check() const {
	return val() == 0;
}

OpType Far::type() const {
	return OpType::FAR;
}

bool Far::check() const {
	return val() == 0;
}

} // namespace x64
