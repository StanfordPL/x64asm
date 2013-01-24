#include "src/cr.h"

#include <cassert>

using namespace std;

namespace x64 {

OpType Cr::type() const {
	return OpType::CR;
}

bool Cr::check() const {
	return val() == 0 || val() == 2 || val() == 3 || val() == 4 || val() == 8;
}

void Cr::write_att(ostream& os) const {
	switch ( val() ) {
		case 0: os << "%cr0"; break;
		case 2: os << "%cr2"; break;
		case 3: os << "%cr3"; break;
		case 4: os << "%cr4"; break;
		case 8: os << "%cr8"; break;

		default: assert(false);
	}				 
}

void Cr::write_intel(ostream& os) const {
}

OpType Cr0234::type() const {
	return OpType::CR_0234;
}

bool Cr0234::check() const {
	return val() == 0 || val() == 2 || val() == 3 || val() == 4;
}

OpType Cr8::type() const {
	return OpType::CR_8;
}

bool Cr8::check() const {
	return val() == 8;
}

} // namespace x64
