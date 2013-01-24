#include "src/sreg.h"

#include <cassert>

using namespace std;

namespace x64 {

OpType Sreg::type() const {
	return OpType::SREG;
}

bool Sreg::check() const {
	return val() < 6;
}

void Sreg::write_att(ostream& os) const {
	switch ( val() ) {
		case 0: os << "%es"; break;
		case 1: os << "%cs"; break;
		case 2: os << "%ss"; break;
		case 3: os << "%ds"; break;
		case 4: os << "%fs"; break;
		case 5: os << "%gs"; break;

		default: assert(false);
	}
}

void Sreg::write_intel(ostream& os) const {
}

OpType Fs::type() const {
	return OpType::FS;
}

bool Fs::check() const {
	return val() == 4;
}

OpType Gs::type() const {
	return OpType::GS;
}

bool Gs::check() const {
	return val() == 5;
}

} // namespace x64
