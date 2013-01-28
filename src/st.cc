#include "src/st.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType St::type() const {
	return OpType::ST;
}

bool St::check() const {
	return val() < 8;
}

void St::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void St::write_intel(ostream& os) const {
	assert(check());
	if ( val() == 0 )
		os << "st";	
	else
		os << "st(" << dec << val() << ")";
}

OpType St0::type() const {
	return OpType::ST_0;
}

bool St0::check() const {
	return val() == 0;
}

} // namespace x64asm
