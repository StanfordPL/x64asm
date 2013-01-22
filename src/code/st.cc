#include "src/code/st.h"

#include <cassert>

using namespace std;

namespace x64 {

OpType St::type() const {
	return OpType::ST;
}

bool St::check() const {
	return val() < 8;
}

void St::write_att(ostream& os) const {
	assert(val() < 8);
	if ( val() == 0 )
		os << "%st";	
	else
		os << "%st(" << dec << val() << ")";
}

void St::write_intel(ostream& os) const {
}

OpType St0::type() const {
	return OpType::ST_0;
}

bool St0::check() const {
	return val() == 0;
}

} // namespace x64
