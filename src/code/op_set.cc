#include "src/code/op_set.h"

using namespace std;

namespace x64 {

OpSet& OpSet::operator+=(const M& rhs) {
	assert(Checker::check(rhs));	
	if ( rhs.get_addr_or() ) {
		if ( rhs.contains_base() )
			*this += *((R32*)rhs.get_base());
		if ( rhs.contains_index() )
			*this += *((R32*)rhs.get_index());
	}
	else {
		if ( rhs.contains_base() )
			*this += *((R64*)rhs.get_base());
		if ( rhs.contains_index() )
			*this += *((R64*)rhs.get_index());
	}
	return *this;
}

void OpSet::write_att(ostream& os) const {

}

void OpSet::write_intel(ostream& os) const {

}

} // namespace x64
