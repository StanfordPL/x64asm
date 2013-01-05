#include "src/code/op_set.h"

namespace x64 {

OpSet& OpSet::operator+=(M rhs) {
	assert(Checker::check(rhs));	
	if ( rhs.get_addr_or() ) {
		if ( !rhs.null_base() )
			*this += (R32)rhs.get_base();
		if ( !rhs.null_index() )
			*this += (R32)rhs.get_index();
	}
	else {
		if ( !rhs.null_base() )
			*this += (R64)rhs.get_base();
		if ( !rhs.null_index() )
			*this += (R64)rhs.get_index();
	}
	return *this;
}

} // namespace x64
