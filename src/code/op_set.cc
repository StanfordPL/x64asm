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
	os << "{ ";
	
	for ( size_t i = 0; i < 16; ++i ) {
		if ( contains(r64s[i]) ) {
			r64s[i].write_att(os);
		 	os << " ";
		}
		else if ( contains(r32s[i]) ) {
			r32s[i].write_att(os);
		 	os << " ";
		}
		else if ( contains(r16s[i]) ) {
			r16s[i].write_att(os);
		 	os << " ";
		}
		else if ( i < 4 ) {
			if ( contains(rls[i]) ) {
				rls[i].write_att(os);
		 		os << " ";
			}
	    else if ( contains(rhs[i]) ) {
				rhs[i].write_att(os);
		 		os << " ";
			}
		}	
		else if ( contains(rbs[i-4]) ) {
			rbs[i-4].write_att(os);
		 	os << " ";
		}
	}

	for ( size_t i = 0; i < 16; ++i ) {
		if ( contains(ymms[i]) ) {
			ymms[i].write_att(os);
		 	os << " ";
		}
		else if ( contains(xmms[i]) ) {
			xmms[i].write_att(os);
		 	os << " ";
		}
	}

	for ( const auto mm : mms )
		if ( contains(mm) ) {
			mm.write_att(os);
			os << " ";
		}

	for ( const auto e : eflags )
		if ( contains(e) ) {
			e.write_att(os);
			os << " ";
		}

	os << "}";
}

void OpSet::write_intel(ostream& os) const {

}

} // namespace x64
