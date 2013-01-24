#include "src/op_set.h"

using namespace std;

namespace x64 {

OpSet& OpSet::operator+=(const M& rhs) {
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
	write_txt(os, true);
}

void OpSet::write_intel(ostream& os) const {
	write_txt(os, false);
}

void OpSet::write_txt(ostream& os, bool att) const {
	os << "{ ";
	
	for ( size_t i = 0; i < 16; ++i ) {
		if ( contains(r64s[i]) ) {
			if ( att )
				r64s[i].write_att(os);
			else
				r64s[i].write_intel(os);
		 	os << " ";
		}
		else if ( contains(r32s[i]) ) {
			if ( att )
				r32s[i].write_att(os);
			else
				r32s[i].write_intel(os);
		 	os << " ";
		}
		else if ( contains(r16s[i]) ) {
			if ( att ) 
				r16s[i].write_att(os);
			else
				r16s[i].write_intel(os);
		 	os << " ";
		}
		else if ( i < 4 ) {
			if ( contains(rls[i]) ) {
				if ( att )
					rls[i].write_att(os);
				else
					rls[i].write_intel(os);
		 		os << " ";
			}
	    else if ( contains(rhs[i]) ) {
				if ( att )
					rhs[i].write_att(os);
				else
					rhs[i].write_intel(os);
		 		os << " ";
			}
		}	
		else if ( contains(rbs[i-4]) ) {
			if ( att ) 
				rbs[i-4].write_att(os);
			else
				rbs[i-4].write_intel(os);
		 	os << " ";
		}
	}

	for ( size_t i = 0; i < 16; ++i ) {
		if ( contains(ymms[i]) ) {
			if ( att ) 
				ymms[i].write_att(os);
			else
				ymms[i].write_intel(os);
		 	os << " ";
		}
		else if ( contains(xmms[i]) ) {
			if ( att ) 
				xmms[i].write_att(os);
			else
				xmms[i].write_intel(os);
		 	os << " ";
		}
	}

	for ( const auto mm : mms )
		if ( contains(mm) ) {
			if ( att ) 
				mm.write_att(os);
			else
				mm.write_intel(os);
			os << " ";
		}

	for ( const auto e : eflags )
		if ( contains(e) ) {
			if ( att ) 
				e.write_att(os);
			else 
				e.write_intel(os);
			os << " ";
		}

	os << "}";
}

} // namespace x64
