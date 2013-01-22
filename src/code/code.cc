#include "src/code/code.h"

using namespace std;

namespace x64 {

bool Code::check() const {
	for ( const auto& i : *this )
		if ( !i.check() )
			return false;
	return true;
}

void Code::read_att(istream& is) {
	// TODO
}

void Code::read_intel(istream& is) {
	// TODO
}

void Code::write_att(ostream& os) const {
	for ( size_t i = 0, ie = size(); i < ie; ++i ) {
		(*this)[i].write_att(os);
		if ( i+1 != ie )
			os << endl;	
	}
}

void Code::write_intel(ostream& os) const {
	for ( size_t i = 0, ie = size(); i < ie; ++i ) {
		(*this)[i].write_intel(os);
		if ( i+1 != ie )
			os << endl;	
	}
}

} // namespace x64
