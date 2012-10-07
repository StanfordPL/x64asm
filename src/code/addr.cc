#include "src/code/addr.h"

using namespace std;

namespace x64 {

void Addr::write_att(ostream& os, BitWidth w) const {
	if ( w != DOUBLE && w != QUAD ) {
		os.setstate(ios::failbit);
		return;
	}

	const auto s = get_seg();
	if ( !s.is_null() ) {
		s.write_att(os);
		os << ":";
	}

	// We follow objdump in presenting displacements as signed hex
	const auto d = (int32_t) get_disp();
	if ( d > 0 )
		os << hex << showbase << d;
	else if ( d < 0 )
		os << "-" << hex << showbase << (-1 * d);

	os << "(";
 	get_base().write_att(os, w);
	const auto i = get_index();
	if ( !i.is_null() ) {
		os << ",";
	 	i.write_att(os, w);
	}	
	const auto sc = get_scale();
	if ( !sc.is_null() ) {
		os << ",";
	 	sc.write_att(os);
	}
	os << ")";
}

} // namespace x64
