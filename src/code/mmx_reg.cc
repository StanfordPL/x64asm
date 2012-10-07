#include "src/code/mmx_reg.h"

using namespace std;

namespace x64 {

const MmxReg mm0 = 0;
const MmxReg mm1 = 1;
const MmxReg mm2 = 2;
const MmxReg mm3 = 3;
const MmxReg mm4 = 4;
const MmxReg mm5 = 5;
const MmxReg mm6 = 6;
const MmxReg mm7 = 7;
const MmxReg mm_null = 8;

void MmxReg::write_att(ostream& os) const {
	switch ( m_ ) {
		case 0: os << "%mm0"; break;
		case 1: os << "%mm1"; break;
		case 2: os << "%mm2"; break;
		case 3: os << "%mm3"; break;
		case 4: os << "%mm4"; break;
		case 5: os << "%mm5"; break;
		case 6: os << "%mm6"; break;
		case 7: os << "%mm7"; break;

		default:
			os.setstate(ios::failbit);
	}	
}

} // namespace x64
