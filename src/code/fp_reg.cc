#include "src/code/fp_reg.h"

using namespace std;

namespace x64 {

const FpReg st0 = 0;
const FpReg st1 = 1;
const FpReg st2 = 2;
const FpReg st3 = 3;
const FpReg st4 = 4;
const FpReg st5 = 5;
const FpReg st6 = 6;
const FpReg st7 = 7;
const FpReg fp_null = 8;

void FpReg::write_att(ostream& os) const {
	switch ( f_ ) {
		case 0: os << "%st";    break;
		case 1: os << "%st(1)"; break;
		case 2: os << "%st(2)"; break;
		case 3: os << "%st(3)"; break;
		case 4: os << "%st(4)"; break;
		case 5: os << "%st(5)"; break;
		case 6: os << "%st(6)"; break;
		case 7: os << "%st(7)"; break;

		default:
			os.setstate(ios::failbit);
	}	
}

} // namespace x64

