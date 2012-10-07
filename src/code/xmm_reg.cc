#include "src/code/xmm_reg.h"

using namespace std;

namespace x64 {

const XmmReg xmm0 = 0;
const XmmReg xmm1 = 1;
const XmmReg xmm2 = 2;
const XmmReg xmm3 = 3;
const XmmReg xmm4 = 4;
const XmmReg xmm5 = 5;
const XmmReg xmm6 = 6;
const XmmReg xmm7 = 7;
const XmmReg xmm8 = 8;
const XmmReg xmm9 = 9;
const XmmReg xmm10 = 10;
const XmmReg xmm11 = 11;
const XmmReg xmm12 = 12;
const XmmReg xmm13 = 13;
const XmmReg xmm14 = 14;
const XmmReg xmm15 = 15;
const XmmReg xmm_null = 16;

void XmmReg::write_att(ostream& os) const {
	switch ( x_ ) {
		#define W(X, s) case X: os << s; break;
		W(0,  "%xmm0")  W(1,  "%xmm1")  W(2,  "%xmm2")  W(3,  "%xmm3")
		W(4,  "%xmm4")  W(5,  "%xmm5")  W(6,  "%xmm6")  W(7,  "%xmm7")
		W(8,  "%xmm8")  W(9,  "%xmm9")  W(10, "%xmm10") W(11, "%xmm11")
		W(12, "%xmm12") W(13, "%xmm13") W(14, "%xmm14") W(15, "%xmm15")
		#undef W

		default:
			os.setstate(ios::failbit);
	}
}

} // namespace x64
