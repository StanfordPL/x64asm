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

const vector<XmmReg> XmmReg::domain_ {{
	xmm0,
	xmm1,
	xmm2,
	xmm3,
	xmm4,
	xmm5,
	xmm6,
	xmm7,
	xmm8,
	xmm9,
	xmm10,
	xmm11,
	xmm12,
	xmm13,
	xmm14,
	xmm15
}};

} // namespace x64
