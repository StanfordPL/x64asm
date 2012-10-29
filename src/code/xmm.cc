#include "src/code/xmm.h"

using namespace std;

namespace x64 {

const vector<Xmm> Xmm::domain_ {{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
}};

const Xmm0 xmm0 = 0;
const Xmm xmm1 = 1;
const Xmm xmm2 = 2;
const Xmm xmm3 = 3;
const Xmm xmm4 = 4;
const Xmm xmm5 = 5;
const Xmm xmm6 = 6;
const Xmm xmm7 = 7;
const Xmm xmm8 = 8;
const Xmm xmm9 = 9;
const Xmm xmm10 = 10;
const Xmm xmm11 = 11;
const Xmm xmm12 = 12;
const Xmm xmm13 = 13;
const Xmm xmm14 = 14;
const Xmm xmm15 = 15;
const Xmm xmm_null = 16;

} // namespace x64
